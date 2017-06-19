-module(nix_bootstrap).

%%% ---------------------------------------------------------------------------
%%% @doc
%%% The purpose of this command is to prepare a rebar3 project so that
%%% rebar3 understands that the dependencies are all already
%%% installed. If you want a hygienic build on nix then you must run
%%% this command before running rebar3. I suggest that you add a
%%% `Makefile` to your project and have the bootstrap command be a
%%% dependency of the build commands. See the nix documentation for
%%% more information.
%%%
%%% This command designed to have as few dependencies as possible so
%%% that it can be a dependency of root level packages like rebar3. To
%%% that end it does many things in a fairly simplistic way. That is
%%% by design.
%%%
%%% ### Assumptions
%%%
%%% This command makes the following assumptions:
%%%
%%% * It is run in a nix-shell or nix-build environment
%%% * that all dependencies have been added to the ERL_LIBS
%%%   Environment Variable

-export([perform/1]).

-record(data, {version
              , registry_only = false
              , debug_info = false
              , compile_ports
              , erl_libs
              , plugins
              , root
              , name
              , registry_snapshot}).

-define(HEX_REGISTRY_PATH, ".cache/rebar3/hex/default/registry").

perform(Opts) ->
    io:format("nix_bootstrap opts: ~p~n", [Opts]),
    {ok, Data1} = parse_args(Opts),
    io:format("nix_bootstrap data1: ~p~n", [Data1]),
    {ok, RequiredData} = gather_required_data_from_the_environment(Data1),
    io:format("nix_bootstrap data: ~p~n", [RequiredData]),
    do_the_bootstrap(RequiredData).

%% @doc There are two modes 'registry_only' where the register is
%% created from hex and everything else.
-spec do_the_bootstrap(#data{}) -> ok.
do_the_bootstrap(RequiredData = #data{registry_only = true}) ->
    ok = bootstrap_registry(RequiredData);
do_the_bootstrap(RequiredData) ->
    ok = bootstrap_registry(RequiredData),
    ok = bootstrap_configs(RequiredData),
    ok = bootstrap_plugins(RequiredData),
    ok = bootstrap_libs(RequiredData).

%% @doc
%% Argument parsing is super simple only because we want to keep the
%% dependencies minimal. For now there can be two entries on the
%% command line, "registry-only" and "debug-info"
-spec parse_args([proplists:property()]) -> {ok, #data{}}.
parse_args(Opts) ->
    RegistryOnly = proplists:get_value(registry_only, Opts),
    DebugInfo = proplists:get_value(debug_info, Opts),
    {ok, #data{registry_only = RegistryOnly,
               debug_info = DebugInfo}}.

-spec bootstrap_configs(#data{}) -> ok.
bootstrap_configs(RequiredData)->
    io:format("Boostrapping app and rebar configurations~n"),
    ok = if_single_app_project_update_app_src_version(RequiredData),
    ok = if_compile_ports_add_pc_plugin(RequiredData),
    ok = if_debug_info_add(RequiredData).

-spec bootstrap_plugins(#data{}) -> ok.
bootstrap_plugins(#data{plugins = Plugins}) ->
    io:format("Bootstrapping rebar3 plugins~n"),
    Target = "_build/default/plugins/",
    Paths = string:tokens(Plugins, " "),
    CopiableFiles =
        lists:foldl(fun(Path, Acc) ->
                            gather_dependency(Path) ++ Acc
                    end, [], Paths),
    lists:foreach(fun (Path) ->
                          ok = link_app(Path, Target)
                  end, CopiableFiles).

-spec bootstrap_libs(#data{}) -> ok.
bootstrap_libs(#data{erl_libs = ErlLibs}) ->
    io:format("Bootstrapping dependent libraries~n"),
    Target = "_build/default/lib/",
    Paths = string:tokens(ErlLibs, ":"),
    CopiableFiles =
        lists:foldl(fun(Path, Acc) ->
                            gather_directory_contents(Path) ++ Acc
                    end, [], Paths),
    lists:foreach(fun (Path) ->
                          ok = link_app(Path, Target)
                  end, CopiableFiles).

-spec gather_dependency(string()) -> [{string(), string()}].
gather_dependency(Path) ->
    FullLibrary = filename:join(Path, "lib/erlang/lib/"),
    case filelib:is_dir(FullLibrary) of
        true ->
            gather_directory_contents(FullLibrary);
        false ->
            [raw_hex(Path)]
    end.

-spec raw_hex(string()) -> {string(), string()}.
raw_hex(Path) ->
    [_, Name] = re:split(Path, "-hex-source-"),
    {Path, erlang:binary_to_list(Name)}.

-spec gather_directory_contents(string()) -> [{string(), string()}].
gather_directory_contents(Path) ->
    {ok, Names} = file:list_dir(Path),
    lists:map(fun(AppName) ->
                 {filename:join(Path, AppName), fixup_app_name(AppName)}
              end, Names).

%% @doc
%% Makes a symlink from the directory pointed at by Path to a
%% directory of the same name in Target. So if we had a Path of
%% {`foo/bar/baz/bash`, `baz`} and a Target of `faz/foo/foos`, the symlink
%% would be `faz/foo/foos/baz`.
-spec link_app({string(), string()}, string()) -> ok.
link_app({Path, TargetFile}, TargetDir) ->
    Target = filename:join(TargetDir, TargetFile),
    make_symlink(Path, Target).

-spec make_symlink(string(), string()) -> ok.
make_symlink(Path, TargetFile) ->
    file:delete(TargetFile),
    ok = filelib:ensure_dir(TargetFile),
    io:format("Making symlink from ~s to ~s~n", [Path, TargetFile]),
    ok = file:make_symlink(Path, TargetFile).

%% @doc
%% This takes an app name in the standard OTP <name>-<version> format
%% and returns just the app name. Why? Because rebar doesn't
%% respect OTP conventions in some cases.
-spec fixup_app_name(string()) -> string().
fixup_app_name(FileName) ->
    case string:tokens(FileName, "-") of
        [Name] -> Name;
        [Name, _Version] -> Name;
        [Name, _Version, _Tag] -> Name
    end.

-spec bootstrap_registry(#data{}) -> ok.
bootstrap_registry(#data{registry_snapshot = RegistrySnapshot}) ->
    io:format("Bootstrapping Hex Registry for Rebar~n"),
    make_sure_registry_snapshot_exists(RegistrySnapshot),
    filelib:ensure_dir(?HEX_REGISTRY_PATH),
    ok = case filelib:is_file(?HEX_REGISTRY_PATH) of
             true ->
                 file:delete(?HEX_REGISTRY_PATH);
             false ->
                 ok
         end,
    ok = file:make_symlink(RegistrySnapshot,
                           ?HEX_REGISTRY_PATH).

-spec make_sure_registry_snapshot_exists(string()) -> ok.
make_sure_registry_snapshot_exists(RegistrySnapshot) ->
    case filelib:is_file(RegistrySnapshot) of
        true ->
            ok;
        false ->
            stderr("Registry snapshot (~s) does not exist!", [RegistrySnapshot]),
            erlang:halt(1)
    end.

-spec gather_required_data_from_the_environment(#data{}) -> {ok, #data{}}.
gather_required_data_from_the_environment(ArgData) ->
    {ok, ArgData#data{ version = guard_env("version")
                     , erl_libs = get_env("ERL_LIBS", [])
                     , plugins = get_env("buildPlugins", [])
                     , root = code:root_dir()
                     , name = guard_env("name")
                     , compile_ports = nix2bool(get_env("compilePorts", ""))
                     , registry_snapshot = guard_env("HEX_REGISTRY_SNAPSHOT")}}.

-spec nix2bool(any()) -> boolean().
nix2bool("1") ->
    true;
nix2bool("") ->
    false.

get_env(Name) ->
    os:getenv(Name).
get_env(Name, Def) ->
    case get_env(Name) of
        false -> Def;
        Val ->   Val
    end.

-spec guard_env(string()) -> string().
guard_env(Name) ->
    case get_env(Name) of
        false ->
            stderr("Expected Environment variable ~s! Are you sure you are "
                   "running in a Nix environment? Either a nix-build, "
                   "nix-shell, etc?~n", [Name]),
            erlang:halt(1);
        Variable ->
            Variable
    end.

%% @doc
%% If debug info is set we need to add debug info to the list of compile options
%%
-spec if_debug_info_add(#data{}) -> ok.
if_debug_info_add(#data{debug_info = true}) ->
    ConfigTerms = add_debug_info(read_rebar_config()),
    Text = lists:map(fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
                     ConfigTerms),
    file:write_file("rebar.config", Text);
if_debug_info_add(_) ->
    ok.

-spec add_debug_info([term()]) -> [term()].
add_debug_info(Config) ->
    ExistingOpts = case lists:keysearch(erl_opts, 1, Config) of
                       {value, {erl_opts, ExistingOptsList}} -> ExistingOptsList;
                       _ -> []
                   end,
    case lists:member(debug_info, ExistingOpts) of
        true ->
            Config;
        false ->
            lists:keystore(erl_opts, 1, Config,
                           {erl_opts, [debug_info | ExistingOpts]})
    end.


%% @doc
%% If the compile ports flag is set, rewrite the rebar config to
%% include the 'pc' plugin.
-spec if_compile_ports_add_pc_plugin(#data{}) -> ok.
if_compile_ports_add_pc_plugin(#data{compile_ports = true}) ->
    ConfigTerms = add_pc_to_plugins(read_rebar_config()),
    Text = lists:map(fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
                     ConfigTerms),
    file:write_file("rebar.config", Text);
if_compile_ports_add_pc_plugin(_) ->
    ok.

-spec add_pc_to_plugins([term()]) -> [term()].
add_pc_to_plugins(Config) ->
    PluginList = case lists:keysearch(plugins, 1, Config) of
                     {value, {plugins, ExistingPluginList}} -> ExistingPluginList;
                     _ -> []
                 end,
    lists:keystore(plugins, 1, Config, {plugins, [pc | PluginList]}).

-spec read_rebar_config() -> [term()].
read_rebar_config() ->
    case file:consult("rebar.config") of
        {ok, Terms} ->
            Terms;
        _ ->
            stderr("Unable to read rebar config!", []),
            erlang:halt(1)
    end.


-spec if_single_app_project_update_app_src_version(#data{}) -> ok.
if_single_app_project_update_app_src_version(#data{name = Name,
                                                   version = Version}) ->
    SrcFile = filename:join("src",
                            lists:concat([Name, ".app.src"])),

    case filelib:is_file(SrcFile) of
        true ->
            update_app_src_with_version(SrcFile, Version);
        false ->
            ok
    end.

-spec update_app_src_with_version(string(), string()) -> ok.
update_app_src_with_version(SrcFile, Version) ->
    {ok, [{application, Name, Details}]} = file:consult(SrcFile),
    NewDetails = lists:keyreplace(vsn, 1, Details, {vsn, Version}),
    ok = file:write_file(SrcFile, io_lib:fwrite("~p.\n", [{application, Name, NewDetails}])).

%% @doc
%% Write the result of the format string out to stderr.
-spec stderr(string(), [term()]) -> ok.
stderr(FormatStr, Args) ->
    io:put_chars(standard_error, io_lib:format(FormatStr, Args)).

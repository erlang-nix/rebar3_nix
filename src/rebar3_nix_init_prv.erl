-module(rebar3_nix_init_prv).

-export([init/1, do/1, format_error/1]).

-include_lib("hex2nix/include/hex2nix.hrl").

-define(SHELL_NIX, "shell.nix").

%% We almost never do this, however, with `prettypr` it actually makes
%% some modicum of sense.
-import(prettypr, [above/2, beside/2, sep/1, par/1, break/1,
                   par/2, empty/0]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create(
                 [
                  {namespace, nix},             %
                  {name, init},            % The 'user friendly' name of the task
                  {module, ?MODULE},            % The module implementation of the task
                  {bare, true},                 % The task can be run by the user, always true
                  {deps, [{default, app_discovery}]},                % The list of dependencies
                  {example, "rebar3 nix init"},      % How to use the plugin
                  {opts, opts()},
                  {short_desc, "Nix integration for rebar3"},
                  {desc, "Nix integration for rebar3"}
                 ]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
    [ {shell_file, $s, "shell_file", {boolean, true}, "create a shell.nix file"}
    , {default_file, $d, "default_file", {boolean, true}, "create a default.nix file"}
    ].

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:format("Doing Nix init...~n"),
    {Opts, _} = rebar_state:command_parsed_args(State),
    DepDesc = rebar_state_to_dep_desc(State),
    case proplists:get_value(Opts, shell_file) of
        true ->
            case ec_file:exists(?SHELL_NIX) of
                true ->
                    rebar_utils:abort("File 'shell.nix' already exists, aborting.", []);
                false ->
                    ec_file:write(?SHELL_NIX,
                                  h2n_generate:prettyprint(
                                    h2n_generate:gen_default(DepDesc)))
            end;
        false ->
            ok
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec rebar_state_to_dep_desc(rebar_state:t()) -> {'ok', h2n_fetcher:dep_desc()} | 'error'.
rebar_state_to_dep_desc(_State) ->

    {Description, Licenses, Link} =
        h2n_fetcher:get_metadata(AppName, h2n_fetcher:get_app_detail_from_hex_pm(AppName)),
    case get_deep_meta_for_package(AppName, AppVsn, AllApps) of
        {Sha, HasNativeCode, BuildPlugins, BuildTool} ->
            {ok, #dep_desc{ app = App
                          , description = Description
                          , position = IsRoot
                          , licenses = Licenses
                          , homepage = Link
                          , sha = Sha
                          , build_plugins = BuildPlugins
                          , has_native_code = HasNativeCode
                          , build_tool = BuildTool
                          , deps = Deps}};
        no_metadata_available ->
            error
    end.

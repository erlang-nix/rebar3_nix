-module(rebar3_nix_lock_prv).

-export([init/1, do/1, format_error/1]).

-define(NIX_DEPS, "# Generated by rebar3_nix
let fetchOnly = { src, ... }: src;
in { builder ? fetchOnly, fetchHex, fetchgit, fetchFromGitHub, overrides ? (x: y: { }) }:
let
  self = packages // (overrides self packages);
  packages = with self; {~s
  };
in self
").

-define(DEP, "
    ~s = builder {
      name = \"~s\";
      version = \"~s\";
      src = ~s;
      beamDeps = [ ~s];
    };").

-define(FETCH_HEX,
"fetchHex {
        pkg = \"~s\";
        version = \"~s\";
        sha256 = \"~s\";
      }").

-define(FETCH_GIT,
"fetchgit {
        url = \"~s\";
        rev = \"~s\";
        sha256 = \"~s\";
      }").

-define(FETCH_FROM_GITHUB,
"fetchFromGitHub {
        owner = \"~s\";
        repo = \"~s\";
        rev = \"~s\";
        sha256 = \"~s\";
      }").

-define(DEFAULT_OUT, "rebar-deps.nix").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
                               {namespace, nix},
                               {name, lock},
                               {module, ?MODULE},
                               {bare, true},
                               {deps, [{default, lock}]},
                               {example, "rebar3 nix lock -o rebar-deps.nix"},
                               {opts, [{out, $o, "out", {string, ?DEFAULT_OUT}, "Output file."}]},
                               {short_desc, "Export rebar3 dependencies for nix"},
                               {desc, "Export rebar3 dependencies for nix"},
                               {profiles, [default, test, prod]}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  JustApps = rebar_state:all_deps(State),
  JustPlugins = rebar_state:all_plugin_deps(State),
  Apps = deduplicate(JustApps ++ JustPlugins), % pick newer one
  AllDepsNames = [to_binary(rebar_app_info:name(A)) || A <- Apps],
  Deps = [to_nix(A, AllDepsNames) || A <- Apps],
  Drv = io_lib:format(?NIX_DEPS, [Deps]),
  ok = file:write_file(out_path(State), Drv),
  {ok, State}.

make_comparator(F) ->
    fun(A, B) ->
            F(A) > F(B)
    end.

deduplicate(Apps) ->
    NewerFirstF = make_comparator(fun rebar_app_info:vsn/1),
    CheckVF =
        fun(App, Acc) ->
                Name = rebar_app_info:name(App),
                L = maps:get(Name, Acc, []),
                [Newer | _] = lists:sort(NewerFirstF, [App | L]),
                maps:put(Name, [Newer], Acc)
        end,
    lists:flatten(maps:values(lists:foldl(CheckVF, maps:new(), Apps))).

out_path(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  proplists:get_value(out, Args, ?DEFAULT_OUT).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

to_nix(AppInfo, AllDepsNames) ->
  Name = rebar_app_info:name(AppInfo),
  {Vsn, Src} = src(Name, rebar_app_info:source(AppInfo)),
  Deps = [[BinName, " "] || BinName <- app_deps(AppInfo),
                            lists:member(BinName, AllDepsNames)],
  io_lib:format(?DEP, [Name, Name, Vsn, Src, Deps]).

src(_, {pkg, PkgName, Vsn, _OldHash, Hash, _Repo}) ->
  {Vsn, io_lib:format(?FETCH_HEX, [PkgName, Vsn, to_sri(Hash)])};
src(_, {git, Url, {ref, Ref}}) ->
  case string:prefix(string:lowercase(Url), "https://github.com/") of
    nomatch ->
      Prefetch = ["nix-prefetch-git --quiet ", Url, " ", Ref],
      {ok, Json} = rebar3_nix_utils:cmd(Prefetch),
      {ok, #{<<"sha256">> := Hash}, _} =
        rebar3_nix_jsone_decode_vendored:decode(unicode:characters_to_binary(Json)),
      {"git", io_lib:format(?FETCH_GIT, [Url, Ref, Hash])};
    Path ->
      [Owner, Repo0] = string:split(Path, "/", trailing),
      Repo = re:replace(Repo0, "\\.git$", "", [{return, list}]),
      Prefetch = ["nix-prefetch-url --unpack https://github.com/",
                  Owner, "/", Repo, "/tarball/", Ref],
      {ok, Hash} = rebar3_nix_utils:cmd(Prefetch),
      {"git", io_lib:format(?FETCH_FROM_GITHUB, [Owner, Repo, Ref, Hash])}
    end;
src(Name, Other) ->
  rebar_api:abort("rebar3_nix: unsupported dependency type ~p for ~s~n", [Other, Name]),
  undefined.

to_sri(Sha256) when is_list(Sha256) ->
  to_sri(list_to_binary(Sha256));
to_sri(<<Sha256Base16:64/binary>>) ->
  Sha256 = binary_to_integer(Sha256Base16, 16),
  ["sha256-", base64:encode(<<Sha256:32/big-unsigned-integer-unit:8>>)].

app_deps(AppInfo) ->
  Names = proplists:get_value(applications, rebar_app_info:app_details(AppInfo), []) ++
    rebar_state:deps_names(rebar_app_info:get(AppInfo, {deps, default}, []) ++
                             rebar_app_info:get(AppInfo, {deps, prod}, [])),
  lists:usort([to_binary(N) || N <- Names]).

to_binary(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom, utf8);
to_binary(List) when is_list(List) ->
  unicode:characters_to_binary(List);
to_binary(Bin) ->
  Bin.

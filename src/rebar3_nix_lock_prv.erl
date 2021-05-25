-module(rebar3_nix_lock_prv).

-export([init/1, do/1, format_error/1]).

-define(NIX_DEPS, "# Generated by rebar3_nix
let fetchOnly = { src, ... }: src;
in { builder ? fetchOnly, fetchHex, fetchFromGitHub }: rec {~s
}
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
"fetchGit {
      url = \"~s\";
      rev = \"~s\";
    }").

-define(FETCH_FROM_GITHUB,
"fetchFromGitHub {
      owner = \"~s\";
      repo = \"~s\";
      rev = \"~s\";
      sha256 = \"~s\";
    }").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
                               {namespace, nix},
                               {name, lock},
                               {module, ?MODULE},
                               {bare, true},
                               {deps, [{default, lock}]},
                               {example, "rebar3 nix lock -o rebar-deps.nix"},
                               {opts, [{out, $o, "out", {string, "rebar-deps.nix"}, "Output file."}]},
                               {short_desc, "Export rebar3 dependencies for nix"},
                               {desc, "Export rebar3 dependencies for nix"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Lock = rebar_state:lock(State),
  Deps = [to_nix(A) || A <- Lock],
  Drv = io_lib:format(?NIX_DEPS, [Deps]),
  ok = file:write_file(out_path(State), Drv),
  {ok, State}.

out_path(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  proplists:get_value(out, Args).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

to_nix(AppInfo) ->
  Name = rebar_app_info:name(AppInfo),
  {Vsn, Src} = src(Name, rebar_app_info:source(AppInfo)),
  Deps = [[DepName, " "] || {DepName, _} <- rebar_app_info:deps(AppInfo)],
  io_lib:format(?DEP, [Name, Name, Vsn, Src, Deps]).

src(_, {pkg, PkgName, Vsn, _OldHash, Hash, _Repo}) ->
  {Vsn, io_lib:format(?FETCH_HEX, [PkgName, Vsn, to_sri(Hash)])};
src(_, {git, Url, {ref, Ref}}) ->
  case string:prefix(string:lowercase(Url), "https://github.com/") of
    nomatch ->
      {"git", io_lib:format(?FETCH_GIT, [Url, Ref])};
    Path ->
      [Owner, Repo0] = string:split(Path, "/", trailing),
      Repo = re:replace(Repo0, "\\.git$", "", [{return, list}]),
      Prefetch = ["nix-prefetch-url --unpack https://github.com/",
                  Owner, "/", Repo, "/tarball/", Ref, " 2>/dev/null"],
      Hash = case string:trim(os:cmd(Prefetch)) of
               [] ->
                 rebar_api:abort(
                   "prefetch failed, make sure nix-prefetch-url is on your PATH",
                   []);
               Hash0 ->
                 Hash0
             end,
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

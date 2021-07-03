-module(rebar3_nix_init_prv).

-export([init/1, do/1, format_error/1]).

-define(LIB_NIX,
"{ pkgs ? import <nixpkgs> { } }:
with pkgs.beamPackages;
buildRebar3 {
  name = \"~s\";
  version = \"0.1.0\";
  src = ./.;
  profile = \"prod\";
  beamDeps = builtins.attrValues
    (import ./rebar-deps.nix {
      inherit (pkgs) fetchHex fetchgit fetchFromGitHub;
      builder = buildRebar3;
  });
}
").

-define(RELEASE_NIX,
"{ pkgs ? import <nixpkgs> { } }:
with pkgs.beamPackages;
rebar3Relx {
  name = \"~s\";
  version = \"0.1.0\";
  src = ./.;
  releaseType = \"~s\";
  profile = \"prod\";
  beamDeps = builtins.attrValues
    (import ./rebar-deps.nix {
      inherit (pkgs) fetchHex fetchgit fetchFromGitHub;
      builder = buildRebar3;
  });
}
").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
                               {namespace, nix},
                               {name, init},
                               {module, ?MODULE},
                               {bare, true},
                               {deps, [lock]},
                               {example, "rebar3 nix init release"},
                               {opts, []},
                               {short_desc, "Initialize rebar3 project for use with nix"},
                               {desc, "Initialize rebar3 project for use with nix"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  case Args of
    [{task, Type}]
      when Type =:= "escript"; Type =:= "release"; Type =:= "lib"; Type =:= "app" ->
      case discover(State) of
        {ok, umbrella, Name} when Type =:= "escript"; Type =:= "release" ->
          umbrella(Type, State, Name),
          {ok, State};
        {ok, app, Name} when Type =:= "lib"; Type =:= "app" ->
          lib(Name),
          {ok, State};
        {ok, app, Name} ->
          release(Type, Name),
          {ok, State};
        {error, Err} ->
          {error, Err}
      end;
    _ ->
      io:format("Usage:~nrebar3 nix init <app|release|escript|lib>~n"),
      {ok, State}
  end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

discover(State) ->
  %% based on rebar_app_dicover
  case rebar_state:project_apps(State) of
    [] ->
      {error};
    [AppInfo] ->
      {ok, app, rebar_app_info:name(AppInfo)};
    [_|_] ->
      {ok, umbrella, filename:basename(filename:dirname(rebar_dir:root_dir(State)))}
  end.

lib(Name) ->
  file:write_file("default.nix", io_lib:format(?LIB_NIX, [Name])).

umbrella(Type, _State, Name) ->
  %% TODO: individual derivations per app
  release(Type, Name).

release(Type, Name) ->
  file:write_file("default.nix", io_lib:format(?RELEASE_NIX, [Name, Type])).

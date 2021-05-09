-module(rebar3_nix_bootstrap_prv).

-export([init/1, do/1, format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create(
                 [
                  {namespace, nix},             %
                  {name, bootstrap},            % The 'user friendly' name of the task
                  {module, ?MODULE},            % The module implementation of the task
                  {bare, true},                 % The task can be run by the user, always true
                  {deps, [{default, app_discovery}]},                % The list of dependencies
                  {example, "rebar3 nix bootstrap"},      % How to use the plugin
                  {opts, opts()},
                  {short_desc, "Nix integration for rebar3"},
                  {desc, "Nix integration for rebar3"}
                 ]),
    {ok, rebar_state:add_provider(State, Provider)}.

opts() ->
    [ {debug_info, $d, "debug_info", {boolean, false}, "enable debug info"}
    ].

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:format("Doing Nix bootstrap...~n"),
    {Opts, _} = rebar_state:command_parsed_args(State),
    nix_bootstrap:perform(Opts),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

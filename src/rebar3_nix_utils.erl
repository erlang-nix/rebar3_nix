-module(rebar3_nix_utils).

-export([cmd/1]).

cmd(Command0) when is_binary(Command0) ->
  Command = unicode:characters_to_list(Command0),
  case rebar_utils:sh([Command, " 2>/dev/null"], [return_on_error]) of
    {ok, Output} ->
      {ok, string:trim(Output)};
    {error, _} ->
      %% Do it again capturing stderr so the user can see the full output
      {error, _} = rebar_utils:sh(Command, [abort_on_error])
  end;
cmd(Command) ->
  cmd(iolist_to_binary(Command)).

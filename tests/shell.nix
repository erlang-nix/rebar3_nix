{ pkgs ? import <nixpkgs> { } }:
let
  rebar3-nix = pkgs.beamPackages.rebar3-nix.overrideAttrs (old: {
    src = pkgs.lib.sourceByRegex ./.. [ "^rebar.(config|lock)$" "^src.*$" ];
  });
in pkgs.mkShell {
  buildInputs = with pkgs; [
    bashInteractive
    (rebar3WithPlugins ({ globalPlugins = [ rebar3-nix ]; }))
    erlang
  ];
}

rebar3_nix
=====

Nix integration for rebar3

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_nix, ".*", {git, "https://github.com/erlang-nix/rebar3_nix.git", {tag, "v0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 nix
    ===> Fetching rebar3_nix
    ===> Compiling rebar3_nix
    <Plugin Output>

Building erlang application
---------------------------

Here's an example

```
$ ls
apps config rebar.config rebar.lock

$ rebar3 nix lock -o deps.nix

$ cat > default.nix <<EOM
{ pkgs }:
let
  deps = import ./deps.nix { inherit (pkgs) fetchHex fetchFromGitHub fetchgit; };
in
pkgs.stdenv.mkDerivation rec {
  name = "app-release";
  version = "1.1.0";
  nativeBuildInputs = [ pkgs.autoPatchelfHook ];
  buildInputs = with pkgs; [
    rebar3 openssl gnutar
  ];
  buildPhase = ''
    mkdir -p _checkouts
    ${toString (pkgs.lib.mapAttrsToList (k: v: ''
      cp -R --no-preserve=mode ${v} _checkouts/${k}
    '') deps)}
    HOME=. rebar3 tar
  '';
  installPhase = ''
    mkdir -p $out
    tar -xzf _build/prod/rel/*/*.tar.gz -C $out/
  '';

  src = pkgs.lib.cleanSource ./.;
}
EOM
```

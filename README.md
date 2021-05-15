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

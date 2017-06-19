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
        { rebar3_nix, ".*", {git, "git@host:user/rebar3_nix.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_nix
    ===> Fetching rebar3_nix
    ===> Compiling rebar3_nix
    <Plugin Output>

NIX_PATH := nixpkgs=https://github.com/NixOS/nixpkgs/archive/56d11e1467123fb1cd6212418aa2703520415524.tar.gz

all: clean
	nix-shell --run 'make test'

PROJECTS := test_app test_lib test_release test_escript test_profiles
DEPS := {cowboy, "2.8.0"}
APP_DEPS := cowboy

clean:
	for proj in $(PROJECTS); do rm -rf $$proj; done

test: $(PROJECTS)

test_profiles: test_lib
	cp -R test_lib test_profiles
	printf "\n{profiles, [{test, [{deps, [meck]}]}]}.\n" >> test_profiles/rebar.config
	cd test_profiles && rebar3 as test nix lock
	nix-build test_profiles
	nix-shell ./test_profiles --run \
		"erl -eval '{ok, _} = application:ensure_all_started(meck), init:stop(0).'"

test_%:
	rebar3 new $* test_$*
	sed -i 's/{deps, \[\]}\./{deps, [$(DEPS)]}./' test_$*/rebar.config
	find -wholename '*/test_$*/*.app.src' -exec \
		sed -i 's/stdlib/stdlib,$(APP_DEPS)/' {} \;
	cd test_$* && rebar3 nix init $* && rm -rf _build
	nix-build test_$* --no-out-link
	$(MAKE) test_result_$*

test_result_release:
	echo "init:stop(0)." | $$(nix-build test_release)/rel/test_release/bin/test_release console

test_result_escript:
	$$(nix-build test_escript)/bin/test_escript

test_result_%:
	nix-shell -p "callPackage ./test_$* {}" --run \
		"erl -eval '{ok, _} = application:ensure_all_started(test_$*), init:stop(0).'"

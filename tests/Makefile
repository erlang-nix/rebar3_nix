all: clean
	nix-shell --run 'make test'

PROJECTS := test_app test_lib test_release test_escript
DEPS := {cowboy, "2.8.0"}
APP_DEPS := cowboy

clean:
	for proj in $(PROJECTS); do rm -rf $$proj; done

test: $(PROJECTS)

test_%:
	rebar3 new $* test_$*
	sed -i 's/{deps, \[\]}\./{deps, [$(DEPS)]}./' test_$*/rebar.config
	find -wholename '*/test_$*/*.app.src' -exec \
		sed -i 's/stdlib/stdlib,$(APP_DEPS)/' {} \;
	cd test_$* && rebar3 nix init -t $* && rm -rf _build
	nix-build test_$* --no-out-link
	$(MAKE) test_result_$*

test_result_release:
	echo "init:stop(0)." | $$(nix-build test_release)/rel/test_release/bin/test_release console

test_result_escript:
	$$(nix-build test_escript)/bin/test_escript

test_result_%:
	nix-shell -p "callPackage ./test_$* {}" --run \
		"erl -eval '{ok, _} = application:ensure_all_started(test_$*), init:stop(0).'"

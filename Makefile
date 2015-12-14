# See LICENSE for licensing information.

REBAR ?= $(shell command -v rebar >/dev/null 2>&1 && echo "rebar" || echo "$(CURDIR)/rebar")

DEPS_PLT := $(CURDIR)/.deps_plt

ERLANG_DIALYZER_APPS := erts \
					    kernel \
					    stdlib

DIALYZER := dialyzer

# Travis CI is slow at building dialyzer PLT
ifeq ($(TRAVIS), true)
	OTP_VSN := $(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /R(\d+).*/g')
	SLOW_DIALYZER := $(shell expr $(OTP_VSN) \<= 14 )

	ifeq ($(SLOW_DIALYZER), 1)
		DIALYZER := : not running dialyzer on TRAVIS with R14
	endif
endif

SRCS := $(wildcard src/* include/* rebar.config)

.PHONY: all
all: deps ebin/jesse.app bin/jesse

# Clean

.PHONY: clean
clean:
	$(REBAR) clean
	$(RM) -r .rebar
	$(RM) -r bin
	$(RM) doc/*.html
	$(RM) doc/edoc-info
	$(RM) doc/erlang.png
	$(RM) doc/stylesheet.css
	$(RM) -r ebin
	$(RM) -r logs

.PHONY: distclean
distclean:
	$(RM) $(DEPS_PLT)
	$(RM) -r deps
	$(MAKE) clean

# Deps

.PHONY: get-deps
get-deps:
	$(REBAR) get-deps

.PHONY: update-deps
update-deps:
	$(REBAR) update-deps

.PHONY: delete-deps
delete-deps:
	$(REBAR) delete-deps

.PHONY: deps
deps: get-deps

# Docs

.PHONY: docs
docs:
	$(REBAR) doc skip_deps=true

# Compile

ebin/jesse.app: compile

bin/jesse: ebin/jesse.app
	$(REBAR) escriptize
	bin/jesse --help

.PHONY: compile
compile: $(SRCS)
	$(REBAR) compile

# Tests.

.rebar/DEV_MODE:
	mkdir -p .rebar
	touch .rebar/DEV_MODE

.PHONY: submodules
submodules:
	git submodule update --init --recursive

.PHONY: test
test: .rebar/DEV_MODE deps submodules eunit ct dialyzer

.PHONY: eunit
eunit:
	$(REBAR) eunit skip_deps=true

.PHONY: ct
ct:
	$(REBAR) ct skip_deps=true suites="jesse_tests_draft3,jesse_tests_draft4"

$(DEPS_PLT):
	$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) -r deps --output_plt $(DEPS_PLT)

.PHONY: dialyzer
dialyzer: $(DEPS_PLT)
	$(DIALYZER) --plt $(DEPS_PLT) --src src
#	@$(DIALYZER) --plt .$(PROJECT).plt --src src --no_native -Werror_handling -Wrace_conditions #-Wunmatched_returns -Wunderspecs

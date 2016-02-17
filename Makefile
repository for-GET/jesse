# See LICENSE for licensing information.

REBAR ?= $(shell command -v rebar3 >/dev/null 2>&1 && echo "rebar3" || echo "$(CURDIR)/rebar3")

DEPS_PLT := $(CURDIR)/.deps_plt

ERLANG_DIALYZER_APPS := erts \
					    kernel \
						stdlib \
						inets

DIALYZER := dialyzer

# Travis CI is slow at building dialyzer PLT
ifeq ($(TRAVIS), true)
	OTP_VSN := $(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /^(?:"R)?(\d+).*/g')
	NO_DIALYZER := $(shell expr $(OTP_VSN) \<= 16 )

	ifeq ($(NO_DIALYZER), 1)
		DIALYZER := : not running dialyzer on TRAVIS with R16 and below
	endif
endif

SRCS := $(wildcard src/* include/* rebar.config)

.PHONY: all
all: _build/default/lib/jesse/ebin/jesse.app _build/default/bin/jesse

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
	$(RM) -r _build
	$(MAKE) clean

# Deps


# Docs

.PHONY: docs
docs:
	$(REBAR) doc

# Compile

_build/default/bin/jesse: compile

_build/default/bin/jesse: _build/default/lib/jesse/ebin/jesse.app
	$(REBAR) escriptize
	_build/default/bin/jesse --help

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
test: .rebar/DEV_MODE submodules eunit ct dialyzer

.PHONY: eunit
eunit:
	$(REBAR) eunit

.PHONY: ct
ct:
	$(REBAR) ct --suite "test/jesse_tests_draft3_SUITE,test/jesse_tests_draft4_SUITE"

.PHONY: dialyzer
dialyzer: _build/default/lib/jesse/ebin/jesse.app
	$(REBAR) dialyzer

.PHONY: elvis
elvis:
	$(REBAR) as lint lint

# See LICENSE for licensing information.

REBAR3 ?= $(shell command -v rebar >/dev/null 2>&1 && echo "rebar3" || echo "$(CURDIR)/rebar3")

ELVIS ?= $(shell command -v elvis >/dev/null 2>&1 && echo "elvis" || echo "$(CURDIR)/elvis")

ERLANG_DIALYZER_APPS := erts \
						kernel \
						stdlib \
						inets

DIALYZER := dialyzer

# Travis CI is slow at building dialyzer PLT
TRAVIS ?=
ifeq (true,$(TRAVIS))
	OTP_VSN := $(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /^(?:"R)?(\d+).*/g')
	NO_DIALYZER := $(shell expr $(OTP_VSN) \<= 16 )

	ifeq ($(NO_DIALYZER), 1)
		DIALYZER := : not running dialyzer on TRAVIS with R16 and below
	endif
endif

SRCS := $(wildcard src/* include/* rebar.config)

SRC_BEAMS := $(patsubst src/%.erl, _build/default/lib/jesse/ebin/%.beam, $(wildcard src/*.erl))

.PHONY: all
all: maybe_dev deps _build/default/lib/jesse/ebin/jesse.app bin/jesse

.PHONY: maybe_dev
maybe_dev:
ifdef CI
	$(MAKE) --no-print-directory _build/DEV_MODE
else
	@:
endif

# Clean

.PHONY: clean
clean:
	$(REBAR3) clean
	$(RM) -r .rebar
	$(RM) -r bin
	$(RM) doc/*.html
	$(RM) doc/edoc-info
	$(RM) doc/erlang.png
	$(RM) doc/stylesheet.css

.PHONY: distclean
distclean: clean
	$(RM) -r _build

# Deps

.PHONY: get-deps
get-deps:
	$(REBAR3) get-deps
	[ -f _build/DEV_MODE ] && git submodule update --init --recursive || true

.PHONY: deps
deps: get-deps

# Docs

.PHONY: docs
docs:
	$(REBAR3) doc

# Compile

_build/default/lib/jesse/ebin/jesse.app: compile

_build/default/bin/jesse: _build/default/lib/jesse/ebin/jesse.app $(SRC_BEAMS)
	$(REBAR3) escriptize

bin/jesse: _build/default/bin/jesse
	mkdir -p bin
	cp -a _build/default/bin/jesse bin/jesse
	bin/jesse --help

.PHONY: compile
compile: $(SRCS)
	$(REBAR3) compile

# Tests.

.PHONY: test
test: _build/DEV_MODE deps eunit ct xref dialyzer

_build/DEV_MODE:
	mkdir -p _build
	touch _build/DEV_MODE

.PHONY: eunit
eunit:
	$(REBAR3) eunit

.PHONY: ct
ct:
	TEST_DIR=$(CURDIR)/_build/test/lib/jesse/test $(REBAR3) ct

.PHONY: xref
xref:
	$(REBAR3) xref

.PHONY: dialyzer
dialyzer:
	$(REBAR3) dialyzer

.PHONY: elvis
elvis:
	$(ELVIS) rock > test/elvis || true
	grep "FAIL" test/elvis | sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g" > test/elvis_warnings
	diff -U0 test/known_elvis_warnings test/elvis_warnings || cat test/elvis

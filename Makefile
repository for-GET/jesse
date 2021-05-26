# See LICENSE for licensing information.

ifdef CI
REBAR = ./rebar3
else
REBAR ?= $(shell command -v rebar3 >/dev/null 2>&1 && echo "rebar3" || echo "$(CURDIR)/rebar3")
endif

SRCS := $(wildcard src/* include/* rebar.config)

.PHONY: all
all: escript

# Clean

.PHONY: clean
clean:
	$(REBAR) clean

.PHONY: distclean
distclean:
	$(MAKE) clean
	$(RM) -r _build
	$(RM) doc/*.html
	$(RM) doc/edoc-info
	$(RM) doc/erlang.png
	$(RM) doc/stylesheet.css
	$(RM) -r logs

.PHONY: clean-tests
clean-tests:
	@ rm -rf _build/test/lib

# Docs

.PHONY: docs
docs:
	$(REBAR) edoc

# Compile

ebin/jesse.app: compile

.PHONY: escript
escript: ebin/jesse.app
	$(REBAR) escriptize
	./_build/default/bin/jesse --help

.PHONY: compile
compile: $(SRCS)
	$(REBAR) compile

# Tests.
test/JSON-Schema-Test-Suite/tests:
	git submodule sync --recursive
	git submodule update --init --recursive

# Would be nice to include elvis to test, but it fails on OTP-18
.PHONY: test
test: eunit ct xref dialyzer cover

.PHONY: eunit
eunit:
	@ $(MAKE) clean-tests
	$(REBAR) eunit

# @ rm -rf _build

.PHONY: ct
ct: test/JSON-Schema-Test-Suite/tests
	@ $(MAKE) clean-tests
	$(REBAR) ct

.PHONY: xref
xref:
	$(REBAR) xref

.PHONY: dialyzer
dialyzer:
	$(REBAR) dialyzer

.PHONY: elvis
elvis:
	$(REBAR) lint

.PHONY: cover
cover:
	@ $(MAKE) clean-tests
	$(REBAR) cover -v

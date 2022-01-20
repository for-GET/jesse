# See LICENSE for licensing information.

CHMOD := $(shell command -v chmod 2>/dev/null)
CURL := $(shell command -v curl 2>/dev/null)
LN := $(shell command -v ln 2>/dev/null)

OTP_RELEASE = $(shell erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().'  -noshell)

ifdef CI
REBAR3 = ./rebar3.OTP$(OTP_RELEASE)
else
REBAR3 ?= $(shell command -v rebar3 2>/dev/null || echo "./rebar3.OTP$(OTP_RELEASE)")
endif

REBAR_CONFIG = rebar.OTP$(OTP_RELEASE).config

SRCS := $(wildcard src/* include/* rebar.config)

.PHONY: all
all: ebin/jesse.app bin/jesse

# Clean

.PHONY: clean
clean:
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) clean

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
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) edoc

# Compile

bin/jesse: escript
	mkdir -p bin
	cp -a _build/default/bin/jesse bin/jesse

ebin/jesse.app: compile

.PHONY: escript
escript: ebin/jesse.app
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) escriptize
	./_build/default/bin/jesse --help

.PHONY: compile
compile: $(SRCS)
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) compile

# Tests

test/JSON-Schema-Test-Suite/tests:
	git submodule sync --recursive
	git submodule update --init --recursive

.PHONY: test
# Would be nice to include elvis to test, but it fails on OTP-18
# test: elvis
test: eunit ct xref dialyzer proper cover

.PHONY: elvis
elvis:
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) lint

.PHONY: eunit
eunit:
	@ $(MAKE) clean-tests
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) eunit

.PHONY: ct
ct: test/JSON-Schema-Test-Suite/tests
	@ $(MAKE) clean-tests
	TEST_DIR=_build/default/test/lib/jesse/test REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) ct

.PHONY: xref
xref:
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) xref

.PHONY: dialyzer
dialyzer:
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) dialyzer

.PHONY: cover
cover:
	@ $(MAKE) clean-tests
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) cover -v

.PHONY: proper
proper:
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) proper

.PHONY: publish
publish: docs
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) hex publish -r hexpm --yes

.PHONY: rebar3.OTP18
rebar3.OTP18:
	$(CURL) -fqsS -L -o $@ https://github.com/erlang/rebar3/releases/download/3.13.3/rebar3
	$(CHMOD) +x $@

.PHONY: rebar3.OTP19
rebar3.OTP19:
	$(CURL) -fqsS -L -o $@ https://github.com/erlang/rebar3/releases/download/3.15.2/rebar3
	$(CHMOD) +x $@

.PHONY: rebar3.OTP20
rebar3.OTP20:
	$(LN) -sf rebar3.OTP19 $@

.PHONY: rebar3.OTP21
rebar3.OTP21:
	$(LN) -sf rebar3.OTP19 $@

.PHONY: rebar3.OTP22
rebar3.OTP22:
	$(CURL) -fqsS -L -o $@ https://github.com/erlang/rebar3/releases/download/3.16.1/rebar3
	$(CHMOD) +x $@

.PHONY: rebar3.OTP23
rebar3.OTP23:
	$(LN) -sf rebar3.OTP22 $@

.PHONY: rebar3.OTP24
rebar3.OTP24:
	$(LN) -sf rebar3.OTP22 $@

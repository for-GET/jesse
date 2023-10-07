# See LICENSE for licensing information.
REBAR3 = ./rebar3
REBAR_CONFIG = rebar.config

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
test: eunit ct xref dialyzer proper cover

.PHONY: elvis
elvis:
	REBAR_CONFIG=$(REBAR_CONFIG) $(REBAR3) lint

.PHONY: check
check: elvis

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

# TODO: there must be a better way
.PHONY: symlinks
symlinks: test/JSON-Schema-Test-Suite/tests
	cd test/jesse_tests_draft6_SUITE_data && \
	ln -sf ../../test/JSON-Schema-Test-Suite/tests/draft6 standard && \
	ln -sf ../../test/JSON-Schema-Test-Suite/remotes remotes
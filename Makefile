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

GIT_DESCRIBE := $(shell git describe --tags --first-parent --always --dirty)

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
	cd test/jesse_tests_draft3_SUITE_data && \
	ln -sf ../../test/JSON-Schema-Test-Suite/tests/draft3 standard && \
	ln -sf ../../test/JSON-Schema-Test-Suite/remotes remotes

	cd test/jesse_tests_draft4_SUITE_data && \
	ln -sf ../../test/JSON-Schema-Test-Suite/tests/draft4 standard && \
	ln -sf ../../test/JSON-Schema-Test-Suite/remotes remotes

	cd test/jesse_tests_draft6_SUITE_data && \
	ln -sf ../../test/JSON-Schema-Test-Suite/tests/draft6 standard && \
	ln -sf ../../test/JSON-Schema-Test-Suite/remotes remotes

.PHONY: docker
docker:
	if git tag | grep -q -Fx "$(GIT_DESCRIBE)"; then \
		$(MAKE) docker-force; \
	else \
		echo "Current version $(GIT_DESCRIBE) isn't in 'git tag'."; \
		echo "Run 'make docker-force' if you really want to build and push a $(GIT_DESCRIBE) version."; \
		exit 1; \
	fi

.PHONY: docker-force
docker-force:
	# docker context create aws-docker-amd64 --docker host=ssh://ec2-13-51-198-153.eu-north-1.compute.amazonaws.com
	# docker context create aws-docker-arm64 --docker host=ssh://ec2-13-48-46-86.eu-north-1.compute.amazonaws.com
	# docker buildx create --name aws-multiarch-builder aws-docker-amd64
	# docker buildx create --name aws-multiarch-builder --append aws-docker-arm64
	# docker buildx use aws-multiarch-builder
	docker buildx build . \
		--push \
    --platform linux/amd64,linux/arm64 \
		--tag ysoftwareab/jesse:$(GIT_DESCRIBE) \
		--tag ysoftwareab/jesse:latest \
		--build-arg FROM=erlang:slim \
		--build-arg LABEL_VCS_REF=$$(git rev-parse HEAD) \
		--build-arg LABEL_BUILD_DATE=$$(date -u +"%Y-%m-%dT%H:%M:%SZ")

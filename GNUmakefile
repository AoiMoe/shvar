REBAR3 ?= ./rebar3
REBAR3_ARGS ?=
BUILD_SUFFIX ?=

ifdef BUILD_SUFFIX
_REBAR3 = BUILDDIR=_build.$(BUILD_SUFFIX) $(REBAR3)
else
_REBAR3 = $(REBAR3)
endif

TARGETS= compile shell xref dialyzer edoc clean eunit

.DEFAULT: compile
.PHONY: $(TARGETS)

$(TARGETS):
	$(_REBAR3) $@ $(REBAR3_ARGS)

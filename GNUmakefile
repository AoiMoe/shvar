REBAR3 ?= ./rebar3
BUILD_SUFFIX ?=

ifdef BUILD_SUFFIX
_REBAR3 = BUILDDIR=_build.$(BUILD_SUFFIX) $(REBAR3)
else
_REBAR3 = $(REBAR3)
endif

.DEFAULT: compile
.PHONY: compile shell xref dialyzer edoc clean

compile shell xref dialyzer edoc clean:
	$(_REBAR3) $@

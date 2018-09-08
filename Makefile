# See LICENSE for licensing information.

PROJECT = solarized
PROJECT_DESCRIPTION = Output and testing in a solarized terminal
PROJECT_VERSION = 0.1.0

DIALYZER_PLT_OPTS = --no_native
PLT_APPS = eunit

include erlang.mk


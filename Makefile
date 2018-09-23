# See LICENSE for licensing information.

PROJECT = solarized
PROJECT_DESCRIPTION = Output and testing in a solarized terminal
PROJECT_VERSION = 0.1.0

EUNIT_OPTS = no_tty, {report, {solarized_eunit, []}}
COVER = 1

# dialyzer is slower but can build the PLT on memory constrained machines
DIALYZER_PLT_OPTS = --no_native

# we call eunit_listener:start/2 and declare -behaviour(eunit_listener).
PLT_APPS = eunit

include erlang.mk


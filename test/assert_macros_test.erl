%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(assert_macros_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("solarized/include/eunit.hrl").

assert_macros() ->
    [ ?_test(?assert(not true()))
    , ?_test(?assert(not true(), "?assert()"))
    , ?_test(?assertNot(true()))
    , ?_test(?assertNot(true(), "?assertNot()"))
    , ?_test(?assertMatch({_}, true()))
    , ?_test(?assertMatch({_}, true(), "?assertMatch()"))
    , ?_test(?assertNotMatch(true, true()))
    , ?_test(?assertNotMatch(true, true(), "?assertNotMatch()"))
    , ?_test(?assertEqual(false, true()))
    , ?_test(?assertEqual(false, true(), "?assertEqual()"))
    , ?_test(?assertNotEqual(true, true()))
    , ?_test(?assertNotEqual(true, true(), "?assertNotEqual()"))
    ].

assert_macro_test() ->
    Tests = assert_macros(),
    Test = fun () ->
        eunit:test(Tests, [no_tty, {report, {solarized_eunit, []}}])
    end,
    ?outputEqualToFile(solarized, assert_macros, Test).

true() ->
    % cheap source of truth
    is_process_alive(self()).


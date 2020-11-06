%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(assert_macros_test).

-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").

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
    , ?_test(?assertException(error, expect, true()))
    , ?_test(?assertException(error, expect, error(unexpected)))
    , ?_test(?assertException(error, expect, exit(unexpected)))
    , ?_test(?assertException(error, expect, throw(unexpected)))
    , ?_test(?assertError(expect, true()))
    , ?_test(?assertError(expect, error(unexpected)))
    , ?_test(?assertError(expect, exit(unexpected)))
    , ?_test(?assertError(expect, throw(unexpected)))
    , ?_test(?assertExit(expect, true()))
    , ?_test(?assertExit(expect, error(unexpected)))
    , ?_test(?assertExit(expect, exit(unexpected)))
    , ?_test(?assertExit(expect, throw(unexpected)))
    , ?_test(?assertThrow(expect, true()))
    , ?_test(?assertThrow(expect, error(unexpected)))
    , ?_test(?assertThrow(expect, exit(unexpected)))
    , ?_test(?assertThrow(expect, throw(unexpected)))
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


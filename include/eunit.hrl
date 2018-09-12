%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-ifdef(EUNIT).

-define(outputEqual(Expect, Expression),
        ?assertEqual((Expect),
                     solarized_capture:output(fun () -> (Expression) end))).
-define(outputEqual(Expect, Expression, Columns, Rows),
        ?assertEqual((Expect),
                     solarized_capture:output(
		       fun () -> (Expression) end,
		       (Columns),
		       (Rows)))).
-define(_outputEqual(Expect, Expression),
        ?_test(?outputEqual(Expect, Expression))).
-define(_outputEqual(Expect, Expression, Columns, Rows),
        ?_test(?outputEqual(Expect, Expression, Columns, Rows))).

-define(resultEqual(Expect, Expression, Columns, Rows),
        ?assertEqual((Expect),
                     element(1, solarized_capture:result_and_output(
                                  fun () -> (Expression) end,
                                  (Columns),
                                  (Rows))))).
-define(_resultEqual(Expect, Expression, Columns, Rows),
        ?_test(?resultEqual(Expect, Expression, Columns, Rows))).

-endif.


%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-ifdef(TEST).

-define(output(Expression),
        solarized_capture:output(fun () -> (Expression) end)).
-define(outputEqual(Expect, Expression),
        ?assertEqual(Expect, ?output(Expression))).
-define(output(Expression, Columns, Rows),
        solarized_capture:output(fun () -> (Expression) end,
                                 (Columns),
                                 (Rows))).
-define(outputEqual(Expect, Expression, Columns, Rows),
        ?assertEqual(Expect, ?output(Expression, Columns, Rows))).
-define(_outputEqual(Expect, Expression),
        ?_test(?outputEqual(Expect, Expression))).
-define(_outputEqual(Expect, Expression, Columns, Rows),
        ?_test(?outputEqual(Expect, Expression, Columns, Rows))).

-define(outputEqualToFile(App, File, Test),
        solarized_assert:output_equal_to_file(
            (App),
            (File),
            (Test),
            (??Test),
            [{module, ?MODULE}, {line, ?LINE}])).
-define(outputEqualToFile(App, File, Test, Comment),
        solarized_assert:output_equal_to_file(
            (App),
            (File),
            (Test),
            (??Test),
            [{module, ?MODULE}, {line, ?LINE}, {comment, (Comment)}])).
-define(outputEqualToFile(App, File, Test, Columns, Rows),
        solarized_assert:output_equal_to_file(
            (App),
            (File),
            (Test),
            (??Test),
            (Columns),
            (Rows),
            [{module, ?MODULE}, {line, ?LINE}])).
-define(outputEqualToFile(App, File, Test, Columns, Rows, Comment),
        solarized_assert:output_equal_to_file(
            (App),
            (File),
            (Test),
            (??Test),
            (Columns),
            (Rows),
            [{module, ?MODULE}, {line, ?LINE}, {comment, (Comment)}])).

-define(result(Expression, Columns, Rows),
        element(1,
                solarized_capture:result_and_output(fun () -> (Expression) end,
                                                    (Columns),
                                                    (Rows)))).
-define(resultEqual(Expect, Expression, Columns, Rows),
        ?assertEqual(Expect, ?result(Expression, Columns, Rows))).
-define(_resultEqual(Expect, Expression, Columns, Rows),
        ?_test(?resultEqual(Expect, Expression, Columns, Rows))).

-endif.


%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.

%% @doc A listener for EUnit that displays EUnit's reports in solarized.
%%
%% Pass option:
%%   {report, {eunit_solarized, Options}}
%% to eunit:test/2.
%%
%% Currently Options are not defined so use [].
%%
%% Pass option no_tty to disable default eunit reporting.
-module(solarized_eunit).

%% Started by eunit based on option:
%%   {report, {eunit_solarized, Options}}
%% passed to eunit:test/2.
-export([ start/1
        ]).

%% eunit_listener callbacks
-behaviour(eunit_listener).
-export([ init/1
        , handle_begin/3
        , handle_end/3
        , handle_cancel/3
        , terminate/2
        ]).

%% helpers used by solarized_ct
-export([report_exception_error/1]).
-export([report_exception_stack/1]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

%=======================================================================
% eunit_listner callbacks
%=======================================================================

-record(state,
    { tests = #{}
    , failed = []
    , skipped = []
    , cancelled = []
    }).

init(_Options) ->
    solarized:title(cyan, <<"eunit">>),
    #state{}.

%=======================================================================

handle_begin(Type, Data, State0) ->
    {_, State1} = merge_test(Type, Data, State0),
    State1.

%=======================================================================

handle_end(Type, Data, State0) ->
    {Test, State1} = merge_test(Type, Data, State0),
    report_end(Type, Test, State1).

%-----------------------------------------------------------------------

report_end(group, _, State) ->
    State;
report_end(test, #{ status := ok }, State) ->
    solarized:green(<<".">>),
    State;
report_end(test, #{ id := Id, status := {error, _} }, State = #state{}) ->
    solarized:red(<<"F">>),
    State#state{failed = [Id | State#state.failed]};
report_end(test, #{ id := Id, status := {skipped, _} }, State = #state{}) ->
    solarized:blue(<<"*">>),
    State#state{skipped = [Id | State#state.skipped]}.

%=======================================================================

handle_cancel(Type, Data, State0) ->
    {Test, State1} = merge_test(Type, Data, State0),
    report_cancel(Test, State1).

%-----------------------------------------------------------------------

report_cancel(#{ id := Id }, State = #state{}) ->
    solarized:blue("#"),
    State#state{ cancelled = [Id | State#state.cancelled] }.

%=======================================================================

terminate({ok, Data}, State) ->
    Pass = proplists:get_value(pass, Data, 0),
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
    Total = Pass + Fail + Skip + Cancel,
    case Total of
        0 ->
            solarized:yellow(<<"There were no tests to run.\n">>);

        _ ->
            solarized:nl(),
            solarized:nl(),
            report_cancelled(State),
            report_skipped(State),
            report_failed(State),
            solarized:title(cyan, <<"eunit">>),
            report_results(Total, Fail, Skip, Cancel)
    end;
terminate({error, Reason}, _State) ->
    solarized:nl(),
    solarized:nl(),
    solarized:title(red, <<"eunit ERROR">>),
    solarized:red("Error:\n"),
    solarized:term(red, Reason, #{ indent => 2 }),
    error.

%=======================================================================

merge_test(Type, Data, State = #state{tests = Tests}) ->
    Id = proplists:get_value(id, Data),
    Test = case Tests of
        #{ Id := Test0 } ->
            maps:merge(Test0, maps:from_list(Data));

        _ ->
            maps:from_list([{type, Type} | Data])
    end,
    {Test, State#state{ tests = Tests#{ Id => Test } }}.

%=======================================================================

report_results(Total, Fail, Skip, Cancel) when Fail > 0 ->
    report_results(red, error, Total, Fail, Skip, Cancel);
report_results(Total, Fail, Skip, Cancel) when Skip > 0 orelse Cancel > 0 ->
    report_results(yellow, error, Total, Fail, Skip, Cancel);
report_results(Total, Fail, Skip, Cancel) ->
    report_results(green, ok, Total, Fail, Skip, Cancel).

%-----------------------------------------------------------------------

report_results(Style, Result, 1, 0, 0, 0) ->
    solarized:styled({Style, <<"Test passed.\n">>}),
    Result;
report_results(Style, Result, 2, 0, 0, 0) ->
    solarized:styled({Style, <<"2 tests passed.\n">>}),
    Result;
report_results(Style, Result, Total, 0, 0, 0) ->
    Text = io_lib:format("All ~p tests passed.~n", [Total]),
    solarized:styled({Style, Text}),
    Result;
report_results(Style, Result, Total, Fail, Skip, Cancel) ->
    SkipText = report_optional_result(Skip, "skipped"),
    CancelText = report_optional_result(Cancel, "cancelled"),
    Text = io_lib:format("~p tests, ~p failures~ts~ts~n"
        , [Total, Fail, SkipText, CancelText]
        ),
    solarized:styled({Style, Text}),
    Result.

%-----------------------------------------------------------------------

report_optional_result(0, _) ->
    [];
report_optional_result(Count, Text) ->
    io_lib:format(", ~p ~ts", [Count, Text]).

%=======================================================================

report_cancelled(#state{cancelled = []}) ->
    ok;
report_cancelled(#state{cancelled = Cancelled, tests = Tests}) ->
    Fold = fun (Id, Count) -> report_cancelled(Id, Count, Tests) end,
    lists:foldr(Fold, 1, Cancelled),
    ok.

%-----------------------------------------------------------------------

report_cancelled(Id, Count, Tests) ->
    #{ Id := Test } = Tests,
    solarized:title(yellow, io_lib:format("cancelled # ~p", [Count])),
    case Test of
        #{ type := group } ->
            report_group_description(Test);

        #{ type := test } ->
            report_test_function(Test)
    end,
    #{ reason := Reason } = Test,
    report_cancelled_reason(Reason),
    solarized:nl(),
    Count + 1.

%-----------------------------------------------------------------------

report_cancelled_reason(undefined) ->
    solarized:text(<<"cancelled.\n">>);
report_cancelled_reason(timeout) ->
    solarized:text(<<"timed out.\n">>);
report_cancelled_reason({startup, Reason}) ->
    solarized:text(<<"could not start test process:\n">>),
    solarized:term(text, Reason, #{ indent => 2 });
report_cancelled_reason({blame, _SubtaskId}) ->
    solarized:text(<<"cancelled because of subtask.\n">>);
report_cancelled_reason({exit, Reason}) ->
    solarized:text(<<"unexpected termination of test process:\n">>),
    solarized:term(text, Reason, #{ indent => 2 });
report_cancelled_reason({abort, Reason}) ->
    report_cancelled_abort(Reason);
report_cancelled_reason(Reason) ->
    solarized:text(<<"reason:\n">>),
    solarized:term(text, Reason, #{ indent => 2 }).

%-----------------------------------------------------------------------

report_cancelled_abort({bad_test, Term}) ->
    solarized:text(<<"bad test descriptor:\n">>),
    solarized:term(text, Term, #{ indent => 2 });
report_cancelled_abort({bad_generator, {{M, F, A}, Term}}) ->
    solarized:text(<<"result from generator:\n">>),
    solarized:blue("  ~ts:~ts/~p~n", [M, F, A]),
    solarized:text(<<"is not a test:\n">>),
    solarized:term(text, Term, #{ indent => 2 });
report_cancelled_abort({generator_failed, {{M,F,A}, Exception}}) ->
    solarized:text(<<"test generator:\n">>),
    solarized:blue("  ~ts:~ts/~p~n", [M, F, A]),
    solarized:text(<<"failed at:\n">>),
    report_exception(Exception);
report_cancelled_abort({no_such_function, {M,F,A}})
        when is_atom(M), is_atom(F), is_integer(A) ->
    solarized:text(<<"no such function:\n">>),
    solarized:blue("  ~ts:~ts/~p~n", [M, F, A]);
report_cancelled_abort({module_not_found, M}) ->
    solarized:text(<<"test module not found:\n">>),
    solarized:blue("  ~ts~n", [M]);
report_cancelled_abort({application_not_found, A}) when is_atom(A) ->
    solarized:text(<<"application not found:\n">>),
    solarized:blue("  ~ts~n", [A]);
report_cancelled_abort({file_read_error, {_R, Msg, F}}) ->
    solarized:text(<<"error reading file:\n">>),
    solarized:blue("  ~ts~n", [F]),
    solarized:text(<<"reason:\n">>),
    solarized:text("  ~ts~n", [Msg]);
report_cancelled_abort({setup_failed, Exception}) ->
    solarized:text(<<"context setup failed:\n">>),
    report_exception(Exception);
report_cancelled_abort({cleanup_failed, Exception}) ->
    solarized:text(<<"context cleanup failed:\n">>),
    report_exception(Exception);
report_cancelled_abort({{bad_instantiator, {{M,F,A}, Term}}, _Exception}) ->
    solarized:text(<<"result from instantiator:\n">>),
    solarized:blue("  ~ts:~ts/~p~n", [M, F, A]),
    solarized:text(<<"is not a test:\n">>),
    solarized:term(text, Term, #{ indent => 2 });
report_cancelled_abort({instantiation_failed, Exception}) ->
    solarized:text(<<"instantiation of subtests failed at:\n">>),
    report_exception(Exception);
report_cancelled_abort(Reason) ->
    solarized:text(<<"abort:\n">>),
    solarized:term(text, Reason, #{ indent => 2 }).

%=======================================================================

report_skipped(#state{skipped = []}) ->
    ok;
report_skipped(#state{skipped = Skipped, tests = Tests}) ->
    Fold = fun (Id, Count) -> report_skipped(Id, Count, Tests) end,
    lists:foldr(Fold, 1, Skipped),
    ok.

%-----------------------------------------------------------------------

report_skipped(Id, Count, Tests) ->
    #{ Id := Test } = Tests,
    solarized:title(red, io_lib:format("skipped # ~p", [Count])),
    solarized:text(<<"test:\n">>),
    solarized:term(text, Test, #{ indent => 2 }),
    solarized:nl(),
    Count + 1.

%=======================================================================

report_failed(#state{failed = []}) ->
    ok;
report_failed(#state{failed = Fails, tests = Tests}) ->
    Fold = fun (Id, Count) -> report_failed(Id, Count, Tests) end,
    lists:foldr(Fold, 1, Fails),
    ok.

%-----------------------------------------------------------------------

report_failed(Id, Count, Tests) ->
    #{ Id := Test } = Tests,
    solarized:title(red, io_lib:format("failure # ~p", [Count])),
    report_test_function(Test),
    case Test of
        #{ status := {skipped, Reason} } ->
            solarized:comment("Skipped:~n  ~p~n", [Reason]);

        #{ status := {error, Exception}, output := Output } ->
            report_exception(Exception, Output)
    end,
    solarized:nl(),
    Count + 1.

%=======================================================================

report_exception({Class, Reason, Stack}) ->
    report_exception_stack(lists:reverse(Stack)),
    report_exception_class(Class, Reason).

%-----------------------------------------------------------------------

report_exception({Class, Reason, Stack}, Output) ->
    report_exception_stack(lists:reverse(Stack)),
    report_exception_output(Output),
    report_exception_class(Class, Reason).

%-----------------------------------------------------------------------

report_exception_stack([{M, _, _, _} | Stack])
        when M =:= eunit_proc orelse
             M =:= eunit_test ->
    % skip stack references to eunit
    report_exception_stack(Stack);
report_exception_stack([{M, F, A, Loc} | Stack])
        when is_integer(A) ->
    report_exception_stack_item(M, F, A, Loc),
    report_exception_stack(Stack);
report_exception_stack([{M, F, A, Loc} | Stack]) ->
    report_exception_stack_item(M, F, length(A), Loc),
    solarized:text(<<"called with:\n">>),
    solarized:term(cyan, A, #{ indent => 2 }),
    report_exception_stack(Stack);
report_exception_stack([]) ->
    ok.

%-----------------------------------------------------------------------

report_exception_stack_item(M, F, A, Loc) ->
    Line = proplists:get_value(line, Loc),
    File = proplists:get_value(file, Loc),
    report_function(M, F, A, Line, File, comment).

%-----------------------------------------------------------------------

report_exception_output(<<>>) ->
    ok;
report_exception_output([]) ->
    ok;
report_exception_output([<<>>]) ->
    ok;
report_exception_output(Output) ->
    solarized:text(<<"output:\n">>),
    solarized:term(text, Output, #{ indent => 2 }).

%-----------------------------------------------------------------------

report_exception_class(error, Reason) ->
    report_exception_error(Reason);
report_exception_class(throw, Reason) ->
    solarized:text(<<"throw:\n">>),
    solarized:term(orange, Reason, #{ indent => 2 });
report_exception_class(exit, Reason) ->
    solarized:text(<<"exit:\n">>),
    solarized:term(orange, Reason, #{ indent => 2 }).

%-----------------------------------------------------------------------

report_exception_error({outputEqualToFile, Props}) when is_list(Props) ->
    Args = [app, file, expression, columns, rows],
    report_assertion(outputEqualToFile, Args, Props),
    report_property_string(<<"expect file">>, green, expect_file, Props),
    report_property_string(<<"output file">>, orange, output_file, Props);
report_exception_error({assert, Props}) when is_list(Props) ->
    case proplists:lookup(expected, Props) of
        {_, true} ->
            report_assertion(assert, ['_', expression], Props);

        {_, false} ->
            report_assertion(assertNot, ['_', expression], Props)
    end,
    report_property_optional(<<"not boolean">>, orange, not_boolean, Props);
report_exception_error({assertEqual, Props}) when is_list(Props) ->
    report_assertion(assertEqual, ['_', expression], Props),
    {E, G} = report_diff(expected, value, Props),
    report_diffed(<<"expected">>, E),
    report_diffed(<<"got">>, G);
report_exception_error({assertNotEqual, Props}) when is_list(Props) ->
    report_assertion(assertNotEqual, ['_', expression], Props),
    report_property(<<"NOT expected">>, orange, value, Props);
report_exception_error({assertMatch, Props}) when is_list(Props) ->
    report_assertion(assertMatch, ['_', expression], Props),
    report_property_string(<<"pattern">>, green, pattern, Props),
    report_property(<<"got">>, orange, value, Props);
report_exception_error({assertNotMatch, Props}) when is_list(Props) ->
    report_assertion(assertNotMatch, ['_', expression], Props),
    report_property_string(<<"pattern">>, green, pattern, Props),
    report_property(<<"got">>, orange, value, Props);
report_exception_error({assertException, Props}) when is_list(Props) ->
    report_assertion(assertException, ['_', '_', expression], Props),
    report_property_string(<<"pattern">>, green, pattern, Props),
    case proplists:lookup(unexpected_exception, Props) of
        none ->
            report_property(<<"unexpected success">>, orange, unexpected_success, Props);

        {_, {Class, Reason, Stack}} ->
            report_header(<<"unexpected exception">>),
            report_term(orange, {Class, Reason, '_'}),
            report_exception_stack(lists:reverse(Stack))
    end;
report_exception_error({badmatch, Value}) ->
    report_header(<<"bad match">>),
    solarized:term(orange, Value, #{ indent => 2 });
report_exception_error(Reason) ->
    report_header(<<"error">>),
    solarized:term(orange, Reason, #{ indent => 2 }).

%-----------------------------------------------------------------------

report_diff(Left, Right, Props) ->
    case {proplists:lookup(Left, Props), proplists:lookup(Right, Props)} of
        {none, none} ->
            {{missing, Left}, {missing, Right}};

        {none, {_, R}} ->
            {{missing, Left}, {term, orange, R}};

        {{_, L}, none} ->
            {{term, green, L}, {missing, Right}};

        {{_, L}, {_, R}} ->
            try
                %Options =
                %    #{ indent => {comment, bold, <<" |">>}
                %     , hanging => false
                %     },
                Options = #{ indent => 2 },
                {Ld, Rd} = solarized:diff(green, orange, L, R, Options),
                {{diffed, Ld}, {diffed, Rd}}
            catch
                Class:Reason:Stack ->
                    solarized:title(magenta, <<"solarized_eunit EXCEPTION">>),
                    io:format("exception ~p:~p~n~p~n", [Class, Reason, Stack]),
                    solarized:title(magenta, <<"solarized_eunit EXCEPTION">>),
                    {{missing, Left}, {missing, Right}}
            end
    end.

%-----------------------------------------------------------------------

report_diffed(Header, Diffed) ->
    report_header(Header),
    case Diffed of
        {diffed, Styled} ->
            report_styled(Styled);

        {term, Style, Term} ->
            report_term(Style, Term);

        {missing, Key} ->
            report_string(red, [$? | atom_to_list(Key)])
    end.

%-----------------------------------------------------------------------

report_property(Header, Style, Key, Props) ->
    report_header(Header),
    case proplists:lookup(Key, Props) of
        {_, Value} ->
            report_term(Style, Value);

        none ->
            report_string(red, [$? | atom_to_list(Key)])
    end.

%-----------------------------------------------------------------------

report_property_string(Header, Style, Key, Props) ->
    report_header(Header),
    case proplists:lookup(Key, Props) of
        {_, Value} ->
            report_string(Style, Value);

        none ->
            report_string(red, [$? | atom_to_list(Key)])
    end.

%-----------------------------------------------------------------------

report_property_optional(Header, Style, Key, Props) ->
    case proplists:lookup(Key, Props) of
        {_, Value} ->
            report_header(Header),
            report_term(Style, Value);

        none ->
            ok
    end.

%-----------------------------------------------------------------------

report_header(Header) ->
    solarized:text([ Header, <<":\n">>]).

%-----------------------------------------------------------------------

report_string(Style, String) ->
    report_styled([<<"  ">>, {Style, String}, <<"\n">>]).

%-----------------------------------------------------------------------

report_styled(Styled) ->
    try
        solarized:styled(Styled)
    catch
        Class:Reason:Stack ->
            solarized:title(magenta, <<"solarized_eunit EXCEPTION">>),
            io:format("exception ~p:~p~n~p~n", [Class, Reason, Stack]),
            io:format("--- styled ---~n~p~n", [Styled]),
            solarized:title(magenta, <<"solarized_eunit EXCEPTION">>),
            ok
    end.

%-----------------------------------------------------------------------

report_term(Style, Term) ->
    try
        solarized:term(Style, Term, #{ indent => 2 })
    catch
        Class:Reason:Stack ->
            solarized:title(magenta, <<"solarized_eunit EXCEPTION">>),
            io:format("exception ~p:~p~n~p~n", [Class, Reason, Stack]),
            io:format("--- term ---~n~p~n", [Term]),
            solarized:title(magenta, <<"solarized_eunit EXCEPTION">>),
            ok
    end.

%=======================================================================

report_assertion(Assertion, Args, Props) ->
    try
        report_assertion_header(Props),
        Styled =
            [ <<"  ">>
            , {red, [$?, atom_to_list(Assertion), $(]}
            | report_assertion_args(Args, Props)
            ],
        solarized:styled(Styled),
        report_assertion_comment(Props)
    catch
        Class:Reason:Stack ->
            solarized:title(magenta, <<"solarized_eunit EXCEPTION">>),
            io:format("exception ~p:~p~n~p~n", [Class, Reason, Stack]),
            solarized:title(magenta, <<"solarized_eunit EXCEPTION">>),
            ok
    end.

%-----------------------------------------------------------------------

report_assertion_header(Props) ->
    Rest = report_assertion_module(Props),
    Styled = [<<"assertion">> | Rest],
    solarized:styled(Styled).

%-----------------------------------------------------------------------

report_assertion_module(Props) ->
    Rest = report_assertion_line(Props),
    case proplists:lookup(module, Props) of
        none ->
            Rest;

        {_, Module} ->
            [<<" in ">>, {blue, atom_to_list(Module)} | Rest]
    end.

%-----------------------------------------------------------------------

report_assertion_line(Props) ->
    case proplists:lookup(line, Props) of
        none ->
            [<<":\n">>];

        {_, Line} ->
            io_lib:format(" (line ~p):~n", [Line])
    end.

%-----------------------------------------------------------------------

report_assertion_args([Arg], Props) ->
    report_assertion_arg(Arg, [{red, <<")">>}, <<"\n">>], Props);
report_assertion_args([Arg | Args], Props) ->
    Rest = report_assertion_args(Args, Props),
    report_assertion_arg(Arg, [{red, <<", ">>} | Rest], Props).

%-----------------------------------------------------------------------

report_assertion_arg('_', Rest, _) ->
    [{red, $_} | Rest];
report_assertion_arg(Arg, Rest, Props) ->
    case proplists:lookup(Arg, Props) of
        none ->
            [{red, [$?, atom_to_list(Arg)]} | Rest];

        {_, Value} ->
            [Value | Rest]
    end.

%-----------------------------------------------------------------------

report_assertion_comment(Props) ->
    case proplists:lookup(comment, Props) of
        none ->
            ok;

        {_, Comment} ->
            solarized:text(<<"comment:\n">>),
            case printable(Comment) of
                true ->
                    solarized:styled(
                        [ <<"  ">>
                        , {magenta, Comment}
                        , <<"\n">>
                        ]);

                false ->
                    solarized:term(magenta, Comment, #{ indent => 2 })
            end
    end.

%-----------------------------------------------------------------------

% based on io_lib:deep_unicode_char_list/1
% extended to handle UTF8 binaries

-define(PRINTABLE(C),
    ( (C >= 0 andalso C < 16#D800) orelse
      (C > 16#DFFF andalso C < 16#FFFE) orelse
      (C > 16#FFFF andalso C =< 16#10FFFF))).

printable(Binary) when is_binary(Binary) ->
    printable_binary(Binary, []);
printable(List) when is_list(List) ->
    printable_list(List, []);
printable(_) ->
    false.

printable_binary(<<C/utf8, Cs/binary>>, More)
        when ?PRINTABLE(C) ->
    printable_binary(Cs, More);
printable_binary(<<>>, [C | More]) ->
    printable_list(C, More);
printable_binary(<<>>, []) ->
    true;
printable_binary(_, _) ->
    false.

printable_list([C | Cs], More) when is_binary(C) ->
    printable_binary(C, [Cs | More]);
printable_list([C | Cs], More) when is_list(C) ->
    printable_list(C, [Cs | More]);
printable_list([C | Cs], More)
        when is_integer(C) andalso ?PRINTABLE(C) ->
    printable_list(Cs, More);
printable_list([], [Cs | More]) ->
    printable_list(Cs, More);
printable_list([], []) ->
    true;
printable_list(_, _) ->
    false.

%=======================================================================

report_group_description(#{ desc := D }) when D =/= undefined ->
    solarized:magenta("  ~ts~n", [D]);
report_group_description(_) ->
    solarized:magenta(<<"  unknown test group\n">>).

%=======================================================================

report_test_function(Test = #{ source := {M, F, A} }) ->
    L = maps:get(line, Test, undefined),
    D = maps:get(desc, Test, undefined),
    report_function(M, F, A, L, D, magenta).

%=======================================================================

report_function(M, F, A, Line, Extra, ExtraColor) ->
    W = solarized:columns(),
    Func = io_lib:format("~ts:~ts/~p", [M, F, A]),
    Length = string:length(Func),
    solarized:styled([<<"  ">>, {blue, Func}]),
    report_function_line(W, 2 + Length, Line, Extra, ExtraColor).

report_function_line(W, C, undefined, Extra, ExtraColor) ->
    report_function_extra(W, C, Extra, ExtraColor);
report_function_line(W, C, L, Extra, ExtraColor) ->
    Line = io_lib:format("(line ~p)", [L]),
    Length = string:length(Line),
    if C + 1 + Length < W ->
        solarized:text([$\s, Line]),
        report_function_extra(W, C + 1 + Length, Extra, ExtraColor);

       true ->
        solarized:text([<<"\n    ">>, Line]),
        report_function_extra(W, 4 + Length, Extra, ExtraColor)
    end.

report_function_extra(_, _, undefined, _) ->
    solarized:nl();
report_function_extra(W, C, Extra, Color) ->
    Length = string:length(Extra),
    if C + 1 + Length < W ->
        solarized:styled([$\s, {Color, Extra}, $\n]);

       true ->
        solarized:styled([<<"\n    ">>, {Color, Extra}, $\n])
    end.


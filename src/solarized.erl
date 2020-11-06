%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized).

-include("styled.hrl").

-export([styled/1, styled/2]).

-export ?STYLED_EXPORT(text).
-export ?STYLED_EXPORT(comment).
-export ?STYLED_EXPORT(emphasize).
-export ?STYLED_EXPORT(blue).
-export ?STYLED_EXPORT(cyan).
-export ?STYLED_EXPORT(green).
-export ?STYLED_EXPORT(magenta).
-export ?STYLED_EXPORT(orange).
-export ?STYLED_EXPORT(red).
-export ?STYLED_EXPORT(violet).
-export ?STYLED_EXPORT(yellow).

-export([title/2, title/3]).
-export([section/2, section/3]).
-export([term/2, term/3, term/4]).
-export([diff/4, diff/5]).
-export([columns/0, columns/1]).
-export([rows/0, rows/1]).
-export([nl/0, nl/1]).

-export_type([ color/0
             , directive/0
             , application/0
             , styled/0
             ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").
-endif.

%=======================================================================

-type color() ::
      text
    | comment
    | emphasize
    | blue
    | cyan
    | green
    | magenta
    | orange
    | red
    | violet
    | yellow
    .

%-----------------------------------------------------------------------

-type directive() ::
    color() |
    bold | bold_off |
    highlight | highlight_off |
    reverse | reverse_off |
    underline | underline_off.

%-----------------------------------------------------------------------

-type application() ::
    directive() |
    {directive(), directive()}.

%-----------------------------------------------------------------------

-type styled() ::
    unicode_binary() |
    applied() |
    maybe_improper_list(unicode_char() | styled(),
                        unicode_binary() | applied() | []).

-type applied() ::
    {Apply :: directive(), Text :: styled()} |
    {Apply1 :: directive(), Apply2 :: directive(), Text :: styled()}.

-type unicode_char() :: char().

-type unicode_binary() :: unicode:unicode_binary().

%-----------------------------------------------------------------------

% Simplified form of io:format() to avoid dialyzer "overlapping domains"
% in color/2 functions.
% Without this the following:
%   red(text, [])
% could be interpreted as:
%   output the formatted string `text` with data `[]` in `red`

-type format() :: string() | binary().

%=======================================================================

-spec solarized:styled(Text) -> ok
    when
      Text :: solarized:styled().

styled(Text) ->
    Styled = styled(?NO_STYLE, [], Text),
    Output = styled_style(Styled, ?NO_STYLE),
    io:put_chars(Output).

%-----------------------------------------------------------------------

-spec solarized:styled(pid(), Text) -> ok
    when
      Text :: solarized:styled().

styled(Io, Text) when is_pid(Io) ->
    Styled = styled(?NO_STYLE, [], Text),
    Output = styled_style(Styled, ?NO_STYLE),
    io:put_chars(Io, Output).

%-----------------------------------------------------------------------

styled(Style, Stack, [[] | Rest]) ->
    styled(Style, Stack, Rest);
styled(Style, Stack, [Text | Rest]) when is_list(Text) ->
    styled_list(Style, styled_push(Rest, Stack), Text);
styled(Style, Stack, [Text | Rest]) when is_tuple(Text) ->
    styled(Style, styled_push(Rest, Stack), Text);
styled(Style, Stack, [Text | Rest])
        when is_binary(Text) orelse
             is_integer(Text) ->
    styled_unit(Style, Stack, Rest, Text);
styled(Style, Stack, {A, Text}) when is_list(Text) ->
    styled_list(styled_apply(A, Style), [Style | Stack], Text);
styled(Style, Stack, {A, Text}) ->
    styled(styled_apply(A, Style), [Style | Stack], Text);
styled(Style, Stack, {A, B, Text}) when is_list(Text) ->
    styled_list(styled_apply(A, B, Style), [Style | Stack], Text);
styled(Style, Stack, {A, B, Text}) ->
    styled(styled_apply(A, B, Style), [Style | Stack], Text);
styled(Style, Stack, Text)
        when is_binary(Text) orelse
             is_integer(Text) ->
    styled_unit(Style, Stack, [], Text);
styled(_, [Style | Stack], []) when is_tuple(Style) ->
    styled(Style, Stack, []);
styled(Style, [Text | Stack], []) ->
    styled(Style, Stack, Text);
styled(_, [], Text = []) ->
    {Text, ?NO_STYLE}.

%-----------------------------------------------------------------------

styled_push([], Stack) -> Stack;
styled_push(On, Stack) -> [On | Stack].

%-----------------------------------------------------------------------

styled_list(Style, Stack, Text = []) ->
    styled(Style, Stack, Text);
styled_list(Style, Stack, Text) ->
    case io_lib:printable_unicode_list(Text) of
        true ->
            styled_unit(Style, Stack, [], Text);

        false ->
            styled(Style, Stack, Text)
    end.

%-----------------------------------------------------------------------

styled_unit(Style, Stack, Rest, Text) ->
    Styled = styled(Style, Stack, Rest),
    styled_style(Styled, Style, Text).

%-----------------------------------------------------------------------

styled_style(Styled, Style, Text) ->
    {[Text | styled_style(Styled, Style)], Style}.

%-----------------------------------------------------------------------

styled_style({Output, Style}, Style) ->
    Output;
styled_style({Output, ?NO_STYLE}, _) ->
    [<<?RESET>> | Output];
styled_style({Output, Want}, Curr) ->
    [styled_color(Curr, Want) | Output].

%-----------------------------------------------------------------------

styled_color(Curr = {C, _, _, _, _}, Want = {C, _, _, _, _}) ->
    styled_bold(Curr, Want);
styled_color(Curr, Want = {text, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?TEXT>>);
styled_color(Curr, Want = {comment, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?COMMENT>>);
styled_color(Curr, Want = {emphasize, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?EMPHASIZE>>);
styled_color(Curr, Want = {blue, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?BLUE>>);
styled_color(Curr, Want = {cyan, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?CYAN>>);
styled_color(Curr, Want = {green, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?GREEN>>);
styled_color(Curr, Want = {magenta, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?MAGENTA>>);
styled_color(Curr, Want = {orange, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?ORANGE>>);
styled_color(Curr, Want = {red, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?RED>>);
styled_color(Curr, Want = {violet, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?VIOLET>>);
styled_color(Curr, Want = {yellow, _, _, _, _}) ->
    styled_bold(Curr, Want, <<?E, ?YELLOW>>).

%-----------------------------------------------------------------------

styled_bold(Curr = {_, B, _, _, _}, Want = {_, B, _, _, _}) ->
    styled_highlight(Curr, Want);
styled_bold(Curr, Want = {_, bold, _, _, _}) ->
    styled_highlight(Curr, Want, <<?E, ?BOLD>>);
styled_bold(Curr, Want) ->
    styled_highlight(Curr, Want, <<?E, ?BOLD_OFF>>).

%-----------------------------------------------------------------------

styled_bold(Curr = {_, B, _, _, _}, Want = {_, B, _, _, _}, Acc) ->
    styled_highlight(Curr, Want, Acc);
styled_bold(Curr, Want = {_, bold, _, _, _}, Acc) ->
    styled_highlight(Curr, Want, <<Acc/binary, $;, ?BOLD>>);
styled_bold(Curr, Want, Acc) ->
    styled_highlight(Curr, Want, <<Acc/binary, $;, ?BOLD_OFF>>).

%-----------------------------------------------------------------------

styled_highlight(Curr = {_, _, H, _, _}, Want = {_, _, H, _, _}) ->
    styled_reverse(Curr, Want);
styled_highlight(Curr, Want = {_, _, highlight, _, _}) ->
    styled_reverse(Curr, Want, <<?E, ?HIGHLIGHT>>);
styled_highlight(Curr, Want) ->
    styled_reverse(Curr, Want, <<?E, ?HIGHLIGHT_OFF>>).

%-----------------------------------------------------------------------

styled_highlight(Curr = {_, _, H, _, _}, Want = {_, _, H, _, _}, Acc) ->
    styled_reverse(Curr, Want, Acc);
styled_highlight(Curr, Want = {_, _, highlight, _, _}, Acc) ->
    styled_reverse(Curr, Want, <<Acc/binary, $;, ?HIGHLIGHT>>);
styled_highlight(Curr, Want, Acc) ->
    styled_reverse(Curr, Want, <<Acc/binary, $;, ?HIGHLIGHT_OFF>>).

%-----------------------------------------------------------------------

styled_reverse(Curr = {_, _, _, R, _}, Want = {_, _, _, R, _}) ->
    styled_underline(Curr, Want);
styled_reverse(Curr, Want = {_, _, _, reverse, _}) ->
    styled_underline(Curr, Want, <<?E, ?REVERSE>>);
styled_reverse(Curr, Want) ->
    styled_underline(Curr, Want, <<?E, ?REVERSE_OFF>>).

%-----------------------------------------------------------------------

styled_reverse(Curr = {_, _, _, R, _}, Want = {_, _, _, R, _}, Acc) ->
    styled_underline(Curr, Want, Acc);
styled_reverse(Curr, Want = {_, _, _, reverse, _}, Acc) ->
    styled_underline(Curr, Want, <<Acc/binary, $;, ?REVERSE>>);
styled_reverse(Curr, Want, Acc) ->
    styled_underline(Curr, Want, <<Acc/binary, $;, ?REVERSE_OFF>>).

%-----------------------------------------------------------------------

styled_underline(_, {_, _, _, _, underline}) ->
    <<?E, ?UNDERLINE, $m>>;
styled_underline(_, _) ->
    <<?E, ?UNDERLINE_OFF, $m>>.

%-----------------------------------------------------------------------

styled_underline({_, _, _, _, U}, {_, _, _, _, U}, Acc) ->
    <<Acc/binary, $m>>;
styled_underline(_, {_, _, _, _, underline}, Acc) ->
    <<Acc/binary, $;, ?UNDERLINE, $m>>;
styled_underline(_, _, Acc) ->
    <<Acc/binary, $;, ?UNDERLINE_OFF, $m>>.

%-----------------------------------------------------------------------

styled_apply(A, B, Style) ->
    styled_apply(B, styled_apply(A, Style)).

%-----------------------------------------------------------------------

styled_apply(C, {_, B, H, R, U}) when ?IS_COLOR(C) ->
    {C, B, H, R, U};
styled_apply(B, {C, _, H, R, U}) when ?IS_BOLD(B) ->
    {C, B, H, R, U};
styled_apply(H, {C, B, _, R, U}) when ?IS_HIGHLIGHT(H) ->
    {C, B, H, R, U};
styled_apply(R, {C, B, H, _, U}) when ?IS_REVERSE(R) ->
    {C, B, H, R, U};
styled_apply(U, {C, B, H, R, _}) when ?IS_UNDERLINE(U) ->
    {C, B, H, R, U}.

%-----------------------------------------------------------------------

-ifdef(TEST).

red_bold_test() ->
    Text = {red, [{bold, <<"bold">>}, <<" text">>]},
    Expect = <<?E, ?RED, $;, ?BOLD, ?M, "bold",
               ?E, ?BOLD_OFF, ?M, " text",
               ?RESET>>,
    ?outputEqual(Expect, ok = solarized:styled(Text)).

bold_red_test() ->
    Text = {bold, [{red, <<"red">>}, <<" bold">>]},
    Expect = <<?E, ?RED, $;, ?BOLD, ?M, "red",
               ?E, ?TEXT, ?M, " bold",
               ?RESET>>,
    ?outputEqual(Expect, ok = solarized:styled(Text)).

green_underline_test() ->
    Text = {green, [<<"text ">>, {underline, <<"underline">>}]},
    Expect = <<?E, ?GREEN, ?M, "text ",
               ?E, ?UNDERLINE, ?M, "underline",
               ?RESET>>,
    ?outputEqual(Expect, ok = solarized:styled(Text)).

repeat_style_test() ->
    Text = [{blue, <<"blue">>}, {blue, <<" text">>}],
    Expect = <<?E, ?BLUE, ?M, "blue text", ?RESET>>,
    ?outputEqual(Expect, ok = solarized:styled(Text)).

empty_apply_test() ->
    Text = {blue, [<<"blue">>, {red, []}, [], <<" text">>]},
    Expect = <<?E, ?BLUE, ?M, "blue text", ?RESET>>,
    ?outputEqual(Expect, ok = solarized:styled(Text)).

styled_test() ->
    Text = styled_test_colors(),
    Test = fun () -> ok = solarized:styled(Text) end,
    ?outputEqualToFile(solarized, styled_test, Test).

styled_test_colors() ->
    Colors =
        [ text
        , comment
        , emphasize
        , blue
        , cyan
        , green
        , magenta
        , orange
        , red
        , violet
        , yellow
        ],
    [ styled_test_color(Color) || Color <- Colors ].

styled_test_color(Color) ->
    Name = io_lib:format("~10s: ", [Color]),
    [{Color, [Name | styled_test_reverse()]}, $\n].

styled_test_reverse() ->
    [ <<"r ">>, styled_test_highlight(), $\s
    , {reverse, [<<"R ">>, styled_test_highlight()]}
    ].

styled_test_highlight() ->
    [ <<"h ">>, styled_test_bold(), $\s
    , {highlight, [<<"H ">>, styled_test_bold()]}
    ].

styled_test_bold() ->
    [ <<"b ">>, styled_test_underline(), $\s
    , {bold, [<<"B ">>, styled_test_underline()]}
    ].

styled_test_underline() ->
    [<<"u ">>, {underline, <<"U">>}].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(text).
?STYLED_FUNC_1(text).

-spec ?STYLED_SPEC_2(text).
?STYLED_FUNC_2(text).

-spec ?STYLED_SPEC_3(text).
?STYLED_FUNC_3(text).

-spec ?STYLED_SPEC_4(text).
?STYLED_FUNC_4(text).

%-----------------------------------------------------------------------

-ifdef(TEST).

text_test_() ->
    Text = <<"text">>,
    Expect = Text,
    [ ?_outputEqual(Expect, ok = solarized:text(Text))
    , ?_outputEqual(Expect, ok = solarized:text("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:text(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:text(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(comment).
?STYLED_FUNC_1(comment).

-spec ?STYLED_SPEC_2(comment).
?STYLED_FUNC_2(comment).

-spec ?STYLED_SPEC_3(comment).
?STYLED_FUNC_3(comment).

-spec ?STYLED_SPEC_4(comment).
?STYLED_FUNC_4(comment).

%-----------------------------------------------------------------------

-ifdef(TEST).

comment_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?COMMENT, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:comment(Text))
    , ?_outputEqual(Expect, ok = solarized:comment("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:comment(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:comment(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(emphasize).
?STYLED_FUNC_1(emphasize).

-spec ?STYLED_SPEC_2(emphasize).
?STYLED_FUNC_2(emphasize).

-spec ?STYLED_SPEC_3(emphasize).
?STYLED_FUNC_3(emphasize).

-spec ?STYLED_SPEC_4(emphasize).
?STYLED_FUNC_4(emphasize).

%-----------------------------------------------------------------------

-ifdef(TEST).

emphasize_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?EMPHASIZE, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:emphasize(Text))
    , ?_outputEqual(Expect, ok = solarized:emphasize("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:emphasize(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:emphasize(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(blue).
?STYLED_FUNC_1(blue).

-spec ?STYLED_SPEC_2(blue).
?STYLED_FUNC_2(blue).

-spec ?STYLED_SPEC_3(blue).
?STYLED_FUNC_3(blue).

-spec ?STYLED_SPEC_4(blue).
?STYLED_FUNC_4(blue).

%-----------------------------------------------------------------------

-ifdef(TEST).

blue_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?BLUE, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:blue(Text))
    , ?_outputEqual(Expect, ok = solarized:blue("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:blue(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:blue(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(cyan).
?STYLED_FUNC_1(cyan).

-spec ?STYLED_SPEC_2(cyan).
?STYLED_FUNC_2(cyan).

-spec ?STYLED_SPEC_3(cyan).
?STYLED_FUNC_3(cyan).

-spec ?STYLED_SPEC_4(cyan).
?STYLED_FUNC_4(cyan).

%-----------------------------------------------------------------------

-ifdef(TEST).

cyan_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?CYAN, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:cyan(Text))
    , ?_outputEqual(Expect, ok = solarized:cyan("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:cyan(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:cyan(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(green).
?STYLED_FUNC_1(green).

-spec ?STYLED_SPEC_2(green).
?STYLED_FUNC_2(green).

-spec ?STYLED_SPEC_3(green).
?STYLED_FUNC_3(green).

-spec ?STYLED_SPEC_4(green).
?STYLED_FUNC_4(green).

%-----------------------------------------------------------------------

-ifdef(TEST).

green_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?GREEN, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:green(Text))
    , ?_outputEqual(Expect, ok = solarized:green("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:green(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:green(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(magenta).
?STYLED_FUNC_1(magenta).

-spec ?STYLED_SPEC_2(magenta).
?STYLED_FUNC_2(magenta).

-spec ?STYLED_SPEC_3(magenta).
?STYLED_FUNC_3(magenta).

-spec ?STYLED_SPEC_4(magenta).
?STYLED_FUNC_4(magenta).

%-----------------------------------------------------------------------

-ifdef(TEST).

magenta_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?MAGENTA, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:magenta(Text))
    , ?_outputEqual(Expect, ok = solarized:magenta("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:magenta(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:magenta(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(orange).
?STYLED_FUNC_1(orange).

-spec ?STYLED_SPEC_2(orange).
?STYLED_FUNC_2(orange).

-spec ?STYLED_SPEC_3(orange).
?STYLED_FUNC_3(orange).

-spec ?STYLED_SPEC_4(orange).
?STYLED_FUNC_4(orange).

%-----------------------------------------------------------------------

-ifdef(TEST).

orange_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?ORANGE, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:orange(Text))
    , ?_outputEqual(Expect, ok = solarized:orange("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:orange(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:orange(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(red).
?STYLED_FUNC_1(red).

-spec ?STYLED_SPEC_2(red).
?STYLED_FUNC_2(red).

-spec ?STYLED_SPEC_3(red).
?STYLED_FUNC_3(red).

-spec ?STYLED_SPEC_4(red).
?STYLED_FUNC_4(red).

%-----------------------------------------------------------------------

-ifdef(TEST).

red_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?RED, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:red(Text))
    , ?_outputEqual(Expect, ok = solarized:red("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:red(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:red(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(violet).
?STYLED_FUNC_1(violet).

-spec ?STYLED_SPEC_2(violet).
?STYLED_FUNC_2(violet).

-spec ?STYLED_SPEC_3(violet).
?STYLED_FUNC_3(violet).

-spec ?STYLED_SPEC_4(violet).
?STYLED_FUNC_4(violet).

%-----------------------------------------------------------------------

-ifdef(TEST).

violet_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?VIOLET, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:violet(Text))
    , ?_outputEqual(Expect, ok = solarized:violet("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:violet(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:violet(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec ?STYLED_SPEC_1(yellow).
?STYLED_FUNC_1(yellow).

-spec ?STYLED_SPEC_2(yellow).
?STYLED_FUNC_2(yellow).

-spec ?STYLED_SPEC_3(yellow).
?STYLED_FUNC_3(yellow).

-spec ?STYLED_SPEC_4(yellow).
?STYLED_FUNC_4(yellow).

%-----------------------------------------------------------------------

-ifdef(TEST).

yellow_test_() ->
    Text = <<"text">>,
    Expect = <<?E, ?YELLOW, ?M, "text", ?RESET>>,
    [ ?_outputEqual(Expect, ok = solarized:yellow(Text))
    , ?_outputEqual(Expect, ok = solarized:yellow("~s", [Text]))
    , ?_outputEqual(Expect, ok = solarized:yellow(bold_off, Text))
    , ?_outputEqual(Expect, ok = solarized:yellow(bold_off, "~s", [Text]))
    ].

-endif.

%=======================================================================

-spec solarized:title(Style, Text) -> ok
    when
      Style :: application(),
      Text :: unicode:chardata().

title(Style, Text) when is_binary(Text) ->
    Repeat = columns() - 5 - erlang:size(Text),
    Line = io_lib:format("== ~s ~*..*s", [Text, Repeat, $=, <<>>]),
    case Style of
        Apply when is_atom(Apply) ->
            styled([{Apply, Line}, $\n]);

        {Apply1, Apply2} ->
            styled([{Apply1, Apply2, Line}, $\n])
    end;
title(Color, Text) ->
    title(Color, unicode:characters_to_binary(Text)).

%-----------------------------------------------------------------------

-spec solarized:title(pid(), Style, Text) -> ok
    when
      Style :: application(),
      Text :: unicode:chardata().

title(Io, Style, Text) when is_binary(Text) ->
    Repeat = columns(Io) - 5 - erlang:size(Text),
    Line = io_lib:format("== ~s ~*..*s", [Text, Repeat, $=, <<>>]),
    case Style of
        Apply when is_atom(Apply) ->
            styled(Io, [{Apply, Line}, $\n]);

        {Apply1, Apply2} ->
            styled(Io, [{Apply1, Apply2, Line}, $\n])
    end;
title(Io, Color, Text) ->
    title(Io, Color, unicode:characters_to_binary(Text)).

%-----------------------------------------------------------------------

-ifdef(TEST).

title_test_() ->
    Binary = <<"Title">>,
    String = "Title",
    Blue =
        <<?E, ?BLUE, ?M
        , "== Title =========="
        , ?RESET
        , $\n
        >>,
    BRed =
        <<?E, ?RED, $;, ?BOLD, ?M
        , "== Title =========="
        , ?RESET
        , $\n
        >>,
    [ ?_outputEqual(Blue, ok = solarized:title(blue, Binary), 20, 25)
    , ?_outputEqual(Blue, ok = solarized:title(blue, String), 20, 25)
    , ?_outputEqual(BRed, ok = solarized:title({bold, red}, String), 20, 25)
    , ?_outputEqual(BRed, ok = solarized:title({red, bold}, String), 20, 25)
    ].

-endif.

%=======================================================================

-spec solarized:section(Style, Text) -> ok
    when
      Style :: application(),
      Text :: unicode:chardata().

section(Style, Text) when is_binary(Text) ->
    Repeat = columns() - 5 - erlang:size(Text),
    Line = io_lib:format("-- ~s ~*..*s", [Text, Repeat, $-, <<>>]),
    case Style of
        Apply when is_atom(Apply) ->
            styled([{Apply, Line}, $\n]);

        {Apply1, Apply2} ->
            styled([{Apply1, Apply2, Line}, $\n])
    end;
section(Color, Text) ->
    section(Color, unicode:characters_to_binary(Text)).

%-----------------------------------------------------------------------

-spec solarized:section(pid(), Style, Text) -> ok
    when
      Style :: application(),
      Text :: unicode:chardata().

section(Io, Style, Text) when is_binary(Text) ->
    Repeat = columns(Io) - 5 - erlang:size(Text),
    Line = io_lib:format("-- ~s ~*..*s", [Text, Repeat, $-, <<>>]),
    case Style of
        Apply when is_atom(Apply) ->
            styled(Io, [{Apply, Line}, $\n]);

        {Apply1, Apply2} ->
            styled(Io, [{Apply1, Apply2, Line}, $\n])
    end;
section(Io, Color, Text) ->
    section(Io, Color, unicode:characters_to_binary(Text)).

%-----------------------------------------------------------------------

-ifdef(TEST).

section_test_() ->
    Binary = <<"Section">>,
    String = "Section",
    Blue =
        <<?E, ?BLUE, ?M
        , "-- Section --------"
        , ?RESET
        , $\n
        >>,
    BRed =
        <<?E, ?RED, $;, ?BOLD, ?M
        , "-- Section --------"
        , ?RESET
        , $\n
        >>,
    [ ?_outputEqual(Blue, ok = solarized:section(blue, Binary), 20, 25)
    , ?_outputEqual(Blue, ok = solarized:section(blue, String), 20, 25)
    , ?_outputEqual(BRed, ok = solarized:section({bold, red}, Binary), 20, 25)
    , ?_outputEqual(BRed, ok = solarized:section({red, bold}, Binary), 20, 25)
    ].

-endif.

%=======================================================================

-spec solarized:term(Style, Term) -> ok
    when
      Style :: application(),
      Term :: term().

term(Style, Term) ->
    styled(solarized_diff:term(Style, Term, #{})).

%-----------------------------------------------------------------------

-spec solarized:term(Style, Term, Options) -> ok
    when
      Style :: application(),
      Term :: term(),
      Options :: solarized_diff:options();
                    (pid(), Style, Term) -> ok
    when
      Style :: application(),
      Term :: term().

term(Io, Style, Term) when is_pid(Io) ->
    styled(Io, solarized_diff:term(Style, Term, #{}));
term(Style, Term, Options) ->
    styled(solarized_diff:term(Style, Term, Options)).

%-----------------------------------------------------------------------

-spec solarized:term(pid(), Style, Term, Options) -> ok
    when
      Style :: application(),
      Term :: term(),
      Options :: solarized_diff:options().

term(Io, Style, Term, Options) when is_pid(Io) ->
    styled(Io, solarized_diff:term(Style, Term, Options)).

%-----------------------------------------------------------------------

-ifdef(TEST).

term_test() ->
    Term =
        #{binary => <<"binary">>
        , list => [a, b, c]
        , improper => [a, b, c | improper]
        , integer => 42
        , tuple => {2018, 9, 20}
        },
    Style = {highlight, blue},
    Options =
        #{ indent => [" ", {bold, comment, "|"}]
        , hanging => false
        },
    Test = fun () ->  solarized:term(Style, Term, Options) end,
    ?outputEqualToFile(solarized, term_test, Test, 40, 25).

-endif.

%=======================================================================

-spec solarized:diff(OldStyle, NewStyle, OldTerm, NewTerm) -> Output
    when
      OldStyle :: application(),
      NewStyle :: application(),
      OldTerm :: term(),
      NewTerm :: term(),
      Output :: {styled(), styled()}.

diff(OldStyle, NewStyle, OldTerm, NewTerm) ->
    solarized_diff:diff(OldStyle, NewStyle, OldTerm, NewTerm, #{}).

%=======================================================================

-spec solarized:diff(OldStyle, NewStyle, OldTerm, NewTerm, Options) -> Output
    when
      OldStyle :: application(),
      NewStyle :: application(),
      OldTerm :: term(),
      NewTerm :: term(),
      Options :: solarized_diff:options(),
      Output :: {styled(), styled()}.

diff(OldStyle, NewStyle, OldTerm, NewTerm, Options) ->
    solarized_diff:diff(OldStyle, NewStyle, OldTerm, NewTerm, Options).

%=======================================================================

-spec solarized:columns() -> Columns
    when
      Columns :: pos_integer().

columns() ->
    case io:columns() of
        {ok, Columns} when is_integer(Columns) andalso Columns > 0 ->
            Columns;

        _ ->
            80
    end.

%-----------------------------------------------------------------------

-spec solarized:columns(pid()) -> Columns
    when
      Columns :: pos_integer().

columns(Io) when is_pid(Io) ->
    case io:columns(Io) of
        {ok, Columns} when is_integer(Columns) andalso Columns > 0 ->
            Columns;

        _ ->
            80
    end.

%-----------------------------------------------------------------------

-ifdef(TEST).

columns_test_() ->
    [ ?_resultEqual(40, solarized:columns(), 40, enotsup)
    , ?_resultEqual(80, solarized:columns(), enotsup, 12)
    ].

-endif.

%=======================================================================

-spec solarized:rows() -> Rows
    when
      Rows :: pos_integer().

rows() ->
    case io:rows() of
        {ok, Rows} when is_integer(Rows) andalso Rows > 0 ->
            Rows;

        _ ->
            25
    end.

%-----------------------------------------------------------------------

-spec solarized:rows(pid()) -> Rows
    when
      Rows :: pos_integer().

rows(Io) when is_pid(Io) ->
    case io:rows(Io) of
        {ok, Rows} when is_integer(Rows) andalso Rows > 0 ->
            Rows;

        _ ->
            25
    end.

%-----------------------------------------------------------------------

-ifdef(TEST).

rows_test_() ->
    [ ?_resultEqual(12, solarized:rows(), enotsup, 12)
    , ?_resultEqual(25, solarized:rows(), 40, enotsup)
    ].

-endif.

%=======================================================================

-spec solarized:nl() -> ok.

nl() ->
    io:nl().

%-----------------------------------------------------------------------

-spec solarized:nl(pid()) -> ok.

nl(Io) when is_pid(Io) ->
    io:nl(Io).

%-----------------------------------------------------------------------

-ifdef(TEST).

nl_test() ->
    Expect = <<$\n>>,
    ?outputEqual(Expect, solarized:nl()).

-endif.


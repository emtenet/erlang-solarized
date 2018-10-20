%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized).

-export([ styled/1
        , text/1, text/2, text/3
        , comment/1, comment/2, comment/3
        , emphasize/1, emphasize/2, emphasize/3
        , blue/1, blue/2, blue/3
        , cyan/1, cyan/2, cyan/3
        , green/1, green/2, green/3
        , magenta/1, magenta/2, magenta/3
        , orange/1, orange/2, orange/3
        , red/1, red/2, red/3
        , violet/1, violet/2, violet/3
        , yellow/1, yellow/2, yellow/3
        , title/2
        , section/2
        , term/2, term/3
        , diff/4, diff/5
        , columns/0
        , rows/0
        , nl/0
        ]).

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

-include("styled.hrl").

-spec solarized:styled(Text) -> ok
    when
      Text :: solarized:styled().

styled(Text) ->
    Styled = styled(?NO_STYLE, [], Text),
    Output = styled_style(Styled, ?NO_STYLE),
    io:put_chars(Output).

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

-spec text(Text) -> ok
    when
      Text :: unicode:chardata().

text(Text) ->
    styled({text, Text}).

%-----------------------------------------------------------------------

-spec text(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;   (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

text(Apply, Text) when is_atom(Apply) ->
    styled({text, Apply, Text});
text(Format, Data) ->
    styled({text, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec text(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

text(Apply, Format, Data) when is_atom(Apply) ->
    styled({text, Apply, io_lib:format(Format, Data)}).

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

-spec comment(Text) -> ok
    when
      Text :: unicode:chardata().

comment(Text) ->
    styled({comment, Text}).

%-----------------------------------------------------------------------

-spec comment(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;      (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

comment(Apply, Text) when is_atom(Apply) ->
    styled({comment, Apply, Text});
comment(Format, Data) ->
    styled({comment, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec comment(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

comment(Apply, Format, Data) when is_atom(Apply) ->
    styled({comment, Apply, io_lib:format(Format, Data)}).

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

-spec emphasize(Text) -> ok
    when
      Text :: unicode:chardata().

emphasize(Text) ->
    styled({emphasize, Text}).

%-----------------------------------------------------------------------

-spec emphasize(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;        (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

emphasize(Apply, Text) when is_atom(Apply) ->
    styled({emphasize, Apply, Text});
emphasize(Format, Data) ->
    styled({emphasize, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec emphasize(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

emphasize(Apply, Format, Data) when is_atom(Apply) ->
    styled({emphasize, Apply, io_lib:format(Format, Data)}).

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

-spec blue(Text) -> ok
    when
      Text :: unicode:chardata().

blue(Text) ->
    styled({blue, Text}).

%-----------------------------------------------------------------------

-spec blue(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;   (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

blue(Apply, Text) when is_atom(Apply) ->
    styled({blue, Apply, Text});
blue(Format, Data) ->
    styled({blue, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec blue(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

blue(Apply, Format, Data) when is_atom(Apply) ->
    styled({blue, Apply, io_lib:format(Format, Data)}).

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

-spec cyan(Text) -> ok
    when
      Text :: unicode:chardata().

cyan(Text) ->
    styled({cyan, Text}).

%-----------------------------------------------------------------------

-spec cyan(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;   (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

cyan(Apply, Text) when is_atom(Apply) ->
    styled({cyan, Apply, Text});
cyan(Format, Data) ->
    styled({cyan, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec cyan(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

cyan(Apply, Format, Data) when is_atom(Apply) ->
    styled({cyan, Apply, io_lib:format(Format, Data)}).

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

-spec green(Text) -> ok
    when
      Text :: unicode:chardata().

green(Text) ->
    styled({green, Text}).

%-----------------------------------------------------------------------

-spec green(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;    (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

green(Apply, Text) when is_atom(Apply) ->
    styled({green, Apply, Text});
green(Format, Data) ->
    styled({green, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec green(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

green(Apply, Format, Data) when is_atom(Apply) ->
    styled({green, Apply, io_lib:format(Format, Data)}).

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

-spec magenta(Text) -> ok
    when
      Text :: unicode:chardata().

magenta(Text) ->
    styled({magenta, Text}).

%-----------------------------------------------------------------------

-spec magenta(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;      (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

magenta(Apply, Text) when is_atom(Apply) ->
    styled({magenta, Apply, Text});
magenta(Format, Data) ->
    styled({magenta, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec magenta(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

magenta(Apply, Format, Data) when is_atom(Apply) ->
    styled({magenta, Apply, io_lib:format(Format, Data)}).

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

-spec orange(Text) -> ok
    when
      Text :: unicode:chardata().

orange(Text) ->
    styled({orange, Text}).

%-----------------------------------------------------------------------

-spec orange(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;     (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

orange(Apply, Text) when is_atom(Apply) ->
    styled({orange, Apply, Text});
orange(Format, Data) ->
    styled({orange, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec orange(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

orange(Apply, Format, Data) when is_atom(Apply) ->
    styled({orange, Apply, io_lib:format(Format, Data)}).

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

-spec red(Text) -> ok
    when
      Text :: unicode:chardata().

red(Text) ->
    styled({red, Text}).

%-----------------------------------------------------------------------

-spec red(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;  (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

red(Apply, Text) when is_atom(Apply) ->
    styled({red, Apply, Text});
red(Format, Data) ->
    styled({red, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec red(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

red(Apply, Format, Data) when is_atom(Apply) ->
    styled({red, Apply, io_lib:format(Format, Data)}).

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

-spec violet(Text) -> ok
    when
      Text :: unicode:chardata().

violet(Text) ->
    styled({violet, Text}).

%-----------------------------------------------------------------------

-spec violet(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;     (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

violet(Apply, Text) when is_atom(Apply) ->
    styled({violet, Apply, Text});
violet(Format, Data) ->
    styled({violet, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec violet(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

violet(Apply, Format, Data) when is_atom(Apply) ->
    styled({violet, Apply, io_lib:format(Format, Data)}).

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

-spec yellow(Text) -> ok
    when
      Text :: unicode:chardata().

yellow(Text) ->
    styled({yellow, Text}).

%-----------------------------------------------------------------------

-spec yellow(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;     (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

yellow(Apply, Text) when is_atom(Apply) ->
    styled({yellow, Apply, Text});
yellow(Format, Data) ->
    styled({yellow, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec yellow(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

yellow(Apply, Format, Data) when is_atom(Apply) ->
    styled({yellow, Apply, io_lib:format(Format, Data)}).

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

%=======================================================================

-spec solarized:term(Style, Term, Options) -> ok
    when
      Style :: application(),
      Term :: term(),
      Options :: solarized_diff:options().

term(Style, Term, Options) ->
    styled(solarized_diff:term(Style, Term, Options)).

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

-ifdef(TEST).

nl_test() ->
    Expect = <<$\n>>,
    ?outputEqual(Expect, solarized:nl()).

-endif.


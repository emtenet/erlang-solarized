%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized).
-include_lib("eunit/include/eunit.hrl").

-export([ output/1
        , text/1, text/2, text/3
        , comment/1, comment/2, comment/3
        , emphasize/1, emphasize/2, emphasize/3
        , underline/1, underline/2, underline/3
        , highlight/1, highlight/2, highlight/3
        , blue/1, blue/2, blue/3
        , cyan/1, cyan/2, cyan/3
        , green/1, green/2, green/3
        , magenta/1, magenta/2, magenta/3
        , orange/1, orange/2, orange/3
        , red/1, red/2, red/3
        , violet/1, violet/2, violet/3
        , yellow/1, yellow/2, yellow/3
        , columns/0
	, rows/0
	, nl/0
        ]).

-export_type([ color/0
             , directive/0
	     , styled/0
             ]).

%=======================================================================

-type color() ::
      text
    | comment
    | emphasize
    | underline
    | highlight
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

-ifdef(EUNIT).

-define(outputEqual(Expect, Expression),
        ?assertEqual((Expect), 
                     solarized_capture:output(fun () -> (Expression) end))).
-define(_outputEqual(Expect, Expression),
        ?_test(?outputEqual(Expect, Expression))).

-define(resultEqual(Expect, Expression, Columns, Rows),
        ?assertEqual((Expect), 
                     element(1, solarized_capture:result_and_output(
                                  fun () -> (Expression) end,
                                  (Columns),
                                  (Rows))))).
-define(_resultEqual(Expect, Expression, Columns, Rows),
        ?_test(?resultEqual(Expect, Expression, Columns, Rows))).

-endif.

%=======================================================================

-spec solarized:output(Text) -> ok
    when
      Text :: solarized:styled().

output(_Text) ->
    ok.

%=======================================================================

-spec text(Text) -> ok
    when
      Text :: unicode:chardata().

text(Text) ->
    output({text, Text}).

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
    output({text, Apply, Text});
text(Format, Data) ->
    output({text, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec text(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

text(Apply, Format, Data) when is_atom(Apply) ->
    output({text, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec comment(Text) -> ok
    when
      Text :: unicode:chardata().

comment(Text) ->
    output({comment, Text}).

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
    output({comment, Apply, Text});
comment(Format, Data) ->
    output({comment, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec comment(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

comment(Apply, Format, Data) when is_atom(Apply) ->
    output({comment, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec emphasize(Text) -> ok
    when
      Text :: unicode:chardata().

emphasize(Text) ->
    output({emphasize, Text}).

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
    output({emphasize, Apply, Text});
emphasize(Format, Data) ->
    output({emphasize, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec emphasize(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

emphasize(Apply, Format, Data) when is_atom(Apply) ->
    output({emphasize, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec underline(Text) -> ok
    when
      Text :: unicode:chardata().

underline(Text) ->
    output({underline, Text}).

%-----------------------------------------------------------------------

-spec underline(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;        (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

underline(Apply, Text) when is_atom(Apply) ->
    output({underline, Apply, Text});
underline(Format, Data) ->
    output({underline, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec underline(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

underline(Apply, Format, Data) when is_atom(Apply) ->
    output({underline, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec highlight(Text) -> ok
    when
      Text :: unicode:chardata().

highlight(Text) ->
    output({highlight, Text}).

%-----------------------------------------------------------------------

-spec highlight(Format, Data) -> ok
    when
      Format :: format(),
      Data :: [term()]
      ;        (Apply, Text) -> ok
    when
      Apply :: solarized:directive(),
      Text :: unicode:chardata().

highlight(Apply, Text) when is_atom(Apply) ->
    output({highlight, Apply, Text});
highlight(Format, Data) ->
    output({highlight, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec highlight(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

highlight(Apply, Format, Data) when is_atom(Apply) ->
    output({highlight, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec blue(Text) -> ok
    when
      Text :: unicode:chardata().

blue(Text) ->
    output({blue, Text}).

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
    output({blue, Apply, Text});
blue(Format, Data) ->
    output({blue, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec blue(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

blue(Apply, Format, Data) when is_atom(Apply) ->
    output({blue, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec cyan(Text) -> ok
    when
      Text :: unicode:chardata().

cyan(Text) ->
    output({cyan, Text}).

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
    output({cyan, Apply, Text});
cyan(Format, Data) ->
    output({cyan, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec cyan(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

cyan(Apply, Format, Data) when is_atom(Apply) ->
    output({cyan, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec green(Text) -> ok
    when
      Text :: unicode:chardata().

green(Text) ->
    output({green, Text}).

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
    output({green, Apply, Text});
green(Format, Data) ->
    output({green, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec green(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

green(Apply, Format, Data) when is_atom(Apply) ->
    output({green, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec magenta(Text) -> ok
    when
      Text :: unicode:chardata().

magenta(Text) ->
    output({magenta, Text}).

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
    output({magenta, Apply, Text});
magenta(Format, Data) ->
    output({magenta, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec magenta(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

magenta(Apply, Format, Data) when is_atom(Apply) ->
    output({magenta, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec orange(Text) -> ok
    when
      Text :: unicode:chardata().

orange(Text) ->
    output({orange, Text}).

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
    output({orange, Apply, Text});
orange(Format, Data) ->
    output({orange, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec orange(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

orange(Apply, Format, Data) when is_atom(Apply) ->
    output({orange, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec red(Text) -> ok
    when
      Text :: unicode:chardata().

red(Text) ->
    output({red, Text}).

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
    output({red, Apply, Text});
red(Format, Data) ->
    output({red, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec red(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

red(Apply, Format, Data) when is_atom(Apply) ->
    output({red, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec violet(Text) -> ok
    when
      Text :: unicode:chardata().

violet(Text) ->
    output({violet, Text}).

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
    output({violet, Apply, Text});
violet(Format, Data) ->
    output({violet, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec violet(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

violet(Apply, Format, Data) when is_atom(Apply) ->
    output({violet, Apply, io_lib:format(Format, Data)}).

%=======================================================================

-spec yellow(Text) -> ok
    when
      Text :: unicode:chardata().

yellow(Text) ->
    output({yellow, Text}).

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
    output({yellow, Apply, Text});
yellow(Format, Data) ->
    output({yellow, io_lib:format(Format, Data)}).

%-----------------------------------------------------------------------

-spec yellow(Apply, Format, Data) -> ok
    when
      Apply :: solarized:directive(),
      Format :: format(),
      Data :: [term()].

yellow(Apply, Format, Data) when is_atom(Apply) ->
    output({yellow, Apply, io_lib:format(Format, Data)}).

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

-ifdef(EUNIT).

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

-ifdef(EUNIT).

rows_test_() ->
    [ ?_resultEqual(12, solarized:rows(), enotsup, 12)
    , ?_resultEqual(25, solarized:rows(), 40, enotsup)
    ].

-endif.

%=======================================================================

-spec solarized:nl() -> ok.

nl() ->
    io:nl().

-ifdef(EUNIT).

nl_test() ->
    Expect = <<$\n>>,
    ?outputEqual(Expect, solarized:nl()).

-endif.


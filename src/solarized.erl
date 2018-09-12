%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized).
-include_lib("eunit/include/eunit.hrl").
-include_lib("solarized/include/eunit.hrl").

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

-spec solarized:styled(Text) -> ok
    when
      Text :: solarized:styled().

styled(_Text) ->
    ok.

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


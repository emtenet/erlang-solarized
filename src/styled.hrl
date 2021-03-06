%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-define(IS_COLOR(C),
        ((C =:= text) orelse
         (C =:= comment) orelse
         (C =:= emphasize) orelse
         (C =:= blue) orelse
         (C =:= cyan) orelse
         (C =:= green) orelse
         (C =:= magenta) orelse
         (C =:= orange) orelse
         (C =:= red) orelse
         (C =:= violet) orelse
         (C =:= yellow))).

-define(IS_BOLD(D), ((D =:= bold) orelse (D =:= bold_off))).
-define(IS_HIGHLIGHT(D), ((D =:= highlight) orelse (D =:= highlight_off))).
-define(IS_REVERSE(D), ((D =:= reverse) orelse (D =:= reverse_off))).
-define(IS_UNDERLINE(D), ((D =:= underline) orelse (D =:= underline_off))).

%-----------------------------------------------------------------------

-define(NO_STYLE, {text, bold_off, highlight_off, reverse_off, underline_off}).

%=======================================================================

-define(E, "\e[").
-define(M, "m").
-define(RESET, "\e[0m").

%-----------------------------------------------------------------------

-define(TEXT, "39").
-define(COMMENT, "38;2;88;110;117").
-define(EMPHASIZE, "38;147;161;161").
-define(BLUE, "38;2;38;139;210").
-define(CYAN, "38;2;42;161;152").
-define(GREEN, "38;2;133;153;0").
-define(MAGENTA, "38;2;211;54;130").
-define(ORANGE, "38;2;203;75;22").
-define(RED, "38;2;220;50;47").
-define(VIOLET, "38;2;108;113;196").
-define(YELLOW, "38;2;181;137;0").

%-----------------------------------------------------------------------

-define(HIGHLIGHT, "48;2;7;54;66").
-define(HIGHLIGHT_OFF, "49").

%-----------------------------------------------------------------------

-define(BOLD, "1").
-define(BOLD_OFF, "22").
-define(REVERSE, "7").
-define(REVERSE_OFF, "27").
-define(UNDERLINE, "4").
-define(UNDERLINE_OFF, "24").

%=======================================================================

-define(STYLED_EXPORT(Name),
    ([Name/1, Name/2, Name/3, Name/4])
).

%-----------------------------------------------------------------------

-define(STYLED_SPEC_1(Name), Name
    (Text) -> ok
        when Text :: unicode:chardata()
).

%-----------------------------------------------------------------------

-define(STYLED_FUNC_1(Name),
    Name(Text) ->
        styled({Name, Text})
).

%-----------------------------------------------------------------------

-define(STYLED_SPEC_2(Name), Name
    (Format, Data) -> ok
        when Format :: format(),
             Data :: [term()];
    (Apply, Text) -> ok
        when Apply :: solarized:directive(),
             Text :: unicode:chardata();
    (pid(), Text) -> ok
        when Text :: unicode:chardata()
).

%-----------------------------------------------------------------------

-define(STYLED_FUNC_2(Name),
    Name(Io, Text) when is_pid(Io) ->
        styled(Io, {Name, Text});
    Name(Apply, Text) when is_atom(Apply) ->
        styled({Name, Apply, Text});
    Name(Format, Data) ->
        styled({Name, io_lib:format(Format, Data)})
).

%-----------------------------------------------------------------------

-define(STYLED_SPEC_3(Name), Name
    (Apply, Format, Data) -> ok
        when Apply :: solarized:directive(),
             Format :: format(),
             Data :: [term()];
    (pid(), Format, Data) -> ok
        when Format :: format(),
             Data :: [term()];
    (pid(), Apply, Text) -> ok
        when Apply :: solarized:directive(),
             Text :: unicode:chardata()
).

%-----------------------------------------------------------------------

-define(STYLED_FUNC_3(Name),
    Name(Io, Apply, Text) when is_pid(Io) andalso is_atom(Apply) ->
        styled({Name, Apply, Text});
    Name(Io, Format, Data) when is_pid(Io) ->
        styled({Name, io_lib:format(Format, Data)});
    Name(Apply, Format, Data) when is_atom(Apply) ->
        styled({Name, Apply, io_lib:format(Format, Data)})
).

%-----------------------------------------------------------------------

-define(STYLED_SPEC_4(Name), Name
    (pid(), Apply, Format, Data) -> ok
        when Apply :: solarized:directive(),
             Format :: format(),
             Data :: [term()]
).

%-----------------------------------------------------------------------

-define(STYLED_FUNC_4(Name),
    Name(Io, Apply, Format, Data) when is_pid(Io) andalso is_atom(Apply) ->
        styled(Io, {Name, Apply, io_lib:format(Format, Data)})
).


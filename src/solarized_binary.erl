%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized_binary).

-export([ sized/2
        , inline/1
        , styled/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-define(ESCAPED(C),
        (C =:= $\n) orelse
        (C =:= $\r) orelse
        (C =:= $\t) orelse
        (C =:= $\v) orelse
        (C =:= $\b) orelse
        (C =:= $\f) orelse
        (C =:= $\e) orelse
        (C =:= $\\) orelse
        (C =:= $\")).

%-----------------------------------------------------------------------

% See: io_lib:printable_unicode_list/1
-define(UNICODE(C),
        (C >= $\040 andalso C =< $\176) orelse
        (C >= 16#A0 andalso C < 16#D800) orelse
        (C > 16#DFFF andalso C < 16#FFFE) orelse
        (C > 16#FFFF andalso C =< 16#10FFFF)).

%=======================================================================

sized(Same, <<>>) ->
    {scalar, Same, 4, <<"<<>>">>};
sized(Same, Binary) ->
    Length = sized(Binary, start, 2),
    {binary, Same, Length, Binary}.

%-----------------------------------------------------------------------

sized([], string, Max) ->
    Max + 3;
sized(<<>>, string, Max) ->
    Max + 3;
sized(<<>>, byte, Max) ->
    Max + 2;
sized(<<C, Rest/binary>>, Mode, Max) when ?ESCAPED(C) ->
    sized(Rest, Mode, Max, string, 2);
sized(Binary, Mode, Max) ->
    case string:next_grapheme(Binary) of
        [C | Rest] when ?UNICODE(C) ->
            sized(Rest, Mode, Max, string, 1);

        [G | Rest] when is_list(G) ->
            sized(Rest, Mode, Max, string, 1);

        _ ->
            <<N, Rest/binary>> = Binary,
            if  N < 10 ->
                    sized(Rest, Mode, Max, byte, 1);

                N < 100 ->
                    sized(Rest, Mode, Max, byte, 2);

                true ->
                    sized(Rest, Mode, Max, byte, 3)
            end
    end.

%-----------------------------------------------------------------------

sized(Rest, Was, Max, Mode, Add) when Mode =:= string ->
    case Was of
        start ->
            sized(Rest, Mode, Max + 1 + Add);

        string ->
            sized(Rest, Mode, Max + Add);

        byte ->
            sized(Rest, Mode, Max + 2 + Add)
    end;
sized(Rest, Was, Max, Mode, Add) when Mode =:= byte ->
    case Was of
        start ->
            sized(Rest, Mode, Max + Add);

        byte ->
            sized(Rest, Mode, Max + 1 + Add);

        string ->
            sized(Rest, Mode, Max + 2 + Add)
    end.

%=======================================================================

inline(Binary) ->
    lists:reverse(inline(Binary, [], empty)).

%-----------------------------------------------------------------------

inline(<<>>, Acc, byte) ->
    [<<">>">> | Acc];
inline(<<>>, Acc, string) ->
    [<<"\">>">> | Acc];
inline(<<C, Rest/binary>>, Acc, Mode)
        when ?ESCAPED(C) ->
    inline_escape(C, Rest, Acc, Mode);
inline(Binary, Acc, Mode) ->
    case string:next_grapheme(Binary) of
        [C, Rest] when ?UNICODE(C) ->
            inline_string(Binary, Rest, Acc, Mode);

        [C | Rest] when ?UNICODE(C) ->
            inline_string(Binary, Rest, Acc, Mode);

        [G | Rest] when is_list(G) ->
            inline_string(Binary, Rest, Acc, Mode);

        _ ->
            inline_byte(Binary, Acc, Mode)
    end.

%-----------------------------------------------------------------------

inline_escape(C, Rest, Acc, empty) ->
    inline_escape_next(C, Rest, [<<"<<\"">> | Acc]);
inline_escape(C, Rest, Acc, byte) ->
    inline_escape_next(C, Rest, [<<",\"">> | Acc]);
inline_escape(C, Rest, Acc, string) ->
    inline_escape_next(C, Rest, Acc).

%-----------------------------------------------------------------------

inline_escape_next(C, Rest, Acc) ->
    Escape = escape(C),
    inline(Rest, [Escape | Acc], string).

%-----------------------------------------------------------------------

inline_string(Start, Stop, Acc, empty) ->
    inline_string(Start, Stop, [<<"<<\"">> | Acc]);
inline_string(Start, Stop, Acc, byte) ->
    inline_string(Start, Stop, [<<",\"">> | Acc]);
inline_string(Start, Stop, Acc, string) ->
    inline_string(Start, Stop, Acc).

%-----------------------------------------------------------------------

inline_string(Start, [], Acc) ->
    [<<"\">>">>, Start | Acc];
inline_string(Start, Binary = <<C, Rest/binary>>, Acc)
        when ?ESCAPED(C) ->
    String = between(Start, Binary),
    Escape = escape(C),
    inline(Rest, [Escape, String | Acc], string);
inline_string(Start, Binary, Acc) ->
    case string:next_grapheme(Binary) of
        [C, Rest] when ?UNICODE(C) ->
            inline_string(Start, Rest, Acc);

        [C | Rest] when ?UNICODE(C) ->
            inline_string(Start, Rest, Acc);

        [G | Rest] when is_list(G) ->
            inline_string(Start, Rest, Acc);

        _ ->
            String = between(Start, Binary),
            inline_byte(Binary, [String | Acc], string)
    end.

%-----------------------------------------------------------------------

inline_byte(Binary, Acc, empty) ->
    inline_byte(Binary, [<<"<<">> | Acc]);
inline_byte(Binary, Acc, byte) ->
    inline_byte(Binary, [<<",">> | Acc]);
inline_byte(Binary, Acc, string) ->
    inline_byte(Binary, [<<"\",">> | Acc]).

%-----------------------------------------------------------------------

inline_byte(Binary, Acc) ->
    {_, B, Rest} = byte(Binary),
    inline(Rest, [B | Acc], byte).

%=======================================================================

% minimum width for a single escaped character or three digit byte
% <<"\n"
% , 255
% >>
% ^^^^^^

styled(W, Binary) when W >= 6 ->
    styled(W, Binary, [], {W, []}, empty).

%-----------------------------------------------------------------------

styled(_, <<>>, Line, Lines, newline) ->
    styled_end(Line, Lines);
styled(_, <<>>, Line, Lines, byte) ->
    styled_end(Line, Lines);
styled(_, <<>>, Line, Lines, string) ->
    styled_end([<<"\"">> | Line], Lines);
styled(W, <<C, Rest/binary>>, Line, Lines, Mode)
        when ?ESCAPED(C) ->
    styled_escape(W, C, Rest, Line, Lines, Mode);
styled(W, Binary, Line, Lines, Mode) ->
    case string:next_grapheme(Binary) of
        [C, Rest] when ?UNICODE(C) ->
            styled_string(W, Binary, Rest, Line, Lines, Mode);

        [C | Rest] when ?UNICODE(C) ->
            styled_string(W, Binary, Rest, Line, Lines, Mode);

        [G | Rest] when is_list(G) ->
            styled_string(W, Binary, Rest, Line, Lines, Mode);

        _ ->
            styled_byte(W, Binary, Line, Lines, Mode)
    end.

%-----------------------------------------------------------------------

styled_escape(W, C, Rest, [], Lines, empty) ->
    styled_escape_next(W - 3, C, Rest, [<<"<<\"">>], Lines);
styled_escape(W, C, Rest, [], Lines, newline) ->
    styled_escape_next(W - 3, C, Rest, [<<", \"">>], Lines);
styled_escape(W, C, Rest, Line, Lines, byte) when W < 5 ->
    Lines1 = {WW, _} = styled_newline(Line, Lines),
    styled_escape_next(WW - 3, C, Rest, [<<", \"">>], Lines1);
styled_escape(W, C, Rest, Line, Lines, byte) ->
    styled_escape_next(W - 2, C, Rest, [<<",\"">> | Line], Lines);
styled_escape(W, C, Rest, Line, Lines, string) when W < 3 ->
    Lines1 = {WW, _} = styled_newline([<<"\"">> | Line], Lines),
    styled_escape_next(WW - 3, C, Rest, [<<", \"">>], Lines1);
styled_escape(W, C, Rest, Line, Lines, string) ->
    styled_escape_next(W, C, Rest, Line, Lines).

%-----------------------------------------------------------------------

styled_escape_next(_, C, Rest, Line, Lines) when C =:= $\n ->
    Escape = escape(C),
    Lines1 = {WW, _} = styled_newline([<<"\"">>, Escape | Line], Lines),
    styled(WW, Rest, [], Lines1, newline);
styled_escape_next(W, C, Rest, Line, Lines) ->
    Escape = escape(C),
    styled(W - 2, Rest, [Escape | Line], Lines, string).

%-----------------------------------------------------------------------

styled_string(W, Start, Stop, [], Lines, empty) ->
    styled_string_rest(W - 4, Start, Stop, [<<"<<\"">>], Lines);
styled_string(W, Start, Stop, [], Lines, newline) ->
    styled_string_rest(W - 4, Start, Stop, [<<", \"">>], Lines);
styled_string(W, Start, Stop, Line, Lines, byte) when W < 4 ->
    Lines1 = {WW, _} = styled_newline(Line, Lines),
    styled_string_rest(WW - 4, Start, Stop, [<<", \"">>], Lines1);
styled_string(W, Start, Stop, Line, Lines, byte) ->
    styled_string_rest(W - 3, Start, Stop, [<<",\"">> | Line], Lines);
styled_string(W, Start, Stop, Line, Lines, string) when W < 2 ->
    Lines1 = {WW, _} = styled_newline([<<"\"">> | Line], Lines),
    styled_string_rest(WW - 4, Start, Stop, [<<", \"">>], Lines1);
styled_string(W, Start, Stop, Line, Lines, string) ->
    styled_string_rest(W - 1, Start, Stop, Line, Lines).

%-----------------------------------------------------------------------

styled_string_next(W, Start, Stop, Rest, Line, Lines) when W < 2 ->
    String = between(Start, Stop),
    Lines1 = {WW, _} = styled_newline([<<"\"">>, String | Line], Lines),
    styled_string_rest(WW - 4, Stop, Rest, [<<", \"">>], Lines1);
styled_string_next(W, Start, _, Rest, Line, Lines) ->
    styled_string_rest(W - 1, Start, Rest, Line, Lines).

%-----------------------------------------------------------------------

styled_string_rest(_, Start, [], Line, Lines) ->
    styled_end([<<"\"">>, Start | Line], Lines);
styled_string_rest(W, Start, Binary = <<C, Rest/binary>>, Line, Lines)
        when ?ESCAPED(C) ->
    String = between(Start, Binary),
    styled_escape(W, C, Rest, [String | Line], Lines, string);
styled_string_rest(W, Start, Binary, Line, Lines) ->
    case string:next_grapheme(Binary) of
        [C, Rest] when ?UNICODE(C) ->
            styled_string_next(W, Start, Binary, Rest, Line, Lines);

        [C | Rest] when ?UNICODE(C) ->
            styled_string_next(W, Start, Binary, Rest, Line, Lines);

        [G | Rest] when is_list(G) ->
            styled_string_next(W, Start, Binary, Rest, Line, Lines);

        _ ->
            String = between(Start, Binary),
            styled_byte(W, Binary, [String | Line], Lines, string)
    end.

%-----------------------------------------------------------------------

styled_byte(W, Binary, [], Lines, empty) ->
    {L, B, Rest} = byte(Binary),
    styled(W - 2 - L, Rest, [B, <<"<<">>], Lines, byte);
styled_byte(W, Binary, [], Lines, newline) ->
    {L, B, Rest} = byte(Binary),
    styled(W - 2 - L, Rest, [B, <<", ">>], Lines, byte);
styled_byte(W, Binary, Line, Lines, byte) ->
    {L, B, Rest} = byte(Binary),
    if W < L + 1 ->
            Lines1 = {WW, _} = styled_newline(Line, Lines),
            styled(WW - 2 - L, Rest, [B, <<", ">>], Lines1, byte);

       true ->
            styled(W - 1 - L, Rest, [B, <<",">> | Line], Lines, byte)
    end;
styled_byte(W, Binary, Line, Lines, string) ->
    {L, B, Rest} = byte(Binary),
    if W < L + 2 ->
            Lines1 = {WW, _} = styled_newline([<<"\"">> | Line], Lines),
            styled(WW - 2 - L, Rest, [B, <<", ">>], Lines1, byte);

       true ->
            styled(W - 2 - L, Rest, [B, <<"\",">> | Line], Lines, byte)
    end.

%-----------------------------------------------------------------------

styled_newline(Line, {W, []}) ->
    {W, [lists:reverse(Line)]};
styled_newline(Line, {W, Lines}) ->
    {W, [lists:reverse(Line), newline | Lines]}.

%-----------------------------------------------------------------------

styled_end([], {_, Lines}) ->
    [<<">>">>, newline | Lines];
styled_end(Line, {_, Lines}) ->
    [<<">>">>, newline, lists:reverse(Line), newline | Lines].

%=======================================================================

between(Start, Stop) ->
    Length = erlang:size(Start) - erlang:size(Stop),
    <<String:Length/binary, Stop/binary>> = Start,
    String.

%-----------------------------------------------------------------------

escape($\n) -> <<"\\n">>;
escape($\r) -> <<"\\r">>;
escape($\t) -> <<"\\t">>;
escape($\v) -> <<"\\v">>;
escape($\b) -> <<"\\b">>;
escape($\f) -> <<"\\f">>;
escape($\e) -> <<"\\e">>;
escape($\\) -> <<"\\\\">>;
escape($\") -> <<"\\\"">>.

%-----------------------------------------------------------------------

byte(<<N, Rest/binary>>) when N < 10 ->
    C = $0 + N,
    {1, <<C>>, Rest};
byte(<<N, Rest/binary>>) when N < 100 ->
    B = $0 + ((N div 10) rem 10),
    C = $0 + (N rem 10),
    {2, <<B, C>>, Rest};
byte(<<N, Rest/binary>>) ->
    A = $0 + ((N div 100) rem 10),
    B = $0 + ((N div 10) rem 10),
    C = $0 + (N rem 10),
    {3, <<A, B, C>>, Rest}.

%=======================================================================

-ifdef(TEST).

inline_1_test_() ->
    Binary = <<"Hello\"\n",0>>,
    Same = diff,
    Sized = {binary, Same, 17, Binary},
    Inline =
        [ <<"<<\"">>, <<"Hello">>, <<"\\\"">>, <<"\\n">>
        , <<"\",">>, <<"0">>, <<">>">>],
    Styled =
        [ <<">>">>
        , newline
        , [<<", ">>, <<"0">>]
        , newline
        , [<<", \"">>, <<"\\\"">>, <<"\\n">>, <<"\"">>]
        , newline
        , [<<"<<\"">>, <<"Hello">>, <<"\"">>]
        ],
    [ ?_assertEqual(Sized, sized(Same, Binary))
    , ?_assertEqual(Inline, inline(Binary))
    , ?_assertEqual(Styled, styled(10, Binary))
    ].

%-----------------------------------------------------------------------

inline_2_test_() ->
    Binary = <<15, 200, "\e[0m">>,
    Same = diff,
    Sized = {binary, Same, 18, Binary},
    Inline =
        [ <<"<<">>, <<"15">>
        , <<",">>, <<"200">>
        , <<",\"">>, <<"\\e">>, <<"[0m">>, <<"\">>">>
        ],
    Styled =
        [ <<">>">>
        , newline
        , [<<", \"">>, <<"\\e">>, <<"[0m">>, <<"\"">>]
        , newline
        , [<<"<<">>, <<"15">>, <<",">>, <<"200">>]
        ],
    [ ?_assertEqual(Sized, sized(Same, Binary))
    , ?_assertEqual(Inline, inline(Binary))
    , ?_assertEqual(Styled, styled(10, Binary))
    ].

%-----------------------------------------------------------------------

inline_3_test_() ->
    Binary = <<"\be", 5, "xt\n">>,
    Same = diff,
    Sized = {binary, Same, 18, Binary},
    Inline =
        [ <<"<<\"">>, <<"\\b">>, <<"e">>
        , <<"\",">>, <<"5">>
        , <<",\"">>, <<"xt">>, <<"\\n">>, <<"\">>">>
        ],
    Styled =
        [ <<">>">>
        , newline
        , [<<", \"">>, <<"xt">>, <<"\\n">>, <<"\"">>]
        , newline
        , [<<"<<\"">>, <<"\\b">>, <<"e">>, <<"\",">>, <<"5">>]
        ],
    [ ?_assertEqual(Sized, sized(Same, Binary))
    , ?_assertEqual(Inline, inline(Binary))
    , ?_assertEqual(Styled, styled(10, Binary))
    ].

%-----------------------------------------------------------------------

vertical_1_test_() ->
    Binary = <<"Hello\n", 0, "World\n", 0>>,
    Expect =
      [ <<">>">>
      , newline
      , [<<", ">>, <<"0">>]
      , newline
      , [<<", ">>, <<"0">>, <<",\"">>, <<"World">>, <<"\\n">>, <<"\"">>]
      , newline
      , [<<"<<\"">>, <<"Hello">>, <<"\\n">>, <<"\"">>]
      ],
    Squash =
      [ <<">>">>
      , newline
      , [<<", ">>, <<"0">>]
      , newline
      , [<<", \"">>, <<"\\n">>, <<"\"">>]
      , newline
      , [<<", \"">>, <<"rld">>, <<"\"">>]
      , newline
      , [<<", ">>, <<"0">>, <<",\"">>, <<"Wo">>, <<"\"">>]
      , newline
      , [<<", \"">>, <<"o">>, <<"\\n">>, <<"\"">>]
      , newline
      , [<<"<<\"">>, <<"Hell">>, <<"\"">>]
      ],
    [ ?_assertEqual(Expect, styled(15, Binary))
    , ?_assertEqual(Squash, styled(8, Binary))
    ].

%-----------------------------------------------------------------------

vertical_2_test_() ->
    Binary = <<0,"\\\fBold\nx\n\vTwo",200,0,200,"\t">>,
    Expect =
      [ <<">>">>
      , newline
      , [<<", ">>, <<"200">>, <<",\"">>, <<"\\t">>, <<"\"">>]
      , newline
      , [<<", ">>, <<"200">>, <<",">>, <<"0">>]
      , newline
      , [<<", \"">>, <<"\\v">>, <<"Two">>, <<"\"">>]
      , newline
      , [<<", \"">>, <<"x">>, <<"\\n">>, <<"\"">>]
      , newline
      , [<<", \"">>, <<"Bold">>, <<"\\n">>, <<"\"">>]
      , newline
      , [ <<"<<">>, <<"0">>, <<",\"">>, <<"\\\\">>, <<"\\f">>, <<"\"">>]
      ],
    [ ?_assertEqual(Expect, styled(10, Binary))
    ].

-endif.


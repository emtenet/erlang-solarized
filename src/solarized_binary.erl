%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized_binary).

-export([ diffed/2
        , sized/2
        , inline/1
        , styled/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("eunit.hrl").
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

diffed(<<>>, <<>>) ->
    Empty = sized(same, <<>>),
    {Empty, Empty};
diffed(Old = <<>>, New) ->
    {sized(diff, Old), sized(diff, New)};
diffed(Old, New = <<>>) ->
    {sized(diff, Old), sized(diff, New)};
diffed(Old, New) ->
    {O, N} = solarized_binary_diff:diff(Old, New),
    Same = case size(Old) =:= size(New) of
        true -> same;
        false -> diff
    end,
    { {binary, Same, sized(Old), O}
    , {binary, Same, sized(New), N}
    }.

%=======================================================================

-ifdef(TEST).

diffed_against_empty_test_() ->
    Empty = <<>>,
    Other = <<"other">>,
    EmptyDiffed = {scalar, diff, 4, <<"<<>>">>},
    OtherDiffed = {binary, diff, 6 + size(Other), [<<>>, Other]},
    [ ?_assertEqual({EmptyDiffed, OtherDiffed}, diffed(Empty, Other))
    , ?_assertEqual({OtherDiffed, EmptyDiffed}, diffed(Other, Empty))
    ].

-endif.

%=======================================================================

sized(Same, <<>>) ->
    {scalar, Same, 4, <<"<<>>">>};
sized(Same = same, Binary) ->
    {binary, Same, sized(Binary), [Binary]};
sized(Same = diff, Binary) ->
    {binary, Same, sized(Binary), [<<>>, Binary]}.

%-----------------------------------------------------------------------

sized(Binary) ->
    sized(Binary, start, 2).

%-----------------------------------------------------------------------

sized([], string, Max) ->
    Max + 3;
sized(<<>>, string, Max) ->
    Max + 3;
sized(<<>>, byte, Max) ->
    Max + 2;
sized(<<C, Rest/binary>>, Mode, Max) when ?ESCAPED(C) ->
    sized_string(Rest, Mode, Max, 2);
sized(Binary, Mode, Max) ->
    case string:next_grapheme(Binary) of
        [C, Rest] when ?UNICODE(C) ->
            sized_string(Rest, Mode, Max, 1);

        [C | Rest] when ?UNICODE(C) ->
            sized_string(Rest, Mode, Max, 1);

        [G | Rest] when is_list(G) ->
            sized_string(Rest, Mode, Max, 1);

        _ ->
            <<N, Rest/binary>> = Binary,
            sized_byte(Rest, Mode, Max, size_of_byte(N))
    end.

%-----------------------------------------------------------------------

sized_string(Rest, Mode, Max, Add) ->
    case Mode of
        start ->
            sized(Rest, string, Max + 1 + Add);

        string ->
            sized(Rest, string, Max + Add);

        byte ->
            sized(Rest, string, Max + 2 + Add)
    end.

%-----------------------------------------------------------------------

sized_byte(Rest, Mode, Max, Add) ->
    case Mode of
        start ->
            sized(Rest, byte, Max + Add);

        byte ->
            sized(Rest, byte, Max + 1 + Add);

        string ->
            sized(Rest, byte, Max + 2 + Add)
    end.

%=======================================================================

inline([Part | Parts]) ->
    inline(Part, Parts, same, [], empty).

%-----------------------------------------------------------------------

inline(<<>>, [], _, Acc, byte) ->
    [<<">>">> | Acc];
inline(<<>>, [], _, Acc, string) ->
    [<<"\">>">> | Acc];
inline(<<>>, [Part | Parts], same, Acc, Mode) ->
    inline(Part, Parts, diff, Acc, Mode);
inline(<<>>, [Part | Parts], diff, Acc, Mode) ->
    inline(Part, Parts, same, Acc, Mode);
inline(<<C, Rest/binary>>, Parts, Same, Acc, Mode)
        when ?ESCAPED(C) ->
    inline_escape(C, Rest, Parts, Same, Acc, Mode);
inline(Binary, Parts, Same, Acc, Mode) ->
    case string:next_grapheme(Binary) of
        [C, Rest] when ?UNICODE(C) ->
            inline_string(Binary, Rest, Parts, Same, Acc, Mode);

        [C | Rest] when ?UNICODE(C) ->
            inline_string(Binary, Rest, Parts, Same, Acc, Mode);

        [G | Rest] when is_list(G) ->
            inline_string(Binary, Rest, Parts, Same, Acc, Mode);

        _ ->
            inline_byte(Binary, Parts, Same, Acc, Mode)
    end.

%-----------------------------------------------------------------------

inline_escape(C, Rest, Parts, Same, Acc, empty) ->
    inline_escape_next(C, Rest, Parts, Same, [<<"<<\"">> | Acc]);
inline_escape(C, Rest, Parts, Same, Acc, byte) ->
    inline_escape_next(C, Rest, Parts, Same, [<<",\"">> | Acc]);
inline_escape(C, Rest, Parts, Same, Acc, string) ->
    inline_escape_next(C, Rest, Parts, Same, Acc).

%-----------------------------------------------------------------------

inline_escape_next(C, Rest, Parts, Same, Acc) ->
    Escape = {Same, escape(C)},
    inline(Rest, Parts, Same, [Escape | Acc], string).

%-----------------------------------------------------------------------

inline_string(Start, Stop, Parts, Same, Acc, empty) ->
    inline_string(Start, Stop, Parts, Same, [<<"<<\"">> | Acc]);
inline_string(Start, Stop, Parts, Same, Acc, byte) ->
    inline_string(Start, Stop, Parts, Same, [<<",\"">> | Acc]);
inline_string(Start, Stop, Parts, Same, Acc, string) ->
    inline_string(Start, Stop, Parts, Same, Acc).

%-----------------------------------------------------------------------

inline_string(Start, [], [], Same, Acc) ->
    [<<"\">>">>, {Same, Start} | Acc];
inline_string(Start, [], Parts, Same, Acc) ->
    inline(<<>>, Parts, Same, [{Same, Start} | Acc], string);
inline_string(Start, Binary = <<C, Rest/binary>>, Parts, Same, Acc)
        when ?ESCAPED(C) ->
    String = {Same, between(Start, Binary)},
    Escape = {Same, escape(C)},
    inline(Rest, Parts, Same, [Escape, String | Acc], string);
inline_string(Start, Binary, Parts, Same, Acc) ->
    case string:next_grapheme(Binary) of
        [C, Rest] when ?UNICODE(C) ->
            inline_string(Start, Rest, Parts, Same, Acc);

        [C | Rest] when ?UNICODE(C) ->
            inline_string(Start, Rest, Parts, Same, Acc);

        [G | Rest] when is_list(G) ->
            inline_string(Start, Rest, Parts, Same, Acc);

        _ ->
            String = {Same, between(Start, Binary)},
            inline_byte(Binary, Parts, Same, [String | Acc], string)
    end.

%-----------------------------------------------------------------------

inline_byte(Binary, Parts, Same, Acc, empty) ->
    inline_byte(Binary, Parts, Same, [<<"<<">> | Acc]);
inline_byte(Binary, Parts, Same, Acc, byte) ->
    inline_byte(Binary, Parts, Same, [<<",">> | Acc]);
inline_byte(Binary, Parts, Same, Acc, string) ->
    inline_byte(Binary, Parts, Same, [<<"\",">> | Acc]).

%-----------------------------------------------------------------------

inline_byte(Binary, Parts, Same, Acc) ->
    {_, B, Rest} = byte(Binary),
    inline(Rest, Parts, Same, [{Same, B} | Acc], byte).

%=======================================================================

% minimum width for a single escaped character or three digit byte
% <<"\n"
% , 255
% >>
% ^^^^^^

-record(styled, {
          width :: pos_integer(),
          lines :: list(),
          part :: same | diff,
          parts :: list()
         }).

styled(W, [Part | Parts]) when W >= 6 ->
    Lines = #styled{width = W, lines = [], part = same, parts = Parts},
    styled(W, Part, [], Lines, empty).

%-----------------------------------------------------------------------

styled(W, <<>>, Line, Lines, Mode) ->
    styled_part(W, Line, Lines, Mode);
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
    Lines1 = styled_newline(Line, Lines),
    Width = Lines1#styled.width,
    styled_escape_next(Width - 3, C, Rest, [<<", \"">>], Lines1);
styled_escape(W, C, Rest, Line, Lines, byte) ->
    styled_escape_next(W - 2, C, Rest, [<<",\"">> | Line], Lines);
styled_escape(W, C, Rest, Line, Lines, string) when W < 3 ->
    Lines1 = styled_newline([<<"\"">> | Line], Lines),
    Width = Lines1#styled.width,
    styled_escape_next(Width - 3, C, Rest, [<<", \"">>], Lines1);
styled_escape(W, C, Rest, Line, Lines, string) ->
    styled_escape_next(W, C, Rest, Line, Lines).

%-----------------------------------------------------------------------

styled_escape_next(_, C, Rest, Line, Lines) when C =:= $\n ->
    Escape = escape(C),
    Line1 = styled_text(Escape, Line, Lines),
    Lines1 = styled_newline([<<"\"">> | Line1], Lines),
    Width = Lines1#styled.width,
    styled(Width, Rest, [], Lines1, newline);
styled_escape_next(W, C, Rest, Line, Lines) ->
    Escape = escape(C),
    Line1 = styled_text(Escape, Line, Lines),
    styled(W - 2, Rest, Line1, Lines, string).

%-----------------------------------------------------------------------

styled_string(W, Start, Stop, [], Lines, empty) ->
    styled_string_rest(W - 4, Start, Stop, [<<"<<\"">>], Lines);
styled_string(W, Start, Stop, [], Lines, newline) ->
    styled_string_rest(W - 4, Start, Stop, [<<", \"">>], Lines);
styled_string(W, Start, Stop, Line, Lines, byte) when W < 4 ->
    Lines1 = styled_newline(Line, Lines),
    Width = Lines1#styled.width,
    styled_string_rest(Width - 4, Start, Stop, [<<", \"">>], Lines1);
styled_string(W, Start, Stop, Line, Lines, byte) ->
    styled_string_rest(W - 3, Start, Stop, [<<",\"">> | Line], Lines);
styled_string(W, Start, Stop, Line, Lines, string) when W < 2 ->
    Lines1 = styled_newline([<<"\"">> | Line], Lines),
    Width = Lines1#styled.width,
    styled_string_rest(Width - 4, Start, Stop, [<<", \"">>], Lines1);
styled_string(W, Start, Stop, Line, Lines, string) ->
    styled_string_rest(W - 1, Start, Stop, Line, Lines).

%-----------------------------------------------------------------------

styled_string_next(W, Start, Stop, Rest, Line, Lines) when W < 2 ->
    String = between(Start, Stop),
    Line1 = styled_text(String, Line, Lines),
    Lines1 = styled_newline([<<"\"">> | Line1], Lines),
    Width = Lines1#styled.width,
    styled_string_rest(Width - 4, Stop, Rest, [<<", \"">>], Lines1);
styled_string_next(W, Start, _, Rest, Line, Lines) ->
    styled_string_rest(W - 1, Start, Rest, Line, Lines).

%-----------------------------------------------------------------------

styled_string_rest(W, Start, [], Line, Lines) ->
    Line1 = styled_text(Start, Line, Lines),
    styled_part(W, Line1, Lines, string);
styled_string_rest(W, Start, Binary = <<C, Rest/binary>>, Line, Lines)
        when ?ESCAPED(C) ->
    String = between(Start, Binary),
    Line1 = styled_text(String, Line, Lines),
    styled_escape(W, C, Rest, Line1, Lines, string);
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
            Line1 = styled_text(String, Line, Lines),
            styled_byte(W, Binary, Line1, Lines, string)
    end.

%-----------------------------------------------------------------------

styled_byte(W, Binary, [], Lines, empty) ->
    Byte = byte(Binary),
    styled_byte_next(W - 2, Byte, [<<"<<">>], Lines);
styled_byte(W, Binary, [], Lines, newline) ->
    Byte = byte(Binary),
    styled_byte_next(W - 2, Byte, [<<", ">>], Lines);
styled_byte(W, Binary, Line, Lines, byte) ->
    Byte = {L, _, _} = byte(Binary),
    if W < L + 1 ->
            Lines1 = styled_newline(Line, Lines),
            Width = Lines1#styled.width,
            styled_byte_next(Width - 2, Byte, [<<", ">>], Lines1);

       true ->
            styled_byte_next(W - 1, Byte, [<<",">> | Line], Lines)
    end;
styled_byte(W, Binary, Line, Lines, string) ->
    Byte = {L, _, _} = byte(Binary),
    if W < L + 2 ->
            Lines1 = styled_newline([<<"\"">> | Line], Lines),
            Width = Lines1#styled.width,
            styled_byte_next(Width - 2, Byte, [<<", ">>], Lines1);

       true ->
            styled_byte_next(W - 2, Byte, [<<"\",">> | Line], Lines)
    end.

%-----------------------------------------------------------------------

styled_byte_next(W, {L, B, Rest}, Line, Lines) ->
    Line1 = styled_text(B, Line, Lines),
    styled(W - L, Rest, Line1, Lines, byte).

%-----------------------------------------------------------------------

styled_text(Text, Line, #styled{part = Same}) ->
    [{Same, Text} | Line].

%-----------------------------------------------------------------------

styled_newline(Line, Lines = #styled{lines = []}) ->
    Lines#styled{lines = [Line]};
styled_newline(Line, Lines = #styled{lines = Lines0}) ->
    Lines#styled{lines = [Line, newline | Lines0]}.

%-----------------------------------------------------------------------

styled_part(_, Line, Lines = #styled{parts = []}, newline) ->
    styled_end(Line, Lines);
styled_part(_, Line, Lines = #styled{parts = []}, byte) ->
    styled_end(Line, Lines);
styled_part(_, Line, Lines = #styled{parts = []}, string) ->
    styled_end([<<"\"">> | Line], Lines);
styled_part(W, Line, Lines = #styled{part = same}, Mode) ->
    [Part | Parts] = Lines#styled.parts,
    styled(W, Part, Line, Lines#styled{part = diff, parts = Parts}, Mode);
styled_part(W, Line, Lines = #styled{part = diff}, Mode) ->
    [Part | Parts] = Lines#styled.parts,
    styled(W, Part, Line, Lines#styled{part = same, parts = Parts}, Mode).

%-----------------------------------------------------------------------

styled_end([], #styled{lines = Lines}) ->
    [<<">>">>, newline | Lines];
styled_end(Line, #styled{lines = []}) ->
    [<<">>">>, newline, Line];
styled_end(Line, #styled{lines = Lines}) ->
    [<<">>">>, newline, Line, newline | Lines].

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

size_of_byte(N) when N < 10 ->
    1;
size_of_byte(N) when N < 100 ->
    2;
size_of_byte(_) ->
    3.

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
    Same = same,
    Parts = [Binary],
    Sized = {binary, Same, 17, Parts},
    Inline =
        [ <<">>">>
        , {same, <<"0">>}
        , <<"\",">>
        , {same, <<"\\n">>}
        , {same, <<"\\\"">>}
        , {same, <<"Hello">>}
        , <<"<<\"">>
        ],
    Styled =
        [ <<">>">>
        , newline
        , [{same, <<"0">>}, <<", ">>]
        , newline
        , [<<"\"">>, {same, <<"\\n">>}, {same, <<"\\\"">>}, <<", \"">>]
        , newline
        , [<<"\"">>, {same, <<"Hello">>}, <<"<<\"">>]
        ],
    [ ?_assertEqual(Sized, sized(Same, Binary))
    , ?_assertEqual(Inline, inline(Parts))
    , ?_assertEqual(Styled, styled(10, Parts))
    ].

%-----------------------------------------------------------------------

inline_2_test_() ->
    Binary = <<15, 200, "\e[0m">>,
    Same = diff,
    Parts = [<<>>, Binary],
    Sized = {binary, Same, 18, Parts},
    Inline =
        [ <<"\">>">>
        , {diff, <<"[0m">>}
        , {diff, <<"\\e">>}
        , <<",\"">>
        , {diff, <<"200">>}
        , <<",">>
        , {diff, <<"15">>}
        , <<"<<">>
        ],
    Styled =
        [ <<">>">>
        , newline
        , [<<"\"">>, {diff, <<"[0m">>}, {diff, <<"\\e">>}, <<", \"">>]
        , newline
        , [{diff, <<"200">>}, <<",">>, {diff, <<"15">>}, <<"<<">>]
        ],
    [ ?_assertEqual(Sized, sized(Same, Binary))
    , ?_assertEqual(Inline, inline(Parts))
    , ?_assertEqual(Styled, styled(10, Parts))
    ].

%-----------------------------------------------------------------------

inline_3_test_() ->
    Binary = <<"\be", 5, "xt\n">>,
    Same = same,
    Parts = [Binary],
    Sized = {binary, Same, 18, Parts},
    Inline =
        [ <<"\">>">>
        , {same, <<"\\n">>}
        , {same, <<"xt">>}
        , <<",\"">>
        , {same, <<"5">>}
        , <<"\",">>
        , {same, <<"e">>}
        , {same, <<"\\b">>}
        , <<"<<\"">>
        ],
    Styled =
        [ <<">>">>
        , newline
        , [<<"\"">>, {same, <<"\\n">>}, {same, <<"xt">>}, <<", \"">>]
        , newline
        , [ {same, <<"5">>}
          , <<"\",">>
          , {same, <<"e">>}
          , {same, <<"\\b">>}
          , <<"<<\"">>
          ]
        ],
    [ ?_assertEqual(Sized, sized(Same, Binary))
    , ?_assertEqual(Inline, inline(Parts))
    , ?_assertEqual(Styled, styled(10, Parts))
    ].

%-----------------------------------------------------------------------

vertical_1_test_() ->
    Binary = [<<"Hello\n">>, <<0, "World\n">>, <<0>>],
    Expect =
      [ <<">>">>
      , newline
      , [{same, <<"0">>}, <<", ">>]
      , newline
      , [ <<"\"">>
        , {diff, <<"\\n">>}
        , {diff, <<"World">>}
        , <<",\"">>
        , {diff, <<"0">>}
        , <<", ">>
        ]
      , newline
      , [<<"\"">>, {same, <<"\\n">>}, {same, <<"Hello">>}, <<"<<\"">>]
      ],
    Squash =
      [ <<">>">>
      , newline
      , [{same, <<"0">>}, <<", ">>]
      , newline
      , [<<"\"">>, {diff, <<"\\n">>}, <<", \"">>]
      , newline
      , [<<"\"">>, {diff, <<"rld">>}, <<", \"">>]
      , newline
      , [<<"\"">>, {diff, <<"Wo">>}, <<",\"">>, {diff, <<"0">>}, <<", ">>]
      , newline
      , [<<"\"">>, {same, <<"\\n">>}, {same, <<"o">>}, <<", \"">>]
      , newline
      , [<<"\"">>, {same, <<"Hell">>}, <<"<<\"">>]
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
      , [<<"\"">>, {diff, <<"\\t">>}, <<",\"">>, {diff, <<"200">>}, <<", ">>]
      , newline
      , [{diff, <<"0">>}, <<",">>, {diff, <<"200">>}, <<", ">>]
      , newline
      , [<<"\"">>, {diff, <<"Two">>}, {diff, <<"\\v">>}, <<", \"">>]
      , newline
      , [<<"\"">>, {diff, <<"\\n">>}, {diff, <<"x">>}, <<", \"">>]
      , newline
      , [<<"\"">>, {diff, <<"\\n">>}, {diff, <<"Bold">>}, <<", \"">>]
      , newline
      , [ <<"\"">>
        , {diff, <<"\\f">>}
        , {diff, <<"\\\\">>}
        , <<",\"">>
        , {diff, <<"0">>}
        , <<"<<">>
        ]
      ],
    [ ?_assertEqual(Expect, styled(10, [<<>>, Binary]))
    ].

%-----------------------------------------------------------------------

black_cat_test() ->
    OldBinary = <<"The black cat in the hat?">>,
    NewBinary = <<"The cat in the black hat!">>,
    {OldDiff, NewDiff} = solarized_binary_diff:diff(OldBinary, NewBinary),
    OldInline =
        [ <<"\">>">>
        , {diff, <<"?">>}
        , {same, <<"hat">>}
        , {same, <<"cat in the ">>}
        , {diff, <<"black ">>}
        , {same, <<"The ">>}
        , <<"<<\"">>
        ],
    NewInline =
        [ <<"\">>">>
        , {diff, <<"!">>}
        , {same, <<"hat">>}
        , {diff, <<"k ">>}
        , {diff, <<"blac">>}
        , {same, <<"cat in the ">>}
        , {same, <<"The ">>}
        , <<"<<\"">>
        ],
    OldStyled =
        [ <<">>">>
        , newline
        , [ <<"\"">>
          , {diff, <<"?">>}
          , {same, <<"hat">>}
          , {same, <<" the ">>}
          , <<", \"">>
          ]
        , newline
        , [ <<"\"">>
          , {same, <<"cat in">>}
          , {diff, <<"black ">>}
          , {same, <<"The ">>}
          , <<"<<\"">>
          ]
        ],
    NewStyled =
        [ <<">>">>
        , newline
        , [ <<"\"">>
          , {diff, <<"!">>}
          , {same, <<"hat">>}
          , {diff, <<"k ">>}
          , {diff, <<"blac">>}
          , {same, <<"cat in the ">>}
          , {same, <<"The ">>}
          , <<"<<\"">>
          ]
        ],
    ?assertEqual(OldInline, solarized_binary:inline(OldDiff)),
    ?assertEqual(NewInline, solarized_binary:inline(NewDiff)),
    ?assertEqual(OldStyled, solarized_binary:styled(20, OldDiff)),
    ?assertEqual(NewStyled, solarized_binary:styled(30, NewDiff)).


%-----------------------------------------------------------------------

-endif.


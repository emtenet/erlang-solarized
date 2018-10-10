%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized_binary_diff).

-export([ diff/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% A diff implementation based on:
% - https://neil.fraser.name/writing/diff/
%
% More complete implementations already exist:
% - https://github.com/mmzeeman/diffy
% - https://github.com/tomas-abrahamsson/tdiff/
%
% This is a new implementation because:
% - the output needs to be matched to solarized_diff's needs,
% - we can give up early and still get good results for solarized_eunit,
% - we want binary and list implementation that can have different trade offs.

%=======================================================================

% Compare two binaries (Left, Right)
% return pair of {Left, Right} difference lists
% where the lists are in the form
%   DiffList = [Same0, Diff1, Same1, Diff2, Same2, ...]
% and
%   SameX alternates with DiffX
% and
%   SameX & DiffX are sub-binaries of the initial binaries

diff(Same, Same) ->
    {[Same], [Same]};
diff(Left, Right) ->
    common_prefix(Left, Right).

%=======================================================================

% used in recursive calls to diff()
% ASSUMES:
% - no common prefix or suffix
% - head of Lt & Rt is a *same* binary
% - returns difference lists with a *diff* head

diff(L, R, Lt, Rt) ->
    single_edit(L, R, Lt, Rt).

%=======================================================================

common_prefix(Left, Right) ->
    case binary:longest_common_prefix([Left, Right]) of
        0 ->
            common_prefix_next(Left, Right, <<>>);

        N ->
            <<Prefix:N/binary, L/binary>> = Left,
            <<_:N/binary, R/binary>> = Right,
            common_prefix_next(L, R, Prefix)
    end.

%-----------------------------------------------------------------------

common_prefix_next(L, R, Prefix) ->
    {Lt, Rt} = common_suffix(L, R),
    {[Prefix | Lt], [Prefix | Rt]}.

%=======================================================================

common_suffix(Left, Right) ->
    case binary:longest_common_suffix([Left, Right]) of
        0 ->
            single_edit(Left, Right, [], []);

        N ->
            Ln = size(Left) - N,
            <<Lm:Ln/binary, Suffix:N/binary>> = Left,
            Rn = size(Right) - N,
            <<Rm:Rn/binary, _/binary>> = Right,
            single_edit(Lm, Rm, [Suffix], [Suffix])
    end.

%=======================================================================

single_edit(L, R, Lt, Rt) when L =:= <<>> orelse R =:= <<>> ->
    {[L | Lt], [R | Rt]};
single_edit(L, R, Lt, Rt) ->
    case size(L) < size(R) of
        true ->
            two_edits(L, R, Lt, Rt);

        false ->
            {Rs, Ls} = two_edits(R, L, Rt, Lt),
            {Ls, Rs}
    end.

%=======================================================================

% L is smaller than R

two_edits(L, R, Lt, Rt) ->
    case binary:match(R, L) of
        {Start, Length} ->
            <<Before:Start/binary, _:Length/binary, After/binary>> = R,
            {[<<>>, L, <<>> | Lt], [Before, L, After | Rt]};

        nomatch when size(L) =:= 1 ->
            {[L | Lt], [R | Rt]};

        nomatch ->
            half_match(L, R, Lt, Rt)
    end.

%=======================================================================

% L is smaller than R

half_match(L, R, Lt, Rt)
        when size(R) < 10 orelse
             size(R) > (size(L) * 2) ->
    % not worth it
    bisect(L, R, Lt, Rt);
half_match(L, R, Lt, Rt) ->
    Rn = size(R),
    A = half_match_seed(L, R, ceil(Rn / 4), floor(Rn / 4)),
    B = half_match_seed(L, R, ceil(Rn / 2), floor(Rn / 4)),
    case half_match_best(A, B) of
        nomatch ->
            bisect(L, R, Lt, Rt);

        {Mn, Li, Ri} ->
            <<Lpre:Li/binary, M:Mn/binary, Lpost/binary>> = L,
            <<Rpre:Ri/binary, _:Mn/binary, Rpost/binary>> = R,
            {La, Ra} = diff(Lpost, Rpost, Lt, Rt),
            diff(Lpre, Rpre, [M | La], [M | Ra])
    end.

%-----------------------------------------------------------------------

half_match_seed(L, R, Start, Length) when Length > 0 ->
    <<_:Start/binary, Seed:Length/binary, _/binary>> = R,
    half_match_slide(L, R, Seed, Start, Length, 0, {0, 0, 0}).

%-----------------------------------------------------------------------

half_match_slide(L, R, S, Si, Sn, Li, B = {Bn, _, _}) ->
    case binary:match(L, S, [{scope, {Li, size(L) - Li}}]) of
        nomatch ->
            case Bn >= size(R) div 2 of
                true ->
                    B;

                false ->
                    {0, 0, 0}
            end;

        {Mi, _} ->
            <<Lpre:Mi/binary, _:Sn/binary, Lpost/binary>> = L,
            <<Rpre:Si/binary, _:Sn/binary, Rpost/binary>> = R,
            Npre = binary:longest_common_suffix([Lpre, Rpre]),
            Npost = binary:longest_common_prefix([Lpost, Rpost]),
            Mn = Npre + Sn + Npost,
            case Mn > Bn of
                true ->
                    Best = {Mn, Mi - Npre, Si - Npre},
                    half_match_slide(L, R, S, Si, Sn, Mi + 1, Best);

                false ->
                    half_match_slide(L, R, S, Si, Sn, Mi + 1, B)
            end
    end.

%-----------------------------------------------------------------------

half_match_best({0, _, _}, {0, _, _}) ->
    nomatch;
half_match_best({0, _, _}, Best) ->
    Best;
half_match_best(Best, {0, _, _}) ->
    Best;
half_match_best(Best = {B, _, _}, {A, _, _}) when B > A ->
    Best;
half_match_best(_, Best) ->
    Best.

%=======================================================================

bisect(L, R, Lt, Rt) ->
    case bisect(L, R) of
        give_up ->
            {[L | Lt], [R | Rt]};

        {M, Lpre, Lpost, Rpre, Rpost} ->
            {La, Ra} = diff(Lpost, Rpost, Lt, Rt),
            diff(Lpre, Rpre, [M | La], [M | Ra])
    end.

%=======================================================================

-record(bisect, {
          x  :: binary(),
          y  :: binary(),
          xn :: pos_integer(),
          yn :: pos_integer(),
          delta :: integer()
         }).

% after N steps:
% - each end has created V lengths of: 1 + 2 + 3 + ... + N/2
% - total sum = (N*N + 2N) / 4
% - total sum for (N = 1000) = 250,500
-define(MAX_BISECT, 1000).

bisect(X, Y) ->
    B = bisect_init(X, Y),
    Delta = B#bisect.delta,
    Stop = min(B#bisect.xn + B#bisect.yn - 1, ?MAX_BISECT),
    case Delta rem 2 of
        0 ->
            % delta is even
            % do back first
            Back = {Delta, [B#bisect.xn], zig, Delta - 1},
            % back will produce a zag compatible V so start fore with a zag
            Fore = {0, [0], zag, 1},
            bisect_back(B, Stop, Fore, Back);

        _ ->
            % delta is odd
            Fore = {0, [0], zig, -1},
            Back = {Delta, [B#bisect.xn], zig, Delta - 1},
            bisect_fore(B, Stop, Fore, Back)
    end.

%-----------------------------------------------------------------------

bisect_init(X, Y) ->
    Xn = size(X),
    Yn = size(Y),
    Delta = Xn - Yn,
    #bisect{x = X, y = Y, xn = Xn, yn = Yn, delta = Delta}.

%-----------------------------------------------------------------------

bisect_answer(#bisect{x = X, y = Y}, {match, Mn, Xi, Yi}) ->
    <<Xpre:Xi/binary, M:Mn/binary, Xpost/binary>> = X,
    <<Ypre:Yi/binary, _:Mn/binary, Ypost/binary>> = Y,
    {M, Xpre, Xpost, Ypre, Ypost}.

%-----------------------------------------------------------------------

bisect_fore(_, 0, _, _) ->
    give_up;
bisect_fore(B, Stop, {_, ForeV, ForeZig, ForeK}, Back = {BackK, BackV, _, _}) ->
    Reverse = {BackK, BackV},
    case bisect_scan(B, fore, ForeZig, ForeK, ForeV, Reverse) of
        Match = {match, _, _, _} ->
            bisect_answer(B, Match);

        NextFore ->
            bisect_back(B, Stop - 1, NextFore, Back)
    end.

%-----------------------------------------------------------------------

bisect_back(_, 0, _, _) ->
    give_up;
bisect_back(B, Stop, NextFore, {_, BackV, BackZig, BackK}) ->
    case bisect_scan(B, back, BackZig, BackK, BackV, no_match) of
        Match = {match, _, _, _} ->
            % should not happen since we sent no_match
            throw({bisect_back, B, Match});

        NextBack ->
            bisect_fore(B, Stop - 1, NextFore, NextBack)
    end.

%-----------------------------------------------------------------------

bisect_scan(B, Dir, Zig, K, V, Reverse) ->
    bisect_scan(B, Dir, Zig, K, [out | V], [], Reverse).

%-----------------------------------------------------------------------

bisect_scan(_, _, zig, K, [], Acc, _) ->
    % we stopped with K two (2) beyond the last accumulated item
    % next zag will start with K one (1) wider
    {K - 2, Acc, zag, K - 1};
bisect_scan(_, _, zag, K, [], Acc, _) ->
    % we stopped with K two (2) beyond the last accumulated item
    % next zag will start with K one (1) wider
    {K + 2, Acc, zig, K + 1};
bisect_scan(B, Dir, Zig, K, V, Acc, Reverse) ->
    %X = bisect_move(Zig, Dir, V),
    Moves = bisect_move_choices(Zig, V),
    Move = bisect_move_choose(Dir, Moves),
    Moved = bisect_move_by(B, K, Move),
    Followed = bisect_follow_k(B, Dir, Moved, K),
    case bisect_match(Zig, Followed, K, Reverse) of
        Match = {match, _, _, _} ->
            Match;

        NextReverse ->
            NextK = case Zig of
                zig -> K + 2;
                zag -> K - 2
            end,
            NextV = tl(V),
            NextAcc = case Followed of
                %out when Acc =:= [] -> [];
                _ -> [Followed | Acc]
            end,
            bisect_scan(B, Dir, Zig, NextK, NextV, NextAcc, NextReverse)
    end.

%-----------------------------------------------------------------------

% original:
%   if k == -D || ( k /= D && V[k - 1] < V[K + 1] )
%   then x = V[k + 1]
%   else x = V[k - 1] + 1
% invert from x-world to y-world
%   if k == -D || ( k /= D && V[k - 1] < V[K + 1] )
%   then x = V[k - 1]
%   else x = V[k + 1] - 1

bisect_move_choices(zig, [Neg]) ->
    {Neg, out};
bisect_move_choices(zig, [Neg, Pos | _]) ->
    {Neg, Pos};
bisect_move_choices(zag, [Pos]) ->
    {out, Pos};
bisect_move_choices(zag, [Pos, Neg | _]) ->
    {Neg, Pos}.

%-----------------------------------------------------------------------

bisect_move_choose(fore, {out, Pos}) ->
    {down, Pos};
bisect_move_choose(fore, {Neg, out}) ->
    {right, Neg};
bisect_move_choose(fore, {Neg, Pos}) when Neg < Pos ->
    {down, Pos};
bisect_move_choose(fore, {Neg, _}) ->
    {right, Neg};
bisect_move_choose(back, {Neg, out}) ->
    {up, Neg};
bisect_move_choose(back, {out, Pos}) ->
    {left, Pos};
bisect_move_choose(back, {Neg, Pos}) when Neg < Pos ->
    {up, Neg};
bisect_move_choose(back, {_, Pos}) ->
    {left, Pos}.

%-----------------------------------------------------------------------

bisect_move_by(_, _, {_, out}) ->
    out;
bisect_move_by(B, _, {right, X}) when X < B#bisect.xn ->
    X + 1;
bisect_move_by(B, K, {down, X}) when X - K =< B#bisect.yn ->
    X;
bisect_move_by(_, _, {left, X}) when X > 0 ->
    X - 1;
bisect_move_by(_, K, {up, X}) when X - K >= 0 ->
    X;
bisect_move_by(_, _, _) ->
    out.

-ifdef(TEST).

bisect_move_by_test_() ->
    B = #bisect{xn = 10, yn = 10},
    % note: k = x - y || x = k + y || y = x - k
    [ ?_assertEqual(10,  bisect_move_by(B, +5, {right, 9}))
    , ?_assertEqual(out, bisect_move_by(B, +5, {right, 10}))
    % DOWN: y = 10, k = -5, x = -5 + 10 = 5
    , ?_assertEqual(5,   bisect_move_by(B, -5, {down, 5}))
    % FAIL DOWN: y = 11, k = -5, x = -5 + 11 = 6
    , ?_assertEqual(out, bisect_move_by(B, -5, {down, 6}))
    , ?_assertEqual(0,   bisect_move_by(B, -5, {left, 1}))
    , ?_assertEqual(out, bisect_move_by(B, -5, {left, 0}))
    % OK UP: y = 0, k = +5, x = +5 + 0 = 5
    , ?_assertEqual(5,   bisect_move_by(B, +5, {up, 5}))
    % FAIL UP: y = -1, k = +5, x = +5 + -1 = 4
    , ?_assertEqual(out, bisect_move_by(B, +5, {up, 4}))
    ].

-endif.

%-----------------------------------------------------------------------

bisect_follow_k(_, _, out, _) ->
    out;
bisect_follow_k(B, Dir, Xi, K) ->
    bisect_follow_xy(B, Dir, Xi, Xi - K).

%-----------------------------------------------------------------------

bisect_follow_xy(B, fore, Xi, Yi)
        when Xi < B#bisect.xn andalso Yi < B#bisect.yn ->
    case binary:at(B#bisect.x, Xi) =:= binary:at(B#bisect.y, Yi) of
        true ->
            bisect_follow_xy(B, fore, Xi + 1, Yi + 1);

        false ->
            Xi
    end;
bisect_follow_xy(B, back, Xi, Yi)
        when Xi > 0 andalso Yi > 0 ->
    case binary:at(B#bisect.x, Xi - 1) =:= binary:at(B#bisect.y, Yi - 1) of
        true ->
            bisect_follow_xy(B, back, Xi - 1, Yi - 1);

        false ->
            Xi
    end;
bisect_follow_xy(_, _, Xi, _) ->
    Xi.

%-----------------------------------------------------------------------

% ASSUME: Only called in the fore direction

bisect_match(_, _, _, Reverse = no_match) ->
    Reverse;
bisect_match(_, _, _, Reverse = {_, []}) ->
    Reverse;
bisect_match(zig, _, K, Reverse = {RevK, _}) when K < RevK ->
    Reverse;
bisect_match(zag, _, K, Reverse = {RevK, _}) when K > RevK ->
    Reverse;
bisect_match(Zig, ForeX, K, {K, [BackX | RevV]}) when ForeX < BackX ->
    case Zig of
        zig -> {K + 2, RevV};
        zag -> {K - 2, RevV}
    end;
bisect_match(_, ForeX, K, {K, [BackX | _]}) ->
    {match, ForeX - BackX, BackX, BackX - K};
bisect_match(Zig, Followed, K, {RevK, [_ | RevV]}) ->
    case Zig of
        zig ->
            bisect_match(Zig, Followed, K, {RevK + 2, RevV});

        zag ->
            bisect_match(Zig, Followed, K, {RevK + 2, RevV})
    end.

%=======================================================================

-ifdef(TEST).

fraser_1_1_test() ->
    Old = <<"Equality">>,
    New = Old,
    Expect = {[Old], [New]},
    ?assertEqual(Expect, solarized_binary_diff:diff(Old, New)).

%-----------------------------------------------------------------------

fraser_1_2_test() ->
    Old = <<"The cat in the hat.">>,
    New = <<"The dog in the hat.">>,
    Expect =
        { [<<"The ">>, <<"cat">>, <<" in the hat.">>]
        , [<<"The ">>, <<"dog">>, <<" in the hat.">>]
        },
    ?assertEqual(Expect, solarized_binary_diff:diff(Old, New)).

%-----------------------------------------------------------------------

fraser_1_3_a_test() ->
    Old = <<"The cat in the hat.">>,
    New = <<"The furry cat in the hat.">>,
    Expect =
        { [<<"The ">>, <<>>, <<"cat in the hat.">>]
        , [<<"The ">>, <<"furry ">>, <<"cat in the hat.">>]
        },
    ?assertEqual(Expect, solarized_binary_diff:diff(Old, New)).

%-----------------------------------------------------------------------

fraser_1_3_b_test() ->
    Old = <<"The cat in the hat.">>,
    New = <<"The cat.">>,
    Expect =
        { [<<"The cat">>, <<" in the hat">>, <<".">>]
        , [<<"The cat">>, <<>>, <<".">>]
        },
    ?assertEqual(Expect, solarized_binary_diff:diff(Old, New)).

%-----------------------------------------------------------------------

fraser_1_4_a_test_() ->
    Old = <<"The cat in the hat.">>,
    New = <<"The happy cat in the black hat.">>,
    Expect =
        { [<<"The ">>, <<>>, <<"cat in the">>, <<>>, <<" hat.">>]
        , [<<"The ">>, <<"happy ">>, <<"cat in the">>, <<" black">>, <<" hat.">>]
        },
    Reverse = { element(2, Expect), element(1, Expect) },
    [ ?_assertEqual(Expect, solarized_binary_diff:diff(Old, New))
    , ?_assertEqual(Reverse, solarized_binary_diff:diff(New, Old))
    ].

%-----------------------------------------------------------------------

fraser_1_4_b_test_() ->
    Old = <<"The cat in the hat.">>,
    New = <<"The ox in the box.">>,
    Expect =
        { [<<"The ">>, <<"cat">>, <<" in the ">>, <<"hat">>, <<".">>]
        , [<<"The ">>, <<"ox">>, <<" in the ">>, <<"box">>, <<".">>]
        },
    Reverse = { element(2, Expect), element(1, Expect) },
    [ ?_assertEqual(Expect, solarized_binary_diff:diff(Old, New))
    , ?_assertEqual(Reverse, solarized_binary_diff:diff(New, Old))
    ].

%-----------------------------------------------------------------------

fraser_2_1_test_() ->
    Old = <<"The cat in the hat.">>,
    New = <<"The bird in the hand.">>,
    Expect =
        { [<<"The ">>, <<"cat">>, <<" in the ha">>, <<"t">>, <<".">>]
        , [<<"The ">>, <<"bird">>, <<" in the ha">>, <<"nd">>, <<".">>]
        },
    Reverse = { element(2, Expect), element(1, Expect) },
    [ ?_assertEqual(Expect, solarized_binary_diff:diff(Old, New))
    , ?_assertEqual(Reverse, solarized_binary_diff:diff(New, Old))
    ].

%-----------------------------------------------------------------------

fraser_2_2_test_() ->
    Old = <<"The black cat in the hat?">>,
    New = <<"The cat in the black hat!">>,
    Expect =
        { [<<"The ">>, <<"black ">>
          , <<"cat in the ">>, <<>>
          , <<>>, <<>>
          , <<"hat">>, <<"?">>
          ]
        , [<<"The ">>, <<>>
          , <<"cat in the ">>, <<"blac">>
          , <<>>, <<"k ">>
          , <<"hat">>, <<"!">>
          ]
        },
    Reverse = { element(2, Expect), element(1, Expect) },
    [ ?_assertEqual(Expect, solarized_binary_diff:diff(Old, New))
    , ?_assertEqual(Reverse, solarized_binary_diff:diff(New, Old))
    ].

%-----------------------------------------------------------------------

-define(ZIG(L), lists:reverse(L)).
-define(ZAG(L), L).

bisect_scan_test() ->
    X = <<"abcabba">>,
    Y = <<"cbabac">>,
    B = bisect_init(X, Y),
    Delta = B#bisect.delta,
    Fore =
        [ {-0, ?ZAG([0]),               zig, -1}
        , {+1, ?ZIG([0, 1]),            zag, +2}
        , {-2, ?ZAG([2, 2, 3]),         zig, -3}
        , {+3, ?ZIG([3, 4, 5, 5]),      zag, +4}
        , {-4, ?ZAG([out, 4, 5, 7, 7]), zig, -5}
        ],
    bisect_scan_test(B, fore, Fore),
    Back =
        [ {Delta-0, ?ZAG([7]),                 zig, Delta-1}
        , {Delta+1, ?ZIG([6, 5]),              zag, Delta+2}
        , {Delta-2, ?ZAG([5, 3, 4]),           zig, Delta-3}
        , {Delta+3, ?ZIG([4, 1, 2, 4]),        zag, Delta+4}
        , {Delta-4, ?ZAG([2, 0, 1, out, out]), zig, Delta-5}
        ],
    bisect_scan_test(B, back, Back).

bisect_scan_test(_, _, [_]) ->
    ok;
bisect_scan_test(B, Dir, [{_, V, Zig, K} | Rest = [Expect | _]]) ->
    ?assertEqual(Expect, bisect_scan(B, Dir, Zig, K, V, no_match)),
    bisect_scan_test(B, Dir, Rest).

-undef(ZIG).
-undef(ZAG).

%-----------------------------------------------------------------------

bisect_scan_matchd_test() ->
    X = <<"abcabba">>,
    Y = <<"cbabac">>,
    B = bisect_init(X, Y),
    Delta = B#bisect.delta,
    % NOTE: Delta is odd!
    % after the back zag for K in {Delta-2, ..., Delta+2}
    BackK = Delta - 2,
    BackV = [5, 3, 4],
    Reverse = {BackK, BackV},
    % after the fore zag for K in {-2, ..., +2}
    ForeK = -2,
    ForeV = [2, 2, 3],
    % so the next
    NextK = ForeK - 1,
    Expect = {match, 2, 3, 2},
    ?assertEqual(Expect, bisect_scan(B, fore, zig, NextK, ForeV, Reverse)).

%-----------------------------------------------------------------------

bisect_test() ->
    X = <<"abcabba">>,
    Y = <<"cbabac">>,
    Expect = {<<"ab">>, <<"abc">>, <<"ba">>, <<"cb">>, <<"ac">>},
    ?assertEqual(Expect, bisect(X, Y)).

%-----------------------------------------------------------------------

bisect_hat_test() ->
    X = <<"black hat!">>,
    Y = <<"hat?">>,
    Expect = {<<>>, <<"blac">>, <<"k hat!">>, <<>>, <<"hat?">>},
    ?assertEqual(Expect, bisect(X, Y)).

%-----------------------------------------------------------------------

-endif.


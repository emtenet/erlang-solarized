%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized_list_diff).

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

% Compare two lists (Left, Right)
% return pair of {Left, Right} difference lists
% where the lists are in the form
%   DiffList = [Same0, Diff1, Same1, Diff2, Same2, ...]
% and
%   SameX alternates with DiffX
% and
%   SameX & DiffX are sub-lists of the initial lists

diff(Same, Same) ->
    { [Same]
    , [Same]
    };
diff(Left, Right) ->
    common_prefix(Left, Right).

%=======================================================================

common_prefix(Ls, Rs) ->
    case common_prefix_find(Ls, Rs, 0) of
        false ->
            common_prefix_next([], Ls, Rs);

        {Lt, Rt, N} ->
            Prefix = lists:sublist(Ls, N),
            common_prefix_next(Prefix, Lt, Rt)
    end.

%-----------------------------------------------------------------------

common_prefix_find([A | Ls], [A | Rs], N) ->
    common_prefix_find(Ls, Rs, N + 1);
common_prefix_find(_, _, 0) ->
    false;
common_prefix_find(Ls, Rs, N) when N > 0 ->
    {Ls, Rs, N}.

%-----------------------------------------------------------------------

common_prefix_next(Prefix, Left, Right) ->
    {Ls, Rs} = common_suffix(Left, Right),
    {[Prefix | Ls], [Prefix | Rs]}.

%=======================================================================

% get length of list
% be aware of improper lists, in that case return
% - length of list less the improper tail
% - a new list without the imporper tail
% - the improper tail

improper_length(L) ->
    improper_length(L, L, 0).

improper_length(L, [], N) ->
    {N, L, []};
improper_length(L, [_ | Ls], N) ->
    improper_length(L, Ls, N + 1);
improper_length(L, Improper, N) ->
    {N, lists:sublist(L, N), Improper}.

-ifdef(TEST).

improper_test_() ->
    [ ?_assertEqual({2, [a, b], []}, improper_length([a, b]))
    , ?_assertEqual({2, [a, b], c}, improper_length([a, b | c]))
    ].

-endif.

%=======================================================================

common_suffix(Left, Right) ->
    common_suffix_balance(improper_length(Left), improper_length(Right)).

%-----------------------------------------------------------------------

% balance tails so we can find a common suffix

common_suffix_balance({Ln, L, Lt}, {Rn, R, Rt}) when Ln > Rn ->
    N = Ln - Rn,
    Found = common_suffix_find(lists:nthtail(N, L), R),
    common_suffix_found(N, 0, L, R, Lt, Rt, Found);
common_suffix_balance({Ln, L, Lt}, {Rn, R, Rt}) when Rn > Ln ->
    N = Rn - Ln,
    Found = common_suffix_find(L, lists:nthtail(N, R)),
    common_suffix_found(0, N, L, R, Lt, Rt, Found);
common_suffix_balance({N, L, Lt}, {N, R, Rt}) ->
    Found = common_suffix_find(L, R),
    common_suffix_found(0, 0, L, R, Lt, Rt, Found).

%-----------------------------------------------------------------------

common_suffix_find(L, R) ->
    common_suffix_guess(L, R, 0).

%-----------------------------------------------------------------------

common_suffix_guess(L, R, N) ->
    common_suffix_check(L, R, N, {N, L}).

%-----------------------------------------------------------------------

common_suffix_check([], [], _, Best) ->
    Best;
common_suffix_check([A | L], [A | R], N, Best) ->
    common_suffix_check(L, R, N + 1, Best);
common_suffix_check([_ | L], [_ | R], N, _) ->
    common_suffix_guess(L, R, N + 1).

%-----------------------------------------------------------------------

common_suffix_found(Ln, Rn, L, R, Lt, Rt, {M, Suffix}) ->
    case Suffix of
        [] ->
            common_suffix_add(M + Ln, M + Rn, L, R, Lt, Rt, Suffix);

        _ ->
            Ls = lists:sublist(L, M + Ln),
            Rs = lists:sublist(R, M + Rn),
            common_suffix_add(M + Ln, M + Rn, Ls, Rs, Lt, Rt, Suffix)
    end.

%-----------------------------------------------------------------------

common_suffix_add(Ln, Rn, Ls, Rs, Lt, Rt, Suffix) ->
    common_suffix_next(Ln, Rn, Ls, Rs, [Suffix | Lt], [Suffix | Rt]).

%-----------------------------------------------------------------------

%common_suffix_next(Ln, Rn, Ls, Rs, Lt, Rt) when Ln > 10 orelse Rn > 10 ->
%    % avoid "large" lists
%    {[Ls | Lt], [Rs | Rt]};
common_suffix_next(Ln, Rn, Ls, Rs, Lt, Rt) when Ln < Rn ->
    single_edit(Ln, Rn, Ls, Rs, Lt, Rt);
common_suffix_next(Ln, Rn, Ls, Rs, Lt, Rt) ->
    % give single_edit() shortest on left
    % swap before and after
    {Re, Le} = single_edit(Rn, Ln, Rs, Ls, Rt, Lt),
    {Le, Re}.

%=======================================================================

% Ls is shorter or equal to Rs

single_edit(0, _, Ls, Rs, Lt, Rt) ->
    { [Ls | Lt]
    , [Rs | Rt]
    };
single_edit(Ln, Rn, Ls, Rs, Lt, Rt) ->
    two_edits(Ln, Rn, Ls, Rs, Lt, Rt).

%=======================================================================

% Ls is shorter or equal to Rs

two_edits(Ln, Rn, Ls, Rs, Lt, Rt) when Ln + 2 > Rn ->
    half_match(Ln, Rn, Ls, Rs, Lt, Rt);
two_edits(Ln, Rn, Ls, Rs, Lt, Rt) ->
    case find_inside(Ls, Rs, 0, Rn - Ln - 1) of
        nomatch ->
            half_match(Ln, Rn, Ls, Rs, Lt, Rt);
        
        {N, Suffix} ->
            Prefix = lists:sublist(Rs, N),
            { [[], Ls, [] | Lt]
            , [Prefix, Ls, Suffix | Rt]
            }
    end.

%-----------------------------------------------------------------------

find_inside(Small, Large, 0, Max) ->
    find_inside(Small, tl(Large), 1, Max);
find_inside(Small, Large, N, Max) ->
    case find_inside_check(Small, Large) of
        nomatch when N >= Max ->
            nomatch;

        nomatch ->
            find_inside(Small, tl(Large), N + 1, Max);

        Suffix ->
            {N, Suffix}
    end.

%-----------------------------------------------------------------------

find_inside_check([], Large) ->
    Large;
find_inside_check([A | Small], [A | Large]) ->
    find_inside_check(Small, Large);
find_inside_check(_, _) ->
    nomatch.

%-----------------------------------------------------------------------

-ifdef(TEST).

find_inside_test_() ->
    [ ?_assertEqual(" hat", find_inside_check("the", "the hat"))
    , ?_assertEqual({3, " hat"}, find_inside("the", "in the hat", 0, 6))
    ].

-endif.

%=======================================================================

% Ls is shorter or equal to Rs

half_match(1, _, Ls, Rs, Lt, Rt) ->
    % not worth it of length(Left) =:= 1
    { [Ls | Lt]
    , [Rs | Rt]
    };
half_match(_, _, Ls, Rs, Lt, Rt) ->
    { [Ls | Lt]
    , [Rs | Rt]
    }.

%=======================================================================

-ifdef(TEST).

fraser_1_1_test() ->
    Old = "Equality",
    New = Old,
    Expect = {[Old], [New]},
    ?assertEqual(Expect, solarized_list_diff:diff(Old, New)).

%-----------------------------------------------------------------------

fraser_1_2_test() ->
    Old = "The cat in the hat.",
    New = "The dog in the hat.",
    Expect =
        { ["The ", "cat", " in the hat."]
        , ["The ", "dog", " in the hat."]
        },
    ?assertEqual(Expect, solarized_list_diff:diff(Old, New)).

%-----------------------------------------------------------------------

fraser_1_3_a_test() ->
    Old = "The cat in the hat.",
    New = "The furry cat in the hat.",
    Expect =
        { ["The ", "", "cat in the hat."]
        , ["The ", "furry ", "cat in the hat."]
        },
    ?assertEqual(Expect, solarized_list_diff:diff(Old, New)).

%-----------------------------------------------------------------------

fraser_1_3_b_test() ->
    Old = "The cat in the hat.",
    New = "The cat.",
    Expect =
        { ["The cat", " in the hat", "."]
        , ["The cat", "", "."]
        },
    ?assertEqual(Expect, solarized_list_diff:diff(Old, New)).

%-----------------------------------------------------------------------

fraser_1_4_a_test_() ->
    Old = "The cat in the hat.",
    New = "The happy cat in the black hat.",
    Expect =
        { ["The ", "", "cat in the", "", " hat."]
        , ["The ", "happy ", "cat in the", " black", " hat."]
        },
    Reverse = { element(2, Expect), element(1, Expect) },
    [ ?_assertEqual(Expect, solarized_list_diff:diff(Old, New))
    , ?_assertEqual(Reverse, solarized_list_diff:diff(New, Old))
    ].

%-----------------------------------------------------------------------

%fraser_1_4_b_test_() ->
%    Old = <<"The cat in the hat.">>,
%    New = <<"The ox in the box.">>,
%    Expect =
%        { [<<"The ">>, <<"cat">>, <<" in the ">>, <<"hat">>, <<".">>]
%        , [<<"The ">>, <<"ok">>, <<" in the ">>, <<"box">>, <<".">>]
%        },
%    Reverse = { element(2, Expect), element(1, Expect) },
%    [ ?_assertEqual(Expect, solarized_list_diff:diff(Old, New))
%    , ?_assertEqual(Reverse, solarized_list_diff:diff(New, Old))
%    ].

%-----------------------------------------------------------------------

-endif.


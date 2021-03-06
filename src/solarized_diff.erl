%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized_diff).

-export([ diff/5
        , term/3
        ]).

-export_type([ options/0 ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-type options() ::
    #{ indent => pos_integer() | solarized:styled(),
       hanging => boolean(),
       common => solarized:application(),
       width => pos_integer()}.

%=======================================================================

-spec solarized_diff:diff(OldStyle, NewStyle, OldTerm, NewTerm, Options)
    -> Output
    when
      OldStyle :: solarized:application(),
      NewStyle :: solarized:application(),
      OldTerm :: term(),
      NewTerm :: term(),
      Options :: options(),
      Output :: {solarized:styled(), solarized:styled()}.

diff(OldStyle, NewStyle, OldTerm, NewTerm, Options) ->
    {F, R, W, Common} = options(Options),
    {OldDiffed, NewDiffed} = diffed(OldTerm, NewTerm),
    OldFirst = {F, Common, OldStyle},
    OldRest = {R, Common, OldStyle},
    OldStyled = styled(OldFirst, OldRest, W, OldDiffed),
    NewFirst = {F, Common, NewStyle},
    NewRest = {R, Common, NewStyle},
    NewStyled = styled(NewFirst, NewRest, W, NewDiffed),
    {OldStyled, NewStyled}.

%=======================================================================

-spec term(Style, Term, Options) -> Styled
    when Style :: solarized:application()
       , Term :: term()
       , Options :: options()
       , Styled :: solarized:styled()
       .

term(Style, Term, Options) ->
    {F, R, W, Common} = options(Options),
    Diffed = sized(diff, Term),
    First = {F, Common, Style},
    Rest = {R, Common, Style},
    styled(First, Rest, W, Diffed).

%=======================================================================

options(Options) when is_map(Options) ->
    Hanging = option_hanging(Options),
    {I, First, Rest} = option_indent(Options, Hanging),
    Width = option_width(Options, I),
    Common = option_common(Options),
    {First, Rest, Width, Common}.

%-----------------------------------------------------------------------

option_common(#{ common := Common }) when is_atom(Common) ->
    Common;
option_common(#{ common := Common = {_, _} }) ->
    Common;
option_common(#{ common := _ }) ->
    error(badarg);
option_common(_) ->
    text.

%-----------------------------------------------------------------------

option_hanging(#{ hanging := true }) ->
    hanging;
option_hanging(#{ hanging := false }) ->
    hanging_off;
option_hanging(#{ hanging := _ }) ->
    error(badarg);
option_hanging(_) ->
    hanging.

%-----------------------------------------------------------------------

option_indent(#{ indent := I }, _) when is_integer(I) ->
    Indent = indent_n(I),
    {I, Indent, Indent};
option_indent(#{ indent := First }, Hanging) ->
    I = indent_width(First),
    case Hanging of
        hanging ->
            Rest = indent_n(I),
            {I, First, Rest};

        hanging_off ->
            {I, First, First}
    end;
option_indent(_, _) ->
    {0, <<>>, <<>>}.

%-----------------------------------------------------------------------

option_width(#{ width := Width }, I) when is_integer(Width) ->
    min(Width, solarized:columns() - I - 1);
option_width(_, I) ->
    solarized:columns() - I - 1.

%=======================================================================

indent_width(Text) ->
    String = indent_to_string(Text),
    string:length(String).

%-----------------------------------------------------------------------

indent_to_string({_, Text}) ->
    indent_to_string(Text);
indent_to_string({_, _, Text}) ->
    indent_to_string(Text);
indent_to_string(Text) when is_binary(Text) ->
    Text;
indent_to_string(Text) when is_list(Text) ->
    case io_lib:printable_unicode_list(Text) of
        true ->
            Text;

        false ->
            lists:map(fun indent_list_to_string/1, Text)
    end.

%-----------------------------------------------------------------------

indent_list_to_string(Char) when is_integer(Char) ->
    Char;
indent_list_to_string(Text) ->
    indent_to_string(Text).

%=======================================================================

indent_n(0) -> <<>>;
indent_n(1) -> <<" ">>;
indent_n(2) -> <<"  ">>;
indent_n(3) -> <<"   ">>;
indent_n(4) -> <<"    ">>;
indent_n(5) -> <<"     ">>;
indent_n(6) -> <<"      ">>;
indent_n(7) -> <<"       ">>;
indent_n(8) -> <<"        ">>;
indent_n(I) when I > 8 ->
    binary:copy(<<" ">>, I).

%=======================================================================

diffed(Old, Old) ->
    Sized = sized(same, Old),
    {Sized, Sized};
diffed(Old, New)
        when is_list(Old) andalso
             is_list(New) ->
    case io_lib:printable_list(Old) of
        true ->
            case io_lib:printable_list(New) of
                true ->
                    { scalar_sized(diff, Old)
                    , scalar_sized(diff, New)
                    };

                false ->
                    list_diffed(Old, New)
            end;

        false ->
            list_diffed(Old, New)
    end;
diffed(Old, New)
        when is_tuple(Old) andalso
             is_tuple(New) ->
    tuple_diffed(Old, New);
diffed(Old, New)
        when is_map(Old) andalso
             is_map(New) ->
    map_diffed(Old, New);
diffed(Old, New)
        when is_binary(Old) andalso
             is_binary(New) ->
    solarized_binary:diffed(Old, New);
diffed(Old, New) ->
    { sized(diff, Old)
    , sized(diff, New)
    }.

%=======================================================================

sized(Same, List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true ->
            scalar_sized(Same, List);

        false ->
            list_sized(Same, List)
    end;
sized(Same, Tuple) when is_tuple(Tuple) ->
    tuple_sized(Same, Tuple);
sized(Same, Map) when is_map(Map) ->
    map_sized(Same, Map);
sized(Same, Binary) when is_binary(Binary) ->
    solarized_binary:sized(Same, Binary);
sized(Same, Term) ->
    scalar_sized(Same, Term).

%=======================================================================

scalar_sized(Same, Scalar) ->
    Text = unicode:characters_to_binary(io_lib:format("~p", [Scalar])),
    {scalar, Same, string:length(Text), Text}.

%=======================================================================

list_diffed(Olds, News)
        when Olds =:= [] orelse
             News =:= [] ->
    { list_sized(diff, Olds)
    , list_sized(diff, News)
    };
list_diffed(Left, Right) ->
    {[L | Ls], [R | Rs]} = solarized_list_diff:diff(Left, Right),
    list_diffed_same(L, R, Ls, Rs, list_start(), list_start()).

%-----------------------------------------------------------------------

list_diffed_same([], [], [], [], La, Ra) ->
    Same = list_same(La, Ra),
    { list_end(Same, La)
    , list_end(Same, Ra)
    };
list_diffed_same([], [], [Ld | Ls], [Rd | Rs], La, Ra) ->
    list_diffed_diff(Ld, Rd, Ls, Rs, La, Ra);
list_diffed_same([], [], Ld, [], La, Ra) ->
    D = sized(diff, Ld),
    { list_improper(diff, D, La)
    , list_end(diff, Ra)
    };
list_diffed_same([], [], [], Rd, La, Ra) ->
    D = sized(diff, Rd),
    { list_end(diff, La)
    , list_improper(diff, D, Ra)
    };
list_diffed_same([], [], Ld, Rd, La, Ra) ->
    Same = list_same(La, Ra),
    {L, R} = diffed(Ld, Rd),
    { list_improper(Same, L, La)
    , list_improper(Same, R, Ra)
    };
list_diffed_same([A | Ls], [A | Rs], Ld, Rd, La, Ra) ->
    S = sized(same, A),
    list_diffed_same(Ls, Rs, Ld, Rd, list_item(S, La), list_item(S, Ra)).

%-----------------------------------------------------------------------

list_diffed_diff([], [], [Ls | Ld], [Rs | Rd], La, Ra) ->
    list_diffed_same(Ls, Rs, Ld, Rd, La, Ra);
list_diffed_diff([L | Ld], [R | Rd], Ls, Rs, La, Ra) ->
    {Li, Ri} = diffed(L, R),
    list_diffed_diff(Ld, Rd, Ls, Rs, list_item(Li, La), list_item(Ri, Ra));
list_diffed_diff([L | Ld], [], Ls, Rs, La, Ra) ->
    Li = sized(diff, L),
    list_diffed_diff(Ld, [], Ls, Rs, list_item(Li, La), Ra);
list_diffed_diff([], [R | Rd], Ls, Rs, La, Ra) ->
    Ri = sized(diff, R),
    list_diffed_diff([], Rd, Ls, Rs, La, list_item(Ri, Ra)).

%-----------------------------------------------------------------------

list_sized(Same, []) ->
    {scalar, Same, 2, <<"[]">>};
list_sized(Same, Items) ->
    list_sized(Same, Items, list_start()).

list_sized(Same, [], Acc) ->
    list_end(Same, Acc);
list_sized(Same, [Item | Items], Acc) ->
    I = sized(Same, Item),
    list_sized(Same, Items, list_item(I, Acc));
list_sized(Same, Item, Acc) ->
    % improper list
    I = sized(Same, Item),
    list_improper(Same, I, Acc).

%-----------------------------------------------------------------------

list_start() ->
    {0, []}.

list_item(Diff = {_, _, L, _}, {Length, Diffs}) ->
    {Length + 2 + L, [Diff | Diffs]}.

list_same({_, L}, {_, R}) when length(L) =:= length(R) ->
    same;
list_same(_, _) ->
    diff.

list_end(Same, {Length, Diffs}) ->
    {list, Same, Length, Diffs}.

list_improper(Same, Diff = {_, _, L, _}, {Length, Diffs}) ->
    {improper, Same, Length + 3 + L, [Diff | Diffs]}.

%=======================================================================

tuple_diffed(Olds, News)
        when Olds =:= {} orelse
             News =:= {} ->
    { tuple_sized(diff, Olds)
    , tuple_sized(diff, News)
    };
tuple_diffed(Old, New) ->
    Olds = tuple_items(Old),
    News = tuple_items(New),
    tuple_diffed(Olds, News, tuple_start(), tuple_start()).

tuple_diffed([], [], OldAcc, NewAcc) ->
    { tuple_end(same, OldAcc)
    , tuple_end(same, NewAcc)
    };
tuple_diffed([], News, OldAcc, NewAcc) ->
    { tuple_end(diff, OldAcc)
    , tuple_sized(diff, News, NewAcc)
    };
tuple_diffed(Olds, [], OldAcc, NewAcc) ->
    { tuple_sized(diff, Olds, OldAcc)
    , tuple_end(diff, NewAcc)
    };
tuple_diffed([Old | Olds], [New | News], OldAcc, NewAcc) ->
    {O, N} = diffed(Old, New),
    tuple_diffed(Olds, News, tuple_item(O, OldAcc), tuple_item(N, NewAcc)).

%-----------------------------------------------------------------------

tuple_sized(Same, {}) ->
    {scalar, Same, 2, <<"{}">>};
tuple_sized(Same, Tuple) ->
    Items = tuple_items(Tuple),
    tuple_sized(Same, Items, tuple_start()).

tuple_sized(Same, [], Acc) ->
    tuple_end(Same, Acc);
tuple_sized(Same, [Item | Items], Acc) ->
    I = sized(Same, Item),
    tuple_sized(Same, Items, tuple_item(I, Acc)).

%-----------------------------------------------------------------------

tuple_items(Tuple) ->
    tuple_to_list(Tuple).

tuple_start() ->
    {0, []}.

tuple_item(Diff = {_, _, L, _}, {Length, Diffs}) ->
    {Length + 2 + L, [Diff | Diffs]}.

tuple_end(Same, {Length, Diffs}) ->
    {tuple, Same, Length, Diffs}.

%=======================================================================

map_diffed(Olds, News)
        when map_size(Olds) =:= 0 orelse
             map_size(News) =:= 0 ->
    { map_sized(diff, Olds)
    , map_sized(diff, News)
    };
map_diffed(Old, New) ->
    Olds = map_items(Old),
    News = map_items(New),
    map_diffed(Olds, News, same, map_start(), map_start()).

map_diffed([], [], Same, OldAcc, NewAcc) ->
    { map_end(Same, OldAcc)
    , map_end(Same, NewAcc)
    };
map_diffed(Olds = [], [{Nk, Nv} | Ns], _, Oa, Na) ->
    K = sized(diff, Nk),
    V = sized(diff, Nv),
    N = pair_end(K, V),
    map_diffed(Olds, Ns, diff, Oa, map_item(N, Na));
map_diffed([{Ok, Ov} | Os], News = [], _, Oa, Na) ->
    K = sized(diff, Ok),
    V = sized(diff, Ov),
    O = pair_end(K, V),
    map_diffed(Os, News, diff, map_item(O, Oa), Na);
map_diffed(Olds = [{Ok, Ov} | Os], News = [{Nk, Nv} | Ns], Same, Oa, Na) ->
    case Ok of
        Nk ->
            K = sized(same, Ok),
            {OV, NV} = diffed(Ov, Nv),
            O = pair_end(K, OV),
            N = pair_end(K, NV),
            map_diffed(Os, Ns, Same, map_item(O, Oa), map_item(N, Na));

        _ when Ok < Nk ->
            K = sized(diff, Ok),
            V = sized(diff, Ov),
            O = pair_end(K, V),
            map_diffed(Os, News, diff, map_item(O, Oa), Na);

        _ ->
            K = sized(diff, Nk),
            V = sized(diff, Nv),
            N = pair_end(K, V),
            map_diffed(Olds, Ns, diff, Oa, map_item(N, Na))
    end.

%-----------------------------------------------------------------------

map_sized(Same, Map) when map_size(Map) =:= 0 ->
    {scalar, Same, 2, <<"#{}">>};
map_sized(Same, Map) ->
    Pairs = map_items(Map),
    map_sized(Same, Pairs, map_start()).

map_sized(Same, [], Acc) ->
    map_end(Same, Acc);
map_sized(Same, [{Key, Value} | Pairs], Acc) ->
    K = sized(Same, Key),
    V = sized(Same, Value),
    P = pair_end(K, V),
    map_sized(Same, Pairs, map_item(P, Acc)).

%-----------------------------------------------------------------------

map_items(Map) ->
    lists:sort(maps:to_list(Map)).

map_start() ->
    {1, []}.

map_item(Diff = {pair, L, _, _}, {Length, Diffs}) ->
    {Length + 2 + L, [Diff | Diffs]}.

map_end(Same, {Length, Diffs}) ->
    {map, Same, Length, Diffs}.

pair_end(Key = {_, _, K, _}, Value = {_, _, V, _}) ->
    {pair, K + 4 + V, Key, Value}.

%=======================================================================

styled(FirstStyling, RestStyling, W, Diffed) ->
    Styled0 = styled(RestStyling, 0, W, [$\n], Diffed),
    [_ | Styled] = styled_newline(FirstStyling, 0, Styled0),
    Styled.

%-----------------------------------------------------------------------

styled(Styling, _I, _W, Acc, {scalar, Same, _, Text}) ->
    styled_text(Styling, Same, Acc, Text);
styled(Styling, _I, W, Acc, Diffed = {_, _, L, _}) when L =< W ->
    styled_inline(Styling, Acc, Diffed);
styled(Styling, I, W, Acc, {list, Same, _, Items}) ->
    styled_items(Styling, I, W, Same, <<"[ ">>, $], Acc, Items);
styled(Styling, I, W, Acc, {improper, Same, _, Items}) ->
    styled_improper(Styling, I, W, Same, Acc, Items);
styled(Styling, I, W, Acc, {tuple, Same, _, Items}) ->
    styled_items(Styling, I, W, Same, <<"{ ">>, $}, Acc, Items);
styled(Styling, I, W, Acc, Map = {map, _, _, _}) ->
    styled_map(Styling, I, W, Acc, Map);
styled(Styling, I, W, Acc, {binary, Same, _, Binary}) ->
    styled_binary(Styling, I, W, Same, Acc, Binary).

%=======================================================================

styled_binary(Styling, I, W, Same, Acc, Binary) ->
    Lines = solarized_binary:styled(W, Binary),
    styled_binary_lines(Styling, I, Same, Acc, Lines).

%-----------------------------------------------------------------------

styled_binary_lines(_, _, _, Acc, []) ->
    Acc;
styled_binary_lines(Styling, I, Same, Acc0, [newline | Lines]) ->
    Acc1 = styled_newline(Styling, I, Acc0),
    styled_binary_lines(Styling, I, Same, Acc1, Lines);
styled_binary_lines(Styling, I, Same, Acc0, [Line | Lines])
        when is_binary(Line) ->
    Acc1 = styled_text(Styling, Same, Acc0, Line),
    styled_binary_lines(Styling, I, Same, Acc1, Lines);
styled_binary_lines(Styling, I, Same, Acc0, [Line | Lines]) ->
    Acc1 = styled_texts(Styling, Same, Acc0, Line),
    styled_binary_lines(Styling, I, Same, Acc1, Lines).

%=======================================================================

styled_inline(Styling, Acc, {scalar, Same, _, Text}) ->
    styled_text(Styling, Same, Acc, Text);
styled_inline(Styling, Acc, {list, Same, _, Items}) ->
    styled_inline_items(Styling, Same, $[, $], Acc, Items);
styled_inline(Styling, Acc, {improper, Same, _, Items}) ->
    styled_inline_improper(Styling, Same, Acc, Items);
styled_inline(Styling, Acc, {tuple, Same, _, Items}) ->
    styled_inline_items(Styling, Same, ${, $}, Acc, Items);
styled_inline(Styling, Acc, {map, Same, _, Items}) ->
    styled_inline_map(Styling, Same, Acc, Items);
styled_inline(Styling, Acc, {binary, Same, _, Binary}) ->
    styled_inline_binary(Styling, Same, Acc, Binary).

%-----------------------------------------------------------------------

styled_inline_binary(Styling, Same, Acc, Binary) ->
    Texts = solarized_binary:inline(Binary),
    styled_texts(Styling, Same, Acc, Texts).

%-----------------------------------------------------------------------

styled_inline_items(Styling, Same, Open, Close, Acc0, Items) ->
    Acc1 = styled_text(Styling, Same, Acc0, Close),
    Acc2 = styled_inline_item(Styling, Same, Acc1, Items),
    styled_text(Styling, Same, Acc2, Open).

%-----------------------------------------------------------------------

styled_inline_item(Styling, _, Acc, [Item]) ->
    styled_inline(Styling, Acc, Item);
styled_inline_item(Styling, Same, Acc0, [Item | Items]) ->
    Acc1 = styled_inline(Styling, Acc0, Item),
    Acc2 = styled_text(Styling, Same, Acc1, <<", ">>),
    styled_inline_item(Styling, Same, Acc2, Items).

%-----------------------------------------------------------------------

styled_inline_improper(Styling, Same, Acc0, [Item | Items]) ->
    Acc1 = styled_text(Styling, Same, Acc0, $]),
    Acc2 = styled_inline(Styling, Acc1, Item),
    Acc3 = styled_text(Styling, Same, Acc2, <<" | ">>),
    Acc4 = styled_inline_item(Styling, Same, Acc3, Items),
    styled_text(Styling, Same, Acc4, $[).

%-----------------------------------------------------------------------

styled_inline_map(Styling, Same, Acc0, Items) ->
    Acc1 = styled_text(Styling, Same, Acc0, $}),
    Acc2 = styled_inline_pairs(Styling, Same, Acc1, Items),
    styled_text(Styling, Same, Acc2, <<"#{">>).

%-----------------------------------------------------------------------

styled_inline_pairs(Styling, Same, Acc, [Item]) ->
    styled_inline_pair(Styling, Same, Acc, Item);
styled_inline_pairs(Styling, Same, Acc0, [Item | Items]) ->
    Acc1 = styled_inline_pair(Styling, Same, Acc0, Item),
    Acc2 = styled_text(Styling, Same, Acc1, <<", ">>),
    styled_inline_pairs(Styling, Same, Acc2, Items).

%-----------------------------------------------------------------------

styled_inline_pair(Styling, Same, Acc0, {pair, _, Key, Value}) ->
    Acc1 = styled_inline(Styling, Acc0, Value),
    Acc2 = styled_text(Styling, Same, Acc1, <<" => ">>),
    styled_inline(Styling, Acc2, Key).

%=======================================================================

styled_items(Styling, I, W, Same, Open, Close, Acc0, Items) ->
    Acc1 = styled_text(Styling, Same, Acc0, Close),
    Acc2 = styled_newline(Styling, I, Acc1),
    styled_items(Styling, I, W, Same, Open, Acc2, Items).

%-----------------------------------------------------------------------

styled_items(Styling, I, W, Same, <<"{ ">>, Acc0, [Item = {scalar, _, _, _}]) ->
    Acc1 = styled_item(Styling, I + 1, W - 1, Acc0, Item),
    styled_text(Styling, Same, Acc1, ${);
styled_items(Styling, I, W, Same, Open, Acc0, [Item]) ->
    Acc1 = styled_item(Styling, I + 2, W - 2, Acc0, Item),
    styled_text(Styling, Same, Acc1, Open);
styled_items(Styling, I, W, Same, Open, Acc0, [Item | Items]) ->
    Acc1 = styled_item(Styling, I + 2, W - 2, Acc0, Item),
    Acc2 = styled_text(Styling, Same, Acc1, <<", ">>),
    Acc3 = styled_newline(Styling, I, Acc2),
    styled_items(Styling, I, W, Same, Open, Acc3, Items).

%-----------------------------------------------------------------------

styled_item(Styling, I, W, Acc, Item) ->
    styled(Styling, I, W, Acc, Item).

%=======================================================================

styled_improper(Styling, I, W, Same, Acc0, [Item | Items]) ->
    Acc1 = styled_text(Styling, Same, Acc0, $]),
    Acc2 = styled_newline(Styling, I, Acc1),
    Acc3 = styled_item(Styling, I + 2, W - 2, Acc2, Item),
    Acc4 = styled_text(Styling, Same, Acc3, <<"| ">>),
    Acc5 = styled_newline(Styling, I, Acc4),
    styled_items(Styling, I, W, Same, <<"[ ">>, Acc5, Items).

%=======================================================================

styled_map(Styling, I, W, Acc0, {map, Same, _, Pairs}) ->
    Acc1 = styled_text(Styling, Same, Acc0, $}),
    Acc2 = styled_newline(Styling, I, Acc1),
    styled_pairs(Styling, I, W, Same, Acc2, Pairs).

%-----------------------------------------------------------------------

styled_pairs(Styling, I, W, Same, Acc0, [{pair, _, K, V}]) ->
    Acc1 = styled_pair(Styling, I + 2, W - 2, Same, Acc0, K, V),
    styled_text(Styling, Same, Acc1, <<"#{">>);
styled_pairs(Styling, I, W, Same, Acc0, [{pair, _, K, V} | Pairs]) ->
    Acc1 = styled_pair(Styling, I + 2, W - 2, Same, Acc0, K, V),
    Acc2 = styled_text(Styling, Same, Acc1, <<", ">>),
    Acc3 = styled_newline(Styling, I, Acc2),
    styled_pairs(Styling, I, W, Same, Acc3, Pairs).

%-----------------------------------------------------------------------

styled_pair(Styling, I, W, Same, Acc0, K = {_, _, L, _}, V)
        when (L * 3) =< W andalso L rem 2 == 0 ->
    Acc1 = styled(Styling, I + L + 4, W - L - 4, Acc0, V),
    Acc2 = styled_text(Styling, Same, Acc1, <<" => ">>),
    styled_inline(Styling, Acc2, K);
styled_pair(Styling, I, W, Same, Acc0, K = {_, _, L, _}, V)
        when (L * 3) =< W ->
    Acc1 = styled(Styling, I + L + 5, W - L - 5, Acc0, V),
    Acc2 = styled_text(Styling, Same, Acc1, <<"  => ">>),
    styled_inline(Styling, Acc2, K);
styled_pair(Styling, I, W, Same, Acc0, K, V) ->
    Acc1 = styled(Styling, I, W, Acc0, V),
    Acc2 = styled_newline(Styling, I, Acc1),
    Acc3 = styled_text(Styling, Same, Acc2, <<" =>">>),
    styled(Styling, I, W, Acc3, K).

%=======================================================================

styled_text(_, Same, Acc, Text) when is_list(Acc) ->
    {Same, Text, Acc};
styled_text(_, Same, {Same, Texts, Acc}, Text)
        when is_integer(Texts) orelse is_binary(Texts) ->
    {Same, [Text, Texts], Acc};
styled_text(_, Same, {Same, Texts, Acc}, Text) ->
    {Same, [Text | Texts], Acc};
styled_text({_, _, text}, same, {_, Texts, Acc}, Text) ->
    {same, Text, [Texts | Acc]};
styled_text({_, _, Diff}, same, {_, Texts, Acc}, Text) ->
    {same, Text, [{Diff, Texts} | Acc]};
styled_text({_, text, _}, diff, {_, Texts, Acc}, Text) ->
    {diff, Text, [Texts | Acc]};
styled_text({_, Same, _}, diff, {_, Texts, Acc}, Text) ->
    {diff, Text, [{Same, Texts} | Acc]}.

%-----------------------------------------------------------------------

styled_texts(_, _, Acc, []) ->
    Acc;
styled_texts(Styling, Same, Acc0, [{SameText, Text} | Texts]) ->
    Acc1 = styled_text(Styling, SameText, Acc0, Text),
    styled_texts(Styling, Same, Acc1, Texts);
styled_texts(Styling, Same, Acc0, [Text | Texts]) ->
    Acc1 = styled_text(Styling, Same, Acc0, Text),
    styled_texts(Styling, Same, Acc1, Texts).

%=======================================================================

styled_newline(Styling, 0, Acc) ->
    styled_newline_acc(Styling, Acc);
styled_newline(Styling, I, Acc0) ->
    Acc1 = styled_text(Styling, same, Acc0, indent_n(I)),
    styled_newline_acc(Styling, Acc1).

%-----------------------------------------------------------------------

styled_newline_acc({Indent, text, _}, {same, Text, Acc}) ->
    styled_newline_indent(Indent, [Text | Acc]);
styled_newline_acc({Indent, {Same1, Same2}, _}, {same, Text, Acc}) ->
    styled_newline_indent(Indent, [{Same1, Same2, Text} | Acc]);
styled_newline_acc({Indent, Same, _}, {same, Text, Acc}) ->
    styled_newline_indent(Indent, [{Same, Text} | Acc]);
styled_newline_acc({Indent, _, text}, {diff, Text, Acc}) ->
    styled_newline_indent(Indent, [Text | Acc]);
styled_newline_acc({Indent, _, {Diff1, Diff2}}, {diff, Text, Acc}) ->
    styled_newline_indent(Indent, [{Diff1, Diff2, Text} | Acc]);
styled_newline_acc({Indent, _, Diff}, {diff, Text, Acc}) ->
    styled_newline_indent(Indent, [{Diff, Text} | Acc]).

%-----------------------------------------------------------------------

styled_newline_indent(<<>>, Acc) ->
    [$\n | Acc];
styled_newline_indent(Indent, Acc) ->
    [$\n, Indent | Acc].

%=======================================================================

-ifdef(TEST).

tuple_diffed_test_() ->
    Old = {2018, 1, 1},
    New = {2018, 10, atom},
    OldDiffed =
        {tuple, same, 12
        , [ {scalar, diff, 1, <<"1">>}
          , {scalar, diff, 1, <<"1">>}
          , {scalar, same, 4, <<"2018">>}
          ]
        },
    NewDiffed =
        {tuple, same, 16
        , [ {scalar, diff, 4, <<"atom">>}
          , {scalar, diff, 2, <<"10">>}
          , {scalar, same, 4, <<"2018">>}
          ]
        },
    ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New)).

%-----------------------------------------------------------------------

tuple_empty_diffed_test_() ->
    Old = {2018, 1, 1},
    New = {},
    OldDiffed =
        {tuple, diff, 12
        , [ {scalar, diff, 1, <<"1">>}
          , {scalar, diff, 1, <<"1">>}
          , {scalar, diff, 4, <<"2018">>}
          ]
        },
    NewDiffed = {scalar, diff, 2, <<"{}">>},
    [ {"forward"
      , ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New))
      }
    , {"backward"
      , ?_assertEqual({NewDiffed, OldDiffed}, diffed(New, Old))
      }
    ].

%-----------------------------------------------------------------------

list_diffed_test_() ->
    Old = [2018, 1, 1],
    New = [2018, atom],
    OldDiffed =
        {list, diff, 12
        , [ {scalar, diff, 1, <<"1">>}
          , {scalar, diff, 1, <<"1">>}
          , {scalar, same, 4, <<"2018">>}
          ]
        },
    NewDiffed =
        {list, diff, 12
        , [ {scalar, diff, 4, <<"atom">>}
          , {scalar, same, 4, <<"2018">>}
          ]
        },
    ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New)).

%-----------------------------------------------------------------------

list_empty_diffed_test_() ->
    Old = [2018, 1, 1],
    New = [],
    OldDiffed =
        {list, diff, 12
        , [ {scalar, diff, 1, <<"1">>}
          , {scalar, diff, 1, <<"1">>}
          , {scalar, diff, 4, <<"2018">>}
          ]
        },
    NewDiffed = {scalar, diff, 2, <<"[]">>},
    [ {"forward"
      , ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New))
      }
    , {"backward"
      , ?_assertEqual({NewDiffed, OldDiffed}, diffed(New, Old))
      }
    ].

%-----------------------------------------------------------------------

list_map_diffed_test_() ->
    Old = [#{key => value}],
    New = Old,
    OldDiffed =
        {list, same, 17
        , [ {map, same, 15
            , [ {pair, 12
                , {scalar, same, 3, <<"key">>}
                , {scalar, same, 5, <<"value">>}
                }
              ]
            }
          ]
        },
    NewDiffed = OldDiffed,
    [ {"forward"
      , ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New))
      }
    , {"backward"
      , ?_assertEqual({NewDiffed, OldDiffed}, diffed(New, Old))
      }
    ].

%-----------------------------------------------------------------------

improper_diff_diffed_test_() ->
    Old = [2018, 1 | 1],
    New = [2018, atom],
    OldDiffed =
        {improper, diff, 13
        , [ {scalar, diff, 1, <<"1">>}
          , {scalar, diff, 1, <<"1">>}
          , {scalar, same, 4, <<"2018">>}
          ]
        },
    NewDiffed =
        {list, diff, 12
        , [ {scalar, diff, 4, <<"atom">>}
          , {scalar, same, 4, <<"2018">>}
          ]
        },
    ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New)).

%-----------------------------------------------------------------------

improper_same_diffed_test_() ->
    Old = [2018, 1 | 1],
    New = [2018, atom | diff],
    OldDiffed =
        {improper, same, 13
        , [ {scalar, diff, 1, <<"1">>}
          , {scalar, diff, 1, <<"1">>}
          , {scalar, same, 4, <<"2018">>}
          ]
        },
    NewDiffed =
        {improper, same, 19
        , [ {scalar, diff, 4, <<"diff">>}
          , {scalar, diff, 4, <<"atom">>}
          , {scalar, same, 4, <<"2018">>}
          ]
        },
    ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New)).

%-----------------------------------------------------------------------

map_diffed_test_() ->
    Old = #{year => 2018, month => 1, day => 1},
    New = #{year => 2018, day => atom},
    OldDiffed =
        {map, diff, 37
        , [ {pair, 12
            , {scalar, same, 4, <<"year">>}
            , {scalar, same, 4, <<"2018">>}
            }
          , {pair, 10
            , {scalar, diff, 5, <<"month">>}
            , {scalar, diff, 1, <<"1">>}
            }
          , {pair, 8
            , {scalar, same, 3, <<"day">>}
            , {scalar, diff, 1, <<"1">>}
            }
          ]
        },
    NewDiffed =
        {map, diff, 28
        , [ {pair, 12
            , {scalar, same, 4, <<"year">>}
            , {scalar, same, 4, <<"2018">>}
            }
          , {pair, 11
            , {scalar, same, 3, <<"day">>}
            , {scalar, diff, 4, <<"atom">>}
            }
          ]
        },
    [ {"forward"
      , ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New))
      }
    , {"backward"
      , ?_assertEqual({NewDiffed, OldDiffed}, diffed(New, Old))
      }
    ].

%-----------------------------------------------------------------------

map_empty_diffed_test_() ->
    Old = #{1 => 1},
    New = #{},
    OldDiffed =
        {map, diff, 9
        , [ {pair, 6
            , {scalar, diff, 1, <<"1">>}
            , {scalar, diff, 1, <<"1">>}
            }
          ]
        },
    NewDiffed = {scalar, diff, 2, <<"#{}">>},
    [ {"forward"
      , ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New))
      }
    , {"backward"
      , ?_assertEqual({NewDiffed, OldDiffed}, diffed(New, Old))
      }
    ].

%-----------------------------------------------------------------------

map_diff_2_test_() ->
    Old = #{duplicate => email},
    New = #{ok => true},
    OldDiffed =
        {map, diff, 21
        , [ {pair, 18
            , {scalar, diff, 9, <<"duplicate">>}
            , {scalar, diff, 5, <<"email">>}
            }
          ]
        },
    NewDiffed =
        {map, diff, 13
        , [ {pair, 10
            , {scalar, diff, 2, <<"ok">>}
            , {scalar, diff, 4, <<"true">>}
            }
          ]
        },
    [ {"forward"
      , ?_assertEqual({OldDiffed, NewDiffed}, diffed(Old, New))
      }
    , {"backward"
      , ?_assertEqual({NewDiffed, OldDiffed}, diffed(New, Old))
      }
    ].

%-----------------------------------------------------------------------

styled_scalar_test_() ->
    Diffed = {scalar, diff, 4, <<"2018">>},
    Line = [<<"- ">>, {yellow, <<"2018">>}, $\n],
    First = {<<"- ">>, blue, yellow},
    Style = {<<"  ">>, blue, yellow},
    ?_assertEqual(Line, styled(First, Style, 77, Diffed)).

%-----------------------------------------------------------------------

styled_tuple_test_() ->
    Diffed =
      {tuple, same, 12
      , [ {scalar, diff, 1, <<"1">>}
        , {scalar, diff, 1, <<"1">>}
        , {scalar, same, 4, <<"2018">>}
        ]
      },
    Inline =
      [ <<"- ">>
      , [${, <<"2018">>, <<", ">>]
      , {yellow, <<"1">>}
      , <<", ">>
      , {yellow, <<"1">>}
      , $}
      , $\n
      ],
    InlineBlue =
      [ <<"- ">>
      , {blue, [${, <<"2018">>, <<", ">>]}
      , {yellow, <<"1">>}
      , {blue, <<", ">>}
      , {yellow, <<"1">>}
      , {blue, $}}
      , $\n
      ],
    Vertical =
      [ <<"- ">>, [${, <<"2018">>], $\n
      , <<"  ">>, <<", ">>, {yellow, <<"1">>}, $\n
      , <<"  ">>, <<", ">>, {yellow, <<"1">>}, $\n
      , <<"  ">>, $}, $\n
      ],
    VerticalBlue =
      [ <<"- ">>, {blue, [${, <<"2018">>]}, $\n
      , <<"  ">>, {blue, <<", ">>}, {yellow, <<"1">>}, $\n
      , <<"  ">>, {blue, <<", ">>}, {yellow, <<"1">>}, $\n
      , <<"  ">>, {blue, $}}, $\n
      ],
    First = {<<"- ">>, text, yellow},
    Style = {<<"  ">>, text, yellow},
    FirstBlue = {<<"- ">>, blue, yellow},
    StyleBlue = {<<"  ">>, blue, yellow},
    [ { "inline"
      , ?_assertEqual(Inline, styled(First, Style, 77, Diffed))
      }
    , { "inline blue"
      , ?_assertEqual(InlineBlue, styled(FirstBlue, StyleBlue, 77, Diffed))
      }
    , { "vertical"
      , ?_assertEqual(Vertical, styled(First, Style, 10, Diffed))
      }
    , { "vertical blue"
      , ?_assertEqual(VerticalBlue, styled(FirstBlue, StyleBlue, 10, Diffed))
      }
    ].

%-----------------------------------------------------------------------

styled_list_test_() ->
    Diffed =
      {list, same, 12
      , [ {scalar, diff, 1, <<"1">>}
        , {scalar, diff, 1, <<"1">>}
        , {scalar, same, 4, <<"2018">>}
        ]
      },
    Styled =
      [ [<<"[ ">>, <<"2018">>], $\n
      , <<", ">>, {yellow, <<"1">>}, $\n
      , <<", ">>, {yellow, <<"1">>}, $\n
      , $], $\n
      ],
    First = {<<>>, text, yellow},
    Style = {<<>>, text, yellow},
    ?_assertEqual(Styled, styled(First, Style, 10, Diffed)).

%-----------------------------------------------------------------------

styled_improper_test_() ->
    Diffed =
      {improper, same, 13
      , [ {scalar, diff, 1, <<"1">>}
        , {scalar, diff, 1, <<"1">>}
        , {scalar, same, 4, <<"2018">>}
        ]
      },
    Inline =
      [ <<"- ">>, [$[, <<"2018">>, <<", ">>]
      , {yellow, <<"1">>}, <<" | ">>
      , {yellow, <<"1">>}, $], $\n
      ],
    Vertical =
      [ <<"- ">>, [<<"[ ">>, <<"2018">>], $\n
      , <<"  ">>, <<", ">>, {yellow, <<"1">>}, $\n
      , <<"  ">>, <<"| ">>, {yellow, <<"1">>}, $\n
      , <<"  ">>, $], $\n
      ],
    First = {<<"- ">>, text, yellow},
    Style = {<<"  ">>, text, yellow},
    [ {"inline"
      , ?_assertEqual(Inline, styled(First, Style, 77, Diffed))
      }
    , {"vertical"
      , ?_assertEqual(Vertical, styled(First, Style, 10, Diffed))
      }
    ].

%-----------------------------------------------------------------------

styled_indent_test_() ->
    Diffed =
      {list, same, 45
      , [ {map, same, 37
          , [ {pair , 12
              , {scalar, same, 4, <<"year">>}
              , {scalar, same, 4, <<"2018">>}
              }
            , {pair, 10
              , {scalar, diff, 5, <<"month">>}
              , {scalar, diff, 1, <<"1">>}
              }
            , {pair, 8
              , {scalar, same, 3, <<"day">>}
              , {scalar, diff, 1, <<"1">>}
              }
            ]
          }
        , {scalar, same, 4, <<"2018">>}
        ]
      },
    Inline =
      [ <<"- ">>
      , [$[, <<"2018">>, <<", ">>, <<"#{">>, <<"day">>, <<" => ">>]
      , {yellow, <<"1">>}
      , <<", ">>, {yellow, <<"month">>}, <<" => ">>, {yellow, <<"1">>}
      , [<<", ">>, <<"year">>, <<" => ">>, <<"2018">>, $}, $]]
      , $\n
      ],
    InlineBlue =
      [ <<"- ">>
      , {blue, [$[, <<"2018">>, <<", ">>, <<"#{">>, <<"day">>, <<" => ">>]}
      , {yellow, <<"1">>}
      , {blue, <<", ">>}, {yellow, <<"month">>}
      , {blue, <<" => ">>}, {yellow, <<"1">>}
      , {blue, [<<", ">>, <<"year">>, <<" => ">>, <<"2018">>, $}, $]]}
      , $\n
      ],
    Vertical =
      [ <<"- ">>, [<<"[ ">>, <<"2018">>], $\n
      , <<"  ">>, [<<", ">>, <<"#{">>, <<"day">>, <<" =>">>], $\n
      , <<"  ">>, <<"    ">>, {yellow, <<"1">>}, $\n
      , <<"  ">>, [<<"  ">>, <<", ">>]
                , {yellow, <<"month">>}, <<" =>">>, $\n
      , <<"  ">>, <<"    ">>, {yellow, <<"1">>}, $\n
      , <<"  ">>, [<<"  ">>, <<", ">>, <<"year">>, <<" =>">>], $\n
      , <<"  ">>, [<<"    ">>, <<"2018">>], $\n
      , <<"  ">>, [<<"  ">>, $}], $\n
      , <<"  ">>, $], $\n
      ],
    VerticalBlue =
      [ <<"- ">>, {blue, [<<"[ ">>, <<"2018">>]}, $\n
      , <<"  ">>, {blue, [<<", ">>, <<"#{">>, <<"day">>, <<" =>">>]}, $\n
      , <<"  ">>, {blue, <<"    ">>}, {yellow, <<"1">>}, $\n
      , <<"  ">>, {blue, [<<"  ">>, <<", ">>]}
                , {yellow, <<"month">>}, {blue, <<" =>">>}, $\n
      , <<"  ">>, {blue, <<"    ">>}
                , {yellow, <<"1">>}, $\n
      , <<"  ">>, {blue, [<<"  ">>, <<", ">>, <<"year">>, <<" =>">>]}, $\n
      , <<"  ">>, {blue, [<<"    ">>, <<"2018">>]}, $\n
      , <<"  ">>, {blue, [<<"  ">>, $}]}, $\n
      , <<"  ">>, {blue, $]}, $\n
      ],
    First = {<<"- ">>, text, yellow},
    Style = {<<"  ">>, text, yellow},
    FirstBlue = {<<"- ">>, blue, yellow},
    StyleBlue = {<<"  ">>, blue, yellow},
    [ {"inline"
      , ?_assertEqual(Inline, styled(First, Style, 77, Diffed))
      }
    , {"inline blue"
      , ?_assertEqual(InlineBlue, styled(FirstBlue, StyleBlue, 77, Diffed))
      }
    , {"vertical"
      , ?_assertEqual(Vertical, styled(First, Style, 10, Diffed))
      }
    , {"vertical blue"
      , ?_assertEqual(VerticalBlue, styled(FirstBlue, StyleBlue, 10, Diffed))
      }
    ].

%-----------------------------------------------------------------------

styled_hanging_test_() ->
    Diffed =
      {map, same, 44
      , [ {pair , 20
          , {scalar, same, 4, <<"even">>}
          , {tuple, same, 12
            , [ {scalar, diff, 1, <<"1">>}
              , {scalar, diff, 1, <<"1">>}
              , {scalar, same, 4, <<"2018">>}
              ]
            }
          }
        , {pair, 19
          , {scalar, same, 3, <<"odd">>}
          , {tuple, same, 12
            , [ {scalar, diff, 1, <<"1">>}
              , {scalar, diff, 1, <<"1">>}
              , {scalar, same, 4, <<"2018">>}
              ]
            }
          }
        ]
      },
    Vertical =
      [ <<"- ">>, [<<"#{">>, <<"odd">>, <<"  => ">>, ${, <<"2018">>], $\n
      , <<"  ">>, [<<"          ">>, <<", ">>], {yellow, <<"1">>}, $\n
      , <<"  ">>, [<<"          ">>, <<", ">>], {yellow, <<"1">>}, $\n
      , <<"  ">>, [<<"          ">>, $}], $\n
      , <<"  ">>, [<<", ">>, <<"even">>, <<" => ">>, ${, <<"2018">>], $\n
      , <<"  ">>, [<<"          ">>, <<", ">>], {yellow, <<"1">>}, $\n
      , <<"  ">>, [<<"          ">>, <<", ">>], {yellow, <<"1">>}, $\n
      , <<"  ">>, [<<"          ">>, $}], $\n
      , <<"  ">>, $}, $\n
      ],
    First = {<<"- ">>, text, yellow},
    Style = {<<"  ">>, text, yellow},
    ?_assertEqual(Vertical, styled(First, Style, 15, Diffed)).

%-----------------------------------------------------------------------

black_cat_test() ->
    Old = <<"The black cat in the hat?">>,
    New = <<"The cat in the black hat!">>,
    Options = #{width => 20},
    Expect =
        { [ [<<"<<\"">>, <<"The ">>]
          , {orange, <<"black ">>}
          , [<<"cat in">>, <<"\"">>]
          , $\n
          , [<<", \"">>, <<" the ">>, <<"hat">>]
          , {orange, <<"?">>}
          , <<"\"">>
          , $\n
          , <<">>">>
          , $\n
          ]
        , [ [<<"<<\"">>, <<"The ">>, <<"cat in the ">>]
          , {yellow, <<"b">>}
          , <<"\"">>
          , $\n
          , <<", \"">>
          , {yellow, [<<"lac">>, <<"k ">>]}
          , <<"hat">>
          , {yellow, <<"!">>}
          , <<"\"">>
          , $\n
          , <<">>">>
          , $\n
          ]
        },
    ?assertEqual(Expect, diff(orange, yellow, Old, New, Options)).

%-----------------------------------------------------------------------

-endif.


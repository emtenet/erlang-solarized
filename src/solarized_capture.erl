%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.

% This module has been derived from:
%    https://github.com/marco-m/mock_io
%    Copyright (c) 2015, Marco Molteni
-module(solarized_capture).
-include_lib("eunit/include/eunit.hrl").

-export([ output/1
        , output/3
        , result_and_output/1
        , result_and_output/3
        ]).

-export_type([ geometry/0
             ]).

-type geometry() ::
      pos_integer()
    | enotsup
    | {ok, pos_integer()}
    | {error, enotsup}.

-type internal_geometry() ::
      pos_integer()
    | {error, enotsup}.

%=======================================================================

-record(state,
       { output = <<>>
       , binary = false
       , columns = 80
       , rows = 25
       }).

%=======================================================================

-spec output(Test) -> Output
    when
      Test :: fun(() -> Result),
      Result :: term(),
      Output :: binary().

output(Test) when is_function(Test, 0) ->
    output(Test, io:columns(), io:rows()).

%=======================================================================

-spec output(Test, Columns, Rows) -> Output
    when
      Test :: fun(() -> Result),
      Columns :: geometry(),
      Rows :: geometry(),
      Result :: term(),
      Output :: binary().

output(Test, Columns, Rows) when is_function(Test, 0) ->
    {_, Output} = result_and_output(Test, Columns, Rows),
    Output.

%=======================================================================

-spec result_and_output(Test) -> {Result, Output}
    when
      Test :: fun(() -> Result),
      Result :: term(),
      Output :: binary().

result_and_output(Test) when is_function(Test, 0) ->
    result_and_output(Test, io:columns(), io:rows()).

%=======================================================================

-spec result_and_output(Test, Columns, Rows) -> {Result, Output}
    when
      Test :: fun(() -> Result),
      Columns :: geometry(),
      Rows :: geometry(),
      Result :: term(),
      Output :: binary().

result_and_output(Test, Columns, Rows) when is_function(Test, 0) ->
    State = #state{columns = geometry(Columns), rows = geometry(Rows)},
    mock(Test, State).

%=======================================================================

-spec geometry(geometry()) -> internal_geometry().

geometry(N) when is_integer(N) andalso N > 0 ->
    N;
geometry(enotsup) ->
    {error, enotsup};
geometry({ok, N}) when is_integer(N) andalso N > 0 ->
    N;
geometry(Error = {error, enotsup}) ->
    Error;
geometry(_) ->
    throw(badarg).

%-----------------------------------------------------------------------

mock(Test, State) ->
    Was = erlang:group_leader(),
    Mock = spawn_link(fun () -> mock_loop(State) end),
    true = erlang:group_leader(Mock, self()),
    Result = try
        Test()
    after
        true = erlang:group_leader(Was, self())
    end,
    {ok, Output} = mock_call(Mock, stop),
    {Result, Output}.

%=======================================================================

mock_loop(State) ->
    receive
        {mock, From, Request} ->
            mock_request(From, Request, State);

        {io_request, From, Opaque, Request} ->
            io_request(From, Opaque, Request, State)
    end.

%-----------------------------------------------------------------------

mock_request(From, stop, State) ->
    mock_reply(From, {ok, State#state.output}),
    stop.

%-----------------------------------------------------------------------

io_request(From, Opaque, {put_chars, Encoding, Chars}, State) ->
    io_put_chars(From, Opaque, Encoding, Chars, State);
io_request(From, Opaque, {put_chars, Encoding, M, F, A}, State) ->
    io_put_chars(From, Opaque, Encoding, M, F, A, State);
io_request(From, Opaque, {get_geometry, Geometry}, State) ->
    io_get_geometry(From, Opaque, Geometry, State);
io_request(From, Opaque, _Request, State) ->
    io_reply(From, Opaque, {error, enotsup}),
    mock_loop(State).

%-----------------------------------------------------------------------

io_put_chars(From, Opaque, _Encoding, Chars, State) when is_binary(Chars) ->
    io_reply(From, Opaque, ok),
    Output = <<(State#state.output)/binary, Chars/binary>>,
    mock_loop(State#state{output = Output});
io_put_chars(From, Opaque, Encoding, Chars, State) ->
    io_put_chars(From, Opaque, Encoding, iolist_to_binary(Chars), State).

%-----------------------------------------------------------------------

io_put_chars(From, Opaque, unicode, io_lib, format, [Format, Data], State) ->
    Chars = io_lib:format(Format, Data),
    io_put_chars(From, Opaque, unicode, Chars, State).

%-----------------------------------------------------------------------

io_get_geometry(From, Opaque, columns, State = #state{columns = Columns}) ->
    io_reply(From, Opaque, Columns),
    mock_loop(State);
io_get_geometry(From, Opaque, rows, State = #state{rows = Rows}) ->
    io_reply(From, Opaque, Rows),
    mock_loop(State);
io_get_geometry(From, Opaque, _Geometry, State) ->
    io_reply(From, Opaque, {error, enotsup}),
    mock_loop(State).

%-----------------------------------------------------------------------

io_reply(To, Opaque, Reply) ->
    To ! {io_reply, Opaque, Reply},
    ok.

%-----------------------------------------------------------------------

mock_reply(To, Reply) ->
    To ! {mock, self(), Reply},
    ok.

%-----------------------------------------------------------------------

mock_call(Mock, Request) ->
    Mock ! {mock, self(), Request},
    receive
        {mock, Mock, Response} ->
            Response
    after
        1000 ->
            erlang:error({?MODULE, timeout})
    end.

%=======================================================================

-ifdef(EUNIT).

%-----------------------------------------------------------------------

basic_test() ->
    Test = fun () -> io:put_chars(<<"Hello">>) end,
    Expect = <<"Hello">>,
    ?assertEqual(Expect, output(Test)).

%-----------------------------------------------------------------------

mixed_test() ->
    Test = fun () ->
        io:put_chars([$H, ["el", $l], <<"o">>]),
        io:format(" ~p!~n", [42])
    end,
    Expect = <<"Hello 42!\n">>,
    ?assertEqual(Expect, output(Test)).

%-----------------------------------------------------------------------

columns_test() ->
    Test = fun () ->
        {ok, Columns} = io:columns(),
        {error, Rows} = io:rows(),
        {Columns, Rows}
    end,
    Expect = {{40, enotsup}, <<>>},
    ?assertEqual(Expect, result_and_output(Test, {ok, 40}, enotsup)).

%-----------------------------------------------------------------------

rows_test() ->
    Test = fun () ->
        {error, Columns} = io:columns(),
        {ok, Rows} = io:rows(),
        {Columns, Rows}
    end,
    Expect = {{enotsup, 12}, <<>>},
    ?assertEqual(Expect, result_and_output(Test, {error, enotsup}, 12)).

%-----------------------------------------------------------------------

input_test() ->
    Test = fun () ->
        io:get_chars(prompt, 10)
    end,
    Expect = {{error, enotsup}, <<>>},
    ?assertEqual(Expect, result_and_output(Test)).

%-----------------------------------------------------------------------

-endif.


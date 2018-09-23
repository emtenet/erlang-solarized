%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized_assert).

-export([ output_equal_to_file/3
        , output_equal_to_file/5
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-spec output_equal_to_file(App, File, Test) -> boolean()
    when
      App :: atom(),
      File :: file:name_all(),
      Test :: fun(() -> Result),
      Result :: term().

output_equal_to_file(App, File, Test)
        when is_atom(App) andalso
             is_function(Test, 0) ->
    Output = solarized_capture:output(Test),
    check_equal_to_file(App, File, Output).

%=======================================================================

-spec output_equal_to_file(App, File, Test, Columns, Rows) -> boolean()
    when
      App :: atom(),
      File :: file:name_all(),
      Test :: fun(() -> Result),
      Columns :: solarized_capture:geometry(),
      Rows :: solarized_capture:geometry(),
      Result :: term().

output_equal_to_file(App, File, Test, Columns, Rows)
        when is_atom(App) andalso
             is_function(Test, 0) ->
    Output = solarized_capture:output(Test, Columns, Rows),
    check_equal_to_file(App, File, Output).

%=======================================================================

check_equal_to_file(App, File, Output) ->
    BaseDir = code:lib_dir(App, tests),
    ok = ensure_tests_dir(BaseDir),
    BaseFile = filename:join(BaseDir, File),
    ExpectFile = [BaseFile, ".expect"],
    OutputFile = [BaseFile, ".output"],
    case equal_to_file(Output, ExpectFile) of
        true ->
            ok = delete_output_file(OutputFile),
            true;

        false ->
            ok = file:write_file(OutputFile, Output, [binary]),
            false
    end.

%=======================================================================

ensure_tests_dir(TestsDir) ->
    case file:make_dir(TestsDir) of
        ok ->
            ok;

        {error, eexist} ->
            ok
    end.

%-----------------------------------------------------------------------

equal_to_file(Output, ExpectFile) ->
    case file:read_file(ExpectFile) of
        {ok, Expect} when Expect =:= Output ->
            true;

        {ok, _} ->
            false;

        {error, enoent} ->
            false
    end.

%-----------------------------------------------------------------------

delete_output_file(OutputFile) ->
    case file:delete(OutputFile) of
        ok ->
            ok;

        {error, enoent} ->
            ok
    end.


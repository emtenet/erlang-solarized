%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(solarized_assert).

-export([ output_equal_to_file/5
        , output_equal_to_file/7
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-spec output_equal_to_file(App, File, Test, TestName, Props) -> ok
    when
      App :: atom(),
      File :: file:name_all(),
      Test :: fun(() -> TestResult),
      TestName :: string(),
      TestResult :: term(),
      Props :: list({module, atom()} | {line, integer()}).

output_equal_to_file(App, File, Test, TestName, Props0)
        when is_atom(App) andalso
             (is_atom(File) orelse is_list(File)) andalso
             is_function(Test, 0) andalso
             is_list(TestName) andalso
             is_list(Props0) ->
    output_equal_to_file(App, File, Test, TestName, 80, 25, Props0).

%=======================================================================

-spec output_equal_to_file(App, File, Test, TestName, Columns, Rows, Props)
    -> ok
    when
      App :: atom(),
      File :: file:name_all(),
      Test :: fun(() -> TestResult),
      TestName :: string(),
      TestResult :: term(),
      Columns :: solarized_capture:geometry(),
      Rows :: solarized_capture:geometry(),
      Props :: list({module, atom()} | {line, integer()}).

output_equal_to_file(App, File, Test, TestName, Columns, Rows, Props0)
        when is_atom(App) andalso
             (is_atom(File) orelse is_list(File)) andalso
             is_function(Test, 0) andalso
             is_list(TestName) andalso
             is_integer(Columns) andalso
             is_integer(Rows) andalso
             is_list(Props0) ->
    Output = solarized_capture:output(Test, Columns, Rows),
    FileName = case is_atom(File) of
        true ->
            atom_to_list(File);

        false ->
            File
    end,
    Props =
        [ {app, atom_to_list(App)}
        , {file, FileName}
        , {expression, TestName}
        , {columns, integer_to_list(Columns)}
        , {rows, integer_to_list(Rows)}
        | Props0
        ],
    check_equal_to_file(App, File, Output, Props).

%=======================================================================

check_equal_to_file(App, File, Output, Props0) ->
    BaseDir = code:lib_dir(App, test),
    ok = ensure_test_dir(BaseDir),
    BaseFile = filename:join(BaseDir, File),
    ExpectFile = [BaseFile, ".expect"],
    OutputFile = [BaseFile, ".output"],
    Props =
        [ {expect_file, ExpectFile}
        , {output_file, OutputFile}
        | Props0
        ],
    equal_to_file(Output, OutputFile, ExpectFile, Props).

%=======================================================================

ensure_test_dir(TestDir) ->
    case file:make_dir(TestDir) of
        ok ->
            ok;

        {error, eexist} ->
            ok
    end.

%-----------------------------------------------------------------------

equal_to_file(Output, OutputFile, ExpectFile, Props) ->
    case file:read_file(ExpectFile) of
        {ok, Expect} when Expect =:= Output ->
            ok = delete_output_file(OutputFile),
            ok;

        {ok, _} ->
            not_equal_to_file(Output, OutputFile, ExpectFile, Props);

        {error, enoent} ->
            not_equal_to_file(Output, OutputFile, ExpectFile, Props)
    end.

%-----------------------------------------------------------------------

-spec not_equal_to_file(binary(), string(), string(), list()) -> no_return().

not_equal_to_file(Output, OutputFile, ExpectFile, Props) ->
    ok = file:write_file(OutputFile, Output, [binary]),
    erlang:error(
        { outputEqualToFile
        , [ {expect_file, ExpectFile}
          , {output_file, OutputFile}
          | Props
          ]}).

%-----------------------------------------------------------------------

delete_output_file(OutputFile) ->
    case file:delete(OutputFile) of
        ok ->
            ok;

        {error, enoent} ->
            ok
    end.


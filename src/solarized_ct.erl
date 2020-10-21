-module(solarized_ct).

-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/4]).
-export([post_init_per_group/5]).
-export([pre_end_per_group/4]).
-export([post_end_per_group/5]).

-export([pre_init_per_testcase/4]).
-export([post_init_per_testcase/5]).
-export([pre_end_per_testcase/4]).
-export([post_end_per_testcase/5]).

-export([on_tc_fail/3]).
-export([on_tc_skip/4]).

-export([terminate/1]).

-record(state, {
    id :: term(),
    suite = undefined :: atom(),
    groups = [] :: list(atom()),
    application_controller :: term()
}).

%%====================================================================
%% ct_hook
%%====================================================================

init(Id, _Options) ->
    Level = logger:get_module_level(application_controller),
    ok = logger:set_module_level(application_controller, none),
    {ok, #state{id = Id, application_controller = Level}}.

%%--------------------------------------------------------------------

pre_init_per_suite(Suite, Config, State) ->
    {Config, State#state{suite = Suite, groups = []}}.

%%--------------------------------------------------------------------

post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

%%--------------------------------------------------------------------

pre_end_per_suite(_Suite, Config, State) ->
    {Config, State}.

%%--------------------------------------------------------------------

post_end_per_suite(_Suite, _Config, Return, State) ->
    {Return, State#state{suite = undefined, groups = []}}.

%%--------------------------------------------------------------------

pre_init_per_group(_Suite, _Group, Config, State) ->
    {Config, State}.

%%--------------------------------------------------------------------

post_init_per_group(_Suite, Group, _Config, Return, State) ->
    Groups = State#state.groups,
    {Return, State#state{groups = [Group | Groups]}}.

%%--------------------------------------------------------------------

pre_end_per_group(_Suite, _Group,Config,State) ->
    {Config, State}.

%%--------------------------------------------------------------------

post_end_per_group(_Suite, _Group, _Config, Return, State) ->
    [_ | Groups] = State#state.groups,
    {Return, State#state{groups = Groups}}.

%%--------------------------------------------------------------------

pre_init_per_testcase(_Suite, _TestCase, Config, State) ->
    {Config, State}.

%%--------------------------------------------------------------------

post_init_per_testcase(_Suite, _TestCase, _Config, Return, State) ->
    {Return, State}.

%%--------------------------------------------------------------------

pre_end_per_testcase(_Suite, _TestCase, Config, State) ->
    {Config, State}.

%%--------------------------------------------------------------------

post_end_per_testcase(Suite, TestCase, _Config, ok, State) ->
    print_testcase(Suite, TestCase, {green, bold, <<"OK">>}, State),
    {ok, State};
post_end_per_testcase(Suite, TestCase, Config, Error, State) ->
    case lists:keyfind(tc_status, 1, Config) of
        {tc_status, ok} ->
            %% Test case passed, but we still ended in an error
            Label = <<"end_per_testcase FAILED">>,
            print_testcase(Suite, TestCase, {magenta, Label}, State),
            print_reason(Error);

        _ ->
            %% Test case failed, in which case on_tc_fail already reports it
            ok
    end,
    {Error, State}.

%%--------------------------------------------------------------------

on_tc_fail({TestCase, _Group}, Reason, State) ->
    Suite = State#state.suite,
    print_testcase(Suite, TestCase, {red, bold, <<"FAILED">>}, State),
    print_reason(Reason),
    State;
on_tc_fail(TestCase, Reason, State) ->
    Suite = State#state.suite,
    print_testcase(Suite, TestCase, {red, bold, <<"FAILED">>}, State),
    print_reason(Reason),
    State.

%%--------------------------------------------------------------------

on_tc_skip(Suite, {TestCase, _Group}, Reason, State) ->
    print_testcase(Suite, TestCase, {magenta, <<"SKIPPED">>}, State),
    print_reason(Reason),
    State#state{suite = Suite};
on_tc_skip(Suite, TestCase, Reason, State) ->
    print_testcase(Suite, TestCase, {magenta, <<"SKIPPED">>}, State),
    print_reason(Reason),
    State#state{suite = Suite}.

%%--------------------------------------------------------------------

terminate(State) ->
    case State#state.application_controller of
        [{_, Level}] ->
            ok = logger:set_module_level(application_controller, Level)
    end,
    ok.

%%====================================================================
%% display
%%====================================================================

print_testcase(Suite, TestCase, Label, #state{groups = []}) ->
    solarized:styled([
        atom_to_binary(Suite, utf8),
        <<" => ">>,
        atom_to_binary(TestCase, utf8),
        <<": ">>,
        Label,
        <<".\n">>
    ]);
print_testcase(Suite, TestCase, Label, #state{groups = [Group]}) ->
    solarized:styled([
        atom_to_binary(Suite, utf8),
        <<" => ">>,
        atom_to_binary(Group, utf8),
        <<".">>,
        atom_to_binary(TestCase, utf8),
        <<": ">>,
        Label,
        <<".\n">>
    ]);
print_testcase(Suite, TestCase, Label, #state{groups = [Group | _]}) ->
    solarized:styled([
        atom_to_binary(Suite, utf8),
        <<" => ...">>,
        atom_to_binary(Group, utf8),
        <<".">>,
        atom_to_binary(TestCase, utf8),
        <<": ">>,
        Label,
        <<".\n">>
    ]).

%%--------------------------------------------------------------------

print_reason({{badmatch, {aborted, {Error, Stack1}}}, Stack2})
        when is_tuple(Error) andalso
             is_list(Stack1) andalso
             is_list(Stack2) ->
    solarized_eunit:report_exception_stack(lists:reverse(Stack1)),
    solarized_eunit:report_exception_error(Error);
print_reason({Error, Stack}) when is_tuple(Error) andalso is_list(Stack) ->
    solarized_eunit:report_exception_stack(lists:reverse(Stack)),
    solarized_eunit:report_exception_error(Error);
print_reason(Reason) ->
    solarized_eunit:report_exception_error(Reason).


-module(erl_mvn_eunit).

-behaviour(eunit_listener).

-export([start/0,
	 start/1,
	 init/1,
	 handle_begin/3,
	 handle_end/3,
	 handle_cancel/3,
	 terminate/2,
         run_test_file_line/2]).


run_test(Test, SourceFile) ->
    io:format("======================================================================~n"),
    io:format("==   Running Test: ~w~n", [Test]),
    io:format("======================================================================~n"),
    Master = self(),
    Tty = {report, {?MODULE, [{report_to, Master}, {source_file, SourceFile}]}},
    spawn(fun() ->
                  try eunit:test(Test, [Tty]) 
                  catch
                      _Class:_Exception -> 
                          Master ! []
                  end
          end),
    receive
        Res -> Res
    end.

run_test_file_line(SourceFile, Line) ->
    case erl_mvn_source_utils:get_test_for_line(SourceFile, Line) of
        {ok, Test} ->
            run_test(Test, SourceFile);
        _ ->
            []
    end.
                

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% record definition section

-record(state, {
          test_results = [] :: [{ok, atom()} | {error, atom(), integer(), string()}],
          source_file       :: string(),
	  report_to         :: pid()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% public function section

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Initialize this module.
%%% @end
%%%-----------------------------------------------------------------------------
start() ->
    start([]).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Initialize this module with options.
%%% @end
%%%-----------------------------------------------------------------------------
start(Options) ->
    eunit_listener:start(?MODULE, Options).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Initialize this module with options. A pid to report to must be given!
%%% @end
%%%-----------------------------------------------------------------------------
init([{report_to, Pid}, {source_file, SourceFile}]) ->
    #state{report_to = Pid, source_file = SourceFile}.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Handles the begin of a test case or suite.
%%% @end
%%%-----------------------------------------------------------------------------
handle_begin(_Arg1, _Data, State) ->
    State.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Handles the end of a test case or suite.
%%% @end
%%%-----------------------------------------------------------------------------
handle_end(test, Data, State = #state{source_file = SourceFile}) ->
%    io:format("-------------------------------handle_end ~p~n", [Data]),
    {_Mod, Function, Arg} = proplists:get_value(source, Data),
    case proplists:get_value(status, Data) of
        ok ->
            Line = erl_mvn_source_utils:get_line_of_function(SourceFile, Function, Arg),
            TestResult = {ok, Function, Line};
        {error, Reason} ->
            Line = get_error_line(Reason, SourceFile),
            TestResult = {error, Function, Line, format_error(Reason, Line)}
    end,   
    State#state{test_results = [TestResult | State#state.test_results]}; 

handle_end(_, _, S) ->
    S.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Handles the cancellation of a test case or suite.
%%% @end
%%%-----------------------------------------------------------------------------
handle_cancel(test, Data, State = #state{source_file = SourceFile}) ->
%    io:format("-------------------------------handle_cancel ~p~n", [Data]),
    {Mod, Function, Arg} = proplists:get_value(source, Data),
    Reason = proplists:get_value(reason, Data),    
    Line = erl_mvn_source_utils:get_line_of_function(SourceFile, Function, Arg),
    TestResult = {error, Function, Line,
                  lists:flatten(
                    io_lib:format("~w.erl:~w Test cancelled, reason: ~w~n", 
                                  [Mod, Line, Reason]))},
    State#state{test_results = [TestResult | State#state.test_results]}; 

handle_cancel(_Arg, _Data, State) ->
    State.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Sends the collected string lines to a specific pid.
%%% @end
%%%-----------------------------------------------------------------------------
terminate(_, #state{test_results = R, report_to = Dest}) ->
    Dest ! R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal function section
get_error_line({error, {_Type, Reasons}, _Stack}, _SourceFile) ->
    proplists:get_value(line, Reasons);

get_error_line({throw, _What, [{_M, F, A}]}, SourceFile) ->    
    erl_mvn_source_utils:get_line_of_function(SourceFile, F, A).

format_error({throw, What, [{M, F, A}]}, Line) ->
    lists:flatten(io_lib:format("~w.erl:~w Exception ~w at ~w/~w~n~n", [M, Line, What, F, A]));
format_error({error, {Type, Reasons}, _Stack}, Line) ->
    Mod = proplists:get_value(module, Reasons),
    Expected = proplists:get_value(expected, Reasons),
    Expression = proplists:get_value(expression, Reasons),
    Actual = proplists:get_value(value, Reasons),
    lists:flatten(io_lib:format("~w.erl:~w ~w~n     Expected:~n          ~w~n     Actual:~n           ~w~n     Expression:~n          ~p~n~n", [Mod, Line, Type, Expected, Actual, Expression])).

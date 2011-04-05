-module(erl_mvn_eunit).

-behaviour(eunit_listener).

-export([start/0,
	 start/1,
	 init/1,
	 handle_begin/3,
	 handle_end/3,
	 handle_cancel/3,
	 terminate/2,
         run_test_file_line/2,
         trace_test_file_line/3
        ]).


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
                          Master ! {test_result, []}
                  end
          end),
    receive
        {test_result, Res} -> 
            Res
    after 10000 ->
            []
    end.

run_test_file_line(SourceFile, Line) ->
    case erl_mvn_source_utils:get_test_for_line(SourceFile, Line) of
        {ok, Test} ->
            run_test(Test, SourceFile);
        _ ->
            []
    end.

trace_test_file_line(SourceFile, TestSourceFile, Line) ->
    {ok, Mod} = erl_mvn_source_utils:get_module(SourceFile),
    {ok, TestMod} = erl_mvn_source_utils:get_module(TestSourceFile),
    RemoteCalls = sets:to_list(
                    sets:union(erl_mvn_source_utils:find_remote_calls(SourceFile), 
                               erl_mvn_source_utils:find_remote_calls(TestSourceFile))),

    case erl_mvn_source_utils:get_test_for_line(TestSourceFile, Line) of
        {ok, {TestMod, FunName}} ->
            LineOfFun = erl_mvn_source_utils:get_line_of_function(TestSourceFile, FunName, 0),
            Master = self(),
            spawn(fun() ->
                          start_tracer([{TestMod, '_', '_'}, {Mod, '_', '_'} | RemoteCalls]),                
                          
                          try TestMod:FunName() of
                              _ ->
                                  Master ! {set, [{ok, FunName, LineOfFun}]}
                          catch 
                              Class:Exception -> 
                                  Master ! {set, [{error, FunName, LineOfFun, lists:flatten(io_lib:format("~w:~p", [Class, Exception]))}]}
                          end
                  end),
            receive 
                {set, T} -> 
                    TestResult = T
            after 10000 ->
                    TestResult = [{error, FunName, LineOfFun, "test time out"}]
            end,
            {ok, TraceResultRaw} = stop_tracer(),
            TraceResult = lists:foldr(fun format_trace/2, [], TraceResultRaw);

        _ ->
            TestResult = [],
            TraceResult = [{"No Testfunction found on current line."}]
    end,
    {trace_test_result, TraceResult, TestResult}.


start_tracer(TracePatterns) ->
    Master = self(),
    register(
      erl_mvn_eunit_tracer,
      spawn(fun() -> 
                    [erlang:trace_pattern(TP, [], [local]) || TP <- TracePatterns],
                    erlang:trace(Master, true, [call, procs, send, 'receive', set_on_spawn]),
                    trace_loop([]) 
            end)).

stop_tracer() ->
    erl_mvn_eunit_tracer ! {stop_tracer, self()},
    receive 
        {tracer_stopped, Result} -> 
            {ok, Result}
    after 1000 ->        
            {error, timeout}
    end.
                                        
trace_loop(Acc) ->                    
    receive
        {stop_tracer, From} ->
            From ! {tracer_stopped,  lists:reverse(Acc)};
        TraceMsg ->
            trace_loop([TraceMsg | Acc])
    end.

format_trace({trace, Pid, call, {M, F, A}},Acc) ->
    MF = lists:flatten(io_lib:format("~w:~w", [M,F])),
    [{Pid, lists:flatten(io_lib:format   ("~-10w  -- ( ) -->  ~-31s ~130p~n~n", [Pid, MF, A]))}| Acc];

format_trace({trace, Pid, 'receive', Msg},Acc) ->
    [{Pid, lists:flatten(io_lib:format   ("~-10w  <---------  ~130p~n~n", [Pid, Msg]))}| Acc];

format_trace({trace, From, send, Msg, To},Acc) ->
    [{From, lists:flatten(io_lib:format  ("~-10w  --------->  ~-31w ~130p~n~n", [From, To, Msg]))}| Acc];

format_trace({trace, Pid, 'register', Name},Acc) ->
    [{Pid, lists:flatten(io_lib:format   ("~-10w   REGISTER   ~w~n~n", [Pid, Name]))}| Acc];

format_trace({trace, Pid, 'unregister', Name},Acc) ->
    [{Pid, lists:flatten(io_lib:format   ("~-10w  UNREGISTER  ~w~n~n", [Pid, Name]))}| Acc];

format_trace({trace, Parent, spawn, Child, How},Acc) ->
    [{Parent, lists:flatten(io_lib:format("~-10w  --- * --->  ~-31w ~130p~n~n", [Parent, Child, How]))}| Acc];

format_trace({trace, Left, link, Right},Acc) ->
    [{Left, lists:flatten(io_lib:format  ("~-10w  ===8======  ~-10w~n~n", [Left, Right]))},
     {Right, lists:flatten(io_lib:format ("~-10w  ======8===  ~-10w~n~n", [Right, Left]))}
     | Acc];

format_trace({trace, Left, unlink, Right},Acc) ->
    [{Left, lists:flatten(io_lib:format  ("~-10w  ===/  /===  ~-10w~n~n", [Left, Right]))}| Acc];

format_trace({trace, Left, getting_unlinked, Right},Acc) ->
    [{Left, lists:flatten(io_lib:format  ("~-10w  ===/  /===  ~-10w~n~n", [Left, Right]))}| Acc];

format_trace({trace, Pid, exit, Reason},Acc) ->
    [{Pid, lists:flatten(io_lib:format   ("~-10w  ***EXIT***  ~130p~n~n", [Pid, Reason]))}| Acc];

format_trace(Other, Acc) ->
    [{lists:flatten(io_lib:format("~p ~n~n", [Other]))}|Acc].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% record definition section

-record(state, {
          test_results = []     :: [{ok, atom()} | {error, atom(), integer(), string()}],
          source_file           :: string(),
	  report_to             :: pid(),
          last_test_fun_started :: {atom(), integer()}}).

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
handle_begin(test, Data, State) ->
%    io:format("XXXXXXXXXXXXXXXXXXXXXXXXXX ~p~n", [Data]),
    {_Mod, Function, Arg} = proplists:get_value(source, Data),
    State#state{last_test_fun_started = {Function, Arg}};

handle_begin(_Arg1, _Data, State) ->
    State.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Handles the end of a test case or suite.
%%% @end
%%%-----------------------------------------------------------------------------
handle_end(test, Data, 
           State = #state{
             source_file = SourceFile,
             last_test_fun_started = CurrentFun
            }) ->
%    io:format("-------------------------------handle_end ~p~n", [Data]),
    {_Mod, Function, Arg} = proplists:get_value(source, Data),
    case proplists:get_value(status, Data) of
        ok ->
            Line = erl_mvn_source_utils:get_line_of_function(SourceFile, Function, Arg),
            TestResult = {ok, Function, Line};
        {error, Reason} ->
            Line = get_error_line(Reason, SourceFile, CurrentFun),
            TestResult = {error, Function, Line, format_error(Reason)}
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
    Dest ! {test_result, R}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal function section
get_error_line({error, {_Type, Reasons}, _Stack}, _SourceFile, _CurrentFun) ->
    proplists:get_value(line, Reasons);

get_error_line({throw, _What, [{_M, F, A}]}, SourceFile, _CurrentFun) ->    
    erl_mvn_source_utils:get_line_of_function(SourceFile, F, A);

get_error_line(_Other, SourceFile, {F, A}) -> 
    erl_mvn_source_utils:get_line_of_function(SourceFile, F, A).


format_error({throw, What, [{_M, F, A}]}) ->
    lists:flatten(io_lib:format("Exception:~n~w~n~nat:~n~w/~w~n~n", [What, F, A]));        

format_error({error, {Type, Reasons}, Stacktrace}) ->
    Expected = proplists:get_value(expected, Reasons),
    Expression = proplists:get_value(expression, Reasons),
    Actual = proplists:get_value(value, Reasons),
    lists:flatten(io_lib:format("~w~nExpected:~n          ~p~n~nActual:~n           ~p~n~nExpression:~n          ~p~n~nat:~n~p~n~n", [Type, Expected, Actual, Expression, Stacktrace]));

format_error({error, Reason, Stacktrace}) ->
    lists:flatten(io_lib:format("Error:~n~p~n~nat:~n~p~n", [Reason, Stacktrace]));

format_error({exit, Reason, Stacktrace}) ->
    lists:flatten(io_lib:format("Exit:~n~p~n~nat:~n~p~n", [Reason, Stacktrace])).


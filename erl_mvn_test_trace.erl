-module(erl_mvn_test_trace).

-export([trace_test_file_line/3]).


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
	    start_tracer([{TestMod, '_', '_'}, {Mod, '_', '_'} | RemoteCalls]),
            spawn(fun() ->
			  try TestMod:FunName() of
			      _ ->
				  Master ! {set, [{ok, FunName, LineOfFun}]}
			  catch
			      Class:Exception ->
				  Master ! {set, [{error, FunName, LineOfFun,
                                                   lists:flatten(io_lib:format("~w:~p", [Class, Exception]))}]}
			  end
		  end),
            receive
                {set, T} ->
                    TestResult = T
            after 5000 ->
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
    Parent = self(),
    register(
      erl_mvn_eunit_tracer,
      spawn(fun() ->
                    [erlang:trace_pattern(TP, [{'_', [], [{return_trace},
                                                          {exception_trace}]}],
                                          [local])
                     || TP <- TracePatterns],
                    erlang:trace(Parent, true,
                                 [call, procs, send, 'receive', return_to,
                                  set_on_spawn]),
		    Parent ! tracer_started,
                    trace_loop([])
            end)),
    receive
	tracer_started -> ok
    after 1000 ->
	    throw (timeout)
    end.

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
            trace_loop(merge_return(TraceMsg, Acc))
    end.


merge_return(T = {trace, Pid, return_from, _, _}, Acc) ->
    MatchingTo = fun({_, P, return_to, _}) -> P == Pid;
                          (_) -> false
                       end,
    merge_or_prepend(T, lists:partition(MatchingTo, Acc));

merge_return(T = {trace, Pid, return_to, _}, Acc) ->
    MatchingFrom = fun({_, P, return_from, _, _}) -> P == Pid;
                      (_) -> false end,
    merge_or_prepend(T, lists:partition(MatchingFrom, Acc));

merge_return(T, Acc) ->
    [T|Acc].

merge_or_prepend(T, {[], Acc}) ->
    [T|Acc];
merge_or_prepend({trace, Pid, return_to, ToMFA},
                         {Ms = [_|_], Acc}) ->
    [{trace, Pid, return_from, FromMFA, RetVal}|T] = lists:reverse(Ms),
    T ++ [{trace, Pid, return_from_to, FromMFA, RetVal, ToMFA}|Acc];
merge_or_prepend({trace, Pid, return_from, FromMFA, RetVal},
                         {Ms = [_|_], Acc}) ->
    [{trace, Pid, return_to, ToMFA}|T] = lists:reverse(Ms),
    T ++ [{trace, Pid, return_from_to, FromMFA, RetVal, ToMFA}|Acc].

format_trace({trace, Pid, call, {M, F, A}},Acc) ->
    MF = lists:flatten(io_lib:format("~w:~w", [M,F])),
    [{Pid, lists:flatten(io_lib:format("~-15w  >> ~s  ~150p~n~n", [Pid, MF, A]))}| Acc];

format_trace({trace, Pid, exception_from, {M, F, A}, RV},Acc) ->
    MF = lists:flatten(io_lib:format("~w:~w/~w", [M,F,A])),
    [{Pid, lists:flatten(io_lib:format(" .  * x\\ \\        ___~n  . . \\x  \\   __ /   \\_~nx +.x* +  ** (_ / /    <\\   /\\   /\\   /\\   /\\   /~n  *+   * *** (__\\/ )   <  /    \\    /    \\    /~n  + / +  *** (____)   </   \\/   \\/   \\/   \\/   \\~n x *  / //    (____)___/~n    / * *~n     /~n~-15w  ***EXCEPTION*** ~s  ~130p~n~n", [Pid, MF, RV]))}| Acc];

format_trace({trace, Pid, return_from, {M, F, A}, RV},Acc) ->
    MF = lists:flatten(io_lib:format("~w:~w/~w", [M,F,A])),
    [{Pid, lists:flatten(io_lib:format("~-15w  << ~s  ~150p~n~n", [Pid, MF, RV]))}| Acc];

format_trace({trace, Pid, return_to, {M, F, A}},Acc) ->
    MF = lists:flatten(io_lib:format("~w:~w/~w <<", [M,F,A])),
    [{Pid, lists:flatten(io_lib:format("~-15w  ~s~n~n", [Pid, MF]))}| Acc];

format_trace({trace, Pid, return_from_to, F, R, T}, Acc) ->
    [{Pid, format_return_from_to(Pid, F, R, T)}| Acc];

format_trace({trace, Pid, 'receive', Msg},Acc) ->
    [{Pid, lists:flatten(io_lib:format   ("~-15w  <---------                                       ~130p~n~n", [Pid, Msg]))}| Acc];

format_trace({trace, From, send, Msg, To},Acc) ->
    [{From, lists:flatten(io_lib:format  ("~-15w  --------->  ~-36w ~150p~n~n", [From, To, Msg]))}| Acc];

format_trace({trace, Pid, 'register', Name},Acc) ->
    [{Pid, lists:flatten(io_lib:format   ("~-15w   REGISTER   ~w~n~n", [Pid, Name]))}| Acc];

format_trace({trace, Pid, 'unregister', Name},Acc) ->
    [{Pid, lists:flatten(io_lib:format   ("~-15w  UNREGISTER  ~w~n~n", [Pid, Name]))}| Acc];

format_trace({trace, Parent, spawn, Child, How},Acc) ->
    [{Parent, lists:flatten(io_lib:format("~-15w  --- * --->  ~-36w ~150p~n~n", [Parent, Child, How]))}| Acc];

format_trace({trace, Left, link, Right},Acc) ->
    [{Left, lists:flatten(io_lib:format  ("~-15w  ===8======  ~-15w~n~n", [Left, Right]))},
     {Right, lists:flatten(io_lib:format ("~-15w  ======8===  ~-15w~n~n", [Right, Left]))}
     | Acc];

format_trace({trace, Left, unlink, Right},Acc) ->
    [{Left, lists:flatten(io_lib:format  ("~-15w  ====/  /==  ~-15w~n~n", [Left, Right]))}| Acc];

format_trace({trace, Left, getting_unlinked, Right},Acc) ->
    [{Left, lists:flatten(io_lib:format  ("~-15w  ==/  /====  ~-15w~n~n", [Left, Right]))}| Acc];

format_trace({trace, Pid, exit, Reason},Acc) ->
    [{Pid, lists:flatten(io_lib:format   ("~-15w  ***EXIT***  ~150p~n~n", [Pid, Reason]))}| Acc];

format_trace(Other, Acc) ->
    [{lists:flatten(io_lib:format("~p ~n~n", [Other]))}|Acc].

format_return_from_to(Pid, {FromM, FromF, FromA}, RetVal, {ToM, ToF, ToA}) ->
    lists:flatten(
      io_lib:format(
        "~-15w  ~w:~w/~w << ~w:~w/~w  ~150p~n~n",
        [Pid, ToM, ToF, ToA, FromM, FromF, FromA, RetVal])).

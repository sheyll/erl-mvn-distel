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
            trace_loop([TraceMsg| Acc])
    end.

format_trace({trace, Pid, call, {M, F, A}},Acc) ->
    acc_str(Pid, format_cols_ss(Pid, "", " >> ", format_mfa({M,F,A}), A), Acc);

format_trace({trace, Pid, exception_from, MFA, Exc},Acc) ->
    acc_str(Pid, fist(),
            acc_str(Pid, format_cols_ss(Pid, format_mfa(MFA), "<===", "*EXCEPTION*", Exc), Acc));

format_trace({trace, Pid, return_from, MFA, RV},Acc) ->
    acc_str(Pid, format_cols_ss(Pid, "", " << ", format_mfa(MFA), RV), Acc);

format_trace({trace, Pid, return_to, MFA},Acc) ->
    acc_str(Pid, format_cols_ss(Pid, format_mfa(MFA), " << ", "", return_to), Acc);

format_trace({trace, Pid, 'receive', Msg},Acc) ->
    acc_str(Pid, format_cols_ss(Pid, "RECEIVE", "<---", "", Msg), Acc);

format_trace({trace, From, send, Msg, To},Acc) ->
    acc_str(From, format_cols_sw(From, "SEND", "--->", To, Msg), Acc);

format_trace({trace, Pid, 'register', Name},Acc) ->
    acc_str(Pid, format_cols_ww(Pid, Name, " :: ", Pid, 'REGISTER'), Acc);

format_trace({trace, Pid, 'unregister', Name},Acc) ->
    acc_str(Pid, format_cols_ww(Pid, Name, " /= ", Pid, 'UNREGISTER'), Acc);

format_trace({trace, Parent, spawn, Child, How},Acc) ->
    acc_str(Parent, format_cols_sw(Parent, "*SPAWN*", "-*->", Child, How), Acc);

format_trace({trace, Left, link, Right},Acc) ->
    acc_str(Left, format_cols_ww(Left, Left, "<==>", Right, 'LINK'), Acc);

format_trace({trace, Left, unlink, Right},Acc) ->
    acc_str(Left, format_cols_ww(Left, Left, "X==X", Right, 'UNLINK'), Acc);

format_trace({trace, _Left, getting_unlinked, _Right},Acc) ->
    %% ignored
    Acc;

format_trace({trace, Pid, exit, Reason},Acc) ->
    acc_str(Pid, format_cols_ss(Pid, "*EXIT*", "<===", "", Reason), Acc);

format_trace(Other, Acc) ->
    [{lists:flatten(io_lib:format("~p ~n~n", [Other]))}|Acc].

acc_str(Pid, Str, Acc) ->
    [{Pid, Str}|Acc].

format_cols_ww(Col1, Col2, Sep, Col3, Col4) ->
    lists:flatten(io_lib:format("~-15w ~40w ~4s ~-40w  ~-70w~n~n", [Col1, Col2, Sep, Col3, Col4])).
format_cols_sw(Col1, Col2, Sep, Col3, Col4) ->
    lists:flatten(io_lib:format("~-15w ~40s ~4s ~-40w  ~-70w~n~n", [Col1, Col2, Sep, Col3, Col4])).
format_cols_ss(Col1, Col2, Sep, Col3, Col4) ->
    lists:flatten(io_lib:format("~-15w ~40s ~4s ~-40s  ~-70w~n~n", [Col1, Col2, Sep, Col3, Col4])).

format_mfa({M,F,A}) when is_list(A) ->
    format_mfa({M,F,length(A)});
format_mfa({M,F,A}) ->
    Str = lists:flatten(io_lib:format("~w:~w/~w", [M,F,A])),
    ShortStr = lists:nthtail(max(0, length(Str) - 40), Str),
    if length(Str) > length(ShortStr) ->
            "(..)" ++  lists:nthtail(4, ShortStr);
       true ->
            ShortStr
    end.

fist() ->
    Fist = " .  * x\\ \\        ___\n  . . \\x  \\   __ /   \\_\nx +.x* +  ** (_ / /    <\\   /\\   /\\   /\\   /\\   /\n  *+   * *** (__\\/ )   <  /    \\    /    \\    /\n  + / +  *** (____)   </   \\/   \\/   \\/   \\/   \\\n x *  / //    (____)___/\n    / * *\n     /\n",
    lists:flatten(io_lib:format("~s", [Fist])).

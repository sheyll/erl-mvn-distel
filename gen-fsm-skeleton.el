(provide 'gen-fsm-skeleton)

(defun gen-fsm-skeleton(module source-dir test-source-dir)
  "Generates a source file and a test source file for a gen-fsm implementation."
  (interactive "sModule Name: \nDSource Directory: \nDTest Source Directory: ")
  (let ((source-file (concat source-dir module ".erl"))
        (test-source-file (concat test-source-dir module "_test.erl")))
    (message (format "Generating %s." source-file))
    (message (format "Generating %s." test-source-file))
    (find-file-literally source-file)
    (insert (gen-fsm-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file-literally test-source-file)
    (insert (gen-fsm-test-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file source-file)))

(defun gen-fsm-source-template(module)
  "Generate a gen-fsm template for a module."
  (format
   "%%%%%%=============================================================================
%%%%%%
%%%%%%               |  o __   _|  _  __  |_   _       _ _   (TM)
%%%%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | |
%%%%%%
%%%%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%%%%% @author Timo Koepke <timo.koepke@lindenbaum.eu>
%%%%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%%%%% @author Olle Toernstroem  <olle.toernstroem@lindenbaum.eu>
%%%%%% @copyright (C) 2012, Lindenbaum GmbH
%%%%%%
%%%%%% @doc
%%%%%% Documentation for this state machine.
%%%%%% @end
%%%%%%=============================================================================

-module(%s).

-behaviour(gen_fsm).

%%%% API
-export([start_link/0]).

%%%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         format_status/2,
         terminate/3,
         code_change/4]).

-record(state,
        {}).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the fsm.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%%%%%=============================================================================
%%%%%% gen_fsm Callbacks
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
init([]) ->
    {ok, start_state, #state{}}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_event(Event, StateName, State) ->
    {stop, unexpected_event, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
    {stop, unexpected_sync_event, {undefined, Event}, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    {next_state, StateName, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

" module))

(defun gen-fsm-test-source-template(module)
  "Generate a gen-fsm test template for a module."
  (format
   "%%%%%%=============================================================================
%%%%%%
%%%%%%               |  o __   _|  _  __  |_   _       _ _   (TM)
%%%%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | |
%%%%%%
%%%%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%%%%% @author Timo Koepke <timo.koepke@lindenbaum.eu>
%%%%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%%%%% @author Olle Toernstroem  <olle.toernstroem@lindenbaum.eu>
%%%%%% @copyright (C) 2012, Lindenbaum GmbH
%%%%%%
%%%%%%=============================================================================

-module(%s_test).

-include_lib(\"eunit/include/eunit.hrl\").

%%%%%%=============================================================================
%%%%%% TESTS
%%%%%%=============================================================================

unhandled_event_test() ->
    process_flag(trap_exit, true),
    Pid = lbm_test_lib:start(%s),
    ?assertEqual(ok, gen_fsm:send_all_state_event(Pid, some_event)),
    receive
        {'EXIT', _, unexpected_event} ->
            ok
    after 100 ->
            throw({test_failed, server_exit_expected})
    end.

unhandled_sync_event_test() ->
    process_flag(trap_exit, true),
    Pid = lbm_test_lib:start(%s),
    ?assertEqual({undefined, some_event},
                 gen_fsm:sync_send_all_state_event(Pid, some_event)),
    receive
        {'EXIT', _, unexpected_sync_event} ->
            ok
    after 100 ->
            throw({test_failed, server_exit_expected})
    end.

unhandled_info_test() ->
    Pid = lbm_test_lib:start(%s),
    Pid ! some_info,
    lbm_test_lib:shutdown(Pid).

code_change_test() ->
    ?assertEqual({ok, state}, %s:code_change(oldvsn, statename, state, extra)).

terminate_test() ->
    ?assertMatch(ok, %s:terminate(reason, statename, state)).

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

" module module module module module module))

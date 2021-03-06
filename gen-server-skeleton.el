(provide 'gen-server-skeleton)

(defun gen-server-skeleton(module source-dir test-source-dir)
  "Generates a source file and a test source file for a gen-server implementation."
  (interactive "sModule Name: \nDSource Directory: \nDTest Source Directory: ")
  (let ((source-file (concat source-dir module ".erl"))
        (test-source-file (concat test-source-dir module "_test.erl")))
    (message (format "Generating %s." source-file))
    (message (format "Generating %s." test-source-file))
    (find-file-literally source-file)
    (insert (gen-server-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file-literally test-source-file)
    (insert (gen-server-test-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file source-file)))

(defun gen-server-source-template(module)
  "Generate a gen-server template for a module."
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
%%%%%% Documentation for this server.
%%%%%% @end
%%%%%%=============================================================================

-module(%s).

-behaviour(gen_server).

%%%% API
-export([start_link/0]).

%%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-registered([?MODULE]).

-record(state,
        {}).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_call(Request, From, State) ->
    {stop, unexpected_call, {undefined, Request}, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_cast(Request, State) ->
    {stop, unexpected_cast, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

" module))

(defun gen-server-test-source-template(module)
  "Generate a gen-server test template for a module."
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

unhandled_call_test() ->
    process_flag(trap_exit, true),
    {ok, Pid} = lbm_test_lib:start(%s),
    Pid ! some_info,
    ?assertEqual({undefined, some_call}, gen_server:call(%s, some_call)),
    receive
        {'EXIT', _, unexpected_call} ->
            ok
    after 100 ->
            throw({test_failed, server_exit_expected})
    end.

unhandled_cast_test() ->
    process_flag(trap_exit, true),
    lbm_test_lib:start(%s),
    ?assertEqual(ok, gen_server:cast(%s, some_cast)),
    receive
        {'EXIT', _, unexpected_cast} ->
            ok
    after 100 ->
            throw({test_failed, server_exit_expected})
    end.

unhandled_info_test() ->
    ?assertEqual({noreply, state}, %s:handle_info(info, state)).

code_change_test() ->
    ?assertEqual({ok, state}, %s:code_change(oldvsn, state, extra)).

terminate_test() ->
    ?assertMatch(ok, %s:terminate(reason, state)).

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

" module module module module module module module module))

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
%%%%%% @copyright (C) 2011, Lindenbaum GmbH
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
    report(error, [unexpected_call, {from, From}, {request, Request}]),
    {stop, unexpected_call, {undefined, Request}, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_cast(Request, State) ->
    report(error, [unexpected_cast, {request, Request}]),
    {stop, unexpected_cast, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_info(Info, State) ->
    report(error, [unexpected_info, {info, Info}]),
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

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec report(info | error, [atom() | {atom(), term()}]) -> ok.
report(info, Report) ->
    DefaultReport = [{module, ?MODULE}, {server, self()}],
    error_logger:info_report(?APP, DefaultReport ++ Report);
report(error, Report) ->
    DefaultReport = [{module, ?MODULE}, {server, self()}],
    error_logger:error_report(?APP, DefaultReport ++ Report).

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
%%%%%% @copyright (C) 2011, Lindenbaum GmbH
%%%%%%
%%%%%%=============================================================================

-module(%s_test).

-include_lib(\"eunit/include/eunit.hrl\").

%%%%%%=============================================================================
%%%%%% TESTS
%%%%%%=============================================================================

unhandled_call_test() ->
    process_flag(trap_exit, true),
    {ok, Pid} = test_utils:start_registered(%s),
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
    test_utils:start_registered(%s),
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

report_test() ->
    logmock:start(self(),
                  [logmock:info_report(?APP,
                                       [{module, %s},
                                        {server, self()},
                                        info]),
                   logmock:error_report(?APP,
                                        [{module, %s},
                                         {server, self()},
                                         error])]),
    %s:report(info, [info]),
    %s:report(error, [error]),
    logmock:verify().

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

" module module module module module module module module module module module module))

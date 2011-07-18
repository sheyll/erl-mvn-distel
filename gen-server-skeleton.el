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
%%%%%%               |  ° __   _|  _  __  |_   _       _ _   (TM)
%%%%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | | 
%%%%%%
%%%%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%%%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%%%%% @author Timo Koepke <timo.koepke@lindenbaum.eu>
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
handle_call(request, _From, 
            State = #state{}) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    utils:default_handle_call(State).


handle_cast(request,
            State = #state{}) ->
    {noreply, State};

handle_cast(_Request, State) ->
    utils:default_handle_cast(State).

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
handle_info(Info, State) ->
    utils:default_handle_info(application_name_here, Info, ?MODULE, State).

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
%%%%%%               |  ° __   _|  _  __  |_   _       _ _   (TM)
%%%%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | | 
%%%%%%
%%%%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%%%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%%%%% @author Timo Koepke <timo.koepke@lindenbaum.eu>
%%%%%% @author Olle Toernstroem  <olle.toernstroem@lindenbaum.eu>
%%%%%% @copyright (C) 2011, Lindenbaum GmbH
%%%%%%
%%%%%%-----------------------------------------------------------------------------

-module(%s_test).

-include_lib(\"eunit/include/eunit.hrl\").

-define(SLEEP(TIMEOUT), receive after TIMEOUT -> ok end).

%%%%%%=============================================================================
%%%%%% TESTS
%%%%%%=============================================================================

code_change_test() ->
    test_utils:assert_default_code_change(%s).

handle_info_test() ->
    test_utils:assert_default_handle_info(application_name_here, 
                                          %s).
handle_cast_test() ->
    test_utils:assert_default_handle_cast(%s).

handle_call_test() ->
    test_utils:assert_default_handle_call(%s).

terminate_test() ->
    ?assertMatch(ok, %s:terminate(reason, state)).

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

setup() ->
    teardown(),
    {ok, P} = %s:start_link(),
    unlink(P),
    P.

teardown() ->
    case whereis(%s) of
        undefined -> 
            ok;
        Pid ->
            exit(Pid, kill)
    end,
    ?SLEEP(50).
" module module module module module module module module))

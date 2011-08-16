(provide 'lbm_dynamic_resource_manager-skeleton)

(defun lbm_dynamic_resource_manager-skeleton(module source-dir test-source-dir)
  "Generates a source file and a test source file for a lbm_dynamic_resource_manager implementation."
  (interactive "sModule Name: \nDSource Directory: \nDTest Source Directory: ")
  (let ((source-file (concat source-dir module ".erl"))
        (test-source-file (concat test-source-dir module "_test.erl")))
    (message (format "Generating %s." source-file))
    (message (format "Generating %s." test-source-file))
    (find-file-literally source-file)
    (insert (lbm_dynamic_resource_manager-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file-literally test-source-file)
    (insert (lbm_dynamic_resource_manager-test-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file source-file)))

(defun lbm_dynamic_resource_manager-source-template(module)
  "Generate a lbm_dynamic_resource_manager template for a module."
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

%%%%%% Documentation for this lbm_dynamic_resource_manager.

%%%%%% @end
%%%%%%=============================================================================

-module(%s).

-behaviour(lbm_dynamic_resource_manager).

%%%% API
-export([]).

%%%% lbm_dynamic_resource_manager callbacks
-export([init/1,
         handle_alloc/2,
         handle_release/2,
         terminate/2]).

-type resource_init_info() :: term().

-record(state,
        {}).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%%%=============================================================================
%%%%%% lbm_dynamic_resource_manager Callbacks
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec init(term()) ->
                  {ok, #state{}}.
init(_) ->
    {ok, #state{}}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec handle_alloc(term(), #state{}) ->
                          {lbm_utils:maybe(resource_init_info()), #state{}}.
handle_alloc(_Request, State = #state{}) ->
    {nothing, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec handle_release(resource_init_info(), #state{}) ->
                            #state{}.
handle_release(_ResourceInitInfo, State = #state{}) ->
    State.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec terminate(term(), #state{}) ->
                       ok.
terminate(_Reason, _State = #state{}) ->
    ok.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

" module))

(defun supervisor-test-source-template(module)
  "Generate a supervisor test template for a module."
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

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

" module))

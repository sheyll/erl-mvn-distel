(provide 'lbm_resource_guard-skeleton)

(defun lbm_resource_guard-skeleton(module source-dir test-source-dir)
  "Generates a source file and a test source file for a lbm_resource_guard implementation."
  (interactive "sModule Name: \nDSource Directory: \nDTest Source Directory: ")
  (let ((source-file (concat source-dir module ".erl"))
        (test-source-file (concat test-source-dir module "_test.erl")))
    (message (format "Generating %s." source-file))
    (message (format "Generating %s." test-source-file))
    (find-file-literally source-file)
    (insert (lbm_resource_guard-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file-literally test-source-file)
    (insert (lbm_resource_guard-test-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file source-file)))

(defun lbm_resource_guard-source-template(module)
  "Generate a lbm_resource_guard template for a module."
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

%%%%%%

%%%%%% @end
%%%%%%=============================================================================
-module(%s).

-behaviour(lbm_resource_guard).

%%%% API
-export([]).

%%%% lbm_resource_guard callbacks
-export([handle_create/1,
         handle_destroy/2,
         handle_call/3,
         handle_cast/2]).

-type resource_init_info() :: term().

-type resource_info() :: term().

-record(state, 
        {}).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%%%=============================================================================
%%%%%% lbm_resource_guard Callbacks
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec handle_create(resource_init_info()) ->
                           {ok, resource_info(), #state{}}.
handle_create(_ResourceInitInfo) ->
   {ok, resource_info, #state{}}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec handle_call(term(), any(), #state{}) ->
                         {reply, term(), #state{}}
                             | {noreply, term(), #state{}}
                             | {stop, term(), term(), #state{}}.
handle_call(_Request, _From, State = #state{}) ->
   {ok, resource_info, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec handle_cast(term(), #state{}) ->                         
                         {reply, term(), #state{}}
                             | {stop, term(), #state{}}.
handle_cast(_Request, State = #state{}) ->
   {noreply, State}.

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
-spec handle_destroy(term(), #state{}) ->
                         ok.
handle_destroy(_Reason, _State = #state{}) ->
   ok.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================

" module))

(defun lbm_resource_guard-test-source-template(module)
  "Generate a lbm_resource_guard test template for a module."
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

" module))

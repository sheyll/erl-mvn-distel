(provide 'application-skeleton)

(defun application-skeleton(module source-dir test-source-dir)
  "Generates a source file and a test source file for a application implementation."
  (interactive "sModule Name: \nDSource Directory: \nDTest Source Directory: ")
  (let ((source-file (concat source-dir module ".erl"))
        (test-source-file (concat test-source-dir module "_test.erl")))
    (message (format "Generating %s." source-file))  
    (message (format "Generating %s." test-source-file))
    (find-file-literally source-file)
    (insert (application-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file-literally test-source-file)
    (insert (application-test-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file source-file)))

(defun application-source-template(module)
  "Generate a application template for a module."
  (format 
   "%%%%%%=============================================================================
%%%%%% @author Name  <user@domain>
%%%%%% @copyright (C) 2011, ACME
%%%%%%
%%%%%% @doc
%%%%%% Documentation for this application.
%%%%%% @end
%%%%%%=============================================================================

-module(%s).

-behaviour(application).

%%%% API
-export([]).

%%%% application callbacks
-export([start/2, stop/1]).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%%%=============================================================================
%%%%%% application Callbacks
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    utils:start_link(%s_sup).

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
stop(_State) ->
    ok.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================
" module module))

(defun application-test-source-template(module)
  "Generate a application test template for a module."
  (format 
   "%%%%%%=============================================================================
%%%%%% @author Name  <user@domain>
%%%%%% @copyright (C) 2011, ACME
%%%%%%=============================================================================

-module(%s_test).

-include_lib(\"eunit/include/eunit.hrl\").

%%%%%%=============================================================================
%%%%%% TESTS
%%%%%%=============================================================================

start_succeeds_test() ->
    M = em:new(),
    em:strict(M, %s_sup, start_link, [],
              {return, {ok, pid}}),
    em:replay(M),
    ?assertMatch({ok, pid},
                 %s:start(startType, startArgs)),
    em:verify(M).

start_error_test() ->
    M = em:new(),
    em:strict(M, %s_sup, start_link, [],
                {return, {error, reason}}),
    em:replay(M),
    ?assertMatch({error, reason},
                 %s:start(startType, startArgs)),
    em:verify(M).

start_unexpected_test() ->
    M = em:new(),
    em:strict(M, %s_sup, start_link, [],
                {return, {other, reason}}),
    em:replay(M),
    ?assertMatch({error, {unexpected, {other, reason}}},
                 %s:start(startType, startArgs)),
    em:verify(M).

stop_test() ->
    ?assertMatch(ok, %s:stop(state)).

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================
" module module module module module module module module))

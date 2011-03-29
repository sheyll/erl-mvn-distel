(provide 'supervisor-skeleton)

(defun supervisor-skeleton(module source-dir test-source-dir)
  "Generates a source file and a test source file for a supervisor implementation."
  (interactive "sModule Name: \nDSource Directory: \nDTest Source Directory: ")  
  (let ((source-file (concat source-dir module ".erl"))
        (test-source-file (concat test-source-dir module "_test.erl")))
    (message (format "Generating %s." source-file))  
    (message (format "Generating %s." test-source-file))
    (find-file-literally source-file)
    (insert (supervisor-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file-literally test-source-file)
    (insert (supervisor-test-source-template module))
    (save-buffer)
    (kill-buffer)
    (find-file source-file)))

(defun supervisor-source-template(module)
  "Generate a supervisor template for a module."
  (format "%%%%%%=============================================================================
%%%%%% @author Name <user@domain>
%%%%%% @copyright (C) 2011, ACME
%%%%%% @doc
%%%%%% @end
%%%%%%=============================================================================
-module(%s).

-behaviour(supervisor).

%%%% API
-export([start_link/1]).

%%%% Supervisor callbacks
-export([init/1]).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Starts the supervisor.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link(Config) ->
    supervisor:start_link(?MODULE, Config).

%%%%%%=============================================================================
%%%%%% supervisor Callbacks
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @private
%%%%------------------------------------------------------------------------------
init(_Config) ->
    RestartStrategy = one_for_all,
    MaxRestarts = 3,
    MaxTSeconds = 1800,

    SupFlags = {RestartStrategy, MaxRestarts, MaxTSeconds},

    {ok, {SupFlags, [{some_child, 
                      {some_child, start_link, []},
                      temporary, 500, worker, [some_child]}]}}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================
" module))

(defun supervisor-test-source-template(module)
  "Generate a supervisor test template for a module."
  (format "%%%%%%=============================================================================
%%%%%% @author Name <user@domain>
%%%%%% @copyright (C) 2011, ACME
%%%%%%=============================================================================
-module(%s_test).

-include_lib(\"eunit/include/eunit.hrl\").

correct_config_and_children_test() ->
    Actual = %s:init([]),
    ?assertMatch({ok, 
                  {{simple_one_for_one, 0, 1800},
                   [{some_child, 
                     {some_child, start_link, []},
                     temporary, 500, worker, [some_child]}]}},
                 Actual).

" module module))

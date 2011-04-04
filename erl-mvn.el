(provide 'erl-mvn)

(require 'application-skeleton)
(require 'supervisor-skeleton)
(require 'gen-server-skeleton)
;; ----------------------------------------------------------------------
;;  Global Vars
;; ----------------------------------------------------------------------

(defvar erl-mvn-erlang-executable "erl"
  "The path to the erlang executable.")

(defvar erl-mvn-maven-executable "mvn"
  "The path to the maven executable.")

(defvar erl-mvn-xterm-executable "gnome-terminal"
  "The terminal emulator to use for erlang remote shells.")

(defvar erl-mvn-hostname 
  (car (process-lines "hostname" "-f"))
  "The hostname of the erlang nodes for maven and distel.")

(defvar erl-mvn-popup-compiler-output 'nil
  "Setting this to non nil will cause a buffer with the
 compilation results to popup everytime an error or warning was
 returned from an erlang buffer compilation")

(defvar erl-mvn-popup-eunit-output 'nil
  "Setting this to non nil will cause a buffer with the
 eunit results to popup everytime tests were run.")

(defconst erl-mvn-erlang-mvn-packaging-types '("erlang-otp" "erlang-std")
  "The packaging types that identify a project as erlang
  project")

(defvar erl-mvn-erl-mvn-erlang-sources
  (file-truename 
   (file-name-directory 
            (or (locate-library "distel") load-file-name)))
   "Path to the erlang sources shipped with erl-mvn.")

(defvar erl-mvn-auto-start-maven-node 't
  "If non-nil an erlang node is started and code is. Do not change unless you want to start each erlang node manually.")

;; ----------------------------------------------------------------------
;;  These variables will be set while working with erl-mvn
;; ----------------------------------------------------------------------

(defvar erl-mvn-code-paths nil
  "Contains the code paths of each project in the current
  workspace as prop list where the key is the artifact-id.")

(defvar erl-mvn-include-paths nil
  "Contains the include paths of each project in the current
  workspace as prop list where the key is the artifact-id.")

(defvar erl-mvn-source-paths nil
  "Contains the source paths of the projects in the current
  workspace as prop list where the key is the artifact-id.")

(defvar erl-mvn-test-source-paths nil
  "Contains the test-source paths of the projects in the current
  workspace as prop list where the key is the artifact-id.")

(defvar erl-mvn-open-projects nil
  "Contains a list of buffers representing maven erlang projects. Each erlang maven project has its own erlang test node.")

(defvar erl-mvn-erl-source-path
  (file-truename
   (file-name-directory (or (locate-library "erl-mvn") load-file-name)))
   "Path to the erlang sources shipped with erl-mvn..")

(defvar erl-mvn-mode-map 
  (let ((the-map
         '(keymap 
           (menu-bar . 
                     (keymap 
                      (erl-mvn "Maven" . 
                               (keymap                          
                                
                                (sep1 . (menu-item "Skeletons:"))
                                (sep1a . (menu-item "--"))
                                (application-skel . 
                                                 (menu-item "Application" 
                                                            application-skeleton :keys "C-c C-v a"))
                                (supervisor-skel . 
                                                 (menu-item "Supervisor" 
                                                            supervisor-skeleton :keys "C-c C-v u"))
                                (gen-server-skeleton . 
                                                     (menu-item "Gen-Server" 
                                                                gen-server-skeleton :keys "C-c C-v g"))
                                (sep1e . (menu-item "--"))

                                
                                (sep2 . (menu-item "Current Buffer:"))
                                (sep2a . (menu-item "--"))
                                (toggle-source-test . 
                                                    (menu-item "Switch Between Source <-> Test" 
                                                               erl-mvn-toggle-source-test :keys "C-c C-v s or F5"))
                                (test-function . 
                                               (menu-item "Eunit-test Function" 
                                                          erl-mvn-eunit-test-function :keys "C-c C-v t or F6"))
                                (test-module . 
                                             (menu-item "Eunit-test Module" 
                                                        erl-mvn-eunit-test-module :keys "C-c C-v T or F7"))
                                (sep2e . (menu-item "--"))
                                                                                                
                                (sep3 . (menu-item "Erlang Node:"))
                                (sep3a . (menu-item "--"))
                                (run-erlang-console . 
                                                    (menu-item "Erlang Node Remoteshell" 
                                                               erl-mvn-erlang-node-remote-shell))
                                (sep3b . (menu-item "--"))
                                (close . 
                                       (menu-item "Shutdown Erlang Node" 
                                                  erl-mvn-close-current-project))
                                )))))))
    (define-key the-map (kbd "C-c C-v a") 'application-skeleton)
    (define-key the-map (kbd "C-c C-v u") 'supervisor-skeleton)
    (define-key the-map (kbd "C-c C-v g") 'gen-server-skeleton)

    (define-key the-map [?\C-c ?\C-v ?s] 'erl-mvn-toggle-source-test)
    (define-key the-map [?\C-c ?\C-v ?t] 'erl-mvn-eunit-test-function)
    (define-key the-map (kbd "C-c C-v C-t") 'erl-mvn-eunit-test-module)

    (define-key the-map (kbd "<f5>") 'erl-mvn-toggle-source-test)
    (define-key the-map (kbd "<f6>") 'erl-mvn-eunit-test-function)
    (define-key the-map (kbd "<f7>") 'erl-mvn-eunit-test-module)
    the-map)
    "Erl Mvn minor mode keymap.")

;; ----------------------------------------------------------------------
;; Erlang source buffer minor mode
;; ----------------------------------------------------------------------
(define-minor-mode erl-mvn-mode
  "Toggle Erlang-Maven integration mode.
With no argument,  this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.
"
  ; initial value
  nil
  ; mode line indicator
  " Erl-Mvn"
  ; mode bindings
  erl-mvn-mode-map)

;; ----------------------------------------------------------------------
;;  Startup and shutdown functions.
;; ----------------------------------------------------------------------

(defun erl-mvn-erlang-node-remote-shell()
  "Open the terminal program defined in the variable erl-mvn-xterm-executable."
  (interactive)
  (let ((node-name (erl-mvn-make-node-name erl-mvn-artifact-id))
        (tmp-node-name (format "erl-mvn-remsh-%s@%s" (random) erl-mvn-hostname)))
    (start-process tmp-node-name (format "*%s*" tmp-node-name) erl-mvn-xterm-executable "-e" 
                   (format "%s -remsh %s -name %s" erl-mvn-erlang-executable node-name tmp-node-name))))

(defun erl-mvn-toggle-source-test ()
  "If the buffer is a relevant erlang buffer open the appropriate test source, or
if the buffer-file is in the test sources folder, visit the source."
  (interactive)
  (erl-mvn-with-directories 
   (lambda (source-dir test-source-dir fn-dir)
     (cond 
         ((string= fn-dir test-source-dir)
          (let ((file-name (file-name-nondirectory buffer-file-name)))
            (string-match "^\\(.+\\)_test.erl$" file-name)
            (let ((target-file-name (concat source-dir (replace-match "\\1.erl" 'nil 'nil file-name))))
              (find-file target-file-name))))
         ((string= fn-dir source-dir)
          (let ((file-name (file-name-nondirectory buffer-file-name)))
            (string-match "^\\(.+\\).erl$" file-name)
            (let ((target-file-name (concat test-source-dir (replace-match "\\1_test.erl" 'nil 'nil file-name))))
              (find-file target-file-name))))))))

(defun erl-mvn-compile-project(pom-file)
  "Asks for a pom to open. And compile the project using maven."
  (interactive 
     (let ((pom-file 
	    (read-file-name "Open POM: " 
			    (file-name-directory buffer-file-name)
			    (erl-mvn-find-pom (file-name-directory buffer-file-name))
			    'confirm)))
       (list (file-truename pom-file))))
  (erl-mvn-start-erlang-and-compile-project pom-file))

(defun erl-mvn-compile-current-project()
  "Compiles the project that the current buffer belongs to using maven."
  (interactive)
  (let ((pom-file (erl-mvn-find-pom (file-name-directory buffer-file-name))))
    (erl-mvn-start-erlang-and-compile-project pom-file)))

(defun erl-mvn-start-erlang-and-compile-project(pom-file)
  "If the pomfile describes a not already open project, a buffer
will be created containing a description of the project and the
associated assets. 

An erlang node will be started, and maven compile will be executed
with the erlang node as target for module upload. 

Distel modules are deployed to the erlang node, and all distel function 
should work properly.

Automatic recompilation on saving erlang sources in that project is activated."
    (if pom-file
	(let* ((artifact-id (erl-mvn-pom-lookup pom-file 'artifactId))
	       (node-name (erl-mvn-make-node-name artifact-id)))
	  (if (not (erl-mvn-is-maven-erlang-project pom-file))
	      (message "No maven-erlang project found in %s" proj-dir)
	    (progn
	      (erl-mvn-start-node node-name)
	      (add-to-list 'erl-mvn-open-projects artifact-id)
	      (erl-mvn-compile-project-maven pom-file)))
	  (erl-mvn-refresh-buffers))))

(defun erl-mvn-close-current-project()
  "Closes a project opened with erl-mvn-open-project. Removes all processes and buffers associated to the artifact-id of the project. The project that is closed is determined by the current buffer."
  (interactive)
  (let* ((pom (erl-mvn-find-pom (file-name-directory buffer-file-name)))
	 (artifact-id (erl-mvn-pom-lookup pom 'artifactId)))
    (setq erl-mvn-open-projects (delete artifact-id erl-mvn-open-projects))
    (let* ((node-name (erl-mvn-make-node-name artifact-id))
	 (erl-buf (erl-mvn-make-buffer-name node-name)))
      (cond ((erl-mvn-node-running node-name)
	     (delete-process erl-buf)
	     (kill-buffer erl-buf))))
    (kill-buffer 
     (erl-mvn-make-mvn-output-buffer-name artifact-id))
    (erl-mvn-refresh-buffers)))

(defun erl-mvn-setup()
  "Adds the compile function to the save hooks of erlang files."
  (interactive)
  (setq erl-mvn-compilation-result-overlays 'nil)
  (make-variable-buffer-local 'erl-mvn-compilation-result-overlays)
  (setq erl-mvn-tmp-source-file "")
  (make-variable-buffer-local 'erl-mvn-tmp-source-file)
  (setq erl-mvn-current-buffer 'nil)
  (setq erl-mvn-irrelevant-buffers 'nil)
  (setq erl-mvn-is-dirty 'nil)
  (make-variable-buffer-local 'erl-mvn-is-dirty)
  (setq erl-mvn-artifact-id 'nil)
  (make-variable-buffer-local 'erl-mvn-artifact-id)
  (setq erl-mvn-pom-file 'nil)
  (make-variable-buffer-local 'erl-mvn-pom-file)

  ; hooks
  (add-hook 
   'erlang-mode-hook
   (lambda ()
     (let ((pom (erl-mvn-find-pom (file-name-directory buffer-file-name))))
       (cond (pom              
              (if (not (or (erl-mvn-node-running-for-pom pom)
                           (not erl-mvn-auto-start-maven-node)))
                  (erl-mvn-compile-current-project))
	      (erl-mvn-setup-buffer (current-buffer)))))))
  (add-hook 'after-save-hook 
	    (function erl-mvn-erl-buffer-saved))
  (add-hook 'pre-command-hook (function erl-mvn-update-distel-node))
  (add-to-list 'after-change-functions (function erl-mvn-mark-buffer-dirty))
  (run-with-idle-timer 1 't (function erl-mvn-check-current-buffer)))

(defun erl-mvn-refresh-buffers()
  "Private function. Clears the irrelevant buffer cache and sets up header line and minor mode for all relevant erlang buffers."
  (setq erl-mvn-irrelevant-buffers 'nil)
  (mapcar 
   'erl-mvn-setup-buffer
   (buffer-list)))

(defun erl-mvn-setup-buffer(buffer)
  "Private function. Sets the header line and minor mode menu."
  (with-current-buffer buffer
    (if (and (not (minibufferp buffer))
             buffer-file-name)
        (let ((pom (erl-mvn-find-pom (file-name-directory buffer-file-name))))
          (cond ((and pom
                      (string-match "^.+\.erl$" buffer-file-name))
                 (let* ((ai (erl-mvn-pom-lookup pom 'artifactId))
                        (output-dir (file-truename 
                                     (concat (file-name-directory pom) 
                                             "target/emacs-compiled/")))
                        (source-file (file-truename 
                                      (concat output-dir 
                                              (file-name-nondirectory buffer-file-name)))))
                   (setq erl-mvn-artifact-id ai)
                   (setq erl-mvn-pom-file pom)
                   (setq erl-mvn-output-dir output-dir)
                   (setq erl-mvn-tmp-source-file source-file)
                   (cond  ((erl-mvn-with-directories 
                            (lambda (src-dir test-src-dir fn-dir)
                              (and (member ai erl-mvn-open-projects)
                                   (or (string= src-dir fn-dir)
                                       (string= test-src-dir fn-dir)))))
                           (erl-mvn-mode 1)
                           (setq header-line-format 
                                 (concat 
                                  (erl-mvn-pom-lookup  pom 'groupId)
                                  "/"
                                  (propertize erl-mvn-artifact-id 'face 'bold)
                                  "  "
                                  (erl-mvn-pom-lookup  pom 'version)
                                  (propertize "  *ERLANG NODE ACTIVE*" 'face 'bold)))
                           (force-mode-line-update))))))))))

(defun erl-mvn-update-distel-node()
  "Updates distel variables to contain the erlang node of the project the current file belongs to." 
  (cond ((and (not (minibufferp))
	      (not (equal erl-mvn-current-buffer (current-buffer))))
	 (setq erl-mvn-current-buffer (current-buffer))
         (if (erl-mvn-is-relevant-erl-buffer)
	     (erl-mvn-update-distel-settings)))))

(defun erl-mvn-check-current-buffer ()
  "Will compile the current buffer and highlight all compilation errors."
  (cond (erl-mvn-is-dirty
	 (setq erl-mvn-is-dirty 'nil)
	 (cond ((erl-mvn-is-relevant-erl-buffer)
		(erl-mvn-compile-buffer 'nil))))))

(defun erl-mvn-mark-buffer-dirty(beginning end old-length)
  "Marks a buffer dirty. This is interpreted by the idle timer callback that decides wether a buffer needs recompilation."
  (setq erl-mvn-is-dirty 't))

(defun erl-mvn-erl-buffer-saved()
  "When a buffer is saved it is automatically compiled. Compiler errors and
 warnings are displayed in a seperate buffer"
  (interactive)
  (setq erl-mvn-is-dirty 'nil)
  (if (erl-mvn-is-relevant-erl-buffer)
      (erl-mvn-compile-buffer 't)))

(defun erl-mvn-is-relevant-erl-buffer()
  "Private function. Determines if the current buffer contains
erlang code managed by the current node."
  erl-mvn-mode)

;; ----------------------------------------------------------------------
;;  Functions for maven interaction
;; ----------------------------------------------------------------------

(defun erl-mvn-compile-project-maven(pom-file)
  "Private function. Invokes maven to compile an erlang project
defined by a pom file. Extracts source folders, include- and
code_paths."
  (let ((pom-file (file-truename pom-file)))
    (let ((artifact-id  (erl-mvn-pom-lookup pom-file 'artifactId))
	  (packaging (erl-mvn-pom-lookup pom-file 'packaging))
	  (include-dirs '())
	  (code-paths '())
	  (src-dir "")
	  (test-src-dir "")
	  (build-failed nil))
      (if (not (member artifact-id erl-mvn-open-projects))
	  (progn
	    (message "Project %s is currently not managed." artifact-id)
	    (erl-mvn-open-project pom-file))
	(progn
	  (message "Building project %s of type %s" artifact-id packaging)
	  (let ((mvn-result (erl-mvn-run-maven pom-file))
                (node-name (erl-mvn-make-node-name artifact-id))
                (node (make-symbol node-name)))
	    (mapcar 
	     (lambda(l) 
	       (cond ((string-match " include_dir: \\(.+\\)$" l)
		      (add-to-list 'include-dirs (file-truename (match-string 1 l))))
		     ((string-match " src_dir: \\(.+\\)$" l)
		      (setq src-dir (concat (file-truename (match-string 1 l)) "/")))
		     ((string-match " test_src_dir: \\(.+\\)$" l)
		      (setq test-src-dir (concat (file-truename (match-string 1 l)) "/")))
		     ((string-match "^\\[ERROR\\] BUILD FAILURE" l)
		      (setq build-failed 't))
		     ((string-match " code_path: \\(.+\\)$" l)
		      (add-to-list 'code-paths (file-truename (match-string 1 l))))))
	     mvn-result)
	    (if build-failed
		(message "Building and adding maven project %s failed!" artifact-id)
	      (progn
		(add-to-list 'erl-mvn-include-paths  `(,artifact-id ,include-dirs))   
		(add-to-list 'erl-mvn-code-paths `(,artifact-id ,code-paths))
		(add-to-list 'erl-mvn-source-paths `(,artifact-id ,src-dir))
		(add-to-list 'erl-mvn-test-source-paths `(,artifact-id ,test-src-dir))
                (erpc node 'code 'add_pathsa (list code-paths))
                (sleep-for 0.2)
                ))))))))

(defun erl-mvn-run-maven(pom)
  "Private function. Runs maven and pastes the output into a
buffer, so the user is able to follow the output."
  (message "Invoking maven with pom %s" pom)
  (let* ((artifact-id (erl-mvn-pom-lookup pom 'artifactId))
	 (node-name (erl-mvn-make-node-name artifact-id))
	 (mvn-output-buffer (erl-mvn-make-mvn-output-buffer-name artifact-id)))
    (save-excursion
      (with-current-buffer 
	  (get-buffer-create mvn-output-buffer)
        (setq buffer-read-only nil)
        (save-selected-window          
          (select-window (or (get-buffer-window (current-buffer))
                             (display-buffer (current-buffer))))          
          (goto-char (point-max))          
          (let ((start (point)))
            (call-process erl-mvn-maven-executable
                          nil ; infile
                          mvn-output-buffer
                          't   ; redisplay
                          "-f" pom
                          (concat "-Dremote=" node-name)
                          "-DwithDependencies=true"
                          "erlang:show-build-info"
			  "erlang:upload-tests")
            (setq buffer-read-only 't)
            (erl-mvn-to-lines (buffer-substring-no-properties start (point-max)))))))))

(defun erl-mvn-is-maven-erlang-project(pom-file)
  "Private function. Returns t if the pom-file exists and defines
an erlang project."
  (if (file-exists-p pom-file) 
       (let ((packaging (erl-mvn-pom-lookup pom-file 'packaging)))
         (some (lambda (allowed) 
                 (string= packaging allowed))
               erl-mvn-erlang-mvn-packaging-types))
    'nil))

(defun erl-mvn-find-pom(fn)
  "Private function. Searches for a file called pom.xml in the
parent directories of fn, returns nil if the pom was not found"
  (let ((dir (file-name-directory fn)))
    (while (and (not (file-exists-p (concat dir "/pom.xml")))
                (not (equal dir (file-truename (concat dir "/..")))))
      (setq dir (file-truename (concat dir "/.."))))
    (let ((pom (concat dir "/pom.xml")))
      (if (not (file-exists-p pom))
          (progn 
            (message "No pom.xml found")
            'nil)
        pom))))

(defun erl-mvn-pom-lookup(pom tag)
  "Private function. Get a child of a the project element from the pom"
   (let* ((root (car (xml-parse-file pom)))
         (res-node (xml-get-children (xml-node-children root) tag)))
     (car (xml-node-children (car res-node)))))

(defun erl-mvn-to-lines(str)
  "Private function. Convert a string with newline characters
into a list where each element contains one line. The newline
character will be discarded"
  (let ((current-line '())
        (lines '()))
    (mapcar 
     (lambda (c) 
       (if (eq c ?\n)
           (progn (setq lines (cons (eval `(string ,@(reverse current-line))) lines))
                  (setq current-line '()))  
         (setq current-line (cons c current-line))))
     str)
    ; append the rest
    (setq lines (cons (eval `(string ,@(reverse current-line))) lines))
    (reverse lines)))


;; ----------------------------------------------------------------------
;;  Functions that care for the erlang node.
;; ----------------------------------------------------------------------

(defun erl-mvn-start-node(node-name)
  "Private function. Starts an erlang node for the node-name that
can be used by maven for tests and debug code"
  (if (erl-mvn-node-running node-name)
      (message (concat "Node already running: " node-name))
    (progn
      (message (concat "Starting node name: " node-name))            
      (start-process (erl-mvn-make-buffer-name node-name)
		     (erl-mvn-make-buffer-name node-name)
		     erl-mvn-erlang-executable
		     "-name" node-name)
      (sleep-for 1)
      (erl-mvn-prepare-erlang-node node-name))))


(defun erl-mvn-node-running-for-pom(pom-file)
  "Private function. Returns 't if an erlang node is running, to wich code of the projected identified by the pom file is uploaded."
  (erl-mvn-node-running 
   (erl-mvn-make-node-name 
    (erl-mvn-pom-lookup pom-file 'artifactId))))

(defun erl-mvn-node-running(node-name)
  "Private function. Returns 't if a an erlang process was
already started for node-name, by checking wether a buffer of
that name exits"
  (not (eq 'nil (member (erl-mvn-make-buffer-name node-name) (mapcar (function buffer-name) (buffer-list))))))

(defun erl-mvn-prepare-erlang-node (node-name) 
  "Private function. Connectes distel to a node identified by an
erlang long node name string, and uploads all erlang modules necessary for erl-mvn, currently only the eunit execution glue-code."
  (let ((n (make-symbol node-name)))
    (setq erl-nodename-cache n)    
    (erl-ping n)
    (sleep-for 1)
    (erl-mvn-upload-erlang-modules n)))
             
(defun erl-mvn-make-node-name(str)
  "Private function. Creates a long erlang node name from a string."
  (concat "erl-mvn-test-node-" str "@" erl-mvn-hostname))

(defun erl-mvn-update-distel-settings()
  "Updates distel settings so that distel functions work for the
current buffer. This is necessary as erl-mvn has a seperate
erlang node for each maven project, and distel always works with
just one node."
  (if (and (not (minibufferp))
	   (erl-mvn-is-relevant-erl-buffer))
      (let ((node 
	     (make-symbol
	      (erl-mvn-make-node-name 
	       (erl-mvn-pom-lookup 
		(erl-mvn-find-pom buffer-file-name) 
		'artifactId)))))
	(setq erl-nodename-cache node))))

(defun erl-mvn-with-directories(k)
  "Private function. Run k with three parameters: source path, test source path and directory of current buffer."
    (let*
      ((src-dir 
	(cadr (assoc erl-mvn-artifact-id erl-mvn-source-paths)))
       (test-src-dir 
	(cadr (assoc erl-mvn-artifact-id erl-mvn-test-source-paths)))
       (fn-dir (file-name-directory buffer-file-name)))
      (apply k (list src-dir test-src-dir fn-dir))))
;; ----------------------------------------------------------------------
;;  Functions interacting with source buffers and an erlang node.
;; ----------------------------------------------------------------------

(defun erl-mvn-upload-erlang-modules(node)
  "Private function. Upload all erl-mvn erlang modules to node"
  (sleep-for 0.2)
  (erpc node 'c 'c (list (concat erl-mvn-erl-source-path "erl_mvn_eunit.erl") `([outdir ,erl-mvn-erl-source-path])))
  (sleep-for 0.2)
  (erpc node 'c 'c (list (concat erl-mvn-erl-source-path "erl_mvn_source_utils.erl") `([outdir ,erl-mvn-erl-source-path])))
  (sleep-for 0.2))

(defun erl-mvn-eunit-test-function()
  "Private function. Runs either the eunit test for a complete module or the test function under the cursor.
Ignores modules not in the test source directory."
  (interactive)
  (erl-mvn-eunit-run-at-line (lambda() (line-number-at-pos))))

(defun erl-mvn-eunit-test-module()
  "Private function. Runs eunit for a complete module.
Ignores modules not in the test source directory."
  (interactive)
  (erl-mvn-eunit-run-at-line (lambda() 0)))
  
(defun erl-mvn-eunit-run-at-line(line-fun)
  "Private function. Runs a a single test function near the point, or 
of line is 0 the complete module. The results will be displayed in a 
buffer and through graphical annotations. The argument must be a function 
that returns the line to consider. It will be called after the switch to 
the buffer containing the test."
  (erl-mvn-prepare-compilation-current-buffer)
  (erl-mvn-with-directories
   (lambda (source-dir test-source-dir fn-dir)
     (if (string= fn-dir source-dir)
         (erl-mvn-toggle-source-test))
     (if (erl-mvn-is-relevant-erl-buffer)
         (let* ((node-name (erl-mvn-make-node-name erl-mvn-artifact-id))
                (erl-popup-on-output-old erl-popup-on-output)             
                (node (make-symbol node-name))
                (line (apply line-fun '()))
                (args (list erl-mvn-tmp-source-file line)))
           (setq erl-popup-on-output erl-mvn-popup-eunit-output)
           (remove-overlays 'nil 'nil 'eunit-overlay 't)
           (setq erl-eunit-source-buffer (current-buffer))
           (erl-spawn
             (erl-send-rpc node 'erl_mvn_eunit 'run_test_file_line args)             
             (erl-receive (erl-popup-on-output-old erl-eunit-source-buffer)
                 ((['rex result]
                   (erl-mvn-show-eunit-results result erl-eunit-source-buffer)              
                   (setq erl-popup-on-output erl-popup-on-output-old))))))))))

(defun erl-mvn-prepare-compilation-current-buffer()
  "Private function. Creates intermediate directories, and stores the contents of the buffer for compilation.
Adds to the codepath all necessare dependencies and load the erl-mvn helper modules to the erlang node for that buffer."
  (let* ((node-name (erl-mvn-make-node-name erl-mvn-artifact-id))
         (node (make-symbol node-name)))
    (make-directory erl-mvn-output-dir 'parents)
    (write-region (point-min) (point-max) erl-mvn-tmp-source-file)))

(defun erl-mvn-compile-buffer (load-module)
  "Compile the file of the buffer with the corresponding erlang
node uploading it as a side effect, if it belongs to a project
currently managed. If only-check is non-nil, no code will
actually be loaded and is checked only for errors and warnings"
  (interactive "Sload-module: ")
    (let* 
        ((old-kill-ring (copy-list kill-ring))
         (fn (file-truename (buffer-file-name)))
         (erl-source-buffer (current-buffer))
         (node-name (erl-mvn-make-node-name erl-mvn-artifact-id))
         (node (make-symbol node-name))
         (erl-popup-on-output-old erl-popup-on-output)
         (args (cons (if load-module
                         fn
                         erl-mvn-tmp-source-file)
                     (list (erl-mvn-get-erlang-compile-options
                            erl-mvn-artifact-id 
                            erl-mvn-output-dir)))))
      (setq erl-popup-on-output nil)
      (erl-mvn-prepare-compilation-current-buffer)
      (erl-spawn
        (message "Compiling %s from project %s" fn erl-mvn-artifact-id)
        (erl-send-rpc node 'compile 'file args)
        (erl-receive (erl-popup-on-output-old erl-mvn-output-dir erl-source-buffer node load-module fn old-kill-ring)
            ((['rex result]
              (mcase result              
                
                (['ok module warnings]
                 (erl-mvn-show-compilation-results '() warnings erl-source-buffer)
                 (message "Successfully compiled module: %s." module)
                 (cond (load-module
                        (erpc node 'erl_mvn_source_utils 'load_module 
                              (list module (format "%s/%s" erl-mvn-output-dir module)))
                        (message "Successfully loaded module %s into node %s." module node)))
                 (setq kill-ring old-kill-ring)
                 (setq erl-popup-on-output erl-popup-on-output-old))
                
                (['error errors warnings] 
                 (message "Compilation failed!")            
                 (erl-mvn-show-compilation-results errors warnings erl-source-buffer)
                 (setq kill-ring old-kill-ring))
                
                (unexpected (message "Unexpected message %s" unexpected)))))))))

(defun erl-mvn-get-erlang-compile-options (artifact-id output-dir)
  "Private function. Returns a list of compiler arguments for
compiling a source file of a project identified by a maven
project artifact-id."
  (append `(debug_info return verbose export_all ,(tuple 'outdir output-dir))
          (mapcar (lambda(dir) (tuple 'i dir)) 
                  (cadr (assoc artifact-id erl-mvn-include-paths)))))

(defun erl-mvn-show-compilation-results(errors warnings mark-errors-buffer)
  "Private function. Show a buffer with formatted erlang
compilation errorsa and warnings if erl-mvn-popup-compiler-output
is not nil."
  (save-excursion
    (with-current-buffer (get-buffer-create "*erl-mvn-compile-errors*")	  
      (if (and erl-mvn-popup-compiler-output
               (not (and (eq errors '()) (eq warnings '()))))
          (save-selected-window        
            (select-window (or (get-buffer-window (current-buffer))
                               (display-buffer (current-buffer))))))
      (fundamental-mode)
      (setq buffer-read-only nil)
      (kill-region (point-min) (point-max))
      (insert "Errors:\n=======\n")
      (erl-mvn-format-compiler-errors errors)
      (insert "\n\nWarnings:\n=========\n")
      (erl-mvn-format-compiler-warnings warnings)
      (compilation-mode)
      (let ((error-lines (erl-mvn-extract-linenumbers (append errors warnings))))
        (erl-mvn-mark-warnings error-lines mark-errors-buffer)))))

(defun erl-mvn-show-eunit-results(results mark-buffer)
  "Private function. Show the results of an eunit run."
  (let ((base-dir 
         (with-current-buffer mark-buffer 
           (file-name-directory buffer-file-name))))
    (save-excursion
      (with-current-buffer (get-buffer-create "*erl-mvn-eunit-results*")	  
        (if erl-mvn-popup-eunit-output
            (save-selected-window        
              (select-window (or (get-buffer-window (current-buffer))
                                 (display-buffer (current-buffer))))))
        (fundamental-mode)
        (setq buffer-read-only nil)
        (kill-region (point-min) (point-max))
        (mapcar 
         (lambda (r)
           (mcase r
             (['error function line reason]
              (insert (concat base-dir reason))
              line)
             
             (['ok function line] 
              line)))
         results)
        (compilation-mode)
        (erl-mvn-mark-eunit-results results mark-buffer)))))

(defun erl-mvn-format-compiler-errors(msgs)
  "Private function that formats a list of compiler errors as
returned by compile:file/2 and inserts them into the current
buffer."
  (mapcar 
   (lambda(file-e)      
     (mlet [file issues] file-e
       (mapcar (lambda (e)
		 (mlet [line type raw-msg] e
		   (let  ((msg 
			   (mcase type                 
			     ('erl_lint (mcase raw-msg
                                          ([p1 p2]
                                           (format "%s %s" p1 p2))
                                          
                                          (p
                                           (format "%s" p))))
			     ('erl_parse (mcase raw-msg
                                           ((p1 p2) 
                                            (format "%s %s" p1 p2))
                                           
                                           (p 
                                            (format "%s" p))))
			     (_ raw-msg))))
		     (insert (format "%s:%s %s\n" file line msg)))))
	       issues)))
   msgs))

(defun erl-mvn-format-compiler-warnings(msgs)
  "Private function that formats a list of compiler warnings as
returned by compile:file/2 and inserts them into the current
buffer."
  (mapcar 
   (lambda(file-e) 
     (mlet [file issues] file-e
       (mapcar 
        (lambda(e)        
          (mlet [line type raw-msg] e
            (insert (format "%s:%s %s\n" file line raw-msg))))
        issues)))
   msgs))

(defun erl-mvn-get-line-pos(line)
  "Private Function. Return '(startpos endpos) of a line."
  (let ((old-point (point)))
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((linestart (point)))
      (move-end-of-line 1)
      (let ((lineend (point)))
        (goto-char old-point)
        (list linestart lineend)))))

(defun erl-mvn-extract-linenumbers(msgs)
  "Private function. Returns a list of line numbers extracted
from the issues returned by the rpc to 'compile:file(...)'"
  (erl-mvn-lists-join
   (mapcar 
    (lambda(file-e) 
      (mlet [file issues] file-e
        (append 
         (mapcar 
          (lambda(e)        
           (mlet [line _ reason] e
             (list file line reason)))
          issues))))
    msgs)))

(defun erl-mvn-mark-warnings(lines buffer)
  "Private Function. Sets font-lock-face to
font-lock-warning-face on each line in the buffer, after removing
all exiting warning face properties."
  (save-excursion
    (with-current-buffer buffer
      (remove-overlays 'nil 'nil 'compilation-error-overlay 't)
      (mapcar
       (lambda (e)
         (mlet (file line reason) e
           (if (string= file erl-mvn-tmp-source-file)
               (mlet (start-pos end-pos) (erl-mvn-get-line-pos line)
                 (let ((ov (make-overlay start-pos end-pos)))
                   (overlay-put ov 'compilation-error-overlay 't)
                   (overlay-put ov 'font-lock-face 'font-lock-warning-face)
                   (overlay-put ov 'help-echo  (format "Problem: %s" reason)))))))
       lines))))

(defun erl-mvn-mark-eunit-results(lines buffer)
  "Private Function. Highlights the eunit test results in a buffer, by setting the background
color to green or red of the line or function header of each test."
  (save-excursion
    (with-current-buffer buffer
      (setq erl-mvn-is-dirty 'nil)
      (remove-overlays 'nil 'nil 'eunit-overlay 't)
      (mapcar
       (lambda (e)
         (if e
             (mcase e
               (['ok function line]
                (mlet (start-pos end-pos) (erl-mvn-get-line-pos line)
                  (let ((ov (make-overlay start-pos end-pos)))
                    (overlay-put ov 'eunit-overlay 't)
                    (overlay-put ov 'face '(:background "green" :foreground "black"))
                    )))
               
               (['error function line reason]
                (mlet (start-pos end-pos) (erl-mvn-get-line-pos line)
                  (let ((ov (make-overlay start-pos end-pos)))
                    (overlay-put ov 'eunit-overlay 't)
                    (overlay-put ov 'face '(:background "red" :foreground "black"))
                    (overlay-put ov 'help-echo reason)))))))
       lines))))

(defun erl-mvn-lists-join(lists)
  "Private function. Joins a list of lists to a list: ((a b
c) (d) (e f)) --> (a b c d e f)."
  (let ((res '()))
    (mapcar 
     (lambda(l)
       (mapcar 
        (lambda(x)
          (setq res (cons x res)))
        l))
     lists)
    res))


(defun erl-mvn-make-buffer-name(str)
  "Private function. Converts a string to a good process buffer name."
  (concat "*" str "*"))

(defun erl-mvn-make-mvn-output-buffer-name(str)
  "Private function. Converts a string to a good name for a maven output buffer"
  (erl-mvn-make-buffer-name (concat "erl-mvn-maven-output-" str)))

;; ----------------------------------------------------------------------
;;  Tests
;; ----------------------------------------------------------------------



	    
	

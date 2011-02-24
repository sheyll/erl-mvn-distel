(require 'derl)
(require 'cl)
(provide 'erl-mvn)

;; ----------------------------------------------------------------------
;;  Global Vars
;; ----------------------------------------------------------------------

(defvar erl-mvn-erlang-executable  "erl"
  "The path to the erlang executable.")

(defvar erl-mvn-maven-executable "mvn"
  "The path to the maven executable.")

(defvar erl-mvn-projects (make-hash-table)
  "A list of recognized maven projects.")

(defvar erl-mvn-node-name "erl-mvn-test-node"
  "The nodename of the test node for maven and distel.")

(defvar erl-mvn-erlang-mvn-packaging-types '("erlang-otp" "erlang-std")
  "The packaging types that identify a project as erlang project")

;; ----------------------------------------------------------------------
;;  Functions that will interact with maven to gather build parameters,
;;  as well as management of the erlang node, to which maven deploys the
;;  modules of the workspace.
;; ----------------------------------------------------------------------

(defun erl-mvn-kill-erlang()
  "Stops the erlang node that is currently running."
  (interactive)
  (kill-buffer erl-mvn-node-name))

(defun erl-mvn-build-workspace(workspace-dir)
  "Finds all subdirectories in workspace-dir, that contain erlang-otp or erlang-std maven projects.\
Starts a single erl node and invokes maven in each project to compile upload module to the erlang node.\
This enables distel to work. The emacs distel environment will automatically be connected to that test node, if not already done."
  (interactive "DWorkspace directory: ")
  (erl-mvn-start-node erl-mvn-node-name)
  (message "Searching for poms in: %s" workspace-dir)
  (let ((ws-sub-dirs (directory-files workspace-dir 'full-name "^[^.].+")))
    (delete-if-not (function file-directory-p) ws-sub-dirs)
    (mapcar (function erl-mvn-build-project)
            ws-sub-dirs))
  (message "Finished building workspace %s" workspace-dir))

(defun erl-mvn-build-project(proj-dir)
  "If the pom file exists, invokes maven to compile and upload the code into the test node"
  (interactive "DProject directory: ")
  (let ((pom-file (concat proj-dir "/pom.xml")))
    (if (erl-mvn-is-maven-erlang-project pom-file)
        (erl-mvn-compile-project-maven pom-file)
      (message "No maven-erlang project found in %s" proj-dir))))

(defun erl-mvn-compile-project-maven(pom-file)
  "Private function. Invokes maven to compile an erlang project defined by a pom file."
  (let ((artifact-id  (erl-mvn-pom-lookup pom-file 'artifactId))
        (packaging (erl-mvn-pom-lookup pom-file 'packaging))
        (include-dirs '())
        (code-paths '()))
    (message "Building project %s of type %s" artifact-id packaging)
    (let ((mvn-result
           (process-lines erl-mvn-maven-executable
                          "-f" pom-file
                          "-DshutdownTestNode=false"
                          (concat "-DtestNode=" (erl-mvn-complete-node erl-mvn-node-name))
                          "erlang:show-build-info")))
      (mapcar 
       (lambda(l) 
         (cond ((string-match "include_dir: \\(.+\\)$" l)
                (setq include-dirs (cons (match-string 1 l) include-dirs)))
               ((string-match "code_path: \\(.+\\)$" l)
                (setq code-paths (cons (match-string 1 l) code-paths)))))
       mvn-result)
      (set (erl-mvn-make-include-path-symbol artifact-id) include-dirs)
      (set (erl-mvn-make-code-path-symbol artifact-id) code-paths))))

(defun erl-mvn-is-maven-erlang-project(pom-file)
  "Private function. Returns t if the pom-file exists and defines an erlang project."  
  (if (file-exists-p pom-file) 
       (let ((packaging (erl-mvn-pom-lookup pom-file 'packaging)))
         (some (lambda (allowed) 
                 (string= packaging allowed))
               erl-mvn-erlang-mvn-packaging-types))
    'nil))

(defun erl-mvn-pom-lookup(pom tag)
  "Private function. Get a child of a the project element from the pom"
   (let* ((root (car (xml-parse-file pom)))
         (res-node (xml-get-children (xml-node-children root) tag)))
     (car (xml-node-children (car res-node)))))

(defun erl-mvn-start-node(node-name)
  "Private function. Starts an erlang node for the node-name that can be used by maven for tests and debug code"
  (if (erl-mvn-node-running node-name)
      (message (concat "Node already running: " node-name))
    (progn
      (message (concat "Starting node name: " node-name))            
      (start-process node-name node-name 
		     erl-mvn-erlang-executable
		     "-sname" (erl-mvn-complete-node node-name))     
      (sleep-for 5)
      (erl-mvn-distel-connect-node node-name))))

(defun erl-mvn-node-running(node-name)
  "Private function. Returns 't if a an erlang process was already started for node-name, by checking wether a buffer of that name exits"
  (not (eq 'nil (member node-name (mapcar (function buffer-name) (buffer-list))))))

(defun erl-mvn-complete-node(node-name)
  "Private function. Return the node with hostname."
  (concat node-name "@" (erl-determine-hostname)))

(defun erl-mvn-complete-node-distel(node-name)
  "Private function. Return the node with hostname as symbol. as distel requires it."
  (make-symbol (erl-mvn-complete-node node-name)))

(defun erl-mvn-distel-connect-node (node-name) 
  "Private function."     
  (let (n (erl-mvn-complete-node-distel node-name))
    (setq erl-nodename-cache n)
    (erl-ping n)))

;; ----------------------------------------------------------------------
;; These functions allow fast compilation and test execution.
;; ----------------------------------------------------------------------

(defun erl-mvn-compile-buffer ()
  "Private function. Compile the file of the buffer with the corresponding erlang node."
  (interactive)
  (let ((fn (file-truename (buffer-file-name)))
	(node (erl-mvn-complete-node-distel (erl-mvn-node-name-for-buffer))))
    (message (symbol-name node))
    (erpc node 'c 'c `(,fn))))

(defun erl-mvn-get-erlang-compile-options (file-name)
  "Private function. Returns a list of compiler arguments for compiling a source file of a\
project identified by a file of that project."
  (let* ((pom-file (erl-mvn-find-pom file-name))
	 (

;; ----------------------------------------------------------------------
;; Common utility functions
;; ----------------------------------------------------------------------
  
(defun erl-mvn-find-pom(fn)
  "Private function. Searches for a file called pom.xml in the parent directories of fn"
  (let ((fn (buffer-file-name)))
    (let ((dir (file-name-directory fn)))
      (while (and (not (file-exists-p (concat dir "/pom.xml")))
                  (not (equal dir (file-truename (concat dir "/..")))))
        (setq dir (file-truename (concat dir "/.."))))
      (let ((pom (concat dir "/pom.xml")))
        (if (not (file-exists-p pom))
            (progn 
              (message "No pom.xml found")
              "")
          pom)))))

(defun erl-mvn-make-code-path-symbol(artifact-id)
  "Private function. Returns a symbol for the variable that contains the code path list\
for the project identified by an artifact id."
  (intern (concat "erl-mvn-project-" artifact-id "-code-paths")))

(defun erl-mvn-make-include-path-symbol(artifact-id)
  "Private function. Returns a symbol for the variable that contains the include path list\
for the project identified by an artifact id."
  (intern (concat "erl-mvn-project-" artifact-id "-include-dirs")))

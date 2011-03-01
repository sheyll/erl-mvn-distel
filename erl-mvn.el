(provide 'erl-mvn)

;; ----------------------------------------------------------------------
;;  Global Vars
;; ----------------------------------------------------------------------

(defvar erl-mvn-erlang-executable "erl"
  "The path to the erlang executable.")

(defvar erl-mvn-maven-executable "mvn"
  "The path to the maven executable.")

(defvar erl-mvn-node-name 
  (concat "erl-mvn-test-node@" (car (process-lines "hostname" "-f")))
  "The nodename of the test node for maven and distel.")

(defvar erl-mvn-popup-compiler-output 't
  "Setting this to non nil will cause a buffer with the
 compilation results to popup everytime an error or warning was
 returned from an erlang buffer compilation")

(defconst erl-mvn-erlang-mvn-packaging-types '("erlang-otp" "erlang-std")
  "The packaging types that identify a project as erlang
  project")

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

(defvar erl-mvn-successful-mvn-built-artifacts nil
  "Contains all artifacts of the current workspace that were
  succesfully built by mvn.")

(defvar erl-mvn-erlang-buffer-name (concat "*" erl-mvn-node-name "*")
  "The name of the buffer associated to the erlang process.")


;; ----------------------------------------------------------------------
;;  Startup and shutdown functions.
;; ----------------------------------------------------------------------

(defun erl-mvn-setup()
  "Adds the compile function to the save hooks of erlang files."
  (interactive)
  (erl-mvn-start-node erl-mvn-node-name)
  (add-hook 
   'erlang-mode-hook
   (lambda ()
     (add-hook 'after-save-hook 'erl-mvn-erl-buffer-saved))))

(defun erl-mvn-erl-buffer-saved()
  "When a buffer is saved it is automatically compiled. Compiler errors and
 warnings are displayed in a seperate buffer"
  (interactive)
  (if (erl-mvn-is-relevant-erl-buffer)
      (let* ((fn buffer-file-name)
             (pom-file (erl-mvn-find-pom fn))
             (artifact-id (erl-mvn-pom-lookup pom-file 'artifactId)))
        (if (member artifact-id erl-mvn-successful-mvn-built-artifacts)
            (erl-mvn-compile-buffer artifact-id)
          (progn
            (message "Project %s was not successfully build yet." artifact-id)
            (erl-mvn-compile-project-maven pom-file)
            (message "Project %s was now built, please try compiling %s again." artifact-id fn))))
    (message "Not erlang compiling buffer because it's boring.")))

(defun erl-mvn-is-relevant-erl-buffer()
  "Private function. Determines if the current buffer contains
erlang code managed by the current node."
  (if (string-match "^.+\.erl$" buffer-file-name)      
      (let* ((fn buffer-file-name)
             (fn-dir (file-name-directory fn))
             (pom-file (erl-mvn-find-pom fn))
             (artifact-id (erl-mvn-pom-lookup pom-file 'artifactId))
             (src-dir (cadr (assoc artifact-id erl-mvn-source-paths)))
             (test-src-dir (cadr (assoc artifact-id erl-mvn-test-source-paths))))
        (or (string= src-dir fn-dir)
            (string= test-src-dir fn-dir)))))
        
(defun erl-mvn-shutdown()
  "Stops the erlang node that is currently running."
  (interactive)  
  (setq erl-mvn-source-paths nil)
  (setq erl-mvn-test-source-paths nil)
  (setq erl-mvn-code-paths nil)
  (setq erl-mvn-successful-mvn-built-artifacts nil)
  (setq erl-mvn-include-paths nil)
  (cond ((erl-mvn-node-running erl-mvn-node-name)      
         (delete-process erl-mvn-erlang-buffer-name)
         (kill-buffer erl-mvn-erlang-buffer-name))))

(defun erl-mvn-reset()
  "Stops and starts the erlang node that is currently running. Resets all state."
  (interactive)
  (erl-mvn-shutdown)
  (erl-mvn-start-node erl-mvn-node-name))

;; ----------------------------------------------------------------------
;;  Functions for maven interaction
;; ----------------------------------------------------------------------

(defun erl-mvn-add-current-buffer-project()
  "Guesses a pom file for the current buffer and adds the project
to the current workspace, if it not already exists(determined by
the project artifact-id).  If the pom file in some parent
directory of the directory of the current buffer exists, it
invokes maven to compile and upload the code into the test node,
and enables the autocompile on save action for all erlang source
files in that project."
  (interactive)
  (let* ((pom-file (erl-mvn-find-pom buffer-file-name))
         (artifact-id (erl-mvn-pom-lookup pom-file 'artifactId)))
    (message "Found project %s:" artifact-id)
    (if (not (member artifact-id erl-mvn-successful-mvn-built-artifacts))
        (if (erl-mvn-is-maven-erlang-project pom-file)
            (erl-mvn-compile-project-maven pom-file)
          (message "No maven-erlang project found in %s" proj-dir))
      (message "Project %s already managed." artifact-id))))

(defun erl-mvn-compile-project-maven(pom-file)
  "Private function. Invokes maven to compile an erlang project
defined by a pom file. Extracts source folders, include- and
code_paths. If a build fails all variables are reset."
  (let ((artifact-id  (erl-mvn-pom-lookup pom-file 'artifactId))
        (packaging (erl-mvn-pom-lookup pom-file 'packaging))
        (include-dirs '())
        (code-paths '())
        (src-dir "")
        (test-src-dir "")
        (build-failed nil))
    (message "Building project %s of type %s" artifact-id packaging)
    (let ((mvn-result (erl-mvn-run-maven pom-file)))
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
          (add-to-list 'erl-mvn-successful-mvn-built-artifacts artifact-id))))))
             
(defun erl-mvn-run-maven(pom)
  "Private function. Runs maven and pastes the output into a
buffer, so the user is able to follow the output."
    (save-excursion
      (with-current-buffer (get-buffer-create "*maven-output*")
        (setq buffer-read-only nil)
        (save-selected-window          
          (select-window (or (get-buffer-window (current-buffer))
                             (display-buffer (current-buffer))))          
          (goto-char (point-max))          
          (let ((start (point)))
            (call-process erl-mvn-maven-executable
                          nil ; infile
                          "*maven-output*"   ; current buffer
                          't   ; redisplay
                          "-f" pom
                          (concat "-Dremote=" erl-mvn-node-name)
                          "erlang:show-build-info"
			  "erlang:upload-tests"
                          "test-compile")
            (setq buffer-read-only 't)
            (erl-mvn-to-lines (buffer-substring-no-properties start (point-max))))))))

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
parent directories of fn"
  (let ((dir (file-name-directory fn)))
    (while (and (not (file-exists-p (concat dir "/pom.xml")))
                (not (equal dir (file-truename (concat dir "/..")))))
      (setq dir (file-truename (concat dir "/.."))))
    (let ((pom (concat dir "/pom.xml")))
      (if (not (file-exists-p pom))
          (progn 
            (message "No pom.xml found")
            "")
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
      (start-process erl-mvn-erlang-buffer-name
		     erl-mvn-erlang-buffer-name
		     erl-mvn-erlang-executable
		     "-name" node-name)
      (sleep-for 5)
      (erl-mvn-distel-connect-node node-name))))

(defun erl-mvn-node-running(node-name)
  "Private function. Returns 't if a an erlang process was
already started for node-name, by checking wether a buffer of
that name exits"
  (not (eq 'nil (member erl-mvn-erlang-buffer-name (mapcar (function buffer-name) (buffer-list))))))

(defun erl-mvn-distel-connect-node (node-name) 
  "Private function."     
  (let ((n (make-symbol node-name)))
    (setq erl-nodename-cache n)
    (erl-ping n)))


;; ----------------------------------------------------------------------
;;  Function for compilation of erlang modules currently open in a buffer.
;; ----------------------------------------------------------------------

(defun erl-mvn-compile-buffer (artifact-id)
  "Compile the file of the buffer with the corresponding erlang
node, if it belongs to a project currently managed."
  (interactive)
  (setq erl-source-buffer (current-buffer))  
  (let* 
      ((fn (file-truename (buffer-file-name)))       
       (warnings-r '())
       (errors-r '())
       (node (make-symbol erl-mvn-node-name)))    
    (let 
        ((erl-popup-on-output-old erl-popup-on-output))
      (setq erl-popup-on-output nil)
      (erpc node 'code 'add_paths
            (cdr (assoc artifact-id erl-mvn-code-paths)))
      (message "Compiling %s from project %s" fn artifact-id)
      (erl-rpc 
       (lambda (result) 
         (mcase result
           
           (['ok module warnings] 
            (message "Successfully compiled module: %s." module)
	    (setq warnings-r warnings)
	    (setq errors-r '())
            (erl-mvn-show-compilation-results '() warnings erl-source-buffer))
           
           (['error errors warnings] 
            (message "Compilation failed!")            
	    (setq warnings-r warnings)
	    (setq errors-r errors)
            (erl-mvn-show-compilation-results errors warnings erl-source-buffer))
           
           (unexpected
	    (setq warnings-r '())
	    (setq errors-r '())
            (message "Unexpected message %s" unexpected)))) 'nil
       node 'compile 'file 
       (cons fn (list (erl-mvn-get-erlang-compile-options artifact-id))))
      (setq erl-popup-on-output erl-popup-on-output-old))
    (list warnings-r errors-r)))

(defun erl-mvn-get-erlang-compile-options (artifact-id)
  "Private function. Returns a list of compiler arguments for
compiling a source file of a project identified by a maven
project artifact-id."
  (append '(debug_info return verbose export_all)
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
      (remove-text-properties (point-min) 
                              (point-max) 
                              '(font-lock-face font-lock-warning-face))
      (remove-text-properties (point-min) 
                              (point-max) 
                              '(help-echo nil))
      (font-lock-fontify-buffer)
      (mapcar
       (lambda (e)
         (mlet (file line reason) e
           (if (string= file buffer-file-name)
               (mlet (start-pos end-pos) (erl-mvn-get-line-pos line)
                 (add-text-properties start-pos
                                      end-pos 
                                      '(font-lock-face font-lock-warning-face))
                 (add-text-properties start-pos
                                      end-pos 
                                      `(help-echo  ,(format "Problem: %s" reason)))))))
         lines)
      (set-buffer-modified-p 'nil))))

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

;; ----------------------------------------------------------------------
;;  Tests
;; ----------------------------------------------------------------------

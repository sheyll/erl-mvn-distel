(require 'derl)
(provide 'erl-mvn)

(defvar erl-mvn-erlang-executable  "erl"
  "The complete path to the erlang executable.")

(defvar erl-mvn-maven-executable ""
  "The complete path to the maven executable.")

(defun erl-mvn-node-name-for-buffer()
  "Returns the name of an erlang node, for the maven project a file in a buffer belongs to."
  (let ((pom (erl-mvn-find-pom (buffer-file-name))))
    (message (concat "found pom: " pom))
    (concat "erl-mvn-" (erl-mvn-artifact-id pom) "-test-node")))
  
(defun erl-mvn-start-node-for-buffer(&optional args)
  "If for the maven project, the file in to current buffer belongs to, no erlang test node is started, a new node will be started."
  (interactive)  
  (erl-mvn-start-node (erl-mvn-node-name-for-buffer)))

(defun erl-mvn-find-pom(fn)
  "Searches for a file called pom.xml in the parent directories of fn"
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

(defun erl-mvn-artifact-id(pom)
  "Parses a pom.xml file and extracts the artifactId."
  (let* ((root (car (xml-parse-file pom)))
         (artifact-id-node (xml-get-children (xml-node-children root) 'artifactId)))
     (car (xml-node-children (car artifact-id-node)))))

(defun erl-mvn-start-node(node-name)
  "Starts an erlang node for the node-name that can be used by maven for tests and debug code"
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
  "Returns 't if a an erlang process was already started for node-name, by checking wether a buffer of that name exits"
  (not (eq 'nil (member node-name (mapcar (function buffer-name) (buffer-list))))))

(defun erl-mvn-complete-node(node-name)
  "Return the node with hostname."
  (concat node-name "@" (erl-determine-hostname)))

(defun erl-mvn-complete-node-distel(node-name)
  "Return the node with hostname as symbol. as distel requires it."
  (make-symbol (erl-mvn-complete-node node-name)))

(defun erl-mvn-distel-connect-node (node-name)    
  (erl-ping (erl-mvn-complete-node-distel node-name)))

(defun erl-mvn-compile-buffer ()
  "Compile the file of the buffer with the corresponding erlang node."
  (interactive)
  (let ((fn (file-truename (buffer-file-name)))
	(node (erl-mvn-complete-node-distel (erl-mvn-node-name-for-buffer))))
    (message (symbol-name node))
    (erpc node 'c 'c `(,fn))))

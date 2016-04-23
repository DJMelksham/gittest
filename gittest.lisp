(in-package :gittest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Gittest provides a simple way to call git from my Common Lisp REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *git-project-path* nil)

(defun set-gittest-active-directory (path/string)
  (setf *git-project-path* (uiop:ensure-directory-pathname path/string)))

(defun make-git-command (command)
  (concatenate 'string "cd " (uiop:native-namestring *git-project-path*) " && git " command))

(defun git (command)
 (nth-value 0 (inferior-shell:run/s (make-git-command command)
			:on-error nil)))

(defun add-emacs-git-ignore ()
  (if (null *git-project-path*)
      (return-from add-emacs-git-ignore nil)
      (with-open-file (stream (uiop:merge-pathnames* *git-project-path* ".gitignore")
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create)

	(format stream "~a~&" "#Ignore emacs editor temporary files")
	(format stream "~a~&" "[#]*[#]")
	(format stream "~a~&" "*~")
	t)))

(defun git-branch ()
  (let ((result (gittest:git "rev-parse --abbrev-ref HEAD")))
    (string-trim '(#\Space #\Newline #\Backspace #\Tab 
		   #\Linefeed #\Page #\Return #\Rubout)
		 result)))
	
(defun git-add ()
  (git "add -A"))

(defun git-commit (message)
  (print (concatenate 'string "commit -m " "'" message "'"))
  (git (concatenate 'string "commit -m " "'" message "'")))

(defun git-push (&key (remote "origin") (branch (git-branch)))
  (git (concatenate 'string "push " remote " " branch)))

(defun git-init ()
  (git "init"))

(defun git-add-commit-push (&key
			      (message "Automatic peasant commit message!")
			      (remote "origin")
			      (branch (git-branch)))
  (git-add)
  (git-commit message)
  (git-push :remote remote :branch branch))	

(defun make-git ()
    ;;init-git
  (git-init)
  (git-add)
  (git-commit "Initial commit automatically generated by the lowly peasant program"))

(defun git-add-remote (url &optional (alias "origin"))
  (git (concatenate 'string "remote add " alias " " url)))

(defun git-change-remote (url &optional (alias "origin"))
  (git (concatenate 'string "remote set-url " alias " " url)))

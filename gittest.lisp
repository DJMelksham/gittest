(in-package :gittest)

(defvar *project-path* nil)

(defun set-gittest-active-directory (path/string)
  (setf *project-path* (uiop:ensure-directory-pathname path/string)))

(defun make-git-command (command)
  (concatenate 'string "cd " (namestring *project-path*) " && git " command))

(defun git (command)
 (nth-value 0 (inferior-shell:run/s (make-git-command command)
			:on-error nil)))

(defun add-emacs-git-ignore ()
  (if (null *project-path*)
      (return-from add-emacs-git-ignore nil)
      (with-open-file (stream (uiop:merge-pathnames* *project-path* ".gitignore")
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)

	(format stream "~a~&" "#Ignore emacs editor temporary files")
	(format stream "~a~&" "[#]*[#]")
	(format stream "~a~&" "*~")))
  t)
    

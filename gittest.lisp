;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; System: Gittest 
;;; Author: Damien John Melksham
;;; Written using Ubuntu 16.04, SBCL 1.3.1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gittest provides a simple way to call git from the Common Lisp REPL
;;; It assumes that git is already installed and setup properly on the
;;; respective system.
;;;
;;; Gittest is not a particular thoroughly architectured system at this
;;; stage.  Its basically a thin wrapper around linux shell commands.
;;;
;;; The project was named after I found my wife's mispronounciation 
;;; of one of my test folders, "gittest", so adorable that 
;;; I couldn't bare to change it.
;;;
;;; The basic philosophy of gittest assumes calling of the (git) function,
;;; with an argument of a single string.  You may think of this string
;;; as the additional commands you pass to git a the terminal/command line.
;;;
;;; Other helper functions exist to automate my most common workflows.
;;; Practically all commands of any significance are exported. There is
;;; only one asdf definition file, one package definition file,
;;; and one source file, so its not exactly rocket science.
;;;
;;; Gittest uses "inferior-shell" to call out to the command line, changes to
;;; the directory listed in *git-project-path* before submitting its commands
;;; and relies on "uiop" for its pathname manipulations.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index
;;;
;;; 1.  Fundamental git functions
;;; 2.  Utility functions that perform git actions and help out around the
;;;     house.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gittest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 1. Fundamental Functions - everything really relies upon the following
;;; dynamic variable and the three functions interacting with it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *git-project-path* is the dynamic variable providing the directory that
;; git commands run in.  You set it via (set-gittest-active-directory)

(defvar *git-project-path* nil)

(defun set-gittest-active-directory (path)
  "A function to set the active directory for git commands"
  (setf *git-project-path* (uiop:ensure-directory-pathname path)))

;; make-git-command inserts a "cd *git-project-path* &&" onto the beginning
;; of the string, typically before returning its result to be consumed by
;; inferior-shell in the calling function.
;; Make-git-command is not designed to be called directly

(defun make-git-command (command)
  "Responsible for turning a string into a compount git 
   command executable by inferior-shell" 
  (concatenate 'string "cd "
	       (uiop:native-namestring *git-project-path*)
	       " && git " command))

;; The general purpose git function that calls git at the command line
;; with the string supplied to it as additional arguments.

(defun git (command)
  "Calls a git command in the active directory."
  (nth-value 0 (inferior-shell:run/s (make-git-command command) :on-error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 2. Utility Functions
;;;    Superfluous But Nice Functions - perform basic git functions and patterns
;;;    rather than having to be called as explicit git commands with strings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Is the provided path in a git repository?

(defun path-in-git-p (&optional (path *git-project-path*))
  "Determine if a directory is in a git repository"
  (let ((*git-project-path* path))
    (if (equal (git "status") "")
	NIL
	T)))

;; Append emacs temporary file patterns to the .gitignore file

(defun add-emacs-git-ignore ()
  "Append emacs temporary file patterns to the .gitignore file"
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

;;  Find the name of the active git branch

(defun git-branch ()
  "Determines the name of the current branch of git in the active directory"
  (let ((result (gittest:git "rev-parse --abbrev-ref HEAD")))
    (string-trim '(#\Space #\Newline #\Backspace #\Tab 
		   #\Linefeed #\Page #\Return #\Rubout)
		 result)))

;; Add all files/changes to the git staging area - read the bloody docstrings people.

(defun git-add ()
  "Add all files/changes to the git staging area"
  (git "add -A"))

;; Makes a git commit of all changes currently in staging.
;; You currently have to be a bit careful though, its best to just
;; not use any quotable characters.

(defun git-commit (message)
  "Make a git commit with a message"
  (print (concatenate 'string "commit -m " "'" message "'"))
  (git (concatenate 'string "commit -m " "'" message "'")))

;; Push the current changes to the remote git repsitory.
;; By default, changes are pushed to the "origin" remote repository.
;; The current branch is automatically determined by default.

(defun git-push (&key (remote "origin") (branch (git-branch)))
  "Push current commits on a branch to a remote depository"
  (git (concatenate 'string "push " remote " " branch)))

;; Initialise a git repository in the active directory

(defun git-init ()
  "Initialise a git repository in the active directory"
  (git "init"))

;; Performs the common combination of git add -A, git commit -m "message",
;; git push origin branch.

(defun git-add-commit-push (&key
			      (message "Automatic peasant commit message!")
			      (remote "origin")
			      (branch (git-branch)))
  "Perform the common combo of git add, git commit, and git push"
  (git-add)
  (git-commit message)
  (git-push :remote remote :branch branch))	

;; When initialising a git repository, you very rarely want to just initialise it.
;; You also need to add current files and make an initial commit.
;; Well, that's what this function does in the currently active directory.

(defun make-git ()
    "Make the active directory a git repository, and establish an initial commit"
  (git-init)
  (git-add)
  (git-commit "Initial commit automatically generated by the lowly peasant program"))

;; Add a remote repo URL to your git project

(defun git-add-remote (url &optional (alias "origin"))
  "Adds a remote reference to your current git project"
  (git (concatenate 'string "remote add " alias " " url)))

;; Changes the URL for a current remote alias in your git project

(defun git-change-remote (url &optional (alias "origin"))
  "Change the URL of a current git remote repository alias"
  (git (concatenate 'string "remote set-url " alias " " url)))

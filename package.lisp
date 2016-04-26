;;;; package.lisp

(defpackage :gittest
  (:use #:cl)
  (:export 
	:git
	:set-gittest-active-directory
	:add-emacs-git-ignore
	:git-branch
	:git-add
	:git-commit
	:git-push
	:git-init
	:git-add-commit-push
	:make-git
	:git-add-remote
	:*git-project-path*
	:path-in-git-p))

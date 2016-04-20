;;;; package.lisp

(defpackage :gittest
  (:use #:cl)
  (:export 
	:git
	:set-gittest-active-directory
	:add-emacs-git-ignore))

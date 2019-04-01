;;;; MAIN for ORC BATTLE

(load "orc-battle.lisp")

;;; Compile with SBCL
;;; sbcl --script main.lisp
(sb-ext:save-lisp-and-die "orc-battle"
    :toplevel #'orc-battle
    :executable t)

;;;; Adapted from Land of Lisp, Chapter 09
;;;; ORC BATTLE

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defparameter *turn-id* nil)
(defparameter *player-attack-phase-id* nil)

;;; Main Game Functions

(defun orc-battle ()
    (init-monsters)
    (init-player)
    (init-game-context)
    (game-loop)
    (when (player-dead)
        (princ "You have been killed.  GAME OVER."))
    (when (monsters-dead)
        (princ "Congratulations!  You have vanquished all of your foes.")))

(defun game-loop ()
    (unless (or (player-dead) (monsters-dead))
        (show-turn)
        (show-player)
        (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
            (unless (monsters-dead)
                (show-player-attack-phase)
                (show-monsters)
                (player-attack)))
        (show-monster-attack-phase)
        (map 'list
             (lambda (m)
                (unless (monster-dead m)
                    (fresh-line)
                    (monster-attack m)))
             *monsters*)
        (game-loop)))

;;; Game Context

(defun init-game-context ()
    (setf *turn-id* 1)
    (setf *player-attack-phase-id* 1))

(defun show-turn ()
    (fresh-line)
    (format t "~%")
    (princ "[TURN ")
    (princ *turn-id*)
    (princ "]")
    (incf *turn-id* 1))

(defun show-player-attack-phase ()
    (fresh-line)
    (format t "~%")
    (princ "[PLAYER ATTACK PHASE ")
    (princ *player-attack-phase-id*)
    (princ "]")
    (incf *player-attack-phase-id* 1))

(defun show-monster-attack-phase ()
    (setf *player-attack-phase-id* 1)
    (fresh-line)
    (format t "~%")
    (princ "[MONSTER ATTACK PHASE]")
    (fresh-line))

;;; Player Management Functions

(defun init-player ()
    (setf *player-health* 30)
    (setf *player-agility* 30)
    (setf *player-strength* 30))

(defun player-dead ()
    (<= *player-health* 0))

(defun show-player ()
    (fresh-line)
    (princ "You are a valiant knight with Health ")
    (princ *player-health*)
    (princ ", Agility ")
    (princ *player-agility*)
    (princ ", and Strength ")
    (princ *player-strength*))

(defun player-attack ()
    (fresh-line)
    (princ "Action: [s]tab, [d]ouble swing, [r]oundhouse, e[x]it: ")
    (finish-output)
    (case (read)
        (x (exit-game))
        (s (monster-hit (pick-monster)
                (+ 2 (randval (ash *player-strength* -1)))))
        (d (let ((x (randval (truncate (/ *player-strength* 6)))))
                (princ "Your double swing has a strength of ")
                (princ x)
                (fresh-line)
                (monster-hit (pick-monster) x)
                (unless (monsters-dead)
                    (fresh-line)
                    (monster-hit (pick-monster) x))))
        (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                        (unless (monsters-dead)
                            (fresh-line)
                            (monster-hit (random-monster) 1))))))

(defun randval (n)
    (1+ (random (max 1 n))))

(defun exit-game ()
    (fresh-line)
    (princ "You ran away!")
    (quit))

;;; Helper Functions for Player Attacks

(defun random-monster ()
    (let ((m (aref *monsters* (random (length *monsters*)))))
        (if (monster-dead m)
            (random-monster)
            m)))

(defun pick-monster ()
    (princ "Monster #: ")
    (finish-output)
    (let ((x (read)))
        (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
            (progn (princ "That is not a valid monster number.")
                   (pick-monster))
            (let ((m (aref *monsters* (1- x))))
                (if (monster-dead m)
                    (progn (princ "That monster is already dead.")
                           (pick-monster))
                    m)))))

;;; Monster Management Functions

(defun init-monsters ()
    (setf *monsters*
        (map 'vector
            (lambda (x)
                (funcall (nth (random (length *monster-builders*))
                          *monster-builders*)))
            (make-array *monster-num*))))

(defun monster-dead (m)
    (<= (monster-health m) 0))

(defun monsters-dead ()
    (every #'monster-dead *monsters*))

(defun show-monsters ()
    (fresh-line)
    (princ "Your foes: ")
    (let ((x 0))
        (map 'list
            (lambda (m)
                (fresh-line)
                (princ "    ")
                (princ (incf x))
                (princ ". ")
                (if (monster-dead m)
                    (princ "**dead**")
                    (progn (princ "(Health=")
                           (princ (monster-health m))
                           (princ ") ")
                           (monster-show m))))
            *monsters*)))

;;; The Monsters

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
    (decf (monster-health m) x)
    (if (monster-dead m)
        (progn (princ "You killed the ")
               (princ (type-of m))
               (princ "! "))
        (progn (princ "You hit the ")
               (princ (type-of m))
               (princ ", knocking off ")
               (princ x)
               (princ " health points! "))))

(defmethod monster-show (m)
    (princ "A fierce ")
    (princ (type-of m)))

(defmethod monster-attack (m))

;;; Monster: The Wicked Orc

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
    (princ "A wicked ORC with a level ")
    (princ (orc-club-level m))
    (princ " club"))

(defmethod monster-attack ((m orc))
    (let ((x (randval (orc-club-level m))))
        (princ "An ORC swings his club at you and knocks off ")
        (princ x)
        (princ " of your health points. ")
        (decf *player-health* x)))

;;; Monster: The Malicious Hydra

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
    (princ "A malicious HYDRA with ")
    (princ (monster-health m))
    (princ " heads"))

(defmethod monster-hit ((m hydra) x)
    (decf (monster-health m) x)
    (if (monster-dead m)
        (princ "The corpse of the fully decapitated and decapacitated HYDRA falls to the floor!")
        (progn (princ "You lop off ")
               (princ x)
               (princ " of the HYDRA's heads! "))))

(defmethod monster-attack ((m hydra))
    (let ((x (randval (ash (monster-health m) -1))))
        (princ "A HYDRA attacks you with ")
        (princ x)
        (princ " of its heads! It also grows back one more head! ")
        (incf (monster-health m))
        (decf *player-health* x)))

;;; Monster: The Slimy Slime Mold

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
    (princ "A SLIME-MOLD with a sliminess of ")
    (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
    (let ((x (randval (slime-mold-sliminess m))))
        (princ "A SLIME-MOLD wraps around your legs and decreases your agility by ")
        (princ x)
        (princ "! ")
        (decf *player-agility* x)
        (when (zerop (random 2))
            (princ "It also squirts in your face, taking away a health point! ")
            (decf *player-health*))))

;;; Monster: The Cunning Brigand

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
    (let ((x (max *player-health* *player-agility* *player-strength*)))
        (cond ((= x *player-health*)
                    (princ "A BRIGAND hits you with his slingshot, taking off 2 health points! ")
                    (decf *player-health* 2))
              ((= x *player-agility*)
                    (princ "A BRIGAND catches your leg with his whip, taking off 2 agility points! ")
                    (decf *player-agility* 2))
              ((= x *player-strength*)
                    (princ "A BRIGAND cuts your arm with his whip, taking off 2 strength points! ")
                    (decf *player-strength* 2)))))

;;; Compile with SBCL
;;; sbcl --script orc-battle.lisp
(sb-ext:save-lisp-and-die "orc-battle"
    :toplevel #'orc-battle
    :executable t)

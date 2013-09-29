(batch* /lib/sys/core/include.clp)
(defglobal MAIN
           ?*stages* = (create$ prompt
                                parse
                                act)
           ?*prompt-symbol* = ">"
           ?*in-development* = TRUE)

(defclass player
  (is-a USER)
  (slot player-name
        (type STRING)
        (visibility public)
        (storage local)
        (default ?NONE))
  (multislot inventory 
             (type LEXEME))
  (slot location
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default-dynamic initial-room)))

(defrule next-stage 
         (declare (salience -9999))
         ?f <- (message (action stages)
                        (contents ? $?rest))
         =>
         (modify ?f (contents $?rest)))

(defrule restart-process
         (declare (salience -10000))
         ?f <- (message (action stages)
                        (contents))
         =>
         (modify ?f (contents ?*stages*)))
;------------------------------------------------------------------------------
(defrule startup
         (declare (salience 10000))
         (initial-fact)

         =>
         (printout t "Welcome to adventure." crlf)
         (printout t "Please enter your name: ")
         (bind ?name (readline))
         (assert (message (action stages)
                          (contents ?*stages*)))
         (make-instance of player 
                        (player-name ?name))
         )
;------------------------------------------------------------------------------
(defrule prompt-for-input 
         (message (action stages)
                  (contents prompt $?))
         =>
         (format t "%s " ?*prompt-symbol*)
         (bind ?tokens (explode$ (readline)))
         (assert (message (action input)
                          (from 1)
                          (contents ?tokens))))

;------------------------------------------------------------------------------

(batch* /logic/parsing.clp)
(batch* /logic/act.clp)

; each room consists of a series of rules and nothing more. This cuts
; down on the amount of state. It also makes the parser far simpler too as its
; possible to even make

; Should always be the last line
(batch* /lib/sys/chunks/reset-run-exit.clp)


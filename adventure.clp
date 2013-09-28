(batch* /lib/sys/core/include.clp)

(defclass thing
 (is-a USER)
 (slot parent
  (type INSTANCE-NAME)))

(defclass player
 (is-a thing)
 (slot player-name
  (type STRING)
  (visibility public)
  (storage local)
  (default ?NONE))
 (multislot inventory
  (type INSTANCE-NAME)
  (allowed-classes thing))
 (slot state
  (type SYMBOL)
  (allowed-values alive dead)))

(defrule startup
 (initial-fact)
 =>
 (printout t "Welcome to adventure." crlf)
 (printout t "Please enter your name: ")
 (bind ?name (readline))
 (make-instance [player] of player 
  (player-name ?name)))



; Should always be the last line
(batch* /lib/sys/core/chunks/reset-run-exit.clp)

;-----------------------------------------------------------------------------
; System Commands (facts and rules)
; System commands are unique but also 
;-----------------------------------------------------------------------------
(deffacts SystemCommands
          ; req   action  req  aliases
          (defaction  quit    <- )
          (defaction  save    <- )
          (defaction  load    <- )
          (defaction  restart <- )
          (defaction  about   <- )
          (defaction  help    <- )
          )

(defrule act:sys:quit
         (message (action stages)
                  (contents act $?))
         (statement (action quit))
         =>
         (exit 0))

(defrule act:sys:save
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action save)
                          (operands ?save))
         =>
         (format werror "ERROR: save functionality not implemented yet!%n")
         (retract ?f))

(defrule act:sys:load
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action load)
                          (operands ?load))
         =>
         (format werror "ERROR: load functionality not implemented yet!%n")
         (retract ?f))

(defrule act:sys:man
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action man)
                          (operands ?man))
         =>
         (format werror "ERROR: man functionality not implemented yet!%n")
         (retract ?f))

(defrule act:sys:about
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action about))
         =>
         (retract ?f)
         (printout t "Adventure by Joshua Scoggins" crlf
                   "Operating System: " (operating-system) crlf
                   "Architecture: " (architecture) crlf))

(defrule act:sys:restart
 (message (action stages)
  (contents act $?))
 ?f <- (statement (action restart))
 =>
 (retract ?f)
 (reset))


;-----------------------------------------------------------------------------
; Manual System Code
;-----------------------------------------------------------------------------
(deffacts ManualCommands
          (defaction  man <- help manual))
; TODO: introduce changes to the help system through srv/efs 
(defrule act:man:process
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action man)
                          (operands $?operands))
         =>
         (retract ?f)
         (help (expand$ $?operands)))

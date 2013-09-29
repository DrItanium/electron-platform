;-----------------------------------------------------------------------------
; Developer Commands (facts and rules)
;-----------------------------------------------------------------------------
(deffacts developer-system-commands 
          (defaction facts <-)
          (defaction rules <-))

(defrule act:sys:facts
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action facts))
         =>
         (retract ?f)
         (facts))

(defrule act:sys:rules
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action rules))
         =>
         (retract ?f)
         (rules))

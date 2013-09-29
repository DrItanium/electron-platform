;-----------------------------------------------------------------------------
; Developer Commands (facts and rules)
;-----------------------------------------------------------------------------
(deffacts developer-commands 
          (defaction facts <-)
          (defaction rules <-))

(defrule act:dev:facts
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action facts))
         =>
         (retract ?f)
         (facts))

(defrule act:dev:rules
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action rules))
         =>
         (retract ?f)
         (rules))

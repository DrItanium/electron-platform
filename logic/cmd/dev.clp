;-----------------------------------------------------------------------------
; Developer Commands (facts and rules)
;-----------------------------------------------------------------------------

(deffacts developer-commands 
          (defaction facts <-)
          (defaction rules <-)
          (defaction watch <-)
          (defaction options <-))


(defrule act:dev:facts
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action facts)
                          (operands $?contents))
         =>
         (retract ?f)
         (facts (expand$ $?contents)))

(defrule act:dev:rules
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action rules)
                          (operands $?rules))
         =>
         (retract ?f)
         (rules (expand$ $?rules)))

(defrule act:dev:watch:no-inputs
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action watch)
                          (operands))
         =>
         (printout t "Watch requires arguments to work correctly" crlf)
         (retract ?f))

(defrule act:dev:watch:inputs
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action watch)
                          (operands ?first $?rest))
         =>
         (retract ?f)
         (watch ?first (expand$ $?rest)))
(defrule act:dev:options 
         (message (action stages)
                  (contents act $?))
         ?f <- (statement (action options))
         =>
         (options)
         (retract ?f))

(defrule unknown-statement
         (declare (salience -1000))
         (message (action stages)
                  (contents act $?))
         ?st <- (statement (action ?action))
         =>
         (format t "Unknown action: %s%n" ?action)
         (retract ?st))


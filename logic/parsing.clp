; This does semi-nlp on the input from the prompt
; The nice thing is that the form I expect things is quite flexible but
; there is some order to it. For instance, the inputs will always go
; <action> <operand> <to/on/at/with> <operand>
; <action> <operand> 
; <action> 

(deftemplate action
             (slot action
                   (type LEXEME)
                   (default ?NONE))
             (multislot aliases
                        (type LEXEME)))
(deftemplate statement
             (slot action
                   (type SYMBOL)
                   (default ?NONE))
             (multislot operands))

;-----------------------------------------------------------------------------
; Setup
;-----------------------------------------------------------------------------
(defrule init:define-actions
         (message (action stages)
                  (contents init $?))
         ?f <- (defaction ?action <- $?aliases) 
         =>
         (retract ?f)
         (assert (action (action ?action)
                         (aliases $?aliases))))

(defrule init:found-action-conflict:duplicate-actions
         (message (action stages)
                  (contents init $?))
         ?f <- (action (action ?z))
         ?f2 <- (action (action ?z))
         (test (neq ?f ?f2))
         =>
         (format werror "ERROR: %s is defined twice!%n" ?z)
         (exit 1))

(defrule init:found-action-conflict:same-alias-different-actions
         (message (action stages)
                  (contents init $?))
         (action (action ?z)
                 (aliases $? ?target $?))
         (action (action ~?z)
                 (aliases $? ?target $?))
         =>
         (format werror "ERROR: alias %s refers to two different commands!%n" ?target)
         (exit 1))

(defrule init:found-action-conflict:declare-as-command-and-alias
         (message (action stages)
                  (contents init $?))
         (action (action ?z))
         (action (aliases $? ?z $?))
         =>
         (format werror "ERROR: %s is defined as a command and alias!%n" ?z)
         (exit 1))

(defrule parse-input:first-is-action
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action input)
                        (contents ?tok $?rest))
         (action (action ?tok))
         =>
         (assert (statement (action ?tok)
                            (operands $?rest)))
         (retract ?f))

(defrule parse-input:first-is-indirect-action
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action input)
                        (contents ?tok $?rest))
         (action (aliases $? ?tok $?)
                 (action ?act))
         =>
         (retract ?f)
         (assert (statement (action ?act)
                            (operands $?rest))))

(defrule parse-input:first-is-not-action
         ; fall-through rather than have goofy logic
         (declare (salience -1))
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action input)
                        (contents ?tok $?rest))
         =>
         (retract ?f)
         (format t "Your statements must start with an action and %s is not an action!%n" ?tok))

;-----------------------------------------------------------------------------
; command declarations go last
;-----------------------------------------------------------------------------
(batch* /logic/cmd/system.clp)

; This does semi-nlp on the input from the prompt
; The nice thing is that the form I expect things is quite flexible but
; there is some order to it. For instance, the inputs will always go
; <action> <operand> <to/on/at> <operand>
; <action> <operand> 
; <action> 

(deftemplate token 
             (slot index
                   (type INTEGER)
                   (default ?NONE))
             (slot value
                   (type LEXEME)
                   (default ?NONE))
             (slot type
                   (type SYMBOL)
                   (default unknown)))
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
         (assert (action 
                   (class ?class)
                   (action ?action)
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

(defrule parse-input:tokenize
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action input)
                        (from ?ind)
                        (contents ?tok $?rest))
         =>
         (modify ?f (from (+ ?ind 1))
                 (contents $?rest))
         (assert (token (index ?ind)
                        (value ?tok))))

(defrule parse-input:finish-tokenizing
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action input)
                        (from ?end)
                        (contents))
         =>
         (modify ?f (action statement)
                 (from 1)
                 (to ?end)))


;-----------------------------------------------------------------------------
; Tokens are processed specially by different rules which represent different
; items and such. Generally speaking it is necessary to do this using as
; simple and independent of rules as possible. Then we can do checks after
; the fact to make sure of things but generally speaking we don't want to
; have too complex of rules. We just match up the different tokens to things
; to see if we have something meaningful and if we do then we simplify it 
; to a form that makes it easy to define rooms, commands, and such through
; the rete network itself only storing state as needed. Each room is actually
; a set of stimuli/hooks that the program responds to. In fact, save games
; should consist solely of parsed output that is run in a non-interactive
; version of the game until all commands have been executed (yes even the
; ones that do nothing)
;-----------------------------------------------------------------------------
(defrule tag-type:direct-command
         (declare (salience 4))
         (message (action stages)
                  (contents parse $?))
         ?f <- (token (value ?z)
                      (type unknown))
         (action (action ?z))
         =>
         (modify ?f (type action)))

(defrule tag-type:indirect-command
         (declare (salience 4))
         (message (action stages)
                  (contents parse $?))
         ?f <- (token (value ?z)
                      (type unknown))
         (not (exists (action (action ?z))))
         (action (aliases $? ?z $?)
                 (action ?q))
         =>
         (modify ?f (value ?q)
                 (type action)))
(defrule tag-type:number
         (declare (salience 3))
         (message (action stages)
                  (contents parse $?))
         ?f <- (token (value ?z)
                      (type unknown))
         (test (numberp ?z))
         =>
         (modify ?f (type number)))

(defrule tag-type:lexeme
         (declare (salience 3))
         (message (action stages)
                  (contents parse $?))
         ?f <- (token (value ?z)
                      (type unknown))
         (test (lexemep ?z))
         =>
         (modify ?f (type lexeme)))

(defrule reconstitute-into-statement
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action statement)
                        (from ?a)
                        (to ?b&:(>= ?b ?a))
                        (contents $?contents))
         (token (index ?a)
                (type ?c))
         =>
         (modify ?f (from (+ ?a 1))
                 (contents $?contents ?c)))

(defrule finished-reconstitution
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action statement)
                        (from ?a)
                        (to ?a)
                        (contents $?symbols))
         =>
         ; Now we need to determine what the hell this line is actually
         ; requesting
         (modify ?f (action check-statement)
                 (from 1)))

(defrule check-statement:starts-with-action
         "Inputs always start with the action to be performed" 
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action check-statement)
                        (contents action $?rest))
         =>
         )

(defrule check-statement:does-not-start-with-action
         (message (action stages)
                  (contents parse $?))
         ?f <- (message (action check-statement)
                        (from ?j)
                        (to ?k)
                        (contents ~action $?rest))
         =>
         ; we need to clear out the tokens and print a message saying unknown
         ; action or something similar
         )
; command declarations go last
(batch* /logic/cmd/system.clp)

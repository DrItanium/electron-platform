; This does semi-nlp on the input from the prompt
; The nice thing is that the form I expect things is quite flexible but
; there is some order to it

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
             (slot class
                   (type SYMBOL)
                   (default ?NONE))
             (slot action
                   (type LEXEME)
                   (default ?NONE))
             (multislot aliases
                        (type LEXEME)))

;-----------------------------------------------------------------------------
; Setup
;-----------------------------------------------------------------------------
(defrule init:define-actions
         (message (action stages)
                  (contents init $?))
         ?f <- (defaction ?class ?action <- $?aliases) 
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
                        (from ?)
                        (contents))
         =>
         (retract ?f))


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
         (message (action stages)
                  (contents parse $?))
         ?f <- (token (value ?z)
                      (type unknown))
         (action (action ?z)
                 (class ?c))
         =>
         (modify ?f (type ?c)))

(defrule tag-type:indirect-command
         (message (action stages)
                  (contents parse $?))
         ?f <- (token (value ?z)
                      (type unknown))
         (not (exists (action (action ?z))))
         (action (aliases $? ?z $?)
                 (action ?q)
                 (class ?c))
         =>
         (modify ?f (value ?q)
                 (type ?c)))

; put these last
(batch* /logic/cmd/system.clp)

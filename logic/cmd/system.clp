;-----------------------------------------------------------------------------
; System Commands
;-----------------------------------------------------------------------------
(deffacts SystemCommands
          ; req   class action  req  aliases
          (defaction sys   quit    <-   exit bye leave)
          (defaction sys   save    <-   dump freeze)
          (defaction sys   load    <-   revive unfreeze)
          (defaction sys   restart <-   )
          (defaction sys   about   <-   sysinfo)
          )

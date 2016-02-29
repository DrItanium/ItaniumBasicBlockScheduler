;(printout t 
;          "Welcome to the Code Scheduler!" crlf
;          "To start scheduling type in (block \"/path/to/file\")" crlf
;          "Then type in (analyze)" crlf
;          "When it is finished the result will be printed out." crlf
;          "Parallel sections of code are separated by ;;." crlf
;          "The groups of instructions separated by ;; are known as Instruction Groups" crlf)
;(block "examples/BlockLarge.clp")
(deffunction start 
             (?a)
             (watch statistics)
             (profile-reset)
             (profile ?a)
             (run)
             (profile off)
             (profile-info))

;(watch rules dependency:identify-waw-and-raw)
;(watch activations)
;(watch statistics)
(profile-reset)
(profile constructs)
(block "examples/BlockLarge.clp")
(run)
(profile off)
(profile-info)
(exit)

;------------------------------------------------------------------------------
;Copyright (c) 2012-2016, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL Joshua Scoggins BE LIABLE FOR ANY
;DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
(defmodule MAIN
           (export ?ALL))
(defrule MAIN::startup
         =>
         (focus schedule 
                schedule-update))

(defmodule schedule
           "identify instructions to schedule"
           (import MAIN 
                   ?ALL)
           (export deftemplate 
                   schedule-directive))

(deftemplate schedule::schedule-directive
             (slot target
                   (default ?NONE)))
(defmodule schedule-update
           "Update, and output, the contents of a packet"
           (import MAIN
                   ?ALL)
           (import schedule
                   deftemplate
                   schedule-directive))

;------------------------------------------------------------------------------
; Scheduling rules
;------------------------------------------------------------------------------
; The use of the producer-count slot instead of producers is because we don't
; actually care what the producer was, only if we have no producers remaining
; This allows us to just decrement the value instead of doing expensive pattern
; matching
;------------------------------------------------------------------------------
(defrule schedule::determine-scheduability
         "An object is able to be scheduled if it has no remaining producers"
         (object (is-a register)
                 (queue ?q $?))
         (test (send ?q 
                     ready-to-schedule))
         =>
         (assert (schedule-directive (target ?q))))

(defrule schedule-update::update-producer-set
         ?f <- (schedule-directive (target ?q))
         =>
         (retract ?f)
         (assert (Restart Scheduling))
         (send ?q notify-scheduling))

(defrule schedule-update::restart-scheduling
         (declare (salience -1))
         ?f <- (Restart Scheduling)
         =>
         (retract ?f)
         (printout ?*output-router* ";;" crlf)
         (focus schedule))

(defrule schedule-update::insert-branch
         (declare (salience -2))
         (object (is-a Instruction)
                 (InstructionType B)
                 (name ?branch))
         =>
         (send ?branch 
               notify-scheduling)
         (printout ?*output-router* ";;" crlf))

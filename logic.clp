;------------------------------------------------------------------------------
;Copyright (c) 2012-2014, Joshua Scoggins 
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
; stage rules
;------------------------------------------------------------------------------
(defrule stages-init
         (declare (salience 10000))
         (initial-fact)
         =>
         (assert (stage (current Imbue) 
                        (rest Analysis-Entry 
                              Analysis 
                              Schedule
                              Schedule-Update))))

(defrule end-stage-generation
         (declare (salience -9999))
         ?f <- (stage (rest))
         =>
         (retract ?f))

(defrule next-stage
         (declare (salience -10000))
         ?f <- (stage (rest ?now $?rest))
         =>
         (modify ?f (current ?now) 
                 (rest $?rest)))
;------------------------------------------------------------------------------
; dependency analysis rules
;------------------------------------------------------------------------------
(defrule imbue-op 
         "Imbue's the operational type into the given instruction"
         (declare (salience 1))
         (stage (current Imbue))
         (object (is-a Operation) 
                 (Class ?Class) 
                 (Name ?oName) 
                 (Length ?oLength))
         (object (is-a Instruction) 
                 (Name ?oName) 
                 (ExecutionLength ~?oLength)
                 (name ?gid))
         =>
         (modify-instance ?gid
                          (InstructionType ?Class)
                          (ExecutionLength ?oLength))
         (if (eq ?Class B) then 
           (assert (Check ?gid))))

(defrule generate-branch-dependencies 
         (stage (current Imbue))
         ?chk <- (Check ?gid)
         (object (is-a Instruction) 
                 (name ?gid) 
                 (InstructionType B)
                 (ExecutionLength ~-1)
                 (producer-count 0)
                 (Name ?name) 
                 (TimeIndex ?ti))
         =>
         (retract ?chk)
         (send ?gid put-producer-count ?ti)
         (loop-for-count (?i 0 (- ?ti 1)) do
                         (assert (BranchImbue ?gid ?i))))

(defrule imbue-branch-dependencies
         (stage (current Imbue))
         ?bd <- (BranchImbue ?name ?i)
         ?inst <- (object (is-a Instruction) 
                          (TimeIndex ?i) 
                          (InstructionType ?IT))
         =>
         (retract ?bd)
         ;Register the branch in the consumer set
         (slot-insert$ ?inst consumers 1 ?name)
         (if (eq ?IT B) then
           ;we don't care what the producer actually is
           ;That is more important for region scheduling
           (send ?name decrement-producer-count)))

(defrule initial-select-first-compare-instruction
         (stage (current Analysis-Entry))
         (object (is-a Instruction) 
                 (TimeIndex 0)
                 (name ?name))
         =>
         (assert (Instruction ?name)))

(defrule decompose-target-instruction-sections
         (stage (current Analysis-Entry))
         (object (is-a Instruction)
                 (InstructionType ~B)
                 (name ?g0)
                 (TimeIndex ?ti)
                 (destination-registers $?dest)
                 (source-registers $?src)
                 (Predicate ?p))
         =>
         (if (neq ?p p0) then
           (assert (register-ref (type predicate)
                                 (time-index ?ti)
                                 (target-register ?p)
                                 (parent ?g0))))

         (progn$ (?s $?src)
                 (if (and (neq ?s p0)
                          (symbolp ?s)) then
                   (assert (register-ref (type source)
                                         (time-index ?ti)
                                         (target-register (if (eq (sub-string 1 1 ?s) "{") then
                                                            (sym-cat (sub-string 2 (- (str-length ?s) 1) ?s)) else ?s))
                                         (parent ?g0)))))
         (progn$ (?d $?dest)
                 (if (neq ?d p0) then
                   (assert (register-ref (type destination)
                                         (time-index ?ti)
                                         (target-register ?d)
                                         (parent ?g0))))))


(defrule skip-current-instruction-if-branch
         (declare (salience 1000))
         (stage (current Analysis))
         ?f <- (Instruction ?name)
         (object (is-a Instruction) 
                 (name ?name) 
                 (InstructionType B))
         =>
         (retract ?f)
         (assert (Attempt Instruction (+ (send ?name get-TimeIndex) 1))))

(defrule define-destination-dependency-WAW
         "Identifies a WAW dependency"
         (stage (current Analysis))
         (Instruction ?g0)
         (register-ref (parent ?g0)
                       (type destination)
                       (time-index ?t0)
                       (target-register ?d))
         (register-ref (time-index ?t1&:(> ?t1 ?t0))
                       (target-register ?d)
                       (type predicate)
                       (parent ?g1))
         =>
         (bind ?*TemporaryList* (create$ ?*TemporaryList* ?g1)))

(defrule define-destination-dependency-RAW
         "Identifies a RAW dependency"
         (stage (current Analysis))
         (Instruction ?g0)
         (register-ref (parent ?g0)
                       (type destination)
                       (time-index ?t0)
                       (target-register ?d))
         (register-ref (time-index ?t1&:(> ?t1 ?t0))
                       (target-register ?d)
                       (type source)
                       (parent ?g1))
         =>
         (bind ?*TemporaryList* (create$ ?*TemporaryList* ?g1)))

(defrule define-destination-dependency-RAW-predicate
         "Identifies a RAW dependency with the predicate"
         (stage (current Analysis))
         (Instruction ?g0)
         (register-ref (parent ?g0)
                       (type destination)
                       (time-index ?t0)
                       (target-register ?d))
         (register-ref (time-index ?t1&:(> ?t1 ?t0))
                       (target-register ?d)
                       (type predicate)
                       (parent ?g1))
         =>
         (bind ?*TemporaryList* (create$ ?*TemporaryList* ?g1)))

; This is a generic scheduler and doesn't take special cases into account
(defrule start-analysis-restart-process
         (declare (salience -1))
         (stage (current Analysis))
         ?f <- (Instruction ?g0)
         =>
         (retract ?f)
         ; commit the dependencies we have found
         (send ?g0 inject-consumers ?*TemporaryList*)

         (bind ?*TemporaryList* (create$))
         (assert (Next (+ (send ?g0 get-TimeIndex) 1))))

(defrule try-restart-analysis-process
         (declare (salience -2))
         (stage (current Analysis))
         ?f2 <- (Next ?i)
         (object (is-a Instruction)
                 (TimeIndex ?i)
                 (name ?name))
         =>
         (retract ?f2)
         (assert (Instruction ?name)))

(defrule finish-analysis-process  
         (declare (salience -3))
         (stage (current Analysis))
         ?f2 <- (Next ?)
         =>
         (retract ?f2))

;------------------------------------------------------------------------------
; Scheduling rules
;------------------------------------------------------------------------------
; The use of the producer-count slot instead of producers is because we don't
; actually care what the producer was, only if we have no producers remaining
; This allows us to just decrement the value instead of doing expensive pattern
; matching
;------------------------------------------------------------------------------
(defrule determine-scheduability
         "An object is able to be scheduled if it has no remaining producers"
         (stage (current Schedule))
         (object (is-a Instruction)
                 (producer-count 0)
                 (scheduled FALSE)
                 (name ?id))
         =>
         (printout t (send ?id as-string) crlf)
         (send ?id put-scheduled TRUE)
         (assert (Scheduled ?id)
                 (close block)))


(defrule close-schedule-round
         (declare (salience -1))
         (stage (current Schedule))
         ?f <- (close block)
         =>
         (retract ?f)
         (printout t ";;" crlf))

(defrule update-producer-set
         (stage (current Schedule-Update))
         ?f <- (Scheduled ?id)
         (object (is-a Instruction)
                 (name ?id)
                 (consumers $?consumers))
         =>
         (retract ?f)
         (progn$ (?c ?consumers)
                 (send ?c decrement-producer-count))
         (assert (Restart Scheduling)))

(defrule restart-scheduling
         (declare (salience -2))
         ?f <- (Restart Scheduling)
         ?stg <- (stage (current Schedule-Update) 
                        (rest $?rest))
         =>
         (retract ?f)
         (modify ?stg (current Schedule) 
                 (rest Schedule-Update $?rest)))

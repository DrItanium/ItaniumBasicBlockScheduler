;Copyright (c) 2012, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;    * Neither the name of Joshua Scoggins nor the
;      names of its contributors may be used to endorse or promote products
;      derived from this software without specific prior written permission.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependency.clp - related to Dependency analysis                           ;;
;; By Joshua Scoggins                                                        ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         (loop-for-count (?i 0 (- ?ti 1)) do
                         (assert (BranchImbue ?name ?i))))

(defrule imbue-branch-dependencies
         (stage (current Imbue))
         ?bd <- (BranchImbue ?name ?i)
         ?inst <- (object (is-a Instruction) 
                          (TimeIndex ?i) 
                          (InstructionType ?IT))
         ?branch <- (object (is-a Instruction) 
                            (Name ?name) 
                            (name ?bid))
         =>
         ;Register the branch in the consumer set
         (slot-insert$ ?inst consumers 1 ?bid)
         (if (neq ?IT B) then
           ;we don't care what the producer actually is
           ;That is more important for region scheduling
           (send ?branch increment-producer-count))
         (retract ?bd))

(defrule initial-select-first-compare-instruction
         (declare (salience 10000))
         (stage (current Analysis-Entry))
         (object (is-a Instruction) 
                 (TimeIndex 0)
                 (name ?name))
         =>
         (assert (Instruction ?name)))

(defrule skip-current-instruction-if-branch
         (declare (salience 1000))
         (stage (current Analysis))
         ?f <- (Instruction ?name)
         (object (is-a Instruction) 
                 (name ?name) 
                 (InstructionType B)
                 (TimeIndex ?tc0))
         (object (is-a Instruction) 
                 (TimeIndex =(+ 1 ?tc0))
                 (name ?nName))
         =>
         (retract ?f)
         (assert (Instruction ?nName)))
(defrule decompose-target-instruction-sections
         (declare (salience 10))
         (stage (current Analysis-Entry))
         (object (is-a Instruction)
                 (name ?g0)
                 (TimeIndex ?ti)
                 (InstructionType ~B)
                 (destination-registers $?dest)
                 (source-registers $?src)
                 (Predicate ?p))
         =>
         (if (neq ?p p0) then
             (assert (Register Predicate ?g0 ?ti ?p)))
         (progn$ (?s $?src)
                 (if (and (neq ?s p0)
                          (symbolp ?s)) then
                     (bind ?first (sub-string 1 1 ?s))
                     (bind ?register (sym-cat (if (eq ?first "{") then
                                                  (sub-string 2 
                                                   (- (str-length ?s) 1) ?s)
                                                  else
                                                  ?s)))
                     (assert (Register Source ?g0 ?ti ?register ?s-index))))
         (progn$ (?d $?dest)
                 (if (neq ?d p0) then
                     (assert (Register Destination ?g0 ?ti ?d)))))
(defrule define-WAW-dependency
         "Identifies a WAW dependency"
         (stage (current Analysis))
         (Instruction ?g0)
         (Register Destination ?g0 ?t0 ?d)
         (Register Destination ?g1&~?g0 ?t1&:(> ?t1 ?t0) ?d)
         =>
         (assert (Dependency (firstInstructionID ?g0)
                             (secondInstructionID ?g1))
                 (Inject)))

(defrule define-RAW-dependency:predicate
         "Finds a RAW dependency with a predicate"
         (stage (current Analysis))
         (Instruction ?g0)
         (Register Destination ?g0 ?t0 ?d)
         (Register Predicate ?g1&~?g0 ?t1&:(> ?t1 ?t0) ?d)
         =>
         (assert (Dependency (firstInstructionID ?g0)
                             (secondInstructionID ?g1))
                 (Inject)))

(defrule define-RAW-dependency
         "Finds a RAW dependency"
         (stage (current Analysis))
         (Instruction ?g0)
         (Register Destination ?g0 ?t0 ?d)
         (Register Source ?g1&~?g0 ?t1&:(> ?t1 ?t0) ?d ?)
         =>
         (assert (Dependency (firstInstructionID ?g0)
                             (secondInstructionID ?g1))
                 (Inject)))

(defrule define-WAR-dependency:predicate
         "Finds a WAR dependency with a predicate"
         (stage (current Analysis))
         (Instruction ?g0)
         (Register Source ?g0 ?t0 ?d ?)
         (Register Predicate ?g1&~?g0 ?t1&:(> ?t1 ?t0) ?d)
         =>
         (assert (Dependency (firstInstructionID ?g0)
                             (secondInstructionID ?g1))
                 (Inject)))
(defrule define-WAR-dependency
         "Finds a WAR dependency"
         (stage (current Analysis))
         (Instruction ?g0)
         (Register Source ?g0 ?t0 ?d ?)
         (Register Destination ?g1&~?g0 ?t1&:(> ?t1 ?t0) ?d)
         =>
         (assert (Dependency (firstInstructionID ?g0)
                             (secondInstructionID ?g1))
                 (Inject)))
(defrule inject-producers-consumers
         (declare (salience -1))
         (stage (current Analysis))
         ?f <- (Inject)
         (Instruction ?g0)
         =>
         (retract ?f)
         (bind ?contents (create$))
         (delayed-do-for-all-facts ((?a Dependency)) 
                                   (eq ?a:firstInstructionID ?g0)
                                   ;reduce the number of messages by asserting facts instead
                                   (send ?a:secondInstructionID increment-producer-count)
                                   (bind ?contents (create$ ?contents ?a:secondInstructionID))
                                   (retract ?a))
         (slot-insert$ ?g0 consumers 1 ?contents))

(defrule start-analysis-restart-process
         (declare (salience -1000))
         (stage (current Analysis))
         ?f <- (Instruction ?g0)
         =>
         (retract ?f)
         (assert (Attempt Instruction (+ (send ?g0 get-TimeIndex) 1))))

(defrule try-restart-analysis-process
         (declare (salience -1000))
         (stage (current Analysis))
         ?f2 <- (Attempt Instruction ?i)
         (object (is-a Instruction)
                 (TimeIndex ?i)
                 (name ?name))
         =>
         (retract ?f2)
         (assert (Instruction ?name)))

(defrule finish-analysis-process  
         (declare (salience -1000))
         (stage (current Analysis))
         ?f2 <- (Attempt Instruction ?i)
         (not (exists (object (is-a Instruction)
                              (TimeIndex ?i))))
         =>
         (retract ?f2))

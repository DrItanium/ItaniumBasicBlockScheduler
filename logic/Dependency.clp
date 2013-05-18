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
			(Stage Imbue $?)
			(object (is-a Operation) 
					  (Class ?Class) 
					  (Name ?oName) 
					  (Length ?oLength))
			?inst <- (object (is-a Instruction) 
								  (Name ?oName) 
								  (ExecutionLength ~?oLength)
								  (name ?gid))
			=>
			(modify-instance ?inst 
								  (InstructionType ?Class)
								  (ExecutionLength ?oLength))
			(if (eq ?Class B) then 
			  (assert (Check ?gid))))

(defrule generate-branch-dependencies 
			(Stage Imbue $?)
			?chk <- (Check ?gid)
			?inst <- (object (is-a Instruction) 
								  (InstructionType B)
								  (ExecutionLength ~-1)
								  (producer-count 0)
								  (name ?gid) 
								  (Name ?name) 
								  (TimeIndex ?ti))
			=>
			(retract ?chk)
			(loop-for-count (?i 0 (- ?ti 1)) do
			                (assert (BranchImbue ?name ?i))))

(defrule imbue-branch-dependencies
			(Stage Imbue $?)
			?bd <- (BranchImbue ?name ?i)
			?branch <- (object (is-a Instruction) 
					  (Name ?name) 
					  (name ?bid))
			?inst <- (object (is-a Instruction) 
					  (TimeIndex ?i) 
					  (InstructionType ?IT))
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
         (Stage Analysis-Entry $?)
			(object (is-a Instruction) 
			        (TimeIndex 0)
					  (name ?name))
         =>
         (assert (Instruction ?name)))

(defrule skip-current-instruction-if-branch
         (declare (salience 1000))
         (Stage Analysis $?)
			?f <- (Instruction ?name)
			(object (is-a Instruction) 
			 (name ?name) 
			 (TimeIndex ?tc0) 
			 (InstructionType B))
			(object (is-a Instruction) 
			 (TimeIndex =(+ 1 ?tc0))
			 (name ?nName))
			=>
			(retract ?f)
			(assert (Instruction ?nName)))

(defrule define-WAW-dependency "Defines/or modifies a dependency"
			(Stage Analysis $?)
			(Instruction ?g0)
			(object (is-a Instruction) 
			        (name ?g0)
					  (TimeIndex ?tc0)
					  (destination-registers $? ?d&~p0 $?))
			(object (is-a Instruction) 
					  (TimeIndex ?tc1&:(< ?tc0 ?tc1)) 
					  (InstructionType ~B)
					  (destination-registers $? ?d|=(sym-cat { ?d }) $?)
					  (name ?g1))
			=>
			(assert (Dependency (firstInstructionID ?g0) 
									  (secondInstructionID ?g1))))
(defrule define-RAW-dependency-predicate 
			"Defines/or modifies a dependency"
			(Stage Analysis $?)
			(Instruction ?g0)
			(object (is-a Instruction) 
			        (name ?g0)
					  (TimeIndex ?tc0)
					  (destination-registers $? ?d&~p0 $?))
			(object (is-a Instruction) 
					  (TimeIndex ?tc1&:(< ?tc0 ?tc1)) 
					  (InstructionType ~B)
					  (Predicate ?d)
					  (name ?g1))
			=>
			(assert (Dependency (firstInstructionID ?g0) 
									  (secondInstructionID ?g1))))

(defrule define-RAW-dependency 
			"Defines/or modifies a dependency"
			(Stage Analysis $?)
			(Instruction ?g0)
			(object (is-a Instruction) 
					  (name ?g0)
					  (TimeIndex ?tc0)
					  (destination-registers $? ?d&~p0 $?))
			(object (is-a Instruction) 
					  (TimeIndex ?tc1&:(< ?tc0 ?tc1)) 
					  (InstructionType ~B)
					  (source-registers $? ?d|=(sym-cat { ?d }) $?)
					  (name ?g1))
			=>
			(assert (Dependency (firstInstructionID ?g0) 
									  (secondInstructionID ?g1))))

(defrule define-WAR-dependency-predicate  
 "Defines/or modifies a WAR dependency"
			(Stage Analysis $?)
			(Instruction ?g0)
			(object (is-a Instruction) 
					  (name ?g0)
					  (TimeIndex ?tc0)
					  (source-registers $? ?s&~p0&:(symbolp ?s) $?))
			(object (is-a Instruction) 
					  (TimeIndex ?tc1&:(< ?tc0 ?tc1))
					  (InstructionType ~B)
					  (Predicate ?s)
					  (name ?g1))
			=>
			(assert (Dependency (firstInstructionID ?g0)
									  (secondInstructionID ?g1))))

(defrule define-WAR-dependency "Defines/or modifies a WAR dependency"
			(Stage Analysis $?)
			(Instruction ?g0)
			(object (is-a Instruction) 
					  (name ?g0)
					  (TimeIndex ?tc0)
					  (source-registers $? ?s&~p0&:(symbolp ?s) $?))
			(object (is-a Instruction) 
					  (TimeIndex ?tc1&:(< ?tc0 ?tc1)) 
					  (InstructionType ~B)
					  (destination-registers $? ?s|=(sym-cat { ?s }) $?)
					  (name ?g1))
			=>
			(assert (Dependency (firstInstructionID ?g0)
									  (secondInstructionID ?g1))))

(defrule inject-producers-consumers
			(declare (salience -1))
			(Stage Analysis $?)
			?d <- (Dependency (firstInstructionID ?d0) 
									      (secondInstructionID ?d1))
			=>
			(send ?d1 increment-producer-count)
			(slot-insert$ ?d0 consumers 1 ?d1)
			(retract ?d))

(defrule start-analysis-restart-process
         (declare (salience -1000))
         (Stage Analysis $?)
         ?f <- (Instruction ?g0)
			(object (is-a Instruction) 
			        (name ?g0) 
			        (TimeIndex ?i))
         =>
         (retract ?f)
         (assert (Attempt Instruction (+ ?i 1))))
          
(defrule try-restart-analysis-process
         (declare (salience -1000))
			(Stage Analysis $?)
         ?f2 <- (Attempt Instruction ?i)
			(object (is-a Instruction)
			        (TimeIndex ?i)
					  (name ?name))
         =>
         (retract ?f2)
         (assert (Instruction ?name)))

(defrule finish-analysis-process  
         (declare (salience -1000))
         (Stage Analysis $?)
         ?f2 <- (Attempt Instruction ?i)
         (not (exists (object (is-a Instruction)
                              (TimeIndex ?i))))
         =>
         (retract ?f2))

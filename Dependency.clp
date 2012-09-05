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
;; Dependency.clp - denotes functions and rules related to Dependency        ;;
;; analysis                                                                  ;;
;; By Joshua Scoggins                                                        ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Templates                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate Dependency "Represents a Data Dependency between two instructions"
 (slot firstInstructionGUID (type NUMBER))
 (slot secondInstructionGUID (type NUMBER))
 (slot dependencyType (type SYMBOL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction instance-to-symbol 
 "Converts an instance name to a symbol"
 (?I)
 (return (instance-name-to-symbol (instance-name ?I))))

(deffunction contains-registerp "Does a check to see if _any_ element in the first list is in the second"
 (?L1 ?L2 $?IGNORE)
 (foreach ?reg0 ?L1
  (foreach ?reg1 ?L2
   (bind ?t0 (sym-cat { ?reg0 }))
   (bind ?t1 (sym-cat { ?reg1 }))
   (if (and (not (subsetp (create$ ?reg0) ?IGNORE))
        (or (eq ?reg0 ?reg1) 
         (eq ?t0 ?reg1)
         (eq ?reg0 ?t1))) then (return TRUE))))
 (return FALSE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rules                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule imbue-op "Imbue's the operational type into the given instruction"
 (Imbue)
 (object (is-a Operation) (Class ?Class) (Name ?oName) (Length ?oLength))
 ?inst <- (object (is-a Instruction) (GUID ?gid) (Name ?name&?oName) 
	 (ExecutionLength ?length&~?oLength))
 =>
 (send ?inst put-InstructionType ?Class)
 (send ?inst put-ExecutionLength ?oLength)
 (if (eq ?Class B) then (assert (Check ?gid))))

(defrule generate-branch-dependencies 
 (Imbue)
 ?chk <- (Check ?gid)
 ?inst <- (object (is-a Instruction) (GUID ?gid) (Name ?name) 
	 (ExecutionLength ?l&:(<> ?l -1)) (TimeIndex ?ti) 
	 (InstructionType B) (DependencyInformation ?dci))
 (test (not (send ?dci .HasProducers)))
 =>
 (retract ?chk)
 (bind ?i 0)
 (while (< ?i ?ti)
  (assert (BranchImbue ?name ?i))
  (bind ?i (+ ?i 1))))

(defrule imbue-branch-dependencies
 (Imbue)
 ?bd <- (BranchImbue ?name ?i)
 ?branch <- (object (is-a Instruction) (Name ?name) 
	 (DependencyInformation ?dci))
 ?other <- (object (is-a Instruction) (TimeIndex ?i) (InstructionType ?IT))
  =>
  (if (neq ?IT B) then
    (send ?branch .AddProducer (instance-to-symbol ?other)))
  (retract ?bd))

(defrule final-rule-imbue
 (declare (salience -1000))
 ?im <- (Imbue)
 =>
 (retract ?im)
 (assert (Analysis)))


(defrule define-WAW-dependency "Defines/or modifies a dependency"
 (Analysis)
 (object (is-a Instruction) (GUID ?g0) (DestinationRegisters ?DR0)
  (TimeIndex ?tc0) (InstructionType ?z0&~B))
 (object (is-a Instruction) (GUID ?g1&~?g0) (Predicate ?p)
  (DestinationRegisters ?DR1) (TimeIndex ?tc1&:(< ?tc0 ?tc1)) (InstructionType ?z1&~B))
 (test (contains-registerp (send ?DR0 get-Contents)
		  (send ?DR1 get-Contents) p0))
 =>
 (assert (Dependency (firstInstructionGUID ?g0) 
			 (secondInstructionGUID ?g1) (dependencyType WAW))))

(defrule define-RAW-dependency "Defines/or modifies a dependency"
 (Analysis)
 (object (is-a Instruction) (GUID ?g0) (DestinationRegisters ?DR0)
  (TimeIndex ?tc0) (InstructionType ?z0&~B))
 (object (is-a Instruction) (GUID ?g1&~?g0) (Predicate ?p)
  (SourceRegisters ?SR0) (TimeIndex ?tc1&:(< ?tc0 ?tc1)) (InstructionType ?z1&~B))
 (test (contains-registerp (send ?DR0 get-Contents)
		  (create$ ?p (send ?SR0 get-Contents)) p0))
 =>
 (assert (Dependency (firstInstructionGUID ?g0) (secondInstructionGUID ?g1)
			 (dependencyType RAW))))

(defrule define-WAR-dependency "Defines/or modifies a WAR dependency"
 (Analysis)
 (object (is-a Instruction) (GUID ?g0) (SourceRegisters ?SR0) (TimeIndex ?tc0)
  (InstructionType ~B))
 (object (is-a Instruction) (GUID ?g1&~?g0) (Predicate ?p)
  (DestinationRegisters ?DR0) (TimeIndex ?tc1&:(< ?tc0 ?tc1)) 
  (InstructionType ~B))
 (test (contains-registerp 
		  (send ?SR0 get-Contents) 
		  (create$ ?p (send ?DR0 get-Contents)) p0))
 =>
 (assert (Dependency (firstInstructionGUID ?g0)
			 (secondInstructionGUID ?g1) 
			 (dependencyType WAR))))

(defrule inject-producers-consumers
 (Analysis)
 ?d <- (Dependency (firstInstructionGUID ?g0) (secondInstructionGUID ?g1))
 ?i0 <- (object (is-a Instruction) (GUID ?g0)) 
 ?i1 <- (object (is-a Instruction) (GUID ?g1)) 
 =>
 (send ?i0 .AddConsumer (instance-to-symbol ?i1))
 (send ?i1 .AddProducer (instance-to-symbol ?i0))
 (retract ?d))

;(defrule printout-dependency-info "Prints out each dependency chain"
; (declare (salience -12))
; (Analysis)
; (object (is-a Instruction) (TimeIndex ?ti) (DependencyInformation ?di))
; =>
; (printout t ?ti ": producers: " (send (send ?di get-Producers) get-Contents) 
;  " consumers: " (send (send ?di get-Consumers) get-Contents) crlf))

(defrule final-analysis-rule
 (declare (salience -1000))
 ?a <- (Analysis)
 =>
 (retract ?a)
 (assert (Collect)))

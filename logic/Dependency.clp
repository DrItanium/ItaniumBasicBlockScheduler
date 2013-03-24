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
(deftemplate Dependency 
				 "Represents a Data Dependency between two instructions"
				 (slot firstInstructionID (type SYMBOL))
				 (slot secondInstructionID (type SYMBOL))
				 (slot dependencyType (type SYMBOL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction instance-to-symbol 
				 "Converts an instance name to a symbol"
				 (?I)
				 (return (instance-name-to-symbol (instance-name ?I))))

(deffunction contains-registerp 
				 "Does a check to see if _any_ element in the first list is in the second"
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

(defrule imbue-op 
			"Imbue's the operational type into the given instruction"
			(Stage Imbue $?)
			(object (is-a Operation) 
					  (Class ?Class) 
					  (Name ?oName) 
					  (Length ?oLength))
			?inst <- (object (is-a Instruction) 
								  (id ?gid) 
								  (Name ?oName) 
								  (ExecutionLength ~?oLength))
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
								  (id ?gid) 
								  (Name ?name) 
								  (ExecutionLength ?l&:(<> ?l -1)) 
								  (TimeIndex ?ti) 
								  (InstructionType B))
			(object (is-a DependencyChain)
					  (parent ?gid)
					  (producers))
			=>
			(retract ?chk)
			(bind ?i 0)
			(while (< ?i ?ti)
					 (assert (BranchImbue ?name ?i))
					 (bind ?i (+ ?i 1))))

(defrule imbue-branch-dependencies
			(Stage Imbue $?)
			?bd <- (BranchImbue ?name ?i)
			(object (is-a Instruction) 
					  (Name ?name) 
					  (id ?bid))
			(object (is-a Instruction) 
					  (TimeIndex ?i) 
					  (id ?oid)
					  (InstructionType ?IT))
			?dc <- (object (is-a DependencyChain)
								(parent ?bid)
								(producers $?prod))
			=>
			(if (neq ?IT B) then
			  (modify-instance ?dc (producers $?prod ?oid)))
			(retract ?bd))


(defrule define-WAW-dependency "Defines/or modifies a dependency"
			(Stage Analysis $?)
			(object (is-a Instruction) 
					  (id ?g0) 
					  (destination-registers $?dr0)
					  (TimeIndex ?tc0) 
					  (InstructionType ~B))
			(object (is-a Instruction) 
					  (id ?g1&~?g0) 
					  (Predicate ?p)
					  (destination-registers $?dr1) 
					  (TimeIndex ?tc1&:(< ?tc0 ?tc1)) 
					  (InstructionType ~B))
			(test (contains-registerp $?dr0 $?dr1 p0))
			=>
			(assert (Dependency (firstInstructionID ?g0) 
									  (secondInstructionID ?g1) 
									  (dependencyType WAW))))

(defrule define-RAW-dependency "Defines/or modifies a dependency"
			(Stage Analysis $?)
			(object (is-a Instruction) 
					  (id ?g0) 
					  (destination-registers $?dr0)
					  (TimeIndex ?tc0) 
					  (InstructionType ~B))
			(object (is-a Instruction) 
					  (id ?g1&~?g0) 
					  (Predicate ?p)
					  (source-registers $?sr0) 
					  (TimeIndex ?tc1&:(< ?tc0 ?tc1)) 
					  (InstructionType ~B))
			(test (contains-registerp $?dr0
											  (create$ ?p $?sr0) p0))
			=>
			(assert (Dependency (firstInstructionID ?g0) 
									  (secondInstructionID ?g1)
									  (dependencyType RAW))))

(defrule define-WAR-dependency "Defines/or modifies a WAR dependency"
			(Stage Analysis $?)
			(object (is-a Instruction) 
					  (id ?g0) 
					  (source-registers $?sr0) 
					  (TimeIndex ?tc0)
					  (InstructionType ~B))
			(object (is-a Instruction) 
					  (id ?g1&~?g0) 
					  (Predicate ?p)
					  (destination-registers $?dr0) 
					  (TimeIndex ?tc1&:(< ?tc0 ?tc1)) 
					  (InstructionType ~B))
			(test (contains-registerp $?sr0
											  (create$ ?p $?dr0) p0))
			=>
			(assert (Dependency (firstInstructionID ?g0)
									  (secondInstructionID ?g1) 
									  (dependencyType WAR))))

(defrule inject-producers-consumers
			(Stage Analysis $?)
			?d <- (Dependency (firstInstructionID ?fi) 
									(secondInstructionID ?si))
			?d0 <- (object (is-a DependencyChain)
								(parent ?fi)
								(consumers $?cons))
			?d1 <- (object (is-a DependencyChain)
								(parent ?si)
								(producers $?prods))


			=>
			(modify-instance ?d0 (consumers $?cons ?fi))
			(modify-instance ?d1 (producers $?prods ?si))
			(retract ?d))

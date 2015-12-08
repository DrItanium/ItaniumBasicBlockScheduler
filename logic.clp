;------------------------------------------------------------------------------
;Copyright (c) 2012-2015, Joshua Scoggins 
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
(deffacts startup
	  (stage (current Analysis-Entry)
		 (rest Analysis
		       Schedule
		       Schedule-Update)))

(defrule end-stage-generation
	 (declare (salience -10000))
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
(defrule prime-first-instruction
	 (stage (current Analysis-Entry))
	 =>
	 (assert (Next (- (time-length) 1))))
; This is a generic scheduler and doesn't take special cases into account
(defrule start-analysis-restart-process
	 (declare (salience -2))
	 (stage (current Analysis))
	 ?f <- (Instruction ?g0)
	 (object (is-a Instruction)
		 (name ?g0)
		 (TimeIndex ?ti))
	 =>
	 (retract ?f)
	 ; commit the dependencies we have found
	 (assert (Next (- ?ti 1))))

(defrule try-restart-analysis-process
	 (declare (salience -2))
	 (stage (current Analysis))
	 ?f2 <- (Next ?i)
	 (object (is-a Instruction)
		 (TimeIndex ?i)
		 (InstructionType ~B)
		 (name ?name))
	 =>
	 (retract ?f2)
	 (assert (Instruction ?name)))

(defrule try-restart-analysis-process:branch
	 (declare (salience -2))
	 (stage (current Analysis))
	 ?f2 <- (Next ?i)
	 (object (is-a Instruction)
		 (TimeIndex ?i)
		 (InstructionType B))
	 =>
	 (retract ?f2)
	 (assert (Next (- ?i 1))))

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
	 (object (is-a register)
		 (queue ?q $?))
	 (test (send ?q ready-to-schedule))
	 =>
	 (assert (schedule (instance-address ?q))))



(defrule update-producer-set
	 (stage (current Schedule-Update))
	 ?f <- (schedule ?q)
	 =>
	 (retract ?f)
	 (send ?q notify-scheduling)
	 (assert (Restart Scheduling)))

(defrule restart-scheduling
	 (declare (salience -2))
	 ?f <- (Restart Scheduling)
	 ?stg <- (stage (current Schedule-Update) 
			(rest $?rest))
	 =>
	 (printout t ";;" crlf)
	 (retract ?f)
	 (modify ?stg 
		 (current Schedule) 
		 (rest Schedule-Update $?rest)))
(defrule insert-branch
	 (declare (salience -3))
	 (stage (current Schedule-Update))
	 (object (is-a Instruction)
		 (InstructionType B)
		 (name ?branch))
	 =>
	 (send ?branch notify-scheduling)
	 (printout t ";;" crlf))


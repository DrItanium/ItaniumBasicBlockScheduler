;------------------------------------------------------------------------------
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
;------------------------------------------------------------------------------
;; Schedule.clp - denotes functions and rules related to Schedules
;; By Joshua Scoggins
;------------------------------------------------------------------------------
; The current problem is that the scheduler is not very smart when it comes to
; doing dependency analysis. It is simple but not very smart and as such there
; is a lot of overhead using the subset command. 
;
; The real problem is that we are not using the set of consumers to trigger the
; next elements to be scheduled. At least, this is one way to do it.
;
; Another way would be to decompose the schedule rule into multiple rules to
; do the subset check within the LHS of the rule instead of the RHS. This is
; similar to how I did it in my thesis. The problem with this approach is that
; we are relying on the subset command when we can use the set of consumers to
; mark instructions for dispatch. 
;
; If we take a destructive approach then we can dispatch each instruction once
; its list of producers is empty. This requires the use of consumers to
; dispatch to the target instructions to remove from the list of producers
;------------------------------------------------------------------------------
(defrule determine-scheduability
         "An object is able to be scheduled if it has no remaining producers"
         (Stage Schedule $?)
			?inst <- (object (is-a Instruction)
								  (scheduled FALSE)
			                 (producers)
								  (consumers $?cs)
								  (id ?id))
			=>
			(printout t (send ?inst as-string) crlf)
			(send ?inst put-scheduled TRUE)
			(progn$ (?c ?cs)
			        (assert (Remove producer ?id from ?c))))
			     
(defrule close-schedule-round
         (declare (salience -1))
			(Stage Schedule $?)
			=>
			(printout t ";;" crlf))

(defrule build-initial-fact
			(Stage Schedule-Update-Init $?)
			(object (is-a Instruction)
					  (scheduled FALSE)
			        (id ?id)
					  (producers $?p))
			(exists (Remove producer ? from ?id))
			=>
			(assert (Instruction ?id has producers list $?p)))
			
;we need to merge the set of producers to remove which we can then 
;easily compare to the original list to figure out 
(defrule strip-producers
         (Stage Schedule-Update $?)
			?f <- (Remove producer ?id from ?c)
			?f2 <- (Instruction ?c has producers list $?b ?id $?a)
			=>
			(retract ?f ?f2)
			(assert (Instruction ?c has producers list $?b $?a)))

(defrule update-producer-set
         (Stage Schedule-Update $?)
		   ?f <- (Instruction ?c has producers list $?list)
			(not (exists (Remove producer ? from ?c)))
			?inst <- (object (is-a Instruction)
				              (id ?c))
			=>
			(retract ?f)
			(modify-instance ?inst (producers $?list))
			(assert (Restart Scheduling)))

(defrule restart-scheduling
         (declare (salience -2))
			?stg <- (Stage Schedule-Update $?rest)
			?f <- (Restart Scheduling)
			=>
			(retract ?f ?stg)
			(assert (Stage Schedule Schedule-Update-Init Schedule-Update $?rest)))

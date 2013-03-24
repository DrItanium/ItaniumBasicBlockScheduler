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
;; Schedule.clp - denotes functions and rules related to Schedules
;; By Joshua Scoggins
(defrule schedule
			(declare (salience 1))
			(Stage Schedule $?)
			?f <- (Schedule Instructions)
			?s <- (object (is-a Schedule)
					  (collect $?collect)
					  (at $?at))
			(test (> (length$ ?collect) 0))
			=>
			(retract ?f)
			(bind ?elements $?collect)
			(bind ?a0 $?at)
			(bind ?leftOver (create$))
			(bind ?result (create$))
			(foreach ?c ?elements
						(bind ?inst (symbol-to-instance-name ?c))
						(bind ?cond0 (not (member$ ?c ?at))) 
						(bind ?cond1 (subsetp (send ?inst get-producers) $?at))
						(if (and ?cond0 ?cond1) then
						  ;success
						  (bind ?a0 (create$ ?a0 ?c))
						  (bind ?result (create$ ?result ?c))
						  else
						  ;failure
						  (bind ?leftOver (create$ ?leftOver ?c))))
			(modify-instance ?s (collect $?leftOver)
			                    (at ?a0))
			(if (> (length$ ?result) 0) then
			  (make-instance of InstructionGroup
								  (TimeIndex (new-igid))
								  (contents ?result))))

(defrule print-instruction-group
			(Stage Schedule $?)
			?ig <- (object (is-a InstructionGroup) 
								(TimeIndex ?ti)
								(Printed FALSE)
								(contents $?contents))
			=>
			(foreach ?v $?contents
						(bind ?tmp (symbol-to-instance-name ?v))
						(printout t (send ?tmp as-string) crlf))
			(printout t ";;" crlf)
			(assert (Restart Scheduling))
			(modify-instance ?ig (Printed TRUE)))

(defrule restart-scheduling
         (declare (salience -2))
			(Stage Schedule $?)
			?f <- (Restart Scheduling)
			(object (is-a Schedule) 
			        (collect $?collect))
			(test (> (length$ ?collect) 0))
			=>
			(retract ?f)
			(assert (Schedule Instructions)))

(defrule end-restart-scheduling 
         (declare (salience -1))
			(Stage Schedule $?)
			;we still have the schedule instructions identifier
			?f <- (Restart Scheduling)
			?s <- (object (is-a Schedule)
			        (collect))
			=>
			(retract ?f)
			(unmake-instance ?s))


(defrule scheduling-error
         (declare (salience -1))
			(Stage Schedule $?)
			;we still have the schedule instructions identifier
			?f <- (Schedule Instructions)
			?s <- (object (is-a Schedule)
			        (collect))
			=>
			(retract ?f)
			(unmake-instance ?s))

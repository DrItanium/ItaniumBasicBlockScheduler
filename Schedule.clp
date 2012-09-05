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
 (Schedule)
 ?collect <- (object (is-a List) (Name Collect))
 (test (not (send ?collect .IsEmpty)))
 ?at <- (object (is-a List) (Name At))
 =>
 (bind ?q (make-instance (gensym*) of InstructionGroup
  (TimeIndex (new-igid))))
 (bind ?elements (quote (send ?collect get-Contents)))
 (foreach ?c ?elements
  (bind ?inst (symbol-to-instance-name ?c))
  (bind ?cond0 (not (send ?at .Contains ?c)))
  (bind ?cond1 (send ?at .ContainsSubset (send ?inst .Producers)))
  (if (and ?cond0 ?cond1) then
   (send ?at .Add ?c)
   (send ?collect .Remove ?c)
   (send ?q .Add ?c)))
 (if (send ?q .IsEmpty) then (unmake-instance ?q)))

(defrule print-instruction-group
 (Schedule)
 ?ig <- (object (is-a InstructionGroup) (TimeIndex ?ti)
	 (Printed FALSE))
 =>
 (foreach ?v (send (send ?ig get-Contents) get-Contents)
  (printout t (send (instance-name ?v) .ToString) crlf))
 (printout t ";;" crlf)
 (send ?ig put-Printed TRUE))

(defrule restart-scheduling
 (declare (salience -999))
 ?s <- (Schedule)
 (test (not (send [Collect] .IsEmpty)))
 =>
 (retract ?s)
 (assert (Schedule)))

(defrule final-scheduling-rule
 (declare (salience -1000))
 ?s <- (Schedule)
 (test (send [Collect] .IsEmpty))
 =>
 (retract ?s))

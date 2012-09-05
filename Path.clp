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
(deftemplate Path "Defines a dependency path"
 (multislot contents (type SYMBOL) (default ?DERIVE))
 (slot length (type INTEGER) (range 0 ?VARIABLE) (default 0))
 (slot GUID (type INTEGER)))

(defrule define-dependency-path "Defines a dependency path"
 (Dependency (firstInstructionGUID ?g0) (secondInstructionGUID ?g1)
  (dependentRegisters ?r))
 =>
 (assert (Path (contents (quote ?g0 ?g1)) (length 2) (GUID (new-guid)))))

(defrule modify-dependency-path "Modifies a dependency by extending it's length"
 ?Path <- (Path (contents $?First ?Last) (length ?length))
 (Dependency (firstInstructionGUID ?g0&:(eq ?Last ?g0)) (secondInstructionGUID ?g1))
 =>
 (modify ?Path (contents $?First ?Last ?g1) (length (1+ ?length))))

(defrule remove-subset-paths "Removes paths that are contained in another path"
 (Path (contents $?C) (GUID ?g0))
 ?subst <- (Path (contents $?C2) (GUID ?g1&~?g0))
 (test (subsetp $?C2 $?C))
 =>
 (retract ?subst))

;(defrule print-paths "Prints all dependencies at the end of the day"
; (declare (salience -10000))
; (Path (contents $?contents))
; =>
; (printout t ?contents crlf))

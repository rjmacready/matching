
(defpackage :pat
  (:use :cl)
  (:export :match-with :and*))

(in-package :pat)

(defun is-binded (bindings varname) 
  ;(print `(:check :if ,varname :is :binded))
  (not (null (assoc varname bindings))))

(defun get-value (bindings varname) 
  ;(print `(:get :value :for ,varname :is ,(cdr (assoc varname bindings))))
  (cdr (assoc varname bindings)))

(defun bind-value (bindings varname value)
  ;(print `(:binding ,value :to ,varname))
  (acons varname value bindings))


(defun match-with (full-pat ls)
  "Matches a pattern with a list"      
    (labels ((match-with-aux (pat ls bindings)
	       (cond
	         ; if pattern is null, all went well
		 ((null pat) 
		  (values t bindings))

	         ; if pattern is not null but list is, we were unable to match
		 ((and (not (null pat)) (null ls))
		  (values nil nil))
		 
		 (t
		    ;(progn
		  (let*
		      ((head-pat (car pat))
		       (head-ls (car ls)))
			
		    (if (or
			 (eq head-pat :any)  ; :any matches any element
			 

			 (and ; conditional bind
			  (listp head-pat)
			  (let ((pred (first head-pat)) (varname (second head-pat)))
			    ;(print `(:pred := ,pred :and :varname := ,varname))
			      (if (and
				   (functionp pred)
				   (symbolp varname))
				  ;(progn
				    ;(print `(,pred :is :a :function :and ,varname :is :a :symbol))
				    (if (funcall pred head-ls)
				      ;(progn
					;(print `(:pred :was :matched!))
					(if (is-binded bindings varname)
					  (eq (get-value bindings varname) head-ls)
					  (progn
					    (setf bindings (bind-value bindings varname head-ls))
					    ;(print `(:bindings :is :now ,bindings))
					    t)
					  )
					;)
				      NIL)
				    ;)
				  NIL))
			  )

			 (and ; bind / match with already binded
			  (symbolp head-pat)
			  (if (is-binded bindings head-pat)
			      (eq (get-value bindings head-pat) head-ls)
			      (progn
				;(print `(:unconditional :bind))
				(setf bindings (bind-value bindings head-pat head-ls))
				;(print `(:bindings :is :now ,bindings))
				t)
			      )
			  )
			 (eq head-pat head-ls)) ; normal compare with eq
			(multiple-value-bind (result new-bindings) (match-with-aux (cdr pat) (cdr ls) bindings)
			  (if result
			      (values T new-bindings)
			      (match-with-aux full-pat (cdr ls) '())
			      ))
			(match-with-aux full-pat (cdr ls) '())
			)
		    )
		  )
		;)
		 )
	       ))
      (match-with-aux full-pat ls '())))


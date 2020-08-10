;(defmacro toggle (obj)
;  `(setf ,obj (not ,obj))) ;wrong

;(define-modify-macro toggle () not)

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
	       args)))

(define-modify-macro toggle2 () not)

(defmacro allf (val &rest args)
  (let ((gval (gensym)))
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
		       args)))))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro nilf (&rest args) `(allf nil ,@args))

(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (appply #'member obj place args)
      (nconc place (list obj)))))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	    (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete ,g ,access ,@args)))
	 ,set))))

(define-modify-macro pull2 (obj &rest args)
  (lambda (seq obj &rest args)
    (apply #'delete obj seq args)))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete-if ,g ,access ,@args)))
	 ,set))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
	      ,@(mapcar #'list vars forms)
	      (,glst ,access)
	      (,(car var) (nthcdr ,gn ,glst)))
	 (prog1 (subseq ,glst 0 ,gn)
	   ,set)))))

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
			    (multiple-value-list
			     (get-setf-expansion p)))
			places))
	 (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
		    (mapcan #'(lambda (m)
				(append (first m)
					(third m)))
			    meths)
		    (mapcan #'(lambda (m)
				(append (second m)
					(list (fifth m))))
			    meths))
       ,@(mapcon #'(lambda (rest)
		     (mapcar
		      #'(lambda (arg)
			  `(unless (,op ,(car rest) ,arg)
			     (rotatef ,(car rest) ,arg)))
		      (cdr rest)))
		 temps)
       ,@(mapcar #'fourth meths))))

(defvar *cache* (make-hash-table))

(defun retrieve (key)
  (multiple-value-bind (x y) (gethash key *cache*)
    (if y
	(values x y)
	(cdr (assoc key *world*)))))

(defsetf retrieve (key) (val)
  `(setf (gethash ,key *cache*) ,val))

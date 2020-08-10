(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x) (if (consp x) (car x) x)) binds) ,@body)
    ,@(mapcar #'(lambda (x) (if (consp x) (cadr x) nil)) binds)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
	 (if ,(caar binds)
	     (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defmacro condlet (clauses &body body)
  (with-gensyms (bodfn)
    (let ((vars (mapcar #'(lambda (v) (cons v (gensym)))
			(remove-duplicates
			 (mapcar #'car
				 (mappend #'cdr clauses))))))
      `(labels ((,bodfn ,(mapcar #'car vars)
		  ,@body))
	 (cond ,@(mapcar #'(lambda (cl)
			     (condlet-clause vars cl bodfn))
			 clauses))))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
		(let ,(condlet-binds vars cl)
		  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
	      (if (consp bindform)
		  (cons (cdr (assoc (car bindform) vars))
			(cdr bindform))))
	  (cdr cl)))

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
	     ((zerop ,g) ,zero)
	     (t ,neg)))))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (x) `(eql ,insym ,x)) choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a) `',a) args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (x) `(funcall ,fnsym ,x)) choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl)) clauses)))))

(defun >casex (g cl)
  (let ((key (car cl))
	(rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
	  ((inq key t otherwise) `(t ,@rest))
	  (t (error "bad >case clause")))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
	`(prog ((,src ,source))
	    (mapc #'(lambda ,parms ,@body)
		  ,@(map0-n #'(lambda (n) `(nthcdr ,n ,src))
			    (1- (length parms))))))))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms (src rest bodfn)
	(let ((len (length parms)))
	  `(let ((,src ,source))
	     (when (nthcdr ,(1- len) ,src)
	       (labels ((,bodfn ,parms ,@body))
		 (do ((,rest ,src (cdr ,rest)))
		     ((not (nthcdr ,(1- len) ,rest))
		      ,@(mapcar #'(lambda (args)
				    `(,bodfn ,@args))
				(dt-args len rest src))
		      nil)
		   (,bodfn ,@(map1-n #'(lambda (n)
					 `(nth ,(1- n)
					       ,rest))
				     len))))))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
	      (map1-n #'(lambda (n)
			  (let ((x (+ m n)))
			    (if (>= x len)
				`(nth ,(- x len) ,src)
				`(nth ,(1- x) ,rest))))
		      len))
	  (- len 2)))

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
	`(prog nil
	    ,label
	    (if ,(car test)
		(return (progn ,@(cdr test))))
	    ,@body
	    ,@(mvdo-rebind-gen rebinds)
	    (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
	(let ((var/s (caar binds))
	      (expr (cadar binds)))
	  (if (atom var/s)
	      `(let ((,var/s ,expr)) ,rec)
	      `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
	((< (length (car rebinds)) 3)
	 (mvdo-rebind-gen (cdr rebinds)))
	(t
	 (cons (list (if (atom (caar rebinds))
			 'setq
			 'multiple-value-setq)
		     (caar rebinds)
		     (third (car rebinds)))
	       (mvdo-rebind-gen (cdr rebinds))))))

(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
	 (syms (mapcar #'(lambda (p)
			   (mapcar #'(lambda (x) (gensym))
				   (mklist (car p))))
		       pairs)))
    (labels ((rec (ps ss)
	       (if (null ps)
		   `(setq
		     ,@(mapcan #'(lambda (p s)
				   (shuffle (mklist (car p)) s))
			       pairs syms))
		   (let ((body (rec (cdr ps) (cdr ss))))
		     (let ((var/s (caar ps))
			   (expr (cadar ps)))
		       (if (consp var/s)
			   `(multiple-value-bind ,(car ss)
				,expr ,body)
			   `(let ((,@(car ss) ,expr))
			      ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  (cond ((null x) y)
	((null y) x)
	(t (list* (car x) (car y)
		  (shuffle (cdr x) (cdr y))))))

(defun group (source n)
  (if (zerop n) (error "Zero Length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
	(temps (mapcar #'(lambda (b)
			   (if (listp (car b))
			       (mapcar #'(lambda (x) (gensym))
				       (car b))
			       (gensym)))
		       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (var b)
			      (list var (cadr b)))
			  temps
			  binds))
       (prog ,(mapcar #'(lambda (b var) (list b var))
		      (mappend #'mklist (mapcar #'car binds))
		      (mappend #'mklist temps))
	  ,label
	  (if ,test
	      (return (progn ,@result)))
	  ,@body
	  (mvpsetq ,@(mapcan #'(lambda (b)
				 (if (third b)
				     (list (car b)
					   (third b))))
			     binds))
	  (go ,label)))))

(defmacro c-for (binds test rebinds &body body)
  (let ((label (gensym)))
    `(prog ,binds
	,label
	(if (not ,test)
	    (return nil))
	,@body
	(setq ,@(flatten-1 rebinds))
	(go ,label))))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun flatten-1 (lst)
  (mappend #'(lambda (x) (if (listp x) x (list x))) lst))

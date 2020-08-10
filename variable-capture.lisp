(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
	(count ,start (1+ count))
	(limit ,stop))
       ((> count limit))
     (funcall b count)))

(defmacro for2 ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(defmacro echo (&rest args)
  `'(,@args amen))

(defun foo ()
  (echo x))

(defun ntha (n lst)
  (if (= n 0)
      (car lst)
      (ntha (- n 1) (cdr lst))))

(defmacro nthb (n lst)
  `(if (= ,n 0)
       (car ,lst)
       (nthb (- ,n 1) (cdr ,lst))))

(defmacro nthc (n lst)
  `(do ((n2 ,n (1- n2))
	(lst2 ,lst (cdr lst2)))
       (((= n2 0) (car lst2)))))

(defmacro nthd (n lst)
  `(ntha ,n ,lst))

(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst)
	      (if (= n 0)
		  (car lst)
		  (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))

(defmacro ora (&rest args)
  (or-expand args))

(defun or-expand (args)
  (if (null args)
      nil
      (let ((sym (gensym)))
	`(let ((,sym ,(car args)))
	   (if ,sym ,sym ,(or-expand (cdr args)))))))

(defmacro orb (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
	`(let ((,sym ,(car args)))
	   (if ,sym ,sym (orb ,@(cdr args)))))))

(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
	(make-node :contents conts
		   :yes yes
		   :no no)))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
	   (format t "~A~%>> " (node-contents n))
	   (case (read)
	     (yes (run-node (node-yes n)))
	     (t (run-node (node-no n)))))
	  (t (node-contents n)))))

(defvar *nodes1* (make-hash-table))

(defun defnode1 (name conts &optional yes no)
  (setf (gethash name *nodes1*)
	(if yes
	    #'(lambda ()
		(format t "~A~%>> " conts)
		(case (read)
		  (yes (funcall (gethash yes *nodes1*)))
		  (t (funcall (gethash no *nodes1*)))))
	    #'(lambda () conts))))

(defnode1 'people "Is the person a man?" 'male 'female)
(defnode1 'male "Is he living?" 'liveman 'deadman)
(defnode1 'deadman "Was he American?" 'us 'them)
(defnode1 'us "Is he on a coin?" 'coin 'cidence)
(defnode1 'coin "Is the coin a penny?" 'penny 'coins)
(defnode1 'penny 'lincoln)

(defvar *nodes2* nil)

(defun defnode2 (&rest args)
  (push args *nodes2*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes2*)))
    (if (null node)
	nil
	(let ((conts (second node))
	      (yes (third node))
	      (no (fourth node)))
	  (if yes
	      (let ((yes-fn (compile-net yes))
		    (no-fn (compile-net no)))
		#'(lambda ()
		    (format t "~A~%>> " conts)
		    (funcall (if (eq (read) 'yes)
				 yes-fn
				 no-fn))))
	      #'(lambda () conts))))))

(defnode2 'people "Is the person a man?" 'male 'female)
(defnode2 'male "Is he living?" 'liveman 'deadman)
(defnode2 'deadman "Was he American?" 'us 'them)
(defnode2 'us "Is he on a coin?" 'coin 'cidence)
(defnode2 'coin "Is the coin a penny?" 'penny 'coins)
(defnode2 'penny 'lincoln)

(in-package #:tpos)

(defmacro with-infile ((&key path name type) &rest body)
  `(with-open-file (infile (make-pathname :directory ,path
					  :name ,name :type ,type))
     (progn ,@body)))

(defun knn (term context dict indicies neighbors proc? dist-fn)
  (labels ((recur (instances proc dist last heap)
	     (cond ((null instances)(loop while (not (heap-empty? heap))
				       collect (cdr (heap-pop heap))))
		   ((< (heap-size heap) neighbors)
		    (recur (cdr instances) proc (dist proc (car instances) dist-fn)
			   (car instances)(heap-push heap (cons dist last))))
		   ((< (car (heap-min heap)) dist)
		    (recur (cdr instances) proc (dist proc (car instances) dist-fn)
			   (car instances)(heap-push (heap-del heap) (cons dist last))))
		   (t (recur (cdr instances) proc (dist proc (car instances) dist-fn)
			     (car instances) heap)))))
  (let ((proc (if proc?
		  context
		  (proc-context term context dict indicies)))
	(instances (gather-instances term)))
    (avg-instances (if (< (length instances) 1) instances
		       (recur (cdr instances) proc (dist proc (car instances) dist-fn)
			      (car instances) (create-min-heap)))))))

(defun avg-instances(collection)
  (if (null collection) (list 0 0)
      (loop for instance in collection for len = (length collection)
	 summing (car instance) into N
	 summing (car (cdr instance)) into V
	 summing (car (cdr (cdr instance))) into M
	 finally (return (list (/ N len)(/ V len)(/ M len))))))

(defun gather-instances (term)
  (with-infile (:path '(:relative "train") :name term :type "txt")
    (loop for instance = (read-line infile nil) while instance
       if (pass? (subseq (read-from-string (concatenate 'string "(" instance ")")) 0 3))
       collect (read-from-string (concatenate 'string "(" instance ")")))))

(defun gather-validation (term)
  (with-infile (:path '(:relative "valid") :name term :type "txt")
    (loop for instance = (read-line infile nil) while instance
       if (pass? (subseq (read-from-string (concatenate 'string "(" instance ")")) 0 3))
       collect (read-from-string (concatenate 'string "(" instance ")")))))
  
(defun dist (inst0 inst1 fn)
  (let ((pos0 (subseq inst0 0 3))
	(prev0 (subseq inst0 3 4))
	(pred0 (subseq inst0 4 5))
	(tokens0 (remove-duplicates (subseq inst0 5)))
	(pos1 (subseq inst1 0 3))
	(prev1 (subseq inst1 3 4))
	(pred1 (subseq inst1 4 5))
	(tokens1 (remove-duplicates (subseq inst1 5))))
    (funcall fn pos0 pos1 prev0 prev1 pred0 pred1 tokens0 tokens1)))

;; distance fns
(defun d-tokens (p0 p1 pv0 pv1 pd0 pd1 t0 t1)
  (+ (if (equal pv0 pv1) 1 0)
     (if (equal pd0 pd1) 1 0)
     (loop for token in t0 if (member token t1) sum 1)))

(defun d-prior (p0 p1 pv0 pv1 pd0 pd1 t0 t1)
  (if (equal pv0 pv1) 1 0))

(defun d-prior-post (p0 p1 pv0 pv1 pd0 pd1 t0 t1)
  (+ (if (equal pv0 pv1) 1 0)
     (if (equal pd0 pd1) 1 0)))

(defun d-weighted-tokens (p0 p1 pv0 pv1 pd0 pd1 t0 t1)
  (+ (* 2 (+ (if (equal pv0 pv1) 1 0)(if (equal pd0 pd1) 1 0)))
     (loop for token in t0 if (member token t1) sum 1)))

;; aliases
(defun heap-empty? (heap)
  (cl-heap:is-empty-heap-p heap))

(defun heap-del (heap)
  (progn (cl-heap:pop-heap heap)
	 heap))

(defun heap-pop (heap)
  (cl-heap:pop-heap heap))

(defun heap-push(heap value)
  (progn (cl-heap:add-to-heap heap value)
	 heap))

(defun heap-size (heap)
  (cl-heap:heap-size heap))

(defun heap-min (heap)
  (cl-heap:peep-at-heap heap))

(defun create-min-heap()
  (make-instance 'cl-heap:fibonacci-heap :key #'first))

(defun collect(line)
  (cl-ppcre:split "\\s+" line))

;; testing
(defun validate(neighbors fn)
  (time
   (let ((pathnames (directory (make-pathname :directory '(:relative "valid") :name :wild :type :wild)))
	      (dicts (load-dict))
	      (indicies (hash-indicies))
	      (correct 0)
	      (total 0))
	  (loop for pathname in pathnames
	     if (probe-file (make-pathname :directory '(:relative "train")
					   :name (pathname-name pathname) :type "txt")) do
	       (loop for context in (gather-validation (pathname-name pathname))
		  if (easily-parsed? context) do
		    (incf correct (correct? (subseq context 0 3)
					    (select-highest (knn (pathname-name pathname)
								 context dicts indicies neighbors t
								 fn))))
		    (incf total 1))
	       (print (list correct total)))
	  (list correct total))))

(defun zeroR-instance()
  (let ((pathnames (directory (make-pathname :directory '(:relative "valid") :name :wild :type :wild)))
	(correct 0)
	(total 0))
    (loop for pathname in pathnames
       if (probe-file (make-pathname :directory '(:relative "train")
				     :name (pathname-name pathname) :type "txt")) do
	 (loop for context in (gather-validation (pathname-name pathname))
	    if (easily-parsed? context) do
	      (incf correct (correct? (subseq context 0 3)
				      (select-highest (avg-instances (gather-instances
								      (pathname-name pathname))))))
	      (incf total 1))
	 (print (list correct total)))
    (list correct total)))

(defun zeroR-avg()
  (let ((pathnames (directory (make-pathname :directory '(:relative "valid") :name :wild :type :wild)))
	(correct 0)
	(total 0)
	(avg (highest-avg)))
    (loop for pathname in pathnames
       if (probe-file (make-pathname :directory '(:relative "train")
				     :name (pathname-name pathname) :type "txt")) do
	 (loop for context in (gather-validation (pathname-name pathname))
	    if (easily-parsed? context) do
	      (incf correct (correct? (subseq context 0 3) avg))
	      (incf total 1))
	 (print (list correct total)))
    (list correct total)))

(defun easily-parsed? (inst)
  (and (= 1 (count 1 (subseq inst 0 3)))
       (= 2 (count 0 (subseq inst 0 3)))))

(defun select-highest(pos)
  (loop for i in pos with max = (reduce #'max pos)
     if (equal i max) collect 1
     else collect 0))

(defun correct? (inst pos)
  (if (equal (subseq inst 0 3) pos) 1 0))

(defun highest-avg()
  (let ((set nil))
    (loop for pathname in (directory (make-pathname :directory '(:relative "train")
						    :name :wild :type :wild))
       do (setq set (append (gather-instances (pathname-name pathname)) set)))
    (select-highest (avg-instances set))))


(defun pp()
  (loop for pathname in (directory (make-pathname :directory '(:relative "train")
						    :name :wild :type :wild))
     do (print (gather-instances (pathname-name pathname)))))

(defun pass? (list)
  (and (not (null (car list)))
       (not (null (cadr list)))
       (not (null (cadr (cdr list))))
       (= 1 (loop for x in list summing x))))

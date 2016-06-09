(in-package #:tpos)

(defparameter dict_lst
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "Q" "X" "Y"))

(defun int-pos(char)
  (cond ((char-equal #\N char)(list 1 0 0))
	 ((char-equal #\A char)(list 0 1 0))
	 ((char-equal #\V char)(list 0 0 1))
	 (t (list 0 0 0))))

(defun merge-pos (l1 l2)
  (loop for e1 in l1 for e2 in l2
     if (> e1 e2) collect e1
     else collect e2))


(defun hash-indicies()
  (let ((hash (make-hash-table :test 'equalp)))
    (with-infile (:path '(:relative "dicts") :name "2of12" :type "txt")
      (loop for line = (read-line infile nil) while line for i from 0 do
	   (with-input-from-string (stream line)
	     (setf (gethash (read stream nil nil) hash) i))))
    hash))

(defun load-dict()
  (let ((hash (make-hash-table :test 'equalp)))
    (loop for letter in dict_lst do
	 (with-infile (:path '(:relative "dicts") :name letter :type "txt")
	   (loop for line = (read-line infile nil) while line do
		(with-input-from-string (stream line)
		  (let ((token (read stream nil nil)))
		    (if (gethash token hash)
			(setf (gethash token hash)
			      (merge-pos (gethash token hash)
					 (int-pos (read-char stream))))
			(setf (gethash token hash)(int-pos (read-char stream)))))))))
    ;; Special Cases
    (setf (gethash 'a hash)(list 2 0 0))
    (setf (gethash 'the hash)(list 2 0 0))
    (setf (gethash 'he hash)(list 3 0 0))
    (setf (gethash 'she hash)(list 3 0 0))
    (setf (gethash 'it hash)(list 3 0 0))
    hash))

(defun dump-hash (hash)
  (loop for key being the hash-keys in hash using (hash-value value)
     do (format t "~a => ~a~%" key value)))

(defun create-instance (word dict indicies cv)
  (with-infile (:path '(:relative "terms") :name word :type "txt")
    (read-line infile nil)
    (loop for line = (read-line infile nil) while line do
	 (with-input-from-string (stream line)
	   (let ((data (json:decode-json stream)))
	     (loop for conv in (cons (ref :text data)(ref :replies data))
		collect (tokenize (normalize conv)) into collection
		finally (format-collection word (eval-collection word dict indicies collection) cv)))))))

(defun proc-context (word context dict indicies)
  (print (car (eval-collection word dict indicies (list (tokenize (normalize context)))))))

(defun nap(x)
  (print x)
  (equal x ""))

(defvar *count* 0)
(defun format-collection(word collection cv)
  (let ((path (if (equal 0 (random cv))
		  (make-pathname :directory '(:relative "valid") :name word :type "txt")
		  (make-pathname :directory '(:relative "train") :name word :type "txt"))))
  (with-open-file (stream path :direction :output :if-exists :append
			  :if-does-not-exist :create)
    (loop for instance in collection do
	 (setq *count* (+ *count* 1))
	 (if (or (not (equal nil (car (cdr (cdr (cdr instance) )))))
		 (not (equal nil (car (cdr (cdr (cdr (cdr instance)) ))))))
	     (format stream "~{~a ~}~&" instance))))))

(defun eval-collection(word dict indicies collection)
  (labels ((get-parts (insent outsent prev)
	     (if (equal nil insent)(return-from get-parts (list nil nil nil)))
	     (let ((first (singularize (car insent))))
	       (if (or (equal first word)(equal (car insent) word))
		   (if (cdr insent)
			      (list (append (cdr outsent) (cdr (cdr insent))) prev (car (cdr insent)))
		       (list (cdr outsent) prev nil))
		   (get-parts (cdr insent)(cons first outsent) first )))))
    (let ((lowest-pos (gethash-s word dict))
	  (tags nil))
      (loop for instance in collection do
	   (let* ((proc (tag dict indicies (get-parts (remove-if
						       (lambda(x)(equal "" x)) instance) nil nil)))
		  (npos (apply-rules lowest-pos proc)))
	     (setq tags (cons (strip-tag proc) tags))
	     (if (and (> (count 1 npos) 0 )
		      (<(count 1 npos)(count 1 lowest-pos)))(setq lowest-pos npos))))
      (loop for tag in tags for instance in collection
	 collect (append (normalize-pos lowest-pos) tag)))))


(defun normalize-pos(pos)
  (map 'list (lambda(x)(/ x (count 1 pos))) pos))

(defun strip-tag(proc)
  (cons (cadr (car (cdr proc)))
	(cons (car (cdr (car (last proc))))
	      (remove-if-not 'identity (map 'list #'cadr (car proc))))))

(defun tag(dict indicies proc)
  (loop for token in (car proc)
     collect (list (gethash-s token dict)
		   (gethash-s token indicies)) into collection finally
       (return (list collection
		     (list (gethash-s (car (cdr proc)) dict)
			   (gethash-s (car (cdr proc)) indicies))
		     (list (gethash-s (car (last proc)) dict)
			   (gethash-s (car (last proc)) indicies))))))


;; predicates
(defun noun-p(pos)(equal 1 (car pos)))
(defun verb-p(pos)(equal 1 (car (cdr pos))))
(defun mod-p(pos)(equal 1 (car (cdr (cdr pos)))))
(defun article-p(pos)(equal 2 (car pos)))
(defun pronoun-p(pos)(equal 3 (car pos)))

;; mutators
(defun -noun(pos)(cons 0 (cdr pos)))
(defun -verb(pos)(cons (car pos)(cons 0 (cdr (cdr pos)))))
(defun -mod (pos)(cons (car pos)(cons (car (cdr pos)) (list 0))))

(defparameter *rules nil) 

(defun add-parse-rule(predicate mutators)
  (setq *rules (cons (list predicate mutators) *rules)))

(add-parse-rule (lambda(prev pred)(and (mod-p prev) (verb-p pred))) ;; M W V => Not V, M
		(list #'-verb #'-mod))
(add-parse-rule (lambda(prev pred)(and (verb-p prev) (noun-p pred))) ;; V W N => Not N
		(list #'-noun))
(add-parse-rule (lambda(prev pred)(article-p prev)) ;; A W => Not V
		(list #'-verb))
(add-parse-rule (lambda(prev pred)(article-p pred)) ;; W A => Not N, M
		(list #'-noun #'-mod))
(add-parse-rule (lambda(prev pred)(pronoun-p pred)) ;; W P => Not V, M
		(list #'-verb #'-mod))
(add-parse-rule (lambda(prev pred)(verb-p pred)) ;; W V => Not M
		(list #'-mod))

(defun apply-rules(pos proc)
  (let ((tokens (car proc))
	(prev (car (car (cdr proc))))
	(pred (car (car (cdr (cdr proc))))))
    (loop for rule in *rules
       if (funcall (car rule) prev pred) do
	 (loop for mutator in (car (cdr rule)) do
	      (setq pos (funcall mutator pos)))))
  pos)

;; Utiltiies
(defun ref (sym obj)
  (cdr (assoc sym obj)))

(defun normalize (str)
  (loop for char across str while char
     if (or (char-equal char #\Space) (alpha-char-p char)) collect char into norm
     finally (return (string-downcase(coerce norm 'string)))))

(defun tokenize (str)
  (loop for token in (cl-ppcre:split "\\s+" str)
       collect token))

(defun singularize (token)
  (vana-inflector:singular-of token))

(defun gethash-s (str hash)
  (if (and str (not (equal str "s"))(not (equal str "")))
      (let ((norm-hash (gethash (read-from-string str) hash)))
	(if (or (equal norm-hash (list 0 0 0))(equal norm-hash nil))
	    (gethash (read-from-string (singularize str)) hash)
	    norm-hash))))

(defun run()
  (time 
   (let ((pathnames (directory (make-pathname :directory '(:relative "terms") :name :wild :type :wild)))
	 (dicts (load-dict))
	 (indicies (hash-indicies)))
     (loop for path in pathnames for i from 1 do
	  (format t "~A of ~A~&" i (length pathnames))
	  (create-instance (pathname-name path) dicts indicies 10))))
  (print *count*))


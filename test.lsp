;; (declaim (optimize (speed 3) (safety 3) (debug 2)))

(ql:quickload "alexandria")
(require :alexandria)
(use-package :alexandria)
(ql:quickload "iterate")
(require :iterate)
(use-package :iterate)
(ql:quickload "str")
(require :str)
(use-package :str)
(ql:quickload "split-sequence")
(require :split-sequence)
(use-package :split-sequence)
(ql:quickload "cl-csv")
(require :cl-csv)
(use-package :cl-csv)

;; (write-line "Hello World")
;; (print " this is me too")

;; (setq hi "hello")
;; (print hi)
;; (setf (lambda () (elt hi 2)) #\#)
;; (print hi)

(defun foo (x)
  (* x 10))

(defun bar (x)
  (cond
    ((eql x 5) :hi)
    ((eql x 10) :bye)
    ((eql x 15) :boo)))

(defun bax (x)
  (case (length x)
    (0 :hi)
    (1 :bye)
    (2 :boo)))

;;Custom problems (or other problems useful enough to move here, for use in solving anything else)
(defmacro check-parens-balance (input)
  (let ((test (remove-if #'(lambda (c) (not (member c '(#\( #\) #\[ #\] #\{ #\})))) (format nil "~s" input))))
    (let ((stack ()))
      (loop for c across test
	    do (cond
		 ((string= c "(") (setq stack (cons "(" stack)))
		 ((string= c "[") (setq stack (cons "[" stack)))
		 ((string= c "{") (setq stack (cons "{" stack)))
		 ((string= c ")") (if (string= (car stack) "(")
				      (setq stack (cdr stack))
				      (return nil)))
		 ((string= c "]") (if (string= (car stack) "[")
				      (setq stack (cdr stack))
				      (return nil)))
		 ((string= c "}") (if (string= (car stack) "{")
				      (setq stack (cdr stack))
				      (return nil)))))
      (not stack))))

;;Pretty useless right now; I can't send its outputs to another function or macro as inputs (or is it only to macros that it doesn't work...)
(defmacro quote-values (&rest inputs)
  `(values-list (mapcar (lambda (n) `(quote ,n)) ',inputs)))

(defmacro funcquote-values (&rest inputs)
  `(values-list (mapcar (lambda (n) `(function ,n)) ',inputs)))


;;A likely-incomplete representation of "rec" from Paul Graham's Arc dialect of LISP, based purely on a passing mention of the function/macro/however-he-implemented-it in http://www.paulgraham.com/power.html
;;NOTE: I still have to look at the actual implementation to see if there are any other useful functions I'm missing
;;NOTE: How to make this actually return the function, instead of just applying it to the "start" input arg? If it could do that it would be really amazing, since e.g. you could use it to make input arguments for other functions. But it seems pretty hard to do with lambdas; I haven't figured out how to insert code that preserves the recursivity without just making this into a block a-la-progn or somehow making it a function; which probably just means recursing the whole thing, inelegant as that is.

;;NOTE 8/31/2021: Switched this from a macro to a function. Added an eval and required that input functions be quoted (e.g. "#'*")

(defun rec (iterator ending combinator end &optional (rec-end :rec-end) (eval-end T))
  (let* ((recfuncname (gensym))
	 (iterateval (gensym))
	 (last-output (if (equal end rec-end)
			  iterateval
			  (subst iterateval rec-end end))))
    (eval `(labels ((,recfuncname (,iterateval)
		      (if (funcall ,ending ,iterateval)
			  ,(if eval-end
			       last-output
			       `(quote ,last-output))
			  ,(if (not (null combinator))
			       `(funcall ,combinator
					 ,iterateval
					 (,recfuncname (funcall ,iterator ,iterateval)))
			       (when (not (null iterator))
				 `(,recfuncname (funcall ,iterator ,iterateval)))))))
	     #',recfuncname))))

;;Make/find a macro for (possibly multi-variate) dynamic programming?

;;OLD (not working, inadequate) VERSION
;; (defmacro rec (iterator ending &optional (func nil func-supplied-p) &key (end nil end-supplied-p) (start nil start-supplied-p))
;;   (let* ((recfuncname (gensym))
;; 	 (iterateval (gensym))
;; 	 (combinator (if (not func-supplied-p)
;; 			 `(lambda (x y) (append (list x) (list y)))
;; 			 func)))
;;     `(labels ((,recfuncname (,iterateval)
;; 		(if (,ending ,iterateval)
;; 		    ,(if (not end-supplied-p)
;; 			 iterateval
;; 			 end)
;; 		    (,combinator
;; 		     ,iterateval
;; 		     (,recfuncname (,iterator ,iterateval))))))
;;        ,(if start-supplied-p
;; 	    `(,recfuncname ,start)
;; 	    recfuncname))))

;;NOTE: How to let the user decide what to replace with arg-name?
(defmacro singleinput (placeholder func &rest args)
  (let* ((arg-name (gensym))
	 (func-input (cons func (subst arg-name placeholder args))))
    `(lambda (,arg-name)
       ,func-input)))

(defmacro doubleinput (placeholder1 placeholder2 func &rest args)
  (let* ((arg-name1 (gensym))
	 (arg-name2 (gensym))
	 (func-input (cons func (subst arg-name1 placeholder1 (subst arg-name2 placeholder2 args)))))
    `(lambda (,arg-name1 ,arg-name2)
       ,func-input)))

(defmacro insert-if (predicate code)
  (when predicate
    code))

(defun set-nth (n val lst)
  (append (subseq lst 0 n) (list val) (nthcdr (1+ n) lst)))

;;Doesn't work yet
;; (labels ((flip (lst)
;; 	   (append
;; 	    (subseq lst 0 (floor (/ 2.0 (length lst))))
;; 	    (nreverse
;; 	     (subseq lst
;; 		     (ceiling (/ 2.0 (length lst))))))))
;;   (loop for i from 0 to (ceiling (/ 2.0 10))
;; 	do (print
;; 	    (flip (set-nth
;; 		   (make-list 10 :initial-element "")
;; 		   i
;; 		   "X")))))

;;Two ways to do [almost] the same thing:
(loop for i from 0 to 10
      do (print (set-nth i "X" (set-nth (- 10 i) "X" (make-list 11 :initial-element " ")))))

(labels ((cross-loop (i top)
	   (let ((line (set-nth i "X" (set-nth (- top i) "X" (make-list (1+ top) :initial-element " ")))))
	     (mapcar (singleinput str format t str) line)
	     (terpri))
	   (unless (= i top)
	     (cross-loop (1+ i) top))))
  (cross-loop 0 10))


;;An example of macros, taken from some online site (I think a StackExchange question)
;; (defmacro create-funtest2 ()
;;   (let ((input-list (gensym)))
;;     `(labels ((fun-created ,input-list
;;                 (reduce #'+ (list ,@input-list))))
;;        (list #'fun-created (quote ,input-list)))))

;;DOESN'T WORK YET!!!!!
;; (defmacro dual-loop-append (first-setting second-setting condition &body body)
;;   `(let ((final nil))
;;      (iter (for a from ,(elt first-setting 0) to ,(elt first-setting 1))
;;        (iter (for b from ,(elt second-setting 0) to ,(elt second-setting 1))
;; 	 (when ,(cond (member 'member condition) `(member ,(elt condition 1) final)
;; 		      (and (member 'not condition) (member 'member (elt condition 1))) `(not (member ,(elt condition 1) final))
;; 		      t @,condition)
;; 	   ,@body)))))

;; (declaim (ftype (function (integer) cons) prime-factorize))
(defun prime-factorize (num)
  (if (< num 2) nil
      (let ((i 2) (factor-list ()))
	(declare (type integer i) (type list factor-list))
	(loop while (<= i (ceiling num i)) do
	  (if (zerop (rem num i))
	      (progn (setq num (/ num i))
		     (setq factor-list (cons i factor-list)))
	      (setq i (1+ i))))
	(setq factor-list (cons num factor-list))
	factor-list)))
(defun hashtable-prime-factorize (num)
  (let ((primes (prime-factorize num)) (realprimes (make-hash-table)))
    (dolist (i primes realprimes)
      (setf (gethash i realprimes) (if (gethash i realprimes)
				       (1+ (gethash i realprimes))
				       1)))
    realprimes))

;;NOTE: includes num itself in the output.
(declaim (ftype (function (integer) cons) list-divisors))
(defun list-divisors (num)
  (if (= num 1)
      '(1)
      (let ((primes (hashtable-prime-factorize num)))
	(labels ((divisor-search (dimensions)
		   (apply
		    #'append
		    (if dimensions
			(iter
			  (for i from 0 to (cdr (car dimensions)))
			  (collect
			      (append
			       ;; (list (car (car dimensions)))
			       (mapcar #'(lambda (n) (* (expt (car (car dimensions)) i) n))
				       (divisor-search (cdr dimensions))))))
			'((1))))))
	  (divisor-search (hash-table-alist primes))))))


;;NOTE: Add ability to determine whether you want to include the top (and bottom?) edges
(declaim (ftype (function (integer &optional integer &key (:step integer) (:incl boolean)) cons) range))
(defun range (a &optional (b 0 b-provided-p) &key (step nil) (incl nil))
  (let* ((ends (if b-provided-p (list a b) (list b a)))
	 (unit (if (or (null step) (= step 0))
		   (if (<= (car ends) (cadr ends)) 1 -1)
		   step)))
    (when incl
      (if (< unit 0)
	  (setf (cadr ends) (1- (cadr ends)))
	  (setf (cadr ends) (1+ (cadr ends)))))
    (if (>= unit 0)
	(loop for n from (car ends) below (car (cdr ends)) by unit
	      collect n)
	(loop for n downfrom (car ends) above (car (cdr ends)) by (- unit)
	      collect n))))


(declaim (ftype (function (number) number) log10))
(defun log10 (n)
  (/ (log n) (log 10)))

(defun multiply (a &rest b)
  (cond
    ((numberp a) (apply #'* (cons a b)))
    ((listp a) (apply #'mapcar (cons #'* (cons a b))))))


;;; Functions in this section are from On Lisp
(defun last1 (lst)
  (car (last lst)))
(defun single (lst)
  (and (consp lst) (not (cdr lst))))
(defun append1 (lst obj)
  (append lst (list obj)))
(defun conc1 (lst obj)
  (nconc lst (list obj)))
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
	     (and (consp x)
		  (or (null y)
		      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
	(> (length x) (length y)))))
;;Rename this to something more appropriate? "filter" makes me think of remove-if-not...
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((recurse (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (recurse rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (recurse source nil) nil)))
(defun remove-if-tree (test tree)
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse acc))
		   ((consp (car tree))
		    (rec (cdr tree)
			 (cons (rec (car tree) nil) acc)))
		   (t (rec (cdr tree)
			   (if (funcall test (car tree))
			       acc
			       (cons (car tree) acc)))))))
    (rec tree nil)))
;;Mixture of find-if and some
;;How to make a type specifier for this?
;;Also, do I need the "or cons list"?
;; (declaim (ftype (function (or cons list)) (t t)) find-return-if)
(defun find-return-if (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
	(if val
	    (values (car lst) val)
	    (find-return-if fn (cdr lst))))))
(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))
(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
	  :test test))
(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src) (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))
;;Highest score element of lst, fn used to get scores
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setq wins obj
		    max score))))
	(values wins max))))
;;Lists all eleemnts which have the highest score, rather than just one element. n is because of nreverse used on internal list of best elements; not truly destructive
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
	    (max (funcall fn (car lst))))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (cond ((> score max)
		   (setq max score
			 result (list obj)))
		  ((= score max)
		   (push obj result)))))
	(values (nreverse result) max))))
;;Element of lst that 'beats' the others according to fn (fn MUST HAVE A TOPOLOGICAL SORTING)
(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
	(dolist (obj (cdr lst))
	  (if (funcall fn obj wins)
	      (setq wins obj)))
	wins)))
;;mapcar on (range 0 n :incl t)
(defun map0-n (fn n)
  (mapa-b fn 0 n))
;;mapcar on (range 1 n :incl t)
(defun map1-n (fn n)
  (mapa-b fn 1 n))
;;mapcar on (range a b :incl t :step step)
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))
;;mapcar fn on the sequence starting from (funcall succ-fn start) and progressing by succ-fn until test-fn is non-nil (INLCUSIVE of this last element)
(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

;;Already implemented in ALEXANDRIA package
;; (defun mappend (fn &rest lsts)
;;   (apply #'append (apply #'mapcar fn lsts)))

;;Instead of mapcar on multiple lists concurrently input to fn, mapcar fn on all the lists separately and then put all the inputs in a single output list (append (mapcar fn list1) (mapcar fn list2)...)
(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result)))
    (nreverse result)))
;;recursive mapcar, applies when it reaches a level where at least one argument is an atom. Output arguments are in same format as tree traversal (e.g. (rmapcar #'+ '(1 (2 3 4) 5 6) '(15 (16 17 18) 19 20)) is (16 (18 20 22) 24 26))
(defun rmapcar (fn &rest args)
  (if (some #' atom args)
      (apply fn args)
      (apply #'mapcar
	     #'(lambda (&rest args)
		 (apply #'rmapcar fn args))
	     args)))

;;I/O utilities from On Lisp
;;reads a list from the input arguments (just input the elements, space-separated)
;;HOW DOES THIS WORK?
(defun readlist (&rest args)
  (values (read-from-string
	   (concatenate 'string "("
			(apply #'read-line args)
			")"))))
;;Creates a prompt using 'format' on the arguments (no need for the stream arg, though), and uses it to convert a line of console input into LISP with (read *query-io*)
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))
;;Uses prompt (on args), and format to create an emulated shell which for every line applies fn to the LISP input gotten from prompt
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (print args)
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
	  (return)
	  (format *query-io* "~A~%" (funcall fn in))))))


;;From the Common Lisp Handbook. Note also that there is a Quicklisp library for complex string manipulations, i.e. regex replacement
;; (defun replace-all (string part replacement &key (test #'char=))
;;   "Returns a new string in which all the occurences of the part 
;; is replaced with replacement."
;;   (with-output-to-string (out)
;; 			 (loop with part-length = (length part)
;; 			       for old-pos = 0 then (+ pos part-length)
;; 			       for pos = (search part string
;; 						 :start2 old-pos
;; 						 :test test)
;; 			       do (write-string string out
;; 						:start old-pos
;; 						:end (or pos (length string)))
;; 			       when pos do (write-string replacement out)
;; 			       while pos)))

;;Project Euler Problems

(declaim (ftype (function (integer &rest integer) integer) sum-of-multiples))
(defun sum-of-multiples (top &rest inputs)
  (let ((factors (sort inputs #'>)) (total 0) (s 0))
    (loop while (not (eq factors ()))
	  do (progn
	       (setq s (pop factors))
	       (print "Factor: ")
	       (prin1 s)
	       (when (> s 0)
		 (loop for x from 1 to (- (ceiling top s) 1)
		       do (when
			      (every #'(lambda (num) (/= 0 (mod x num))) factors)
			    (progn
			      (print "x is")
			      (prin1 x)
			      (setq total (+ total (* s x)))
			      (print (* s x))))))
	       (print "Sum is: ")
	       (prin1 total)))
    total))

(declaim (ftype (function (integer integer integer integer) boolean) sum-of-even-fibonacci))
(defun sum-of-even-fibonacci (a b sum top)
  (setq b (+ a (setq a b)))
  (print "sum is ")
  (prin1 sum)
  (unless (> b top)
    (progn
      (when (= 0 (mod b 2))
	(setf sum (+ sum (print b))))
      (sum-of-even-fibonacci a b sum top))))

;;Is the input a palindrome
(declaim (ftype (function (t) boolean) palindrome))
(defun palindrome (input)
  (let ((str ""))
    (declare (type string str))
    (if (not (stringp input))
	(setq str (write-to-string input))
	(setq str (string input)))
    (if (eq (length str) 0)
	T
	;; (let ((string_symmetry_len (ceiling (length string) 2)))
	;;   (string=
	;;    (subseq string 0 string_symmetry_len)
	;;    (reverse (subseq string (- (length string) string_symmetry_len)))))
	(string= str (reverse str)))))

;;largest palindromic product of two natural numbers with "digits" digits
(declaim (ftype (function (integer) integer)))
(defun largest-palindrome-product (digits)
  (if (< digits 1)
      (progn
	(print "Give a valid input")
	-1)
      (progn
	(let
	    ((answer
	       (loop for i
		     downfrom
			(1-
			 (expt 10
			       (+ 2 (* 2 (1- digits)))))
		       to (expt 10 (* 2 (1- digits)))
		     by 1
		     when (palindrome i)
		       do
			  (let
			      ((output
				 (loop for
				       j
				       downfrom (1- (expt 10 digits))
					 to (expt 10 (1- digits))
				       by 1
				       when (and (= (rem i j) 0)
						 (<= (expt 10 (1- digits)) (/ i j) (1- (expt 10 digits))))
					 do (return (cons 'T `(,i)))
				       finally (return (cons 'nil `(,-1))))))
			    (when (car output)
			      (return output))))))
	  (if (car answer)
	      (cadr answer)
	      0)))))


;; compare square of sum and sum of squares 
;; (let ((squaresum 0) (sum 0))
;;   (loop for i from 1 to 100
;; 	do (progn
;; 	     (setf squaresum (+ squaresum (expt i 2)))
;; 	     (setf sum (+ sum i))))
;;   (setf sum (expt sum 2))
;;   (print (- squaresum sum)))

;; n-th prime number
(declaim (ftype (function (integer) integer) get-nth-prime))
(defun get-nth-prime (n)
  (let ((count 0) (i 1))
    (declare (type integer count) (type integer i))
    (loop while (< count n)
	  do (progn
	       (setf i (1+ i))
	       (when (= i (car (prime-factorize i)))
		 (setf count (1+ count)))))
    i))

;; sum of primes below a number
;; (let ((sum 0) (i 1))
;;   (loop while (< i 2000000)
;; 	do (progn
;; 	     (setf i (1+ i))
;; 	     (when (equal i (car (prime-factorize i)))
;; 	       (setf sum (+ sum i)))))
;;   sum)


;; Search this number for the 13 digits with the highest product(incomplete)
(defun product-of-13-elements ()
  (let ((temp (coerce
	       (mapcar #'digit-char-p (coerce "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450" 'cons))
	       'sequence))
	(temp2 (list)))
    (dotimes (i (- (length temp) 13))
      (setf temp2 (push (apply '* (subseq temp i (+ i 13))) temp2)))
    (apply #'max temp2)))

;;; Pythagorean triple summing to 1000.
;; (let ((c 0) (c2 0))
;;   (loop for a from 1 to 1000
;; 	append (loop for b from a to 1000
;; 		     when (and (equal (+ a b (setf c (round (sqrt (setf c2 (+ (expt a 2) (expt b 2))))))) 1000)
;; 			       (equal
;; 				(expt
;; 				 c
;; 				 2)
;; 				c2))
;; 		     do (return `(,a ,b ,c)))))

(defun triangle-number-divisor-test (target)
  (let ((total 1) (i 2)
	(number-of-divisors (lambda (n)
			      (let ((primes (prime-factorize n)) (realprimes (make-hash-table)) (number 1))
				(dolist (i primes realprimes)
				  (setf (gethash i realprimes) (if (gethash i realprimes)
								   (1+ (gethash i realprimes))
								   1)))
				(maphash #'(lambda (k v)
					     (setf number (* number (1+ v))))
					 realprimes)
				number))))
    (loop while (<= (funcall number-of-divisors total) target)
	  do (progn (setf total (+ total i))
		    (setf i (1+ i))))
    (print total)))

(defun last-digits-of-sum (digits separator numlist)
  (let ((nums (mapcar #'parse-integer (split-sequence:SPLIT-SEQUENCE separator numlist)))
	(sum 0)
	(power (expt 10 digits)))
    (dolist (i nums sum)
      (setq sum (mod (+ sum i) power)))))
(defun first-digits-of-sum (digits separator numlist)
  (let ((nums (mapcar #'parse-integer (split-sequence:SPLIT-SEQUENCE separator numlist)))
	(sum 0))
    (dolist (i nums sum)
      (setq sum (+ sum i)))
    (parse-integer (subseq (write-to-string sum) 0 digits))))

(defun longest-collatz-sequence (&optional (minstart 1) (maxstart 1000000))
  (let ((collatz (lambda (in) (let ((n in) (i 0))
			   (loop while (> n 1)
				 do (progn (setq i (1+ i))
					   (setq n (if (= (mod n 2) 0)
						       (/ n 2)
						       (+ (* 3 n) 1)))))
			   i))))
    (iter (for j from minstart to maxstart)
      (finding j maximizing (funcall collatz j)))))

(defun sum-of-digits (n)
  (iter (for num in (mapcar #'digit-char-p (coerce (write-to-string n) 'list)))
    (sum num)))

;; How many total characters in the word forms of the first 1000 numbers? An 'and' is added after every 'hundred'
(defun sum-of-digit-names (&optional (add-and t))
  (iter (for i from 1 to 1000)
    (sum (let ((name (str:replace-all "-" "" (str:replace-all " " "" (format nil "~r" i)))))
	   (+ (length name)
	      (if (and add-and (str:containsp "hundred" name))
		  (if (not (> (mod i 100) 0))
		      0
		      3)
		  0))))))

;;Largest sum moving through a 2-D pyramid of numbers
(defun largest-pyramid-sum (pyramidstring)
  (let ((pyramid (mapcar #'(lambda (x) (mapcar #'parse-integer (split-sequence #\Space x))) (split-sequence #\Newline pyramidstring))))
    (labels ((maximize-sum (row col)
	       (if (= row (1- (length pyramid)))
		   (elt (elt pyramid row) col)
		   (+ (elt (elt pyramid row) col) (max (maximize-sum (1+ row) col) (maximize-sum (1+ row) (1+ col)))))))
      (maximize-sum 0 0))))

;;Number of sundays that are also the first day of a month
(defun number-of-sundays ()
  (labels ((monthdays (month year)
	     (elt
	      (list 31 (if (or (and (= (mod year 100) 0) (= (mod year 400) 0)) (and (not (= (mod year 100) 0)) (= (mod year 4) 0))) 28 29) 31 30 31 30 31 31 30 31 30 31 30 31)
	      month)))
    (let ((day 0))
      (iter yearloop
	(for year from 1901 to 2000)
	(iter monthloop
	  (for month from 1 to 12)
	  (in yearloop
	      (sum
	       (prog1
		   (if (= day 0) 1 0)
		 (setq day
		       (mod (+ day (monthdays month year)) 7))))))))))

;;Sum of 'amicable numbers' (numbers such that the sum of the proper divisors of each is equal to the value of the other) under 1000
(defun sum-amicable-numbers (top)
  (let ((amicable-hash
	  (make-hash-table)))
    (iter
      (for i from 1 below top)
      (setf (gethash i amicable-hash)
	    (apply #'+ (remove i (list-divisors i)))))
    (iter (for (k v) in-hashtable amicable-hash)
      (sum (if (and (not (= k v)) (gethash v amicable-hash) (= (gethash v amicable-hash) k))
	       k
	       0)))))

;;Sort words in a text file in alphabetical order, then get the sum of their 'alphabetical positions' (i.e. #\A=1, #\B=2, etc) and 
(defun alphabetical-position-sum (&optional (filename))
  (let* ((words (cl-csv:read-csv
		 (pathname (if filename filename "c:/Users/swapn/Downloads/Project_Euler_p022_names.txt"))))
	 (scores
	   (mapcar
	    #'(lambda (seq)
		(apply
		 #'+
		 (mapcar
		  #'(lambda (n) (- (char-code n) (1- (char-code #\A)))) 
		  (coerce seq 'list))))
	    (sort (elt words 0) #'string-lessp))))
    (print (iter (for i from 0 below (length scores))
	     (sum (* (1+ i) (elt scores i)))))))

;;Numbers that cannot be summed into by 2 abundant numbers (perfect num = sum of its divisors, abundant num < sum of its divisors, deficient num > sum of its divisors)
(defun abundant-non-sums ()
  (let ((abundants nil))
    (declare (type list abundants))
    (iter (for i from 1 to 28143)
      (sum
       (progn
	 (when (> (apply #'+ (remove i (list-divisors i))) i)
	   (setq abundants (cons i abundants)))
	 (when (= 0 (mod i 1000)) (print (concatenate 'string (write-to-string i) "s")))
	 (if
	  (or (and
	       (zerop (rem i 2))
	       (member (/ i 2) abundants))
	      (iter (for subset on abundants)
		(let ((a (car subset)))
		  (thereis (iter (for b in (reverse subset))
			     (thereis
			      (let ((cursum (+ a b)))
				(when (> cursum i) (leave))
				(if (= i cursum) i nil))))))))
	  0 i))))))



(defun without-elt (n target)
  (append (subseq target 0 n) (subseq target (1+ n))))

;; (defun permutations (inlist)
;;   (if (null (cdr inlist))
;;       (list inlist)
;;       (let ((combined nil))
;; 	(iter
;; 	  (for i in inlist)
;; 	  (prog1
;; 	      (setq combined (append
;; 			      (mapcar
;; 			       #'(lambda (elem) (cons (first inlist) elem))
;; 			       (permutations (rest inlist)))
;; 			      combined))
;; 	    (setq inlist (append (rest inlist) (list (first inlist))))))
;; 	combined)))

;;Outputs permutations in lexicographic ordered
(defun permutations (inlist)
  (if (null (cdr inlist))
      (list inlist)
      (let ((combined nil))
	(iter
	  (for i from (1- (length inlist)) downto 0)
	  (setq combined (append
			  (mapcar
			   (singleinput elem cons (elt inlist i) elem)
			   (permutations (without-elt i inlist)))
			  combined)))
	combined)))

;; (let ((seq (permutations '(0 1 2))))
;;   (iter (for i from (1- (length (elt seq 1))) downto 0)
;;     (setq seq (sort seq #'(lambda (x y) (< (elt x i) (elt y i))))))
;;   seq)

;;Outputs the nth permutation in lexicographic order (n starting from 0)
(defun nthpermutation (n input)
  (let ((sum n) (inlist input))
    (iter (for f from (1- (length inlist)) downto 0)
      (collect (iter (for i from 0 below (length inlist))
		 (thereis (if (null (cdr inlist))
			      (car inlist)
			      (when (> (factorial f) (- sum (* (factorial f) i)))
				(prog1
				    (elt inlist i)
				  ;; (print "success")
				  ;; (print f)
				  ;; (print i)
				  (setq sum (- sum (* (factorial f) i)))
				  (setq inlist (without-elt i inlist)))))))))))
;; ((let ((sum 1000000) (digitsleft '(0 1 2 3 4 5 6 7 8 9)))
;;    (iter (for i from 9 downto 0)
;;      (collect (let ((step (factorial i)))
;; 		(iter (for j upfrom 0)
;; 		  (thereis
;; 		   (if (> (* (+ 2 j) step) sum)
;; 		       (progn
;; 			 (setq sum (- (print sum) (* (print (1+ j)) step)))
;; 			 (prog1
;; 			     (print (elt digitsleft j))
;; 			   (setq digitsleft
;; 				 (append
;; 				  (subseq digitsleft 0 j)
;; 				  (subseq digitsleft (1+ j))))))
;; 		       nil))))))))

(defun fibonacci-above-max (top a b idx)
  (if (> b top)
      idx
      (fibonacci-above-max top b (+ a b) (1+ idx))))

(defun longest-repeating-decimal (top-denominator)
  (labels ((repeating-decimal-length (fraction)
	     (let ((temp (numerator fraction)) (tempdem (denominator fraction)) (remainderlist (list nil)))
	       (iter (until (member (rem temp tempdem) remainderlist))
		 (progn
		   (setq remainderlist (cons (rem temp tempdem) remainderlist))
		   (setq temp (* 10 (rem temp tempdem)))))
	       (if (zerop temp)
		   0
		   (1+ (position (rem temp tempdem) remainderlist))))))
    (iter (for i from 1 to top-denominator)
      (finding i maximizing (repeating-decimal-length (/ i))))))

;;find the linear coefficient and constant term for the quadratic function that produces the most consecutive primes, with the linear coefficient bounded by (-top, top) and the constant term bounded by [-top, top]
(defun most-consecutive-primes (top)
  (let ((inner-output (list (- top) (- top) (- top))))
    (iter (for a from (- 1 top) below top)
      (iter
	(for b from (- top) to top)
	(let ((output
		(iter (for n upfrom 0)
		  (while (let ((prime (+ (expt n 2) (* a n) b)))
			   (and
			    (> prime 0)
			    (null (remove prime (prime-factorize prime))))))
		  (count 1))))
	  (when (> output (car inner-output))
	    (setq inner-output (list output a b))))))
    (print (car inner-output))
    (cdr inner-output)))

;;See Project Euler problem 28
(defun sum-of-diagonals (topwidth)
  (let ((curr 1))
    (1+ (iter (for i upfrom 2 by 2)
	  (until (>= curr (expt topwidth 2)))
	  (sum (iter (for j from 1 to 4)
		 (until (> curr (expt topwidth 2)))
		 (sum (setq curr (+ curr i)))))))))

;;How many distinct terms in the set of values of a^b for a and b both in the interval [2,100]
(defun distinct-series-exponents (low high)
  (length
   (let ((distincts nil))
     (declare (type list distincts) (type number low high))
     (iter (for a from low to high)
       (iter (for b from low to high)
	 (let ((product (expt a b)))
	   (when (not (member product distincts)) (setq distincts (cons product distincts))))))
     distincts)))


;; (defun sum-combinations (top steps)
;;   (let ((unreachables ()))
;;     (labels ((ways-to-reach (top steps)
;; 	       (if (member top unreachables)
;; 		   0
;; 		   (if (< top 0) (prog1 0
;; 				   (setq unreachables (cons top unreachables)))
;; 		       (if (zerop top) 1
;; 			   (if (null steps)
;; 			       0
;; 			       (let
;; 				   ((cursum
;; 				      (+ (ways-to-reach (- top (car steps)) steps)
;; 					 (ways-to-reach top (cdr steps)))))
;; 				 ;; (when (zerop cursum) (setq unreachables (cons top unreachables)))
;; 				 cursum)))))))
;;       (prog1
;; 	  (ways-to-reach top steps)
;; 	(print unreachables)
;; 	(print (member 2 unreachables))))))

;; (defun sum-combinations (top steps)
;;   (labels ((ways-to-reach (top steps)
;; 	     (if (< top 0) 0
;; 		 (if (zerop top) 1
;; 		     (if (null steps)
;; 			 0
;; 			 (+ (ways-to-reach (- top (car steps)) steps)
;; 			    (ways-to-reach top (cdr steps))))))))
;;     (ways-to-reach top steps)))

(defun sum-combinations (top steps)
  (let ((unreachable (make-hash-table)))
    (labels ((ways-to-reach (top steps)
	       (if (gethash top unreachable) 0
		   (if (< top 0) (setf (gethash top unreachable) 0)
		       (if (zerop top) 1
			   (if (null steps)
			       0
			       (+ (ways-to-reach (- top (car steps)) steps)
				  (ways-to-reach top (cdr steps)))))))))
      (ways-to-reach top steps))))

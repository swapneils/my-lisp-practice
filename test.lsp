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



;;NOTE: How to let the user decide what to replace with arg-name?
(defmacro singlein (placeholder func &rest more)
  (let* ((arg-name (gensym))
	 (func-input (subst arg-name placeholder (cons func more))))
    (nconc `(lambda (,arg-name)) func-input)))

(defmacro doublein (placeholder1 placeholder2 func &rest args)
  (let* ((arg-name1 (gensym))
	 (arg-name2 (gensym))
	 (func-input (subst arg-name1 placeholder1 (subst arg-name2 placeholder2 (cons func args)))))
    (nconc `(lambda (,arg-name1 ,arg-name2)) func-input)))


(defun repeat (n code)
  (iter (for i from 0 below n)
    (eval code)))


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
(defun draw-Xs ()
  (loop for i from 0 to 10
	do (print (set-nth i "X" (set-nth (- 10 i) "X" (make-list 11 :initial-element " ")))))

  (terpri)
  
  (labels ((cross-loop (i top)
	     (let ((line (set-nth i "X" (set-nth (- top i) "X" (make-list (1+ top) :initial-element " ")))))
	       (mapcar (singlein str (format t str)) line)
	       (terpri))
	     (unless (= i top)
	       (cross-loop (1+ i) top))))
    (terpri)
    (cross-loop 0 20)))


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


(defun square (num)
  "Returns the square of num"
  (* num num))

;; (declaim (ftype (function (integer) cons) prime-factorize))
(declaim (ftype (function (integer) list) prime-factorize))
(defun prime-factorize (num)
  (if (< num 2) nil
      (let ((i 2) (factor-list ()))
	(declare (type integer i) (type list factor-list))
	(loop while (<= i (floor (sqrt num))) do
	  (if (zerop (rem num i))
	      (progn (setq num (/ num i))
		     (setq factor-list (cons i factor-list)))
	      (setq i (1+ i))))
	(setq factor-list (cons num factor-list))
	factor-list)))

(declaim (ftype (function (integer) boolean) primep))
(defun primep (num)
  (cond
    ((= 2 num) t)
    ((or (<= num 1) (and (> num 2) (evenp num))) nil)
    (t (iter (for i from 3 to (sqrt num))
	 (never (zerop (rem num i)))))))

(declaim (ftype (function (list) hash-table) frequency-hashtable))
(defun frequency-hashtable (input)
  (let ((output (make-hash-table)))
    (dolist (i input output)
      (setf (gethash i output) (if (gethash i output)
				   (1+ (gethash i output))
				   1)))))
(declaim (ftype (function (cons) list) frequency-alist))
(defun frequency-alist (input)
  "An alist with frequency values for every element in the input list"
  (hash-table-alist (frequency-hashtable input)))

;;NOTE: includes num itself in the output.
(declaim (ftype (function (integer) cons) list-divisors))
(defun list-divisors (num)
  (if (= num 1)
      '(1)
      (let ((primes (frequency-alist (prime-factorize num))))
	(labels ((divisor-search (dimensions)
		   (apply
		    #'append
		    (if dimensions
			(iter
			  (for i from 0 to (cdr (car dimensions)))
			  (collect
			      (mapcar #'(lambda (n) (* (expt (car (car dimensions)) i) n))
				      (divisor-search (cdr dimensions)))))
			'((1))))))
	  (divisor-search primes)))))


(declaim (ftype (function (integer &optional integer integer atom) list) range))
(defun range (a &optional (b 0 b-provided-p) (step nil) (incl :auto))
  (if (and (zerop a) (zerop b)) ()
      (let* ((ends (if b-provided-p (list a b) (list b a)))
	     (unit (if (or (null step) (= step 0))
		       (if (<= (car ends) (cadr ends)) 1 -1)
		       step)))
	(when (if (equal incl :auto) (if b-provided-p t nil) incl)
	  (if (< unit 0)
	      (setf (cadr ends) (1- (cadr ends)))
	      (setf (cadr ends) (1+ (cadr ends)))))
	(if (>= unit 0)
	    (loop for n from (car ends) below (car (cdr ends)) by unit
		  collect n)
	    (loop for n downfrom (car ends) above (car (cdr ends)) by (- unit)
		  collect n)))))


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
(defun mkinteger (obj)
  (cond ((numberp obj) (round obj))
	((listp obj) (mkinteger (car obj)))
	((characterp obj) (digit-char-p obj))
	((stringp obj) (parse-integer obj))
	(t nil)))

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
(defun mod-if (predicate fn obj)
  (if predicate
      (funcall fn obj)
      obj))
(defun mod-if-applies (predicate fn obj)
  (if (funcall predicate obj)
      (funcall fn obj)
      obj))
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

;;Copied from http://www.lee-mac.com/baseconversion.html#base2base
(declaim (ftype (function (integer integer) string) dec->stringbase))
(defun dec->stringbase ( n b )
  (if (< n b)
      (if (< n 10) (write-to-string n) (string (code-char (+ 55 n))))
      (str:concat (dec->stringbase (floor n b) b) (dec->stringbase (rem n b) b))))

;;Mine again
;;A likely-incomplete representation of "rec" from Paul Graham's Arc dialect of LISP, based purely on a passing mention of the function/macro/however-he-implemented-it in http://www.paulgraham.com/power.html
;;NOTE: I still have to look at the actual implementation to see if there are any other useful functions I'm missing
;;NOTE: How to make this actually return the function, instead of just applying it to the "start" input arg? If it could do that it would be really amazing, since e.g. you could use it to make input arguments for other functions. But it seems pretty hard to do with lambdas; I haven't figured out how to insert code that preserves the recursivity without just making this into a block a-la-progn or somehow making it a function; which probably just means recursing the whole thing, inelegant as that is.

;;NOTE 8/31/2021: Switched this from a macro to a function. Added an eval and required that input functions be quoted (e.g. "#'*")

(defun rec (iterator ending combinator end &optional (start nil) (rec-end :rec-end) (eval-end T))
  (let* ((recfuncname (gensym))
	 (iterateval (gensym))
	 (last-output (if (equal end rec-end)
			  iterateval
			  (subst iterateval rec-end end))))
    (mod-if (not (null start))
	    (singlein fun (funcall fun start))
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
		     #',recfuncname)))))

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

(declaim (ftype (function (integer) integer) sum-of-even-fibonacci))
(defun sum-of-even-fibonacci (top)
  (labels ((get-sum (a b sum top)
	     (if (> (+ a b) top)
		 sum
		 (get-sum b (+ a b) (if (zerop (rem b 2)) (setq sum (+ sum b)) sum) top))))
    (get-sum 1 1 0 top)))

;;Is the input a palindrome
(declaim (ftype (function (t) boolean) palindrome))
(defun palindrome (input)
  (let ((str ""))
    (declare (type string str))
    (if (not (or (stringp input) (characterp input)))
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
  (mod
   (apply #'+
	  (mapcar #'parse-integer
		  (split-sequence:SPLIT-SEQUENCE separator numlist)))
   (expt 10 digits)))
(defun first-digits-of-sum (digits separator numlist)
  (parse-integer
   (subseq
    (write-to-string
     (apply #'+
	    (mapcar #'parse-integer
		    (split-sequence:SPLIT-SEQUENCE separator numlist))))
    0 digits)))

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
  (apply #'+ (mapcar #'digit-char-p (coerce (write-to-string n) 'list))))

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

;;Largest sum moving down through a 2-D pyramid of numbers
(defun largest-pyramid-sum (pyramidstring)
  (let ((pyramid (mapcar #'(lambda (x) (mapcar #'parse-integer (split-sequence #\Space x))) (split-sequence #\Newline pyramidstring))))
    (labels ((maximize-sum (row col)
	       (if (= row (1- (length pyramid)))
		   (elt (elt pyramid row) col)
		   (+ (elt (elt pyramid row) col) (max (maximize-sum (1+ row) col) (maximize-sum (1+ row) (1+ col)))))))
      (maximize-sum 0 0))))

;;Number of sundays that are also the first day of a month from 1901 to 2000
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
(defun alphabetical-position-sum (&optional (filename "c:/Users/swapn/Downloads/Project_Euler_p022_names.txt"))
  (let* ((words (cl-csv:read-csv
		 (pathname filename)))
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
  (let ((almost-ending (nthcdr n target)))
    (if almost-ending
	(append (subseq target 0 n) (cdr almost-ending))
	target)))

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

;;Outputs permutations of a list in lexicographic ordered
(defun permutations (inlist)
  (if (null (cdr inlist))
      (list inlist)
      (let ((combined nil))
	(declare (type list combined))
	(iter
	  (for i from (1- (length inlist)) downto 0)
	  (setq combined (nconc
			  (mapcar
			   (singlein elem (cons (elt inlist i) elem))
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
      (let ((fact (factorial f)))
	(declare (type number fact))
	(collect (iter (for i from 0 below (length inlist))
		   (thereis (if (null (cdr inlist))
				(car inlist)
				(when (> fact (- sum (* fact i)))
				  (prog1
				      (elt inlist i)
				    (setq sum (- sum (* fact i)))
				    (setq inlist (without-elt i inlist))))))))))))
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
      (values idx b)
      (fibonacci-above-max top b (+ a b) (1+ idx))))

(defun longest-repeating-decimal (top-denominator)
  (labels ((repeating-decimal-length (fraction)
	     (let ((temp (numerator fraction))
		   (tempdem (denominator fraction))
		   (remainderlist (list nil)))
	       (iter (until (member (rem temp tempdem) remainderlist))
		 (progn
		   (setq remainderlist (cons (rem temp tempdem) remainderlist))
		   (setq temp (* 10 (rem temp tempdem)))))
	       (if (zerop temp)
		   0
		   (1+ (position (rem temp tempdem) remainderlist))))))
    (iter (for i from 1 to top-denominator)
      (finding i maximizing (repeating-decimal-length (/ i))))))
;;NOT WORKING RIGHT:
(defun efficient-longest-repeating-decimal (top-denominator)
  (labels ((repeating-decimal-length (fraction)
	     (let ((temp (numerator fraction))
		   (tempdem (denominator fraction))
		   (remainderlist (make-hash-table))
		   (i 0))
	       (iter
		 (for i upfrom 0)
		 (until (gethash (rem temp tempdem) remainderlist))
		 (progn
		   (setf (gethash (rem temp tempdem) remainderlist) i)
		   (setq temp (* 10 (rem temp tempdem)))))
	       (if (zerop temp)
		   0
		   (- (gethash (rem temp tempdem) remainderlist) (1+ i))))))
    (iter (for i from 1 to top-denominator)
      (finding i maximizing (repeating-decimal-length (/ i))))))

;;find the linear coefficient and constant term for the quadratic function that produces the most consecutive primes, with the linear coefficient bounded by (-top, top) and the constant term bounded by [-top, top]
(defun most-consecutive-primes (top)
  (let ((inner-output (list (- top) (- top) (- top))))
    (iter (for a from (- 1 top) below top)
      (iter
	(for b from (mod-if-applies (singlein n (zerop (mod n 2))) #'1+ (- top)) to top by 2)
	(let ((output
		(iter (for n upfrom 0)
		  (while (let ((prime (+ (* (+ n a) n) b)))
			   (and
			    (> prime 0)
			    (null (remove prime (prime-factorize prime))))))
		  (count 1))))
	  (when (> output (car inner-output))
	    (setq inner-output (list output a b))))))
    (values (cdr inner-output)
	    (car inner-output))))
(defun efficient-most-consecutive-primes (top)
  (let ((inner-output (list (- top) nil nil))
	(prime-list (remove-if-not (singlein num (primep (abs num))) (range (mod-if-applies (singlein n (zerop (mod n 2))) #'1+ (- top)) top 2))))
    (iter (for b in prime-list)
      (iter
	(for a from (- 1 top) below top)
	(let ((output
		(iter (for n upfrom 0)
		  (while (let ((prime (+ (* (+ n a) n) b)))
			   (and
			    (> prime 1)
			    (null (remove prime (prime-factorize prime))))))
		  (count 1))))
	  (when (> output (car inner-output))
	    (setq inner-output (list output a b))))))
    (values (cdr inner-output)
	    (car inner-output))))

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
(defun efficient-distinct-series-exponents (low high)
  (length
   (hash-table-keys
    (let ((distincts (make-hash-table)))
      (iter (for a from low to high)
	(iter (for b from low to high)
	  (setf (gethash (expt a b) distincts) 1)))
      distincts))))


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

(defun digit-power-products (top e)
  "Gives all the numbers < top whose digits have eth powers that sum to the original number"
  (let ((powerlist (mapcar (singlein n (expt n e)) (range 10))))
    (iter (for i from 2 below top)
      (when (= i (apply #'+
			(mapcar (singlein n (elt powerlist n))
				(mapcar #'mkinteger
					(coerce (write-to-string i) 'list)))))
	(collect i)))))

(declaim (ftype (function (integer list) integer) sum-combinations))
(defun sum-combinations (top steps)
  (labels ((ways-to-reach (top steps)
	     (cond
	       ((or (< top 0) (null steps)) 0)
	       ((or (zerop top) (equal steps '(1))) 1)
	       (t (+ (ways-to-reach (- top (car steps)) steps)
		     (ways-to-reach top (cdr steps)))))))
    ;; (setf (fdefinition #'ways-to-reach) (memoize #'ways-to-reach))
    (ways-to-reach top (sort steps #'>))))

(defun product-components-pandigital (top)
  "Gives all numbers below top which have a matching pair of factors that, together with the original number, are 1 to 9 pandigital"
  (let ((output-nums (make-hash-table)) (digitlist (range 1 9)))
    (iter (for i from 2 below (ceiling (sqrt top)))
      (let ((num1string (write-to-string i)))
	i
	(iter (for j from 2 to (ceiling (expt 10 (- (log10 top) (length num1string)))))
	  (when (equal digitlist (sort (mapcars #'digit-char-p (coerce num1string 'list) (coerce (write-to-string j) 'list) (coerce (write-to-string (* i j)) 'list)) #'<)) (setf (gethash (* i j) output-nums) t)))))
    (mapcar #'car (hash-table-alist output-nums))))
(defun efficient-product-components-pandigital (&optional (bottom 100) (top 9990))
  (let ((digitlist (range 1 9)))
    (labels ((pandigital-string (num f1 f2)
	       (let ((numstring
		       (sort
			(mapcar #'digit-char-p
				(coerce (apply #'str:concat
					       (mapcar #'write-to-string
						       (list num f1 f2)))
					'list))
			#'<)))
		 (and
		  (= (length numstring) 9)
		  (equal (sort numstring #'<) digitlist))))
	     (is-pandigital-triple (num)
	       (let ((s (min (ceiling (sqrt num))
			     (expt 10
				   (- 9
				      (length (write-to-string num)))))))
		 (if (< s 10000)
		     (find-if
		      (singlein
		       i
		       (and
			(zerop (rem num i))
			(pandigital-string num i (/ num i))))
		      (range 2 s))
		     (iter
		       (for i
			    from 2
			    to s)
		       (thereis
			(when (and
			       (zerop (rem num i))
			       (pandigital-string num i (/ num i)))
			  num)))))))
      (iter (for i from bottom to top)
	(when (and
	       (not (primep i))
	       (is-pandigital-triple i))
	  (collect i))))))

(defun product-of-digit-cancelling ()
  "doc"
  (let ((fracs (make-hash-table)) (possible (range 10 100 1 t)))
    (iter (for i in possible)
      (iter (for j in possible)
	(when (and (not (or
			 (zerop (floor j 10))
			 (zerop (mod j 10))
			 (>= i j)))
		   (or
		    (and (= (mod i 10) (mod j 10))
			 (= (/ i j) (/ (floor i 10) (floor j 10))))
		    (and (= (mod i 10) (floor j 10))
			 (= (/ i j) (/ (floor i 10) (mod j 10))))
		    (and (= (floor i 10) (mod j 10))
			 (= (/ i j) (/ (mod i 10) (floor j 10))))
		    (and (= (floor i 10) (floor j 10))
			 (= (/ i j) (/ (mod 1 10) (mod j 10))))))
	  (setf (gethash (/ i j) fracs) t))))
    (iter (for (k nil) in-hashtable fracs)
      (multiply (print k)))))

(defun digit-factorial-sums (top)
  "Gives all the numbers < top whose digits have factorials that sum to the original number"
  (iter (for i from 3 below top)
    (let ((digitlist (mapcar #'digit-char-p (coerce (write-to-string i) 'list))))
      (when (= i (apply #'+ (mapcar #'factorial digitlist))) (collect i)))))

(defun circular-primes (top)
  "Gives all circular primes up to top (inclusive)"
  (let ((outputs (make-hash-table)))
    (iter (for i from 1 to top)
      (let ((istring (write-to-string i)))
	(when (iter (for j below (length istring))
		(always (prog1 (primep (parse-integer istring))
			  (rotate istring))))
	  (setf (gethash i outputs) 1))))
    (hash-table-alist outputs)))

(defun double-base-palindromes (top b1 &rest bases)
  "Gives all numbers from 1 up to top that are palindromes in all bases."
  (iter (for i from 1 to top)
    (if (every #'palindrome (mapcar (singlein b (dec->stringbase i b)) (cons b1 bases)))
	(collect i))))

(defun truncatable-primes (top)
  (let* ((checksize (expt 10 (floor (/ (log10 top) 4))))
	 (cached-non-prime (remove-if #'primep (range checksize))))
    (labels
	((check-prime-recursive (val)
	   (if (or (zerop (length val))
		   (member (parse-integer (s-first val)) cached-non-prime)
		   (member (parse-integer (substring (1- (length val)) (length val) val)) cached-non-prime))
	       nil
	       (iter (for point from (1- (length (write-to-string checksize))) below (length val))
		 (always (and (primep (parse-integer (substring 0
								(1+ point)
								val)))
			      (primep (parse-integer (substring point
								(length val)
								val)))))))))
      (iter (for values
		 in
		 (remove-if-not #'primep (range 11 top 2)))
	(when
	    (check-prime-recursive (write-to-string values))
	  (collect values))))))

(defun pandigital-multiples (top n)
  "See Project Euler Problem 38"
  (let ((digitlist (range 1 9)) (output-table (make-hash-table)))
    (iter (for i from 1 to top)
      (let ((current ""))
	(iter (for j from 1 to n)
	  (setq current (str:concat current (write-to-string (* i j)))))
	(when (equal digitlist (sort (mapcar #'mkinteger (coerce current 'list)) #'<))
	  (setf (gethash (parse-integer current) output-table) 1))))
    (car (sort (mapcar #'car (hash-table-alist output-table)) #'>))))

(defun number-of-integer-right-triangles (top)
  (let ((perimeters (make-hash-table)))
    (iter (for i from 3 to (ceiling top 2))
      (iter (for j from 4 to (- top i))
	(let ((c (sqrt (+ (square i) (square j)))))
	  (until (> (+ i j c) top))
	  (when (zerop (rem c 1))
	    (setf (gethash (+ i j c) perimeters)
		  (1+ (ensure-gethash (+ i j c) perimeters 0)))))))
    (iter (for (k v) in-hashtable perimeters)
      (finding k maximizing v))))

(defun champernowe-constant (sum-lengths)
  (let ((composite "")
	(marks (sort sum-lengths #'<)))
    (iter (for i upfrom 0)
      (until (null marks))
      (progn
	(setq composite (str:concat composite (write-to-string i)))
	(when (> (length composite) (car marks))
	  (progn (multiply (digit-char-p (elt composite (car marks))))
		 (setq marks (cdr marks))))))))
(defun efficient-champernowe-constant ()
  (let ((composite (iter (for i from 0 to 100001)
		     (accumulate
		      (write-to-string i)
		      by (doublein m n (str:concat n m))))))
    (iter (for i from 1 to 5)
      (multiply (digit-char-p (elt composite (expt 10 i)))))))


;;;; score based splitting and mapping.
;;;; prototype

(defvar *knowledge*
  '(((A B) 0.5) ((C D E) 0.9) ((D E) 0.5)))

;; assoc use eql by default. (eql '(a b) '(a b)) => nil
(defun get-score-for-compound-word (w)
  (or (cadr (assoc w *knowledge* :test 'equal)) 0.0))

(mytest (get-score-for-compound-word '(A B)) 0.5)

(defun get-score (seq)
  "get score for a list of compound words."
  (reduce '+ (map 'list 'get-score-for-compound-word seq)))

(mytest (get-score '((A B) (C D E)))
	(+ 0.5 0.9))

(mytest (get-score '((A B C) (D E)))
	0.5)

(defun firstn (n seq)
  "return the first n elements in seq"
  (subseq seq 0 n))

(mytest (firstn 2 '(1 2 3 4 5))
	'(1 2))

(defun lastn (n seq)
  "return the last n elements in seq"
  (subseq seq (- (length seq) n)))

(mytest (lastn 2 '(1 2 3 4 5))
	'(4 5))

(defun insert-merge (e lst)
  "merge list e to every element in lst, return the new list"
  (if (null lst)
      (list (list e))
      (map 'list (lambda (x)
		   (cons e x)) lst)))

;;; final result:
;;; lst: ( ((1) (2) (3)) ((1 2) 3) )
;;; insert-merge return:

(mytest (insert-merge '(1 2) nil)
	'(((1 2))))
(mytest (insert-merge '(1) '(((2))))
	'(((1) (2))))
(mytest (insert-merge '(1 2) '(((a) (b)) ((a b))))
	 '(((1 2) (a) (b)) ((1 2) (a b))))


(defun get-splits (seq)
  "return all possible splits of seq, as a list."
  (if (null seq)
      nil
      (reduce 'append
	      (let ((l (length seq)))
		(loop for i from 1 to l collecting
		     (insert-merge
		      (firstn i seq) (get-splits (lastn (- l i) seq))))))))

(mytest (reduce 'append '((1 2 3) (4 5)))
	'(1 2 3 4 5))

;; prove this theorem in ACL2:
;; (thm (implies (true-listp x)
;; 	      (= (length (get-splits x)) (pow 2 (1- (length x))))))

(mytest (get-splits '(1 2 3))
	'(((1) (2) (3))
	  ((1) (2 3))
	  ((1 2) (3))
	  ((1 2 3))))
(mytest (get-splits '(1))
	'(((1))))
(mytest (get-splits '(1 2))
	'(((1) (2))
	  ((1 2))))

(defun list-scores (seq &key unsorted)
  "list all possible scores for all possible splits, sorted by score."
  (let ((lst (map 'list (lambda (x)
			   (list (get-score x) x))
		   (get-splits seq))))
    (if unsorted
	lst
	(sort lst (lambda (x y)
		    (> (car x) (car y)))))))

(mytest (length (list-scores '(A B C D E) :unsorted t))
	(expt 2 4))
(list-scores '(A B C D E))
;; ((1.4 ((A B) (C D E)))
;;  (1.0 ((A B) (C) (D E)))
;;  (0.9 ((A) (B) (C D E)))
;;  (0.5 ((A) (B) (C) (D E)))
;;  (0.5 ((A) (B C) (D E)))
;;  (0.5 ((A B) (C) (D) (E)))
;;  (0.5 ((A B) (C D) (E)))
;;  (0.5 ((A B C) (D E)))
;;  (0.0 ((A) (B) (C) (D) (E)))
;;  (0.0 ((A) (B) (C D) (E)))
;;  (0.0 ((A) (B C) (D) (E)))
;;  (0.0 ((A) (B C D) (E)))
;;  (0.0 ((A) (B C D E)))
;;  (0.0 ((A B C) (D) (E)))
;;  (0.0 ((A B C D) (E)))
;;  (0.0 ((A B C D E))))

;; done. this is workable.

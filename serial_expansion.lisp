
;; Symbols
;; ----------------
;; c = concrete arg
;; 0 = proposition
;; 1 = property
;; 2 = relation
;; 3 = ...
;; jado = lambda


;; Replace the first elem e of ls such that (test e) is true
;; with (transform e)
(defun replace-first (ls test transform)
  (if (funcall test (car ls))
      (cons (funcall transform (car ls)) (cdr ls))
    (cons (car ls) (replace-first (cdr ls) test transform))))

(defun is-slot-marker (e)
  (or (eq e 'c) (numberp e)))

(defun is-filled-slot (e)
  (and (not (is-slot-marker e))
       (not (listp e))))


;; Fill the slots of a predicate with the terms given in a list.
;; Examples:
;;   fill (mai c c) (jado ji)        -> (mai jado ji)
;;   fill (dua c (mai c c)) (ji suq) -> (dua ji (mai suq c))

(defun has-open-slot (predicate)
  (or (is-slot-marker predicate)
      (and (not (is-filled-slot predicate))
           (some (lambda (x) (has-open-slot x))
                 (cdr predicate)))))

(defun fill-one-slot (predicate arg)
  (if (is-slot-marker predicate)
      arg
    (cons (car predicate)
          (replace-first (cdr predicate)
                         #'has-open-slot
                         (lambda (x) (fill-one-slot x arg))))))

(defun fill-slots (predicate args)
  (reduce #'fill-one-slot
          (cons predicate args)))


;; Expand a two-part predicate into one predicate.
;; Examples:
;;   (dua c 0) (mai c c) = (dua c (mai c c))
;;   (leo c 1) (mai c c) = (leo c (mai jado c))
;;   (cheo c 2) (mai c c) = (cheo c (mai jado jado))

(defun expand (head tail)
  (let ((shuf (lambda (ls) (append (cddr ls) (list (cadr ls)))))
        (unshuf (lambda (ls)
                  (append (list (car head))
                          (last ls)
                          (reverse (cdr (reverse ls)))))))
    (funcall
     unshuf
     (replace-first
      (funcall shuf head)
      #'numberp
      (lambda (s)
        (fill-slots tail
                    (make-list s :initial-element 'jado)))))))
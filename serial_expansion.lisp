
;; Symbols
;; ----------------
;; c = concrete arg
;; 0 = proposition
;; 1 = property
;; 2 = relation
;; 3 = ...
;; jado = lambda


(defun replace-first (ls test transform)
  (if (funcall test (car ls))
      (cons (funcall transform (car ls)) (cdr ls))
    (cons (car ls) (replace-first (cdr ls) test transform))))

(defun is-slot-marker (e)
  (or (eq e 'c) (numberp e)))

(defun is-filled-slot (e)
  (and (not (is-slot-marker e))
       (not (listp e))))


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



;; Examples:
;;   (dua c 0) (mai c c) = (dua c (mai c c))
;;   (leo c 1) (mai c c) = (leo c (mai jado c))
;;   (cheo c 2) (mai c c) = (cheo c (mai jado jado))

;;(defun collapse (head tail)
;;  (let ((positions (append (loop for i from 2 to (- (length head 1))
    ;;                             collect i)
  ;;                         '(1))))
    
    ;;(loop for p in positions
;;          do (let ((arg (nth p head)))
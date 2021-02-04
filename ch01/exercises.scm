;;; 15
(define (duple n x)
  (define (helper i ans)
    (if (= n i)
      ans
      (helper (+ i 1) (cons x ans))))
  (helper 0 '()))

(define (duple1 n x)
  (if (zero? n)
    '()
    (cons x (duple1 (- n 1) x))))

;;; 16
;;; lst is a list of 2-lists
(define (invert lst)
  (if (null? lst)
    '()
    (cons (list (cadar lst) (caar lst))
          (invert (cdr lst)))))

;;; 17
;;; wraps parentheses around each top-level element of lst
(define (down lst)
  (if (null? lst)
    '()
    (cons (list (car lst)) (down (cdr lst)))))

;;; 18
;;; swapper swaps all occurrences of s1 and s2
;;; s-list ::= ()
;;;        ::= (s-exp . s-list)
;;; s=exp  ::= symbol | s-list
(define (swapper s1 s2 slist)
  (if (null? slist)
    '()
    (cons (swapper-in-s-exp s1 s2 (car slist))
          (swapper s1 s2 (cdr slist)))))

(define (swapper-in-s-exp s1 s2 s-exp)
  (if (symbol? s-exp)
    (cond
      ((eqv? s1 s-exp) s2)
      ((eqv? s2 s-exp) s1)
      (else s-exp))
    (swapper s1 s2 s-exp)))

;;; 19
(define (list-set list n x)
  (if (zero? n)
    (cons x (cdr list))
    (cons (car list) (list-set (cdr list) (- n 1) x))))

;;; 20
(define (count-occurrences s slist)
  (if (null? slist)
    0
    (+ (count-occurrences-in-s-exp s (car slist))
       (count-occurrences s (cdr slist)))))

(define (count-occurrences-in-s-exp s s-exp)
  (if (symbol? s-exp)
    (if (eqv? s s-exp) 1 0)
    (count-occurrences s s-exp)))

;;; 21
(define (product sos1 sos2)
  (define (distribute sym lst)
    (map (lambda (x) (list sym x)) lst))
  (if (null? sos1)
    '()
    (append (distribute (car sos1) sos2) (product (cdr sos1) sos2))))

;;; 22
(define (filter-in pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter-in pred (cdr lst)))
      (filter-in pred (cdr lst)))))

;;; 23
(define (list-index pred lst)
  (define (from lst n)
    (if (null? lst)
      #f
      (if (pred (car lst))
        n
        (from (cdr lst) (+ n 1)))))
  (from lst 0))

;;; 24
(define (every? pred lst)
  (if (null? lst)
    #t
    (if (pred (car lst))
      (every? pred (cdr lst))
      #f)))

;;; 25
(define (exists? pred lst)
  (if (null? lst)
    #f
    (if (pred (car lst))
      #t
      (exists? pred (cdr lst)))))

;;; 26
(define (up1 lst)
  (define (unwrap ans lst)
    (if (null? lst)
      ans
      (if (list? (car lst))
        (unwrap (append ans (car lst)) (cdr lst))
        (unwrap (append ans (list (car lst))) (cdr lst)))))
  (unwrap '() lst))

(define (up2 lst)
  (if (null? lst)
    '()
    (let ((cur (car lst)))
      (if (and (list? cur) (not (null? cur)))
        (cons (car cur) (up2 (cons (cdr cur) (cdr lst))))
        (cons cur (up2 (cdr lst)))))))


(define (up lst)
  (define (unwrap elem)
    (if (list? elem)
      elem
      (list elem)))
  (if (null? lst)
    '()
    (append (unwrap (car lst)) (up (cdr lst)))))
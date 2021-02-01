(load "../libs/r5rs.scm")
(load "../libs/sllgen.scm")
(load "../libs/define-datatype.scm")

(load "../ch02/env-proc.scm")

;;; key concepts of 3.3
; lexical scope rule
; a procedure consists of 3 parts: the bound var,
;                                  the body,
;                                  the environment
; closures are self contained: they contain everything a procedure needs in order
;                              to be appled


;;; grammar
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression
      ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression
      ("let" identifier "=" expression "in" expression) let-exp)
    (expression
      ("proc" "(" identifier ")" expression) proc-exp)
    (expression
      ("(" expression expression ")") call-exp)))

;;; sllgen boilerplate
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define (show-the-datatypes)
  (sllgen:list-define-datatypes the-lexical-spec the-grammar))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;; expval
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (proc-val (proc proc?)))


; a temporary solution
(define (environment? env)
  (procedure? env))

(define-datatype proc proc?
  (procedure
    (var symbol?)
    (body expression?)
    (saved-env environment?)))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (report-expval-extractor-error 'num val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (report-expval-extractor-error 'bool val))))

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (report-expval-extractor-error 'proc val))))

(define (report-expval-extractor-error who val)
  (eopl:error 'expval-extractors "Looking for a ~s, found ~s" who val))

;;; value-of
(define (init-env)
  (extend-env 'i (num-val 1)
    (extend-env 'v (num-val 5)
      (extend-env 'x (num-val 10)
        (empty-env)))))

(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1) (value-of exp1 (init-env)))))

(define (apply-proc proc1 val)
  (cases proc proc1
    (procedure (var body saved-env)
      (value-of body (extend-env var val saved-env)))))

(define (value-of exp env)
  (cases expression exp
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env env var))
    (diff-exp (exp1 exp2)
      (let ((val1 (value-of exp1 env))
            (val2 (value-of exp2 env)))
        (let ((num1 (expval->num val1))
              (num2 (expval->num val2)))
          (- num1 num2))))
    (zero?-exp (exp1)
      (let ((val1 (value-of exp1 env)))
        (let ((num1 (expval->num val1)))
          (if (zero? num1)
            (bool-val #t)
            (bool-val #f)))))
    (if-exp (exp1 exp2 exp3)
      (let ((val1 (value-of exp1 env)))
        (if (expval->bool val1)
          (value-of exp2 env)
          (value-of exp3 env))))
    (let-exp (var exp1 body)
      (let ((val1 (value-of exp1 env)))
        (value-of body (extend-env var val1 env))))
    (proc-exp (var body)
      (proc-val (procedure var body env)))
    (call-exp (rator rand)
      (let ((proc (expval->proc (value-of rator env)))
            (arg (value-of rand env)))
        (apply-proc proc arg)))))

;;; top
(define (run string)
  (value-of-program (scan&parse string)))

(define repl
  (sllgen:make-rep-loop ">> " value-of-program
    (sllgen:make-stream-parser the-lexical-spec the-grammar)))
(load "../libs/r5rs.scm")

(define (empty-env)
  (lambda (search-var)
    (report-no-binding-found search-var)))

(define (extend-env var val env)
  (lambda (search-var)
    (if (eqv? search-var var)
      val
      (apply-env env search-var))))

(define (apply-env env search-var)
  (env search-var))

(define (report-no-binding-found var)
  (eopl:error 'apply-env "No binding for ~s" var))
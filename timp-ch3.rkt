#lang racket
(define-syntax match
  (syntax-rules (guard)
    ((_ v) (error 'match "~s" v))
    ((_ v (pat (guard g ...) e ...) cs ...)
     (let ((fk (lambda () (match v cs ...))))
       (ppat v pat (if (and g ...) (let () e ...) (fk)) (fk))))
    ((_ v (pat e ...) cs ...)
     (let ((fk (lambda () (match v cs ...))))
       (ppat v pat (let () e ...) (fk))))))
(define-syntax ppat
  (syntax-rules (unquote)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (ppat vx x (ppat vy y kt kf) kf))
         kf))
    ((_ v lit kt kf) (if (eqv? v (quote lit)) kt kf))))
(define (extend env vars vals)
  (cons (cons vars (map box vals)) env))
(define lookup
  (lambda (var e)
    (let nxtrib ([e e])
      (let nxtelt ([vars (caar e)] [vals (cdar e)])
        (cond
          [(null? vars) (nxtrib (cdr e))]
          [(eq? (car vars) var) (car vals)]
          [else (nxtelt (cdr vars) (cdr vals))])))))
(define (apply-env env var)
  (unbox (lookup var env)))
(define (set-val! env var val)
  (set-box! (lookup var env) val))
(define (tail? next)
  (eq? (car next) 'return))
(define (make-tail c ret)
  (if (tail? ret)
      c
      `(frame ,ret ,c)))
(define (compile x next)
  (match x
    [,var (guard (symbol? var)) `(refer ,var ,next)]
    [,obj (guard (not (pair? obj))) `(constant ,obj ,next)]
    [(quote ,obj) `(constant ,obj ,next)]
    [(if ,test ,then ,else)
     (let ([thenc (compile then next)]
           [elsec (compile else next)])
       (compile test `(test ,thenc ,elsec)))]
    [(set! ,var ,exp)
     (compile exp `(assign ,var ,next))]
    [(call/cc ,exp)
     (let ([c `(conti (argument ,(compile exp '(apply))))])
       (make-tail c next))]
    [(,rator . ,rand*)
     (let loop ([rand* rand*]
                [c (compile rator '(apply))])
       (if (null? rand*)
           (make-tail c next)
           (loop (cdr rand*)
                 (compile (car rand*)
                          `(argument ,c)))))]))
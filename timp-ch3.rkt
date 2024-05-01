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
    [(lambda ,var* ,body)
     `(close ,var* ,(compile body '(return)) ,next)]
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
(define (closure body env vars)
  `(closure ,body ,env ,vars))
(define (continuation s)
  `(closure (nuate ,s v) () (v)))
(define (call-frame x e r s)
  `(call-frame ,x ,e ,r ,s))
(define (addv a r)
  (cons (box a) r))
(define (extend env vars vals)
  (cons (cons vars vals) env))
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
(define (VM a x e r s)
  (match x
    [(halt) a]
    [(refer ,var ,x) (VM (apply-env e var) x e r s)]
    [(constant ,obj ,x) (VM obj x e r s)]
    [(close ,var* ,body ,x) (VM (closure body e var*) x e r s)]
    [(test ,then ,else) (VM a (if a then else) e r s)]
    [(assign ,var ,x) (set-val! e var a) (VM a x e r s)]
    [(conti ,x) (VM (continuation s) x e r s)]
    [(nuate ,s ,var) (VM (apply-env e var) '(return) e r s)]
    [(frame ,ret ,x) (VM a x e '() (call-frame ret e r s))]
    [(argument ,x) (VM a x e (addv a r) s)]
    [(apply) (match a
               [(closure ,body ,e ,var*)
                (VM a body (extend e var* r) '() s)])]
    [(return) (match s
                [(call-frame ,x ,e ,r ,s) (VM a x e r s)])]))
(define (run x)
  (VM '() (compile x '(halt)) '() '() '()))
(define (make-machine exp)
  (define a '())
  (define x (compile exp '(halt)))
  (define e '())
  (define r '())
  (define s '())
  (define (print-state)
    (printf "val: ~s\nexp: ~s\nenv: ~s\narg*: ~s\nstk: ~s\n" a x e r s))
  (define (step)
    (match x
      [(halt)
       (printf "The computation has been finished, and current state of the machine is\n")
       (print-state)]
      [(refer ,var ,next)
       (set! a (apply-env e var))
       (set! x next)
       (print-state)]
      [(constant ,obj ,next)
       (set! a obj)
       (set! x next)
       (print-state)]
      [(close ,var* ,body ,next)
       (set! a (closure body e var*))
       (set! x next)
       (print-state)]
      [(test ,then ,else)
       (if a
           (set! x then)
           (set! x else))
       (print-state)]
      [(assign ,var ,next)
       (set! x next)
       (set-val! e var a)
       (print-state)]
      [(conti ,next)
       (set! a (continuation s))
       (set! x next)
       (print-state)]
      [(nuate ,stk ,var)
       (set! a (apply-env e var))
       (set! x '(return))
       (set! s stk)
       (print-state)]
      [(frame ,ret ,next)
       (set! x next)
       (set! r '())
       (set! s (call-frame ret e r s))
       (print-state)]
      [(argument ,next)
       (set! x next)
       (set! r (addv a r))
       (print-state)]
      [(apply)
       (match a
         [(closure ,body ,env ,var*)
          (set! x body)
          (set! e (extend env var* r))
          (set! r '())
          (print-state)])]
      [(return)
       (match s
         [(call-frame ,exp ,env ,rib ,stk)
          (set! x exp)
          (set! e env)
          (set! r rib)
          (set! s stk)
          (print-state)])]))
  (lambda (msg)
    (case msg
      [(step) (step)]
      [(print-state) (print-state)])))
(define m0
  (make-machine '((lambda (x) (x x))
                  (lambda (x) (x x)))))

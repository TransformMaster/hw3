#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type REnv = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive +
    [(PrimN p es)
     (if (CheckN (interp*-env es r))
         'err
         (interp-primN p (interp*-env es r))
       )
     ]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs e) (if (clause-err-bool cs r) 'err (if (clause-bool cs r) (clause-result cs r) (interp-env e r)))]
    ;; TODO: implement case
    [(Case ev cs el) (match (interp-env ev r)
                       ['err 'err]
                       [v (if (case-err-bool cs r) 'err (if (case-bool cs ev r) (case-result cs ev r) (interp-env el r)))])]
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let (list x) (list e1) e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]
    ;; TODO: implement let, let*
    [(Let  xs es e) 'err]
    [(Let* xs es e) 'err]))


;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

(define (CheckN es)
  (match es
    ['() #f]
    [(cons e e1) (match e
                   ['err #t]
                   [v (or #f (CheckN e1))]
                   ) ]
    )
  )

(define (clause-err-bool cs r)
  (match cs
    ['() #f]
    [(cons a b) (match a
                  [(Clause p b1) (match (interp-env p r)
                                   ['err #t]
                                   [v (or #f (clause-err-bool b r))])
                                     ]
                  )
                ]
   )
  )


(define (clause-bool cs r)
  (match cs
    ['() #f]
    [(cons a b) (match a
                  [(Clause p b1) (if (interp-env p r) #t (clause-bool b r))]
                  )
                ]
   )
  )

(define (clause-result cs r)
  (match cs
    [(cons a '()) (match a
                  [(Clause p b1) (interp-env b1 r)]
                  )]
    [(cons a b) (match a
                  [(Clause p b1) (if (interp-env p r) (interp-env b1 r) (clause-result b r))]
                  )
                ]
   )
  )

(define (case-err-bool cs r)
  (match cs
    ['() #f]
    [(cons a b) (match a
                  [(Clause p b1) (if (check-err-in p r) #t (or #f (case-err-bool b r)))])])
  
  )

(define (check-err-in p r)
  (match p
    ['() #f]
    [(cons a b) (match (interp-env a r)
                  ['err #t]
                  [v (or #f (check-err-in b r))]
                  )]
    )
  )

(define (case-bool cs e r)
  (match cs
    ['() #f]
    [(cons a b) (match a
                  [(Clause p b1) (if (check-in p e r) #t (case-bool b e r))]
                  )
                ]
   )
  )

(define (case-result cs e r)
  (match cs
    [(cons a '()) (match a
                  [(Clause p b1) (interp-env b1 r)]
                  )]
    [(cons a b) (match a
                  [(Clause p b1) (if (check-in p e r) (interp-env b1 r) (case-result b e r))]
                  )
                ]
   )
  )

(define (check-in p e r)
  (match p
    ['() #f]
    [(cons a b) (if (equal? (interp-env a r) (interp-env e r)) #t (check-in b e r))]
   )
  )
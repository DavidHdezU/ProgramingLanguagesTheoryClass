#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp) 
     (case (car sexp)


       [(add1) (if  (equal? (length (cdr sexp)) 1)
                    (op add1 (map parse(cdr sexp)))
                    (error 'parse "n-ariedad equivocada"))]
       [(sub1) (if  (equal?(length(cdr sexp)) 1)
                    (op sub1 (map parse(cdr sexp)))
                    (error 'parse "n-ariedad equivocada"))]
       [(+) (if  (> (length(cdr sexp)) 1)
                    (op + (map parse(cdr sexp)) )
                    (error 'parse "n-ariedad equivocada"))]
       [(-) (if  (>(length(cdr sexp)) 1)
                     (op - (map parse(cdr sexp)) )
                    (error 'parse "n-ariedad equivocada"))]
       [(*) (if  (>(length(cdr sexp)) 1)
                     (op * (map parse(cdr sexp)) )
                    (error 'parse "n-ariedad equivocada"))]
       [(/) (if  (>(length(cdr sexp)) 1)
                     (op / (map parse(cdr sexp)) )
                    (error 'parse "n-ariedad equivocada"))]
       [(modulo) (if  (equal?(length(cdr sexp)) 2)
                    (op modulo (map parse(cdr sexp)) )
                    (error 'parse "n-ariedad equivocada"))]
       [(expt) (if  (equal?(length(cdr sexp)) 2)
                     (op expt (map parse(cdr sexp)) )
                    (error 'parse "n-ariedad equivocada"))])]))
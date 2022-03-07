#lang plai

;; multiplos: number number -> (listof number)
(define (multiplos n r)
  (map (λ(x) (* x n)) (lista_naturales 1 r)))

;; lista_naturales: number -> procedure -> (listof number)
(define (lista_naturales m n)
  (if (> m n)
      empty
      (cons m (lista_naturales (+ m 1) n))))


;; ;; divisor?: number number -> boolean
(define (divisor? m n)
  (if (= 0 m)
      (error "El cero no es divisor de nadie")
      (= 0 (modulo n m))))


;; divisores: number -> (listof number)
(define (divisores n)
  (filter (λ(x) (divisor? x n)) (lista_naturales 1 n)))











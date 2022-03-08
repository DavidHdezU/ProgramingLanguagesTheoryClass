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



;; Función que calcula el cambio que tenemos que devovler según el
;; monto a cobrar y el monto pagado. Devuelve la cantidad de monedas de las
;; denominaciones $50, $20, $10, $5, $2, $1.

;; cambio: number -> nunmber -> (listof number)
(define (cambio total pago)
  (auxCambio (- pago total) '(50 20 10 5 2 1)))

;; auxCambio: -> number -> (listof number) -> (listof number)
(define (auxCambio curr_cambio lista_denom)
  (if (empty? lista_denom)
      empty
      (cons (exact-floor (/ curr_cambio (car lista_denom))) (auxCambio (modulo curr_cambio (car lista_denom)) (cdr lista_denom)))))


;; masRepetido: (listof a) -> a
(define (masRepetido lista)
  (if (empty? lista)
      (error "La lista es vacía")
      (auxCuenta lista lista (car lista))))
  
;; cuentaApariciones: (listof a) -> a -> number 
(define (cuentaApariciones lista x cont)
  (cond
    [(empty? lista) cont]
    [(equal? (car lista) x) (cuentaApariciones (cdr lista) x (+ cont 1))]
    [else (cuentaApariciones (cdr lista) x cont)]))

;; auxCuenta: (listof a) -> (listof a) -> a -> a
(define (auxCuenta curr_lista lista_org elem)
  (cond
    [(empty? curr_lista) elem]
    [(equal? (car curr_lista) elem) (auxCuenta (cdr curr_lista) lista_org elem)] ;; Para evitar contar de nuevo el mismo elmento
    [(> (cuentaApariciones lista_org (car curr_lista) 0) (cuentaApariciones lista_org elem 0)) (auxCuenta (cdr curr_lista) lista_org (car curr_lista))]
    [else (auxCuenta (cdr curr_lista) lista_org elem)]))












          
            
   


      















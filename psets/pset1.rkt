#lang plai

;; esPar?: number -> boolean
(define (esPar? n)
  (= (modulo n 2) 0))

;; absoluto: number -> number
(define (absoluto z)
  (if (< z 0)
      (* z -1)
      z))

;; pares: number -> (listof number)
(define (pares n)
  (cuenta-desde-cero n 0))

;; cuenta-desde-cero: number -> number -> (listof number)
;; función auxiliar para crear una lista con un contador auxiliar que empieza desde 0
(define (cuenta-desde-cero n m)
  (cond
    [(< n m) empty] ;; Esto ya que buscamos todo los números menores a n, o sea aunque tengamos n = 10, no podemos tomar en cueta a 10 aunque (modulo 10 5) = 0
    [else (cons m (cuenta-desde-cero n (+ m 2)))]))

;; suma-cuadrados: number -> number
(define (suma-cuadrados n)
  (/ (* n (* (+ n 1) (+ 1 (* 2 n)))) 6))

;; discriminante: number number number -> number
(define (discriminante a b c)
  (- (* b b) (* 4 (* a c))))



;; raicesReales? : number number number -> boolean
(define (raicesReales? a b c)
  (or (= 0 (discriminante a b c)) (> (discriminante a b c) 0)))

;; general1: number number number -> number
(define (general1 a b c)
  (if (raicesReales? a b c)
      (/ (+ (* -1 b) (sqrt (discriminante a b c))) (* 2 a))
      (error "El polinomio no tiene raices reales")))

;; general2: number number number -> number
(define (general2 a b c)
  (if (raicesReales? a b c)
      (/ (- (* -1 b) (sqrt (discriminante a b c))) (* 2 a))
      (error "El polinomio no tiene raices reales")))

;; reversa-lista: (listof a) -> (listof a)
(define (reversa-lista lista)
  (aux-reversa lista '()))

;; Función auxiliar para hacer la reversa de una lista, esto es para evitar concatenar de mala manera
;; aux-reversa: (listof a) ->  (listof a)
(define (aux-reversa lista aux) ;; esta función es para crear una lista a partir de una lista vacia, es decir, le vamos pegando la cabeza de la cola de lista
  (if (empty? lista)            ;; esto, ya que al concatenar de la forma tradicional (como se hace en Haskell) nos daba una cosa extraña
      aux 
      (aux-reversa (cdr lista) (cons (car lista) aux))))

;; palindromo-lista?: (listof a) -> Boolean
(define (palindromo? lista)
  (lista-iguales? lista (reversa-lista lista)))

;; lista-iguales?: (listof a) -> (listof a) -> Boolean
(define (lista-iguales? lista1 lista2)
  (cond
    [(and (empty? lista1) (empty? lista2)) #t]
    [(or (and (empty? lista1) (not (empty? lista2))) (and (not (empty? lista1)) (empty? lista2))) #f]
    [(not (equal? (car lista1) (car lista2))) #f]
    [else (lista-iguales? (cdr lista1) (cdr lista2))]))

;; Funcion que nos da el elemento maximo de una lista
;; maximo: (listof a) -> number
(define (maximo lista)
  (maximo-aux lista -inf.0))

;; Función auxiliar para actualizar el maximo cada vez que encontremos un valor mayor al actual
;; maximo-aux: (listof a) -> number -> number
(define (maximo-aux lista curr_max)
  (cond
    [(empty? lista) curr_max]
    [(> (car lista) curr_max) (maximo-aux (cdr lista) (car lista))]
    [else (maximo-aux (cdr lista) curr_max)]))

;; (eliminaRepetidos (listof a) -> (listof a)
(define (eliminaRepetidos lista)  ;; Esto O(n^2), pero si usaramos un Conjunto (Hash-Set) podriamos hacerlo en O(n)
  (eliminaRepetidos-aux lista '())) 



;; Función auxiliar para ver si un elemento ya está contenido en una lista
;; esta-en-lista: (listof a) -> number -> Boolean
(define (esta-en-lista lista x)
  (cond
    [(empty? lista) #f]
    [(equal? (car lista) x) #t]
    [else (esta-en-lista (cdr lista) x)]))

;; Función auxiliar para eliminarRepetidos, lo que se hace es crear una lista mientras vamos checando en lista original que los elementos que tengamos en la lista que vayamos creando no se repitan
;; eliminaRepetidos-aux: (listof a) -> (listof a) -> (listof a)
(define (eliminaRepetidos-aux lista curr-res)
  (cond
    [(empty? lista) (reversa-lista curr-res)] ;; regresamos la reversa ya que la forma en la que concatenamos nos da la reversa de lista original sin repeticiones, esto lo hacemos para no crear más funciones auxiliares para evitar problemas de concatenación
    [(esta-en-lista curr-res  (car lista)) (eliminaRepetidos-aux (cdr lista) curr-res)]
    [else (eliminaRepetidos-aux (cdr lista) (cons (car lista) curr-res))]))


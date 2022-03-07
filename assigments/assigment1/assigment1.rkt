#lang plai

(define (longitud n)
  (if (< n 1) ;; como al dividir nos da reales, cuando n < 1 significa que tenemos un número decimal, por ejemplo 0.1729
      0
      (+ 1 (longitud (/ n 10)))))


(define (filter-pos l)
  (cond
    [(empty? l) empty]
    [(< (car l) 0) (filter-pos (cdr l))]
    [else (cons (car l) (filter-pos (cdr l)))]))


(define (multiplos-3-5 n)
  (aux-cuenta n 0))


(define (aux-cuenta n m)
  (cond
    [(= n m) 0] ;; Esto ya que buscamos todo los números menores a n, o sea aunque tengamos n = 10, no podemos tomar en cueta a 10 aunque (modulo 10 5) = 0
    [(or (= (modulo m 3) 0) (= (modulo m 5) 0)) (+ m (aux-cuenta n (+ m 1)))]
    [else (aux-cuenta n (+ m 1))]))
  



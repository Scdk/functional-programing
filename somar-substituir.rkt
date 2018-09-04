#lang racket
(define (comprimento lista)
  (if (empty? lista)
      0
  (+ 1 (comprimento (cdr lista)))))
(define (soma lista)
  (if (empty? lista)
      0
  (+ (car lista) (soma (cdr lista)))))
(define (comprimento2 lista)
  (define (comp-aux lista ac)
    (if (empty? lista)
        ac
        (comp-aux (cdr lista) (+ 1 ac))))
  (comp-aux lista 0))
(define (soma2 lista)
  (define (soma-aux lista ac)
    (if (empty? lista)
        ac
        (soma-aux (cdr lista) (+ (car lista) ac))))
  (soma-aux lista 0))
(define (substitui a b lista)
  (define (sub-aux lista listab)
    (if (empty? lista)
        (inverte lista listab)
        (if (equal? a (car lista))
        (sub-aux (cdr lista) (cons b listab))
        (sub-aux (cdr lista) (cons (car lista) listab)))))
  (sub-aux lista empty))
  (define (inverte lista listab)
    (define (list-aux lista listab)
      (if (empty? listab)
          lista
          (list-aux (cons (car listab) lista) (cdr listab))))
    (list-aux lista listab))
  

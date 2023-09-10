#lang eopl
;;  Taller 1
;; Valentina Barbetty Arango 2310050
;; Brayan Julio Gómez 2310016
;; Jheison Gómez 


;; PUNTO 1
;; invert :
;; Proposito:
;; L -> L' : Procedimiento que recibe una lista con pares x,y e invierte esos pares de forma que queden y,x
;;
;; <lista> := ()
;;         := (<lista>)

(define invert
  (lambda (L)
    (if (null? L)
        '()
        (cons (list (cadr (car L)) (car (car L)))
        (invert (cdr L)))))) 

;; Pruebas
(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))


;; PUNTO 2

;; down :
;; Proposito:
;; L -> L' : Procedimiento que recibe una lista y a cada elemento de esa lista le añade un nivel más de paréntesis
;;
;; <lista> := ()
;;         := (<lista>)

(define down
  (lambda (L)
    (if (null? L)
        '()
        (cons (list (car L))
        (down (cdr L)))))) 

;; Pruebas
(down '(1 2 3))
(down '((una) (buena) (idea)))
(down '(un (objeto (mas)) complicado))


;; PUNTO 2


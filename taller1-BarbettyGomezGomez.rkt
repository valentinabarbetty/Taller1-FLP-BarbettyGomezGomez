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
;;         := (<valor-scheme> <lista>)

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
;;         := (<valor-scheme> <lista>)

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


;; PUNTO 4

;; filter-in :
;; Proposito:
;; P x L -> L' : Procedimiento que recibe una lista y un predicado,
;; a cada elemento de esa lista va a verificar si cumple el predicado
;; si lo cumple, lo añade a una lista
;;
;; <lista> := ()
;;         := (<predicado> <lista>)

(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))

;; pruebas
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))


;; Punto 7
;; cartesian-product :
;; Proposito:
;; L1 x L2 -> L' : Procedimiento que recibe dos listas y
;; realiza el producto cartesiano entre ellas
;;
;; <lista> := ()
;;         := (<lista> <lista>)


(define cartesian-product
  (lambda (L1 L2)
    (cond [(null? L1) '()]
          [(null? L2) '()]
          [else (append (cart-prod-helper (car L1) L2) (cartesian-product (cdr L1) L2))]
          )
    )
  )

;; cart-prod-helper :
;; Proposito:
;; x X L -> L' : Procedimiento que recibe un elemento x y lo agrega a
;; cada elemento de la lista. 
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define cart-prod-helper
  (lambda (x L)
    (cond [(null? L) '()]
          [else (cons (list x (car L)) (cart-prod-helper x (cdr L)))]
          )
    )
  )


;; pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))



;; punto 17

;; mult:
;; Proposito:
;; L1 x L2 -> L' : Procedimiento que recibe dos listas y
;; multiplica elemento por elemento estas dos,
;; devolviendo una nueva lista con los productos correspondientes.
;;
;; <lista> := ()
;;         := (<lista> <lista>)


(define mult
  (lambda (l1 l2)
    (if (null? l1)
        '()
        (cons (* (car l1) (car l2))
              (mult (cdr l1) (cdr l2))))))


;; mult:
;; Proposito:
;; L1 x L2 -> L' : Procedimiento que recibe dos listas y realiza la
;; multiplicación escalar entre la lista1 que es la matriz, y la lista 2
;; que es el vector
;;
;; <lista> := ()
;;         := (<lista> <lista>)


(define prod-scalar-matriz
  (lambda (mat vec)
    (if (null? mat)
        '()
        (cons (mult (car mat) vec)
              (prod-scalar-matriz (cdr mat) vec)))))


;; pruebas
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))


;; PUNTO 18
;; sum:
;; Proposito:
;; L1 x L2 -> L': Procedimiento que recibe dos listas y realiza la
;; suma entre ellas
;;
;; <lista> := ()
;;         := (<lista> <lista>)

(define sum
  (lambda (l1 l2)
    (if (null? l1)
        '()
        (cons (+ (car l1) (car l2))
             (sum (cdr l1) (cdr l2))
             )))) 

;; cero-izq:
;; Proposito:
;; L -> L': Procedimiento que recibe una lista
;; y añade un cero en la primera posición de esta
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)


(define (cero-izq lst)
  (cons 0 lst))

;; cero-der:
;; Proposito:
;; L -> L': Procedimiento que recibe una lista
;; y añade un cero en la ultima posición de esta
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)


(define (cero-der lst)
  (append lst (list 0)))


;; sum:
;; Proposito:
;; x -> L: Procedimiento que recibe un número y calcula los números pascal
;; hasta la fila x
;;
;; <lista> := ()
;;         := (<lista><lista>)


(define pascal
  (lambda (x)
   
    (define (inner-function y c)
      (if (= c 0)
          y
          (inner-function (sum(cero-izq y)(cero-der y)) (- c 1))
          ))

    (inner-function '(1) (- x 1))))


;; pruebas
(pascal 5)
(pascal 1)











      
    





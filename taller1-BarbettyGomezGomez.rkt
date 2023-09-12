#lang eopl
;;  Taller 1
;; Valentina Barbetty Arango
;; Brayan Julio Gómez
;; Jheison Gómez


;5. (4.5pts) Elabore una funcion llamada list-index que debe recibir dos
;argumentos: un predicado P y una lista L. La funcion retorna (desde una
;posicion inicial 0) el primer elemento de la lista que satisface el predicado
;L. Si llega a suceder que ningun elemento satisface el predicado recibido, la
;funcion debe retornar #f.

;;list-index
;; Proposito: El propósito de la función list-index es buscar el primer elemento en una lista L que cumple con un cierto predicado P
;; y devolver la posición de ese elemento en la lista, comenzando desde la posición 0. Si ningún elemento en la lista cumple con el predicado
;; P, la función debe retornar #f.

;; P x L -> number o false : Un procedimiento para comprobar si el predicado está presente dentro de una lista.
;;
;;<list-index> := ()
;; := (<valor-de-scheme> <lista>)
;; := (<valor-de-scheme> <lista>)

; Ejemplos:
;list-index number? ’(a 2 (1 3) b 7))
;1
;list-index symbol? ’(a (b c) 17 foo))
;0
;list-index symbol? ’(1 2 (a b) 3))
;#f

(define list-index
  (lambda (P L)
    (cond
      [(empty? L) '#f]             
      [(P (car L)) 0]              
      [else
       (if (not (equal? (list-index P (cdr L)) '#f))
           (+ 1 (list-index P (cdr L))) 
           '#f                       
       )]
    )
  )
)

;pruebas
(check-expect (list-index number? '(a 2 (1 3) b 7)) 1)
(check-expect (list-index symbol? '(a (b c) 17 foo)) 0)
(check-expect (list-index symbol? '(1 2 (a b) 3)) #false)


;9. (4.5pts) Elabore una funcíon llamada inversions que recibe como entrada
;una lista L, y determina el ńumero de inversiones de la lista L. De manera formal, sea A = (a1a2...an) una lista de n ńumeros diferentes, si i < j (posicíon)
;y ai > aj (dato en la posicíon) entonces la pareja (i j) es una inversíon de A.

;;inversions
;; Proposito:La función "inversions" tiene como objetivo calcular el número de inversiones en una lista de números.
;;Una inversión se produce cuando, al comparar las posiciones i y j en la lista, se cumple que i < j y el valor en la posición i es mayor que el valor en la posición j.
;;Esta función utiliza un enfoque recursivo para contar todas las inversiones presentes en la lista hasta que se haya procesado por completo.
;;Su resultado proporciona una medida de cuántas veces ocurren inversiones en la lista,

;; L -> number: Esta funcion principal le ingresa una lista y su respuesta es un numero > o igual a 0 que hace referencia a las inversiones
;;
;;<list-index> := ()
;; := (<valor-de-scheme> <lista>)
;; := (<valor-de-scheme> <lista>)

;Ejemplos:
;> (inversions ’(2 3 8 6 1))
;5
;> (inversions ’(1 2 3 4))
;0
;> (inversions ’(3 2 1))
;3

;;Funcion auxiliar validarLista
;;El proposito de La función auxiliar validarLista tiene como propósito verificar si el primer elemento de la lista
;;es mayor que los elementos restantes en la lista. Cuenta el número de inversiones,
;;donde una inversión se produce cuando un elemento en una posición anterior en la lista es mayor que un elemento en una posición posterior.
;; L x number -> number

(define validarLista
  (lambda (L numero)
    (cond
      [(empty? L) 0]
      [(> numero (car L)) (+ 1 (validarLista (cdr L) numero))]
      [else (+ (validarLista (cdr L) numero))])))


(define inversions
  (lambda (L)
    (cond[(empty? L) 0]
         [(empty? (cdr L)) 0]
         [else  (+ (validarLista (cdr L) (car L)) (inversions (cdr L)) )]
         )
    ))
;pruebas
(check-expect (inversions '(2 3 8 6 1)) 5)
(check-expect (inversions '(1 2 3 4)) 0)
(check-expect (inversions '(3 2 1)) 3)


;11. (4.5pts) Elabore una funcion llamada zip que recibe como entrada tres
;paŕametros: una funcion binaria (funcion que espera recibir dos argumentos)
;F, y dos listas L1 y L2, ambas de igual tamãno. El procedimiento zip
;debe retornar una lista donde la posicíon n- ́esima corresponde al resultado
;de aplicar la funcíon F sobre los elementos en la posicíon n- ́esima en L1 y
;L2.

;;zip
;; Proposito:El propósito de esta función llamada "zip" es combinar dos listas (L1 y L2) elemento por elemento utilizando una función binaria (F)
;;y retornar una nueva lista donde cada elemento en la posición n-ésima es el resultado de aplicar la función F a los elementos correspondientes de L1 y L2

;; F L1 L2 -> L3: Esta función principal toma como entrada una función binaria junto con dos listas numéricas y devuelve una lista que resulta de aplicar las
;operaciones especificadas por la función a los elementos correspondientes de las dos listas.


;;
;;<list-index> := ()
;; := (<valor-de-scheme> <lista>)
;; := (<valor-de-scheme> <lista>)

;Ejemplos:
;> (zip + ’(1 4) ’(6 2))
;(7 6)
;> (zip * ’(11 5 6) ’(10 9 8))
;(110 45 48)

;funcion auxiliar
;tiene como propósito calcular la longitud (cantidad de elementos) de una lista
(define length-list
  (lambda (L)
    (cond [(empty? L) 0]
          [else (+ 1 (length (cdr L)))]
    )
  )
)

;funcion auxiliar
;La función (define perform-operation ...) tiene como propósito realizar una operación binaria (representada por la función F)
;en dos listas (L1 y L2) elemento por elemento y devolver una nueva lista que contiene los resultados de aplicar F a los elementos correspondientes de las dos listas.
(define perform-operation
  (lambda (F L1 L2)
       (cond [(empty? L1) empty]
             [else (cons (F (car L1) (car L2)) (perform-operation F (cdr L1) (cdr L2)))]
             )
    )
  )

(define zip
  (lambda (F L1 L2)
  (cond [(not (procedure? F)) "Not an operator"]
        [(not (= (length-list L1) (length-list L2))) "Lists of different lengths"]
        [else (perform-operation F L1 L2)]
        )
  )
)

(check-expect (zip + '(1 4) '(6 2)) (list 7 6))
(check-expect (zip * '(11 5 6) '(10 9 8)) (list 110 45 48))
(check-expect (zip * '(11 5 6) '(10 9 8 1)) "Lists of different lengths")

;12. (4.5pts) Elabore una funcion llamada filter-acum que recibe como entrada 5 par ́ametros: dos n ́umeros a y b, una funci ́on binaria F, un valor inicial
;acum y una funci ́on unaria filter. El procedimiento filter-acum aplicar ́a la
;funci ́on binaria F a todos los elementos que est ́an en el intervalo [a, b] y que
;a su vez todos estos elementos cumplen con el predicado de la funci ́on filter,
;el resultado se debe ir conservando en acum y debe retornarse el valor final
;de acum.
;;filter-acum

;; Proposito:El propósito de la función "filter-acum" es aplicar una función binaria (F) a todos los elementos que se encuentran dentro de un intervalo dado [a, b],
;; y además, estos elementos deben cumplir con un cierto criterio especificado por una función unaria (filter).
;; El resultado de cada aplicación de F se acumula en un valor inicial llamado "acum," y al final de este proceso, la función retorna el valor final de "acum."

;;a b F acum filter -> acum:recibe cinco parámetros: 'a' y 'b' son dos números que definen el rango de valores a considerar. 'F'
;representa una operación matemática a aplicar a estos valores. 'acum' es una variable que almacena el resultado acumulativo y 'filter'
;es una función que determina si el número consecutivo es par o impar en el proceso. 

;;
;;<list-index> := ()
;; := (<valor-de-scheme> <lista>)
;; := (<valor-de-scheme> <lista>)

;Ejemplos:
;> (filter-acum 1 10 + 0 odd?)
;25
;> (filter-acum 1 10 + 0 even?)
;30

(define filter-acum
  (lambda (a b F acum filter)
    (cond [(not (and (number? a) (number? b))) "the parameter must be a number"]
          [(not(procedure? F))  "Not an operator"]
          [(not(procedure? filter))  "Not an operator"]
          [(> a b) acum]
          [else (cond [(filter a) (filter-acum (+ a 1) b F (F a (cond [(equal? (F a acum) 0) 1] [else acum])) filter )]
                      [else (filter-acum (+ a 1) b F acum filter)])]

                      )
          )
    )
(define filter-acum2
  (lambda (a b F acum filter)
        (cond [(and (equal? F *) (equal? F /)) (+ acum 1)] [else acum])
    ))
(check-expect (filter-acum 1 10 + 0 odd?) 25)
(check-expect (filter-acum 1 10 + 0 even?)30)
(check-expect (filter-acum 1 10 'a 0 even?) "Not an operator")

;13. (5pts) Elabore una funci ́on llamada (operate lrators lrands) donde
;lrators es una lista de funciones binarias de tama ̃no n y lrands es una lista
;de n ́umeros de tama ̃no n + 1. La funci ́on retorna el resultado de aplicar
;sucesivamente las operaciones en lrators a los valores en lrands.

;;proposito: El propósito de la función "(operate lrators lrands)" es tomar dos listas como entrada: "lrators," que contiene funciones binarias, y "lrands,"
;;que contiene números. Luego, la función realiza una serie de operaciones sucesivas, aplicando las funciones en "lrators" a los valores en "lrands"
;;de manera secuencial. El resultado final de estas operaciones se devuelve como el resultado de la función.
;operate

;lrators lrands ->La función 'operate' toma dos listas como entrada: 'lrators,' que contiene operaciones matemáticas, y 'lrands,' que contiene números.
;Su propósito es ejecutar las operaciones matemáticas almacenadas en 'lrators' en secuencia,
;utilizando los números contenidos en 'lrands.' El resultado final de esta secuencia de operaciones se devuelve como el resultado de la función.


;Ejemplos
;> (operate (list + * + - *) ’(1 2 8 4 11 6))
;102
;> (operate (list *) ’(4 5))
;20
;5
;En el ejemplo anterior, el resultado es 102 puesto  que (((((1 + 2) ∗ 8) +
;4) - 11) ∗ 6) = 102 y 20 puesto que (4 ∗ 5) = 20.

(define resultado
  (lambda (lrators lrands acum)
          (cond
            [(and (empty? lrators) (empty? lrands)) acum]
            [else  (resultado (cdr lrators) (cdr lrands) ((car lrators) acum (car lrands)))]
            )
         )
  )

(define operate
  (lambda (lrators lrands)
  (cond
    [(and (empty? (car lrators)) (empty?(car lrands))) 0]
    [(not (equal? (- (length-list lrands) (length-list lrators)) 1)) "Lists of different lengths"]
    [(not (procedure? (car lrators)))  "Not an operator"]
    [else (resultado (cdr lrators) (cddr lrands) ((car lrators) (car lrands) (car (cdr lrands))))]
  )))

(check-expect (operate (list + * + - *) '(1 2 8 4 11 6)) 102)
(check-expect (operate (list *) '(4 5))20)
(check-expect (operate (list * *) '(1 1)) "Lists of different lengths")

;14. (8pts) Elabore una funci ́on llamada path que recibe como entrada dos
;par ́ametros: un n ́umero n y un  ́arbol binario de b ́usqueda (representando
;con listas) BST (el  ́arbol debe contener el n ́umero entero n). La funci ́on
;debe retornar una lista con la ruta a tomar (iniciando desde el nodo ra ́ız del
; ́arbol), indicada por cadenas left y right, hasta llegar al n ́umero n recibido.
;Si el n ́umero n es encontrado en el nodo ra ́ız, el procedimiento debe retornar
;una lista vac ́ıa.
;;Proposito: El propósito de la función "path" es calcular y devolver la ruta desde el nodo raíz de un árbol binario de búsqueda (BST)
;hasta un número entero específico "n," representado como una lista de instrucciones "left" y "right."
;Esta ruta indicará cómo llegar desde el nodo raíz hasta el nodo que contiene el número "n" en el árbol. Si el número "n" se encuentra en el nodo raíz,
;la función retornará una lista vacía para indicar que no se necesita moverse desde la raíz.
; number x Arbol(BST) -> list(string): La función 'path' recibe dos valores de entrada: un número entero 'num' que se busca en el árbol binario de búsqueda, y
;un árbol binario de búsqueda 'BST' en el cual se realizará la búsqueda. Como resultado, esta función devuelve una lista que contiene una secuencia de cadenas 'left' y 'right,'
;representando la ruta desde el nodo raíz del árbol hasta el nodo que contiene el número 'num.' Si el número 'num' se encuentra en el nodo raíz, la lista estará vacía, indicando
;que no se requiere navegar desde la raíz hasta el nodo objetivo.;;Ejemplo:

;> (path 17 ’(14 (7 () (12 () ())) (26 (20 (17 () ())
;())
;(31 () ()))))
;(right left left)
;Nota aclaratoria: Para el ejercicio se utiliza la representaci ́on de Arbol Bina-  ́
;rio de B ́usqueda con Listas en Racket, y podr ́ıa representarse con la ayuda
;de la siguiente gram ́atica BNF:
;< ́arbol-binario> := ( ́arbol-vac ́ıo) empty
;:= (nodo) n ́umero < ́arbol-binario> < ́arbol-binario>
;Es decir que este Arbol Binario de B ́usqueda, representado en Racket con  ́
;listas y usando la anterior gram ́atica, ser ́ıa:
;6
;’(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ())))

(define (reversar-lista lista1 lista2)
  (cond
    [(empty? lista1) lista2]
    [else (reversar-lista (cdr lista1) (cons (car lista1) lista2))]))

(define (left-right num BST result)
  (cond
    [(empty? BST) "The number is not found in the tree"]
    [(equal? (car BST) num) (reversar-lista result '())]
    [(> num (car BST)) (left-right num (car (cddr BST)) (append result (list (string-append  "right"))))]
    [else (left-right num (car (cdr BST)) (append result (list (string-append "left"))))]))

(define (path num BST)
  (cond
    [(empty? BST) empty]
    [(not (number? num)) "error"]
    [(equal? (car BST) num) empty]
    [else (left-right num BST empty)]))

  

(check-expect (path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ())))) (list "left" "left" "right"))
(check-expect (path 14 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ())))) '())
(check-expect (path 1 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ())))) "The number is not found in the tree")
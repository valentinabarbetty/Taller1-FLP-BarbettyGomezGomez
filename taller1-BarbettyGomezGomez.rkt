#lang eopl
;; Taller 1
;; Valentina Barbetty Arango
;; Brayan Julio Gómez - 2310016
;; Jheison Gómez

;; PUNTO 1
;;
;; invert : Funcion principal
;; Proposito:
;; L -> L' : Procedimiento que recibe una lista con pares x,y e invierte esos pares de forma que queden y,x
;;
;; <lista> := ()
;;         := (<lista>)


(define invert
  (lambda (L)
    (cond
      [(null? L) '()]
       [else (cons (list (cadr (car L)) (car (car L)))(invert (cdr L)))]))) 


;; Pruebas
(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))


;; PUNTO 2
;;
;; down : Funcion principal
;; Proposito:
;; L -> L' : Procedimiento que recibe una lista y a cada elemento de esa lista le añade un nivel más de paréntesis
;;
;; <lista> := ()
;;         := (<lista>)

(define down
  (lambda (L)
    (cond
      [(null? L) '()]
        [else (cons (list (car L))(down (cdr L)))]))) 

;; Pruebas
(down '(1 2 3))
(down '((una) (buena) (idea)))
(down '(un (objeto (mas)) complicado))


;; PUNTO 4
;;
;; filter-in : Funcion principal
;; Proposito:
;; P x L -> L' : Procedimiento que recibe una lista y un predicado, a cada elemento de esa lista va a verificar si cumple el predicado
;; si lo cumple, lo añade a una lista
;;
;; <lista> := ()
;;         := (<predicado> <lista>)

(define filter-in
  (lambda (P L)
    (cond
      [(null? L)'()]
      [(P (car L)) (cons (car L) (filter-in P (cdr L)))] 
      [else (filter-in P (cdr L))])))

;; pruebas
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))


;;PUNTO 5
;;
;;list-index: Funcion principal
;; Proposito:
;; P x L -> INT | BOOLEAN: La función "list-index" es buscar el primer elemento en una lista L que cumple con un cierto predicado P
;; y devolver la posición de ese elemento en la lista, comenzando desde la posición 0. Si ningún elemento en la lista cumple con el
;; predicado P, la función debe retornar #f.
;;
;;<List> := ()
;;       := (<Scheme-value> <lista>)

(define list-index
  (lambda (P L)
    (cond
      [(null? L) #f]             
      [(P (car L)) 0]              
      [else
       (cond [(not (equal? (list-index P (cdr L)) #f))  (+ 1 (list-index P (cdr L))) ]
         [else #f]
                              
       )]
    )
  )
)

;;pruebas
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))

;; Punto 7
;; cart-prod-helper : Funcion auxiliar
;; Proposito:
;; x X L -> L' : Procedimiento que recibe un elemento x y lo agrega a cada elemento de la lista.
;
;; <lista> := ()
;;         := (<int> <lista>)

(define cart-prod-helper
  (lambda (x L)
    (cond [(null? L) '()]
          [else (cons (list x (car L)) (cart-prod-helper x (cdr L)))]
          )
    )
  )
;; append-list : Funcion auxiliar
;; Proposito:
;; L1 X L2 -> L' : Procedimiento que recibe dos listas y las une en una lista.
;
;; <lista> := ()
;;         := (<lista> <lista>)


(define (append-list lista1 lista2)
  (cond
    [(null? lista1) lista2]
    [else (cons (car lista1) (append-list (cdr lista1) lista2))]))



;; pruebas
(cart-prod-helper 2 '(a b c))
(cart-prod-helper  2'(p q r))

;; cartesian-product : Funcion principal
;; Proposito:
;; L1 x L2 -> L' : Procedimiento que recibe dos listas y realiza el producto cartesiano entre ellas
;;
;; <lista> := ()
;;         := (<lista> <lista>)

(define cartesian-product
  (lambda (L1 L2)
    (cond [(null? L1) '()]
          [(null? L2) '()]
          [else (append-list (cart-prod-helper (car L1) L2) (cartesian-product (cdr L1) L2))]
          )
    )
  )

;; pruebas
(cartesian-product '(4 5) '(6 7))
(cartesian-product '(2) '(6 7 8 9))



;;PUNTO 9
;;
;;validarLista: Funcion auxiliar 
;;Proposito:
;; L x INT -> INT: La función "validarLista:" tiene como propósito verificar si el primer elemento de la lista
;;es mayor que los elementos restantes en la lista. Cuenta el número de inversiones, donde una inversión
;;se produce cuando un elemento en una posición anterior en la lista es mayor que un elemento en una posición posterior.
;;<List> := ()
;;       := (<lista> <int>)

(define validarLista
  (lambda (L numero)
    (cond
      [(null? L) 0]
      [(> numero (car L)) (+ 1 (validarLista (cdr L) numero))]
      [else (+ (validarLista (cdr L) numero))])))

;;pruebas
(validarLista '(1 2 3) 2)
(validarLista '(1 2 3) 1)

;;inversions: Funcion principal
;;Proposito:
;; L -> INT: La función "inversions" tiene como objetivo calcular el número de inversiones en una lista de números.
;;Una inversión se produce cuando, al comparar las posiciones i y j en la lista, se cumple que i < j y el valor en la posición i es mayor que 
;;el valor en la posición j. Esta función utiliza un enfoque recursivo para contar todas las inversiones presentes en la lista hasta que se haya procesado por completo.
;;Su resultado proporciona una medida de cuántas veces ocurren inversiones en la lista,
;;
;;<List> := ()
;;       := (<lista>)

(define inversions
  (lambda (L)
    (cond
      [(null? L) 0]
      [else (+ (validarLista (cdr L) (car L)) (inversions (cdr L)))]
    )
  )
)

;;pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))

;;PUNTO 11
;;
;length-list: funcion auxiliar
;Proposito:
;L -> int :  La funcion "length-list" tiene como propósito calcular la longitud (cantidad de elementos) de una lista.
;;<List> := ()
;;       := (<lista>)
(define length-list
  (lambda (L)
    (cond [(null? L) 0]
          [else (+ 1 (length (cdr L)))]
    )
  )
)

;;pruebas
(length-list '(1 2 3))
(length-list '(1 3))

;perform-operation: funcion auxiliar
;Proposito: 
;F x L1 x L2 -> L3: Los valores de entrada son una operación binaria (denominada F) y dos listas, y la función devuelve como resultado otra lista.
;;<List> := ()
;;       := (<Scheme-value> <lista> <lista>)
(define perform-operation
  (lambda (F L1 L2)
       (cond [(null? L1) empty]
             [(and (eq? F /) (= 0 (car L2))) (cons "Error" (perform-operation F (cdr L1) (cdr L2)))]
             [else (cons (F (car L1) (car L2)) (perform-operation F (cdr L1) (cdr L2)))]
             )
    )
  )
;;pruebas
(perform-operation * '(1 2 3) '(4 5 6))
(perform-operation + '(1 2 3) '(4 5 6))

;;zip: Funcion principal
;;Proposito:
;; F L1 L2 -> L3: La función "perform-operation" tiene como propósito realizar una operación binaria (representada por la función F)
;en dos listas (L1 y L2) elemento por elemento y devolver una nueva lista que contiene los resultados de aplicar F a los elementos correspondientes de las dos listas.
;;<List> := ()
;;       := (<Scheme-value> <lista> <lista>)
(define zip
  (lambda (F L1 L2)
  (cond [(not (procedure? F)) "Not an operator"]
        [(not (= (length-list L1) (length-list L2))) "Lists of different lengths"]
        [else (perform-operation F L1 L2)]
        )
  )
)
;;pruebas
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))


;;PUNTO 12
;;
;;filter-acum: Funcion principal
;; Proposito:
;;a b F acum filter -> acum: El propósito de la función "filter-acum" es aplicar una función binaria (F) a todos los elementos que se encuentran dentro de un intervalo dado [a, b],
;; y además, estos elementos deben cumplir con un cierto criterio especificado por una función unaria (filter).
;; El resultado de cada aplicación de F se acumula en un valor inicial llamado "acum," y al final de este proceso, la función retorna el valor final de "acum."
;;<valor-de-scheme> := ()
;;       := (<int> <int> <valor-de-scheme> <int><valor-de-scheme>)
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

(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)


;;PUNTOS 13
;;
;resultado: Funcion auxiliar
;;Proposito:
;lrators lrands acum -> INT :tiene el propósito de realizar la operación binaria entre los elementos de las listas lrators y lrands, acumulando los resultados parciales en la variable acum.
;A medida que procesa los elementos de ambas listas de manera recursiva, su objetivo es calcular un resultado final que represente la aplicación sucesiva de
;operadores binarios a operandos correspondientes en las listas dadas.
;;<List> := ()
;;       := (<lista><lista><int>)

(define resultado
  (lambda (lrators lrands acum)
    (cond
     [(null? lrators) acum]  
     [(null? lrands) acum]    
     [else (resultado (cdr lrators) (cdr lrands) ((car lrators) acum (car lrands)))]
     )))

(resultado (list * + - *) '(1 2 8 4 11 6) 0)
(resultado (list * / - *) '(1 1 1 1 ) 0)

;;operate:Funcion principal
;;Proposito:
;lrators lrands ->la función "(operate lrators lrands)" es tomar dos listas como entrada: "lrators," que contiene funciones binarias, y "lrands,"
;;que contiene números. Luego, la función realiza una serie de operaciones sucesivas, aplicando las funciones en "lrators" a los valores en "lrands"
;;de manera secuencial. El resultado final de estas operaciones se devuelve como el resultado de la función.
;;<List> := ()
;;       := (<lista><lista>)
(define operate
  (lambda (lrators lrands)
  (cond
    [(and (null? (car lrators)) (null?(car lrands))) 0]
    [(not (equal? (- (length-list lrands) (length-list lrators)) 1)) "Lists of different lengths"]
    [(not (procedure? (car lrators)))  "Not an operator"]
    [else (resultado (cdr lrators) (cddr lrands) ((car lrators) (car lrands) (car (cdr lrands))))]
  )))
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list * *) '(1 1 2))


;;PUNTO 14
;;
;reversar-lista: funcion  auxiliar
;Proposito: 
;lista1 x lista2 -> lista3 : Tiene como proposito tomar una lista "lista1" y construir una nueva lista "lista2" con los datos invertidos
;;<List> := ()
;;       := (<lista><lista>)
(define (reversar-lista lista1 lista2)
  (cond
    [(null? lista1) lista2]
    [else (reversar-lista (cdr lista1) (cons (car lista1) lista2))]))

;left-right: funcion auxiliar
;proposito: 
;num x BST x result -> lista3 : El proposito de la funcion "reversar-lista" se utiliza para encontrar la ruta desde la raíz de un árbol binario
;de búsqueda hasta un número específico num dentro del árbol. Su propósito es determinar si el número está en el árbol y,
;si es así, proporcionar la ruta (indicada por cadenas "left" y "right") desde la raíz hasta ese número.

;<arbol-binario> ::=()
;;               ::= <int>
;;               ::(<int> <arbol-binario><arbol-binario>)
(define (left-right num BST result)
  (cond
    [(null? BST) "The number is not found in the tree"]
    [(equal? (car BST) num) (reversar-lista result '())]
    [(> num (car BST)) (left-right num (car (cddr BST)) (append-list result (list (string-append  "right"))))]
    [else (left-right num (car (cdr BST)) (append-list result (list (string-append "left"))))]))

; number x Arbol(BST) -> list(string): La función "path" es calcular y devolver la ruta desde el nodo raíz de un árbol binario de búsqueda (BST)
;hasta un número entero específico "n," representado como una lista de instrucciones "left" y "right."
;Esta ruta indicará cómo llegar desde el nodo raíz hasta el nodo que contiene el número "n" en el árbol. Si el número "n" se encuentra en el nodo raíz,
;la función retornará una lista vacía para indicar que no se necesita moverse desde la raíz.
;<arbol-binario> ::=()
;;               ::= <int>
;;               ::(<int> <arbol-binario><arbol-binario>)
(define (path num BST)
  (cond
    [(null? BST) empty]
    [(not (number? num)) "error"]
    [(equal? (car BST) num) empty]
    [else (left-right num BST empty)]))

  
(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))))
(path 14 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))))


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



(define (cero-izq lst)
  (cons 0 lst))

;; cero-der:
;; Proposito:
;; L -> L': Procedimiento que recibe una lista
;; y añade un cero en la ultima posición de esta
;;



(define (cero-der lst)
  (append lst (list 0)))


;; pascal: 
;; Proposito:
;; x -> L: Procedimiento que recibe un número y calcula los números pascal
;; hasta la fila x
;;
;; <lista> := ()
;;         := (<int>)


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
(pascal 3)


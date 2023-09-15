#lang eopl
;; Taller 1
;; Integrantes
;; Valentina Barbetty Arango - 2310050
;; Brayan Julio Gómez - 2310016
;; Jheison Gómez - 2310215

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

;; PUNTO 3
;;
;; list-set : Funcion principal
;; Proposito:
;; L x N x X -> L :  la función list-set que toma una lista L, un número n y un elemento x, y
;; devuelve una lista similar a L, pero con x en la posición n (índice 0) de la lista.
;;
;; <lista> := ()
;;         := (<lista>)

(define (list-set L n x)
  (cond [(null? L) '() ]  
     [(= n 0) (cons x (cdr L))]
        [else (cons (car L) (list-set (cdr L) (- n 1) x))]))

;; pruebas
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))

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


;; PUNTO 5
;;
;; list-index: Funcion principal
;; Proposito:
;; P x L -> INT | BOOLEAN: La función "list-index" que, dado a un predicado P y una lista L, encuentra la posición del
;; primer elemento en L que cumple con P (comenzando desde 0) o devuelve #f si ninguno cumple.
;;
;; <List> := ()
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

;; pruebas
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))

;; Punto 6
;; swapper : Funcion principal
;; Proposito:
;; E1 x E2 x L -> L : la función swapper que toma tres argumentos: dos elementos E1 y E2, y una lista L.
;; La función debe devolver una lista similar a L, pero intercambiando todas las ocurrencias de E1 por
;; E2 y todas las ocurrencias de E2 por E1. Los elementos E1 y E2 deben estar presentes en la lista L.
;
;; <lista> := ()
;;         := (<int> <lista>)

(define (swapper E1 E2 L)
  (define (swap-elements elem)
    (if (equal? elem E1)
        E2
        (if (equal? elem E2)
            E1
            elem)))

               (define (swap-list L)
                 (if (null? L)'()      
                      (cons (swap-elements (car L)) (swap-list (cdr L)))))
                          (swap-list L))


;; pruebas
(swapper 'a 'd '(a b c d)) 
(swapper 'a 'd '(a d () c d)) 

;; Punto 7
;; cart-prod-helper : Funcion auxiliar
;; Proposito:
;; X x L -> L' : Procedimiento que recibe un elemento x y lo agrega a cada elemento de la lista.
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
;; prueba
(cart-prod-helper 1 '(1 2 3))
(cart-prod-helper 3 '(6 8 7 9))

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

;; PUNTO 8
;;
;; mapping Funcion principal 
;; Proposito:
;; F x L1 x L2 -> L3:la función mapping que recibe tres argumentos: una función unaria llamada F,
;; y dos listas de números L1 y L2. La función debe devolver una lista de pares (a, b) donde a es
;; un elemento de L1 y b es un elemento de L2, y se cumple la propiedad de que al aplicar la función
;; unaria F a a, obtendremos b como resultado, es decir, F(a) = b. Las dos listas deben tener la misma longitud.

;; <List> := ()
;;       := (<lista> <int>)
(define (mapping F L1 L2)
    (cond ((null? L1) '())
      ((null? L2) '())
         ((= (F (car L1))
           (car L2))
             (cons (list (car L1) (car L2))
               (mapping F (cdr L1) (cdr L2))))
                 (else (mapping F (cdr L1) (cdr L2)))))       


;; pruebas
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6)) 
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6)) 
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12)) 

;; PUNTO 9
;;
;; validarLista: Funcion auxiliar 
;; Proposito:
;; L x INT -> INT: La función "validarLista:" tiene como propósito verificar si el primer elemento de la lista
;; es mayor que los elementos restantes en la lista. Cuenta el número de inversiones, donde una inversión
;; se produce cuando un elemento en una posición anterior en la lista es mayor que un elemento en una posición posterior.
;; <List> := ()
;;       := (<lista> <int>)

(define validarLista
  (lambda (L numero)
    (cond
      [(null? L) 0]
      [(> numero (car L)) (+ 1 (validarLista (cdr L) numero))]
      [else (+ (validarLista (cdr L) numero))])))

;; pruebas
(validarLista '(1 2 3) 2)
(validarLista '(1 2 3) 1)

;; inversions: Funcion principal
;; Proposito:
;; L -> INT: La función inversions que toma una lista L como entrada y determina el número de inversiones
;; en esa lista. En términos formales, si tenemos una lista A = (a1, a2, ..., an) de n números
;; distintos, y si i < j (posición) y ai > aj (valor en la posición), entonces el par (i, j) representa una inversión en A.
;; Se debe contar y devolver el número total de inversiones en la lista L
;;
;; <List> := ()
;;       := (<lista>)

(define inversions
  (lambda (L)
    (cond
      [(null? L) 0]
      [else (+ (validarLista (cdr L) (car L)) (inversions (cdr L)))]
    )
  )
)

;; pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))

;; PUNTO 10
;;
;; up : Funcion principal
;; Proposito:
;; X x L -> L' : la función up que toma una lista L como entrada y elimina un par de paréntesis de
;; cada elemento del nivel más alto de la lista. Si un elemento de ese nivel no es una lista (no tiene paréntesis),
;; se incluirá en la salida resultante sin cambios.
;
;; <lista> := ()
;;         := (<int> <lista>)

(define (up L)
  (cond
    ((null? L) '())
       ((list? (car L)) 
          (append (car L) (up (cdr L)))) 
             (else
                (cons (car L) (up (cdr L))))))


;; pruebas
(up '((1 2) (3 4)))
(up '((x (y)) z))

;; PUNTO 11
;;
;; length-list: funcion auxiliar
;; Proposito:
;; L -> INT :  La funcion "length-list" tiene como propósito calcular la longitud (cantidad de elementos) de una lista.
;; <List> := ()
;;       := (<lista>)

(define length-list
  (lambda (L)
    (cond [(null? L) 0]
          [else (+ 1 (length (cdr L)))]
    )
  )
)

;; pruebas
(length-list '(1 2 3))
(length-list '(1 3))

;; perform-operation: funcion auxiliar
;; Proposito: 
;; F x L1 x L2 -> L3: Los valores de entrada son una operación binaria (denominada F) y dos listas, y la función devuelve como resultado otra lista.
;; <List> := ()
;;       := (<Scheme-value> <lista> <lista>)

(define perform-operation
  (lambda (F L1 L2)
       (cond [(null? L1) empty]
             [(and (eq? F /) (= 0 (car L2))) (cons "Error" (perform-operation F (cdr L1) (cdr L2)))]
             [else (cons (F (car L1) (car L2)) (perform-operation F (cdr L1) (cdr L2)))]
             )
    )
  )
;; pruebas
(perform-operation * '(1 2 3) '(4 5 6))
(perform-operation + '(1 2 3) '(4 5 6))

;; zip: Funcion principal
;; Proposito:
;; F x L1 x L2 -> L3: La función "perform-operation" tiene como propósito realizar una operación binaria (representada por la función F)
;; en dos listas (L1 y L2) elemento por elemento y devolver una nueva lista que contiene los resultados de aplicar F a los elementos correspondientes de las dos listas.
;; <List> := ()
;;       := (<Scheme-value> <lista> <lista>)

(define zip
  (lambda (F L1 L2)
  (cond [(not (procedure? F)) "Not an operator"]
        [(not (= (length-list L1) (length-list L2))) "Lists of different lengths"]
        [else (perform-operation F L1 L2)]
        )
  )
)
;; pruebas
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))


;; PUNTO 12
;;
;; filter-acum: Funcion principal
;; Proposito:
;; A x B x F x ACUM x FILTER -> INT: El propósito de la función "filter-acum" es aplicar una función binaria (F) a todos los elementos que se encuentran dentro de un intervalo dado [a, b],
;; y además, estos elementos deben cumplir con un cierto criterio especificado por una función unaria (filter).
;; El resultado de cada aplicación de F se acumula en un valor inicial llamado "acum," y al final de este proceso, la función retorna el valor final de "acum."
;; <valor-de-scheme> := ()
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

;; pruebas
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)


;; PUNTOS 13
;;
;; resultado: Funcion auxiliar
;; Proposito:
;; L1 x L2 x INT -> INT :tiene el propósito de realizar la operación binaria entre los elementos de las listas lrators y lrands, acumulando los resultados parciales en la variable acum.
;; A medida que procesa los elementos de ambas listas de manera recursiva, su objetivo es calcular un resultado final que represente la aplicación sucesiva de
;; operadores binarios a operandos correspondientes en las listas dadas.
;; <List> := ()
;;       := (<lista><lista><int>)

(define resultado
  (lambda (lrators lrands acum)
    (cond
     [(null? lrators) acum]  
     [(null? lrands) acum]    
     [else (resultado (cdr lrators) (cdr lrands) ((car lrators) acum (car lrands)))]
     )))

;; pruebas
(resultado (list * + - *) '(1 2 8 4 11 6) 0)
(resultado (list * / - *) '(1 1 1 1 ) 0)

;; operate: Funcion principal
;; Proposito:
;; L1 x L2 -> INT: La función "(operate lrators lrands)" es tomar dos listas como entrada: "lrators," que contiene funciones binarias, y "lrands,"
;; que contiene números. Luego, la función realiza una serie de operaciones sucesivas, aplicando las funciones en "lrators" a los valores en "lrands"
;; de manera secuencial. El resultado final de estas operaciones se devuelve como el resultado de la función.
;; <List> := ()
;;       := (<lista><lista>)
(define operate
  (lambda (lrators lrands)
  (cond
    [(and (null? (car lrators)) (null?(car lrands))) 0]
    [(not (equal? (- (length-list lrands) (length-list lrators)) 1)) "Lists of different lengths"]
    [(not (procedure? (car lrators)))  "Not an operator"]
    [else (resultado (cdr lrators) (cddr lrands) ((car lrators) (car lrands) (car (cdr lrands))))]
  )))
;; pruebas
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list * *) '(1 1 2))


;; PUNTO 14
;;
;; reversar-lista: funcion  auxiliar
;; Proposito: 
;; L1 x L2 -> L3 : La funcion reversar-lista: toma una lista "lista1" y construir una nueva lista "lista2" con los datos invertidos
;; <List> := ()
;;       := (<lista><lista>)
(define (reversar-lista lista1 lista2)
  (cond
    [(null? lista1) lista2]
    [else (reversar-lista (cdr lista1) (cons (car lista1) lista2))]))
;; pruebas
(reversar-lista '(1 2 3) '())
(reversar-lista '(9 5 1 7 5 3) '())

;; left-right: funcion auxiliar
;; proposito: 
;; INT x BST x L1 -> L3 : La funcion "left-right:" se utiliza para encontrar la ruta desde la raíz de un árbol binario
;; de búsqueda hasta un número específico num dentro del árbol. Su propósito es determinar si el número está en el árbol y,
;; si es así, proporcionar la ruta (indicada por cadenas "left" y "right") desde la raíz hasta ese número.
;; <arbol-binario> ::=()
;;               ::= <int>
;;               ::(<int> <arbol-binario><arbol-binario>)

(define (left-right num BST result)
  (cond
    [(null? BST) "The number is not found in the tree"]
    [(equal? (car BST) num) (reversar-lista result '())]
    [(> num (car BST)) (left-right num (car (cddr BST)) (append-list result (list (string-append  "right"))))]
    [else (left-right num (car (cdr BST)) (append-list result (list (string-append "left"))))]))

;; pruebas
(left-right 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))) '())
(left-right 31 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))) '())

;; path: funcion principal
;; proposito:
;; INT x ARBOL(BST) -> L: La función "path" es calcular y devolver la ruta desde el nodo raíz de un árbol binario de búsqueda (BST)
;; hasta un número entero específico "n," representado como una lista de instrucciones "left" y "right."
;; Esta ruta indicará cómo llegar desde el nodo raíz hasta el nodo que contiene el número "n" en el árbol. Si el número "n" se encuentra en el nodo raíz,
;; la función retornará una lista vacía para indicar que no se necesita moverse desde la raíz.
;; <arbol-binario> ::=()
;;               ::= <int>
;;               ::(<int> <arbol-binario><arbol-binario>)

(define (path num BST)
  (cond
    [(null? BST) empty]
    [(not (number? num)) "error"]
    [(equal? (car BST) num) empty]
    [else (left-right num BST empty)]))

;; pruebas
(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))))
(path 14 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))))

;; PUNTO 15
;; validarParImpar: Funcion auxiliar
;; proposito:
;; INT -> F | V : La función validarParImpar toma un número como entrada y verifica si es par o impar


(define validarParImpar
  (lambda (num)
    (cond [(= (remainder num 2)0) #true]
          [else #false])
              ))
;; pruebas
(validarParImpar 2)
(validarParImpar 3)

;; validarParImpar: Funcion auxiliar
;; proposito:
;; ARBOL -> INT : La función calcularPar busca en el arbol todo los pares que encuentre y hace un conteo retornando los pares
;; encontrados.

(define calcularPar
  (lambda (arbol)
    (cond[(null? arbol)  0 ]
         [(validarParImpar (car arbol))  (+ 1 (calcularPar (cadr arbol)) (calcularPar (caddr arbol)))]
         [else   (+ 0(calcularPar (cadr arbol)) (calcularPar (caddr arbol)))]
     )
  )
)
;; pruebas
(calcularPar '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))))
(calcularPar '(2 (4 () (10 () ())) (26 (20 (14 () ())()) (31 () ()))))

;; calcularImpar Funcion auxiliar
;; proposito:
;; ARBOL -> INT : La función calcularImpar busca en el arbol todo los impares que encuentre y hace un conteo retornando los impares
;; encontrados.
(define calcularImpar
  (lambda (arbol)
    (cond[(null? arbol)  0 ]
         [(not (validarParImpar (car arbol)))  (+ 1 (calcularImpar (cadr arbol)) (calcularImpar (caddr arbol)))]
         [else   (+ 0(calcularImpar (cadr arbol)) (calcularImpar (caddr arbol)))]
    )
  )
)
;; pruebas
(calcularImpar '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))))
(calcularImpar '(3 (4 () (11 () ())) (26 (21 (14 () ())()) (31 () ()))))

;; count-odd-and-even Funcion principal
;; proposito:
;; ARBOL -> L1 : La función count-odd-and-even que toma un árbol binario como entrada y
;; devuelve una lista con dos elementos, representando la cantidad de números pares e impares en el árbol. 
(define count-odd-and-even
  (lambda (arbol)
    (cond [(null? arbol) "Arbol vacio"]
          [else (cons (calcularImpar arbol) (cons (calcularPar arbol) empty))]
    )
   )
)

;; pruebas
(count-odd-and-even '(3 (4 () (11 () ())) (26 (21 (14 () ())()) (31 () ()))))
(count-odd-and-even '(2 (4 () (10 () ())) (26 (20 (14 () ())()) (31 () ()))))

;; PUNTO 16
;; Operar-binarias Funcion principal
;; proposito:
;; ARBOL -> INT : r la función Operar-binarias que toma una operación binaria válida como entrada y devuelve el
;; resultado de realizar operaciones de suma, resta y multiplicación correspondientes según la operación proporcionada.

(define (Operar-binarias operacionB)
   (cond
     ((number? operacionB) operacionB) 
       ((list? operacionB)
        (let* ((operador (cadr operacionB))
          (op1 (Operar-binarias (car operacionB)))
            (op2 (Operar-binarias (caddr operacionB))))          
               (cond              
                   ((equal? operador 'suma) (+ op1 op2))
                     ((equal? operador 'resta) (- op1 op2))
                        ((equal? operador 'multiplica) (* op1 op2))                   
                            (else ( "Operador desconocido")))))
                                   (else ( "La operacion no es válida"))))

;; pruebas
(Operar-binarias '((2 multiplica (4 suma 1))multiplica((2 multiplica 4) resta 1)))
(Operar-binarias '((2 multiplica 3) suma (5 resta 1)))
;; PUNTO 17
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


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname dANAcEJAS) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(require racket/bool)

(define MENSAJE1 (list 0 0 1 1 0 0 0 1 1 1 0))
(define MENSAJE2 (list 1 0 1 0 0 1 0 1 0 0 1))
(define MENSAJE3 (list 1 0 1 0 0 1 0 1 0 0 0))

;n-bits-redundancia: Number -> Number
(define (n-bits-redundancia n)
  (local [(define aprox (inexact->exact (ceiling (/ (log (+ n 1)) (log 2)))))]
    (if (>= (expt 2 aprox) (+ n aprox 1))
        aprox
        (+ aprox 1))))

;tamaño-bloque: Number -> Number
(define (tamaño-bloque largo-mensaje)
  (+ largo-mensaje (n-bits-redundancia largo-mensaje) 1))

;posiciones: Number -> List(Number)
(define (posiciones n)
  (range 0 n 1))

;bloque-a-bits-redundancia: Number -> Number
(define (bloque-a-bits-redundancia n)
  (/ (log n) (log 2)))

(define-struct Par [p s])
;Par es un (Any, Any)
;p es la primera componente del par
;s es la segunda componente del par

;============================= 1 ===============================;

;Diseñe una función que dada una lista de ceros y unos (un mensaje)
;devuelva un 1 si la cantidad de unos en la lista es impar y 0 en caso contrario.

;--- DISE;O DE DATOS ---;
;la lista de 0s y 1s sera una ListOf(Number) cada number 0 o 1

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;paridad: ListOf(Number) -> Number

;dada una lista de numeros cuento la cantidad total de 1s y devuelvo 1 si es impar o 0 si es par

;--- EJEMPLOS ---;
;in: (list 1 1 0 0 1 0) out: 1
;in: (list 1 0 0 0 1 0) out: 0

;--- DEFINICION ---;
(define (paridad l)
  (local [(define (contar1 li)
            (cond [(empty? li) 0]
                  [(cons? li) (if (= (first li) 1)
                                  (+ 1 (contar1 (rest li)))
                                  (contar1 (rest li)))]))]

    (if (odd? (contar1 l))
        1
        0)))

(define (paridad2 l)
  (local [(define (uno? x)
            (= x 1))]
  (if (odd? (foldr + 0 (filter uno? l)))
      1
      0)))

;--- EVALUACION ---;
(check-expect (paridad (list 1 1 0 0 1 0)) 1)
(check-expect (paridad (list 1 0 0 0 1 0)) 0)
(check-expect (paridad2 (list 1 1 0 0 1 0)) 1)
(check-expect (paridad2 (list 1 0 0 0 1 0)) 0)



;============================ 2 ================================;

;--- DISE;O DE DATOS ---;
;represento el n natural como Natural y la lista del numero en binario como ListOf(Number)

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;natural->binario: Natural -> ListOf(Number)

;dado un número natural n, devuelvo una lista de ceros y unos que representa n en binario,
;comenzando desde la cifra menos significativa.

;--- EJEMPLOS ---;
;in: 3     out: (list 1 1)
;in: 5     out: (list 1 0 1)
;in: 6     out: (list 0 1 1)
;in: 34    out: (list 0 1 0 0 0 1)

;--- DEFINICION ---;
(define (natural->binario n)
  (cond [(zero? n) (cons 0 empty)]
        [(zero? (sub1 n)) (cons 1 empty)]
        [else (cons (remainder n 2) (natural->binario (quotient n 2)))]))

(check-expect (natural->binario 3)  (list 1 1))
(check-expect (natural->binario 5)  (list 1 0 1))
(check-expect (natural->binario 6)  (list 0 1 1))
(check-expect (natural->binario 34) (list 0 1 0 0 0 1))

;============================= 3 ===============================;

;--- DISE;O DE DATOS ---;
;represento la lista de 0s y 1s como una ListOf(Number) y al numero que devuelve
;como un natural

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;binario->natural: ListOf(Number) -> Natural

;dada una lista de ceros y unos que representa un número en binario
;desde la cifra menos significativa, devuelvo el número natural equivalente.

;--- EJEMPLOS ---;
;in: (list 1 1)         out: 3
;in: (list 1 0 1)       out: 5
;in: (list 0 1 1)       out: 6
;in: (list 0 1 0 0 0 1) out: 34
;
; Estrategia: cada bit en posición i contribuye bit * 2^i.
; Usamos un acumulador de posición.

;--- DEFINCION ---;
(define (binario->natural l)
  (local [(define (aux lb pos)
            (cond
              [(empty? lb) 0]
              [(cons? lb) (+ (* (first lb) (expt 2 pos))
                       (aux (rest lb) (+ pos 1)))]))]
    (aux l 0)))

;--- EVALUACION ---;
(check-expect (binario->natural (list 1 1))         3)
(check-expect (binario->natural (list 1 0 1))       5)
(check-expect (binario->natural (list 0 1 1))       6)
(check-expect (binario->natural (list 0 1 0 0 0 1)) 34)

;============================= 4 ===============================;

;--- DISE;O DE DATOS ---;
;represento la lista de 0s y 1s como ListOf(Number) y al numero natural como Natural

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;uno-en-pos?: ListOf(Number) natural -> Boolean

;dado un número binario (lista desde cifra menos significativa)
;y una posición p, determina si hay un 1 en la posición p.
;Si p es mayor o igual al largo de la lista, se considera que hay un 0 (extensión con ceros).

;--- EJEMPLOS ---;
;in: (list 1 0 0) 0      out: #true
;in: (list 0 1 0) 1      out: #true
;in: (list 0 0 1) 2      out: #true
;in: (list 1 1 1) 3      out: #false
;in: (list 1 1 1) 100    out: #false

(define (uno-en-pos? l p)
  (cond
    [(empty? l) #false]         
    [(zero? p) (= (first l) 1)]
    [(positive? p) (uno-en-pos? (rest l) (- p 1))]))

;--- EVALUACION ---;
(check-expect (uno-en-pos? (list 1 0 0) 0)   #true)
(check-expect (uno-en-pos? (list 0 1 0) 1)   #true)
(check-expect (uno-en-pos? (list 0 0 1) 2)   #true)
(check-expect (uno-en-pos? (list 1 1 1) 3)   #false)
(check-expect (uno-en-pos? (list 1 1 1) 100) #false)

;============================= 5 ===============================;

;--- DISE;O DE DATOS ---;
;represento el tamaño del bloque como un Natural
;las posiciones como ListOf(Natural)
;el resultado como ListOf(ListOf(Natural))

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;obtener-grupos: Natural -> ListOf(ListOf(Natural))

;dado el tamaño de bloque de un mensaje con corrección Hamming,
;devuelvo una lista de listas donde cada sublista contiene las posiciones
;asociadas a un bit de paridad. Cada bit de paridad está incluido en su propio grupo.

;--- EJEMPLOS ---;
;in: 16 out: (list (list 1 3 5 7 9 11 13 15)
;                  (list 2 3 6 7 10 11 14 15)
;                  (list 4 5 6 7 12 13 14 15)
;                  (list 8 9 10 11 12 13 14 15))

;--- DEFINICION ---;
(define (obtener-grupos n)
  (local [
    ;todas las posiciones del 0 al tamaño del bloque
    (define todas-pos (posiciones n))
    
    ;convertimos cada posicion a su representacion en binario
    (define pos-en-binario (map natural->binario todas-pos))
    
    ;cantidad de bits de paridad para saber cuantos grupos armar
    (define n-paridades (round (bloque-a-bits-redundancia n)))
    
    ;por cada bit de paridad, nos quedamos con las posiciones
    ;que tienen un 1 en ese bit (ultimo para bit 1, penultimo para bit 2, etc)
    ;y las convertimos de vuelta a naturales
    (define (grupo-para-bit p)
      (local [(define (tiene-uno-en-p? bin)
        (uno-en-pos? bin p))]
      (map binario->natural (filter tiene-uno-en-p? pos-en-binario))))]
    
    ; 5. construimos un grupo para cada bit de paridad
    (map grupo-para-bit (posiciones n-paridades))))

;--- EVALUACION ---;
(check-expect (obtener-grupos 16)
              (list (list 1 3 5 7 9 11 13 15)
                    (list 2 3 6 7 10 11 14 15)
                    (list 4 5 6 7 12 13 14 15)
                    (list 8 9 10 11 12 13 14 15)))

;============================ 6 ================================;

;--- DISE;O DE DATOS ---;
;represento la lista de valores como ListOf(Number) y la lista de positciones como
;ListOf(Number)

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;valores-en-posiciones: ListOf(Number) ListOf(Number) -> ListOf(Number)

;dada una lista de valores lv y una lista de posiciones ln (ordenada ascendentemente),
;devuelvo los elementos de lv que están en las posiciones indicadas.

;--- EJEMPLOS ---;
;in: (list 12 21 34 47) (list 0 1 2)       out: (list 12 21 34)
;in: empty (list 0 1 2)                    out: empty
;in: (list "a" "b" "c") empty              out: empty
;in: (list 1 "d" #true) (list 0 1 3 5)     out: (list 1 "d")

;--- DEFINICION ---;
(define (valores-en-posiciones lv ln)
  (local [(define (aux l ps pos-actual)
            (cond
              [(or (empty? l) (empty? ps)) empty]
              [(= (first ps) pos-actual)
               (cons (first l)
                     (aux (rest l) (rest ps) (+ pos-actual 1)))]
              [else
               (aux (rest l) ps (+ pos-actual 1))]))]
    (aux lv ln 0)))

;--- EVALUACION ---;
(check-expect (valores-en-posiciones (list 12 21 34 47) (list 0 1 2))     (list 12 21 34))
(check-expect (valores-en-posiciones empty (list 0 1 2))                  empty)
(check-expect (valores-en-posiciones (list "a" "b" "c") empty)            empty)
(check-expect (valores-en-posiciones (list 1 "d" #true) (list 0 1 3 5))  (list 1 "d"))

;============================= 7 ===============================;
 
;--- DISE;O DE DATOS ---;
;represento el numero como Number y el resultado como Boolean

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;es-potencia-de-dos?: Number -> Boolean

;dado un numero devuelve #true si el número es 0 o potencia de 2, #false en otro caso
;buscando en la web, una muy buena forma para saber si un numero en potencia de otro es
;chequear si and n (- n 1) = 0

;--- EJEMPLOS ---;
;in: 0   out: #true
;in: 256 out: #true
;in: 1   out: #true
;in: 5   out:  #false
;in: 6   out: #false
 
;--- DEFINCION ---;
(define (es-potencia-de-dos? n)
  (cond
    [(= n 0) #t]
    [(= n 1) #t]
    [(not (= (remainder n 2) 0)) #f]
    [else (es-potencia-de-dos? (/ n 2))]))

;--- EVALUACION ---;
(check-expect (es-potencia-de-dos? 0)   #true)
(check-expect (es-potencia-de-dos? 256) #true)
(check-expect (es-potencia-de-dos? 1)   #true)
(check-expect (es-potencia-de-dos? 5)   #false)
(check-expect (es-potencia-de-dos? 6)   #false)

;============================= 8 ===============================;

;--- DISE;O DE DATOS ---;
;represento el mensaje como una ListOf(Number) y la lista de numero como
;ListOf(Number)

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;ubicar-mensaje: ListOf(Number) ListOf(Number) -> ListOf(Number)

;dado un mensaje y la lista de posiciones (range 0 tam-bloque 1),
;devuelve el bloque con el mensaje ubicado: en posiciones que son
; potencias de 2 o 0 coloca 0 (reservadas para paridad),
; en el resto ubica los bits del mensaje en orden.

;--- EJEMPLOS ---;
;in: (list 1 0 1 0 0 1 0 1 0 0 1) (range 0 16 1)  out: (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1)

;--- DEFINICION ---;
(define (ubicar-mensaje mensaje posiciones-bloque)
  (local [
    ;recorremos la lista de posicione, cuando la posición NO es paridad, consumimos un bit del mensaje.
    (define (aux ps msg)
      (cond
        [(empty? ps) empty]
        [(es-potencia-de-dos? (first ps))
         (cons 0 (aux (rest ps) msg ))]
        [(empty? msg)
         (cons 0 (aux (rest ps) empty))]
        [(cons? msg)
         (cons (first msg) (aux (rest ps) (rest msg)))] ))]

    (aux posiciones-bloque mensaje)))

;--- EVALUACION ---;
(check-expect (ubicar-mensaje (list 1 0 1 0 0 1 0 1 0 0 1)
                              (range 0 16 1))
              (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1))

;============================= 9 ===============================;

;--- DISE;O DE DATOS ---;
;represento la lista de listas como ListOf(ListOf(Number)) y el bloque del mensaje como
;ListOf(Number)

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;calcular-paridades: ListOf(ListOf(Number)) ListOf(Number) -> ListOf(Number)

;dada una lista de grupos de posiciones y un bloque,
;devuelve la lista de paridades: para cada grupo, calcula la paridad de los bits del
;bloque en las posiciones del grupo.

;--- EJEMPLOS ---;
;in: (obtener-grupos 16) (list 0 0 0 0 0 0 1 1 0 0 0 0 1 1 1 0) out: (list 0 1 1 1)
;in: (obtener-grupos 16) (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1) out: (list 0 0 1 1)

;--- DEFINICION ---;
(define (calcular-paridades grupos bloque)
  (local [(define (paridadDelGrupo grupo)
            (paridad (valores-en-posiciones bloque grupo)))]
    (map paridadDelGrupo grupos)))

;--- EVALUACION ---;
(check-expect (calcular-paridades (obtener-grupos 16)
                                  (list 0 0 0 0
                                        0 0 1 1
                                        0 0 0 0
                                        1 1 1 0))
              (list 0 1 1 1))

(check-expect (calcular-paridades (obtener-grupos 16)
                                  (list 0 0 0 1
                                        0 0 1 0
                                        0 0 1 0
                                        1 0 0 1))
              (list 0 0 1 1))

;============================= 10 ===============================;

;--- DISE;O DE DATOS ---;
;represento las listas como ListOf(X) y ListOf(Y) y devuelvo una ListOf(Par [X,Y])

;--- SIGNATURA Y DECLARACION PROPOSITO ---;
;emparejar: ListOf(X) ListOf(Y) -> ListOf(Par[X,Y])

;dada dos listas y devuelvo una lista de pares donde la primera componente es de la
;primera lista y la segunda de la segunda, el largo del resultado es el de la lista mas corta

;--- EJEMPLOS ---;
;in: (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1) (range 0 16 1) out: (list (make-Par 0 0)
                                                                       ; (make-Par 0 1)
                                                                       ; (make-Par 0 2)
                                                                       ; (make-Par 1 3)
                                                                       ; (make-Par 0 4)
                                                                       ; (make-Par 0 5)
                                                                       ; (make-Par 1 6)
                                                                       ; (make-Par 0 7)
                                                                       ; (make-Par 0 8)
                                                                       ; (make-Par 0 9)
                                                                       ; (make-Par 1 10)
                                                                       ; (make-Par 0 11)
                                                                       ; (make-Par 1 12)
                                                                       ; (make-Par 0 13)
                                                                       ; (make-Par 0 14)
                                                                       ; (make-Par 1 15))

;--- DEFINCION ---;
(define (emparejar l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) empty]
    [else (cons (make-Par (first l1) (first l2))
                (emparejar (rest l1) (rest l2)))]))

;--- EVALUACION ---;
(check-expect (emparejar (list 0 0 0 1
0 0 1 0
0 0 1 0
1 0 0 1)
(range 0 16 1)) (list (make-Par 0 0) (make-Par 0 1)(make-Par 0 2) (make-Par 1 3) (make-Par 0 4)
(make-Par 0 5)
(make-Par 1 6)
(make-Par 0 7)
(make-Par 0 8)
(make-Par 0 9)
(make-Par 1 10)
(make-Par 0 11)
(make-Par 1 12)
(make-Par 0 13)
(make-Par 0 14)
(make-Par 1 15)))

;============================== 11 ==============================;

;--- DISE;O DE DATOS ---;
;represento la lista de paridades como ListOf(Number) y el bloque del mensaje
;como ListOf(Number)

;--- SIGNATURA Y DECLATACION DE PROPOSITO ---;
;ubicar-paridades: ListOf(Number) ListOf(Number) -> ListOf(Number)

;dada una lista de paridades (el bit 0 es la paridad del bloque completo,
;seguido de las paridades de Hamming) y un bloque con mensaje, devuelvo
;el bloque con las paridades insertadas en las posiciones potencias de 2 y 0
 
;--- EJEMPLOS ---;
;in: (list 0 1 1 1 0) (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1) out: (list 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1)

;--- DEFINICION ---;
(define (ubicar-paridades paridades bloque)
  (local [(define pares (emparejar bloque (posiciones (length bloque))))
          (define (aux pares-restantes par-rest)
            (cond [(empty? pares-restantes) empty]
                  [(es-potencia-de-dos? (Par-s (first pares-restantes)))
                   (cons (first par-rest)
                         (aux (rest pares-restantes) (rest par-rest)))]
                  [else
                   (cons (Par-p (first pares-restantes))
                         (aux (rest pares-restantes) par-rest))]))]
    (aux pares paridades)))

;--- EVALUACION ---;
(check-expect (ubicar-paridades (list 0 1 1 1 0)
                                (list 0 0 0 1
                                      0 0 1 0
                                      0 0 1 0
                                      1 0 0 1))
              (list 0 1 1 1
                    1 0 1 0
                    0 0 1 0
                    1 0 0 1))

;============================== 12 ==============================;

;--- DISE;O DE DATOS ---;
;represento el mensaje como ListOf(Number), y el codigo de Hamming como ListOf(Number)

;--- SIGNATURA Y DECLARACION DE PROPOSITO ---;
;mensaje-a-bloque: ListOf(Number) -> ListOf(Number)

;dado un mensaje, devuelvo un bloque con mensaje y códigos
;de corrección de Hamming.
 
;pasos:
;calculo el tamaño del bloque y lista de posiciones 
;ubico el mensaje en el bloque (ceros en posiciones de paridad) 
;obtengo los grupos y calculo las paridades de Hamming 
;armo el bloque provisorio con bit-0 en 0 y paridades de Hamming 
;calculo la paridad global (bit 0) sobre ese bloque provisorio 
;y armo la lista completa de paridades para ubicarlas en el bloque final 
 
;--- EJEMPLOS ---;
;in: (list 0 0 1 1 0 0 0 1 1 1 0) out: (list 0 0 1 0  1 0 1 1  1 0 0 0  1 1 1 0)

;--- DEFINICION ---;
(define (mensaje-a-bloque mensaje)
  (local [
    (define tam (tamaño-bloque (length mensaje)))
    (define pos (posiciones tam))
    
    (define bloque-msg (ubicar-mensaje mensaje pos))
    
    (define grupos (obtener-grupos tam))
    (define par-hamming (calcular-paridades grupos bloque-msg))
    
    (define bloque-prov (ubicar-paridades (cons 0 par-hamming) bloque-msg))
    
    (define par-global (paridad bloque-prov))
    
    (define todas-par (cons par-global par-hamming))]

    (ubicar-paridades todas-par bloque-prov)))

;--- EVALUACION ---;
(check-expect (mensaje-a-bloque (list 0
                                      0 1 1
                                      0 0 0
                                      1 1 1 0))
              (list 0 0 1 0
                    1 0 1 1
                    1 0 0 0
                    1 1 1 0))

  
 

 
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname BerardoMatias-Final) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "testing.rkt" "teachpack" "htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "testing.rkt" "teachpack" "htdp") (lib "web-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(require racket/bool)

; Representación de datos.
; Representamos mensajes o bloques de información con listas de ceros y unos.

; Constantes para casos de prueba
(define MENSAJE1 (list 0 0 1 1 0 0 0 1 1 1 0))
(define MENSAJE2 (list 1 0 1 0 0 1 0 1 0 0 1))
(define MENSAJE3 (list 1 0 1 0 0 1 0 1 0 0 0))


; -- Esta función es parte de la plantilla dada --
; n-bits-redundancia: Number -> Number
; n-bits-redundancia recibe el largo de un mensaje y calcula
; la cantidad de bits de paridad que se requieren para cubrir
; el mensaje completo según la técnica de corrección de Hamming
(check-expect (n-bits-redundancia 1) 2)
(check-expect (n-bits-redundancia 4) 3)
(check-expect (n-bits-redundancia 11) 4)
(check-expect (n-bits-redundancia 13) 5)
(check-expect (n-bits-redundancia 26) 5)
(check-expect (n-bits-redundancia 27) 6)
(define (n-bits-redundancia n)
  (local [(define aprox (inexact->exact (ceiling (/ (log (+ n 1)) (log 2)))))]
    (if (>= (expt 2 aprox) (+ n aprox 1))
        aprox
        (+ aprox 1))))

; -- Esta función es parte de la plantilla dada --
; tamaño-bloque: Number -> Number
; tamaño-bloque recibe el largo de un mensaje y devuelve
; el tamaño de bloque que se requiere para agregarle bits
; de corrección de Hamming
(define (tamaño-bloque largo-mensaje)
  (+ largo-mensaje (n-bits-redundancia largo-mensaje) 1))

; -- Esta función es parte de la plantilla dada --
; posiciones: Number -> List(Number)
; posiciones recibe un número y devuelve una lista con números
; del 0 al n sin incluir.
(define (posiciones n)
    (range 0 n 1))

; -- Esta función es parte de la plantilla dada --
; bloque-a-bits-redundancia: Number -> Number
; bloque-a-bits-redundancia recibe el tamaño de un bloque
; con corrección de Hamming y devuelve la cantidad de bits
; de paridad que se necesitan para cubrirlo
(define (bloque-a-bits-redundancia n)
  (/ (log n) (log 2)))

(define-struct Par [p s])
; Par es un (Any, Any)
; p es la primera componente del par
; s es la segunda componente del par


; Ejercicio 1

; paridad : List(Number) -> Number
; devuelve 1 si la cantidad de unos es impar
; y 0 en caso contrario

(check-expect (paridad (list 1 1 0 0 1 0)) 1)
(check-expect (paridad (list 1 0 0 0 1 0)) 0)

;(sin patrones de alto orden)
(define (paridad l)
  (cond
    [(empty? l) 0]
    [(= (first l) 1) (if (= (paridad (rest l)) 0) 1 0)]
    [else (paridad (rest l))]
    )
  )

;(con patrones de alto orden)
(define (paridadO l)
  (if (even? (foldr + 0 l))
      0
      1))

; Ejercicio 2

; natural->binario : Number -> List(Number)
; dado un natural devuelve su representación binaria,
; comenzando por la cifra menos significativa

(check-expect (natural->binario 3) (list 1 1))
(check-expect (natural->binario 5) (list 1 0 1))
(check-expect (natural->binario 6) (list 0 1 1))
(check-expect (natural->binario 34) (list 0 1 0 0 0 1))

(define (natural->binario n)
  (cond
    [(< n 2) (list n)]
    [else (cons (remainder n 2) (natural->binario (quotient n 2)))]
    )
  )

;Ejercicio 3

; binario->natural : List(Number) -> Number
; dada una lista de ceros y unos (binario), devuelve su natural equivalente

(check-expect (binario->natural (list 1 1)) 3)
(check-expect (binario->natural (list 1 0 1)) 5)
(check-expect (binario->natural (list 0 1 1)) 6)
(check-expect (binario->natural (list 0 1 0 0 0 1)) 34)

(define (binario->natural l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) (* 2 (binario->natural (rest l))))]
    )
  )

; Ejercicio 4

; uno-en-pos? : List(Number) Number -> Boolean
; dada una lista de ceros y unos (número binario) y
; una posición p (número natural), determina si hay un 1 en la posición p


(check-expect (uno-en-pos? (list 1 0 0) 0) #true)
(check-expect (uno-en-pos? (list 0 1 0) 1) #true)
(check-expect (uno-en-pos? (list 0 0 1) 2) #true)
(check-expect (uno-en-pos? (list 1 1 1) 3) #false)
(check-expect (uno-en-pos? (list 1 1 1) 100) #false)

(define (uno-en-pos? l p)
  (cond
    [(empty? l) #false]
    [(zero? p) (= (first l) 1)]
    [else (uno-en-pos? (rest l) (sub1 p))]))

; Ejercicio 5

; indices : Number -> List(Number)
; dado un natural n devuelve la lista (list 0 1 ... n-1)

(check-expect (indices 0) empty)
(check-expect (indices 1) (list 0))
(check-expect (indices 4) (list 0 1 2 3))

(define (indices n)
  (cond [(zero? n) empty]
        [else (cons 0 (map add1 (indices (sub1 n))))]
    )
  )

; grupo : Number List(Number) -> List(Number)
; dado un índice de bit de paridad p y una lista
; de posiciones devuelve aquellas posiciones que
; tienen un 1 en la posición p de su representación binaria.

(check-expect (grupo 0 (rest (posiciones 16))) (list 1 3 5 7 9 11 13 15))
(check-expect (grupo 1 (rest (posiciones 16))) (list 2 3 6 7 10 11 14 15))

(define (grupo p lp)
  (local
    ((define (pertenece? x)
       (uno-en-pos? (natural->binario x) p)))
    (filter pertenece? lp))
  )

; construir-grupos : List(Number) List(Number)
; recibe una lista de índices de bits de paridad
; y una lista de posiciones, y construye los grupos.

(check-expect (construir-grupos empty (rest (posiciones 16))) empty)
(check-expect (construir-grupos (list 0) (rest (posiciones 16))) (list (list 1 3 5 7 9 11 13 15)))

(define (construir-grupos bits pos)
  (cond
    [(empty? bits) empty]
    [else
     (cons (grupo (first bits) pos)
           (construir-grupos (rest bits) pos))]
    )
  )

; obtener-grupos : Number -> List(List(Number))
; dado el tamaño de un bloque con corrección Hamming
; devuelve los grupos asociados a cada bit de paridad.

(check-expect (obtener-grupos 16)
              (list (list 1 3 5 7 9 11 13 15) ; posiciones asociadas al bit de paridad en 1
                    (list 2 3 6 7 10 11 14 15) ; posiciones asociadas al bit de paridad en 2
                    (list 4 5 6 7 12 13 14 15) ; posiciones asociadas al bit de paridad en 4
                    (list 8 9 10 11 12 13 14 15))) ; posiciones asociadas al bit de paridad en 8


(define (obtener-grupos n)
  (local
    ((define POS (rest (posiciones n))))
    (construir-grupos
     (indices
      (inexact->exact
       (bloque-a-bits-redundancia n)))
     POS)))

; Ejercicio 6

; valores-en-posiciones : List(Any) List(Number) -> List(Any)
; devuelve los elementos de lv ubicados en las posiciones de ln

(check-expect (valores-en-posiciones (list 12 21 34 47) (list 0 1 2)) (list 12 21 34))
(check-expect (valores-en-posiciones empty (list 0 1 2)) empty)
(check-expect (valores-en-posiciones (list "a" "b" "c") empty) empty)
(check-expect (valores-en-posiciones (list 1 "d" #true) (list 0 1 3 5)) (list 1 "d"))


(define (valores-en-posiciones lv ln)
  (cond
    [(or (empty? lv) (empty? ln)) empty]
    [(zero? (first ln)) (cons (first lv) (valores-en-posiciones (rest lv) (map sub1 (rest ln))))]
    [else (valores-en-posiciones (rest lv) (map sub1 ln))]
    )
  )

; Ejercicio 7

; es-potencia-de-dos? : Number -> Boolean
; devuelve #true si n es 0 o una potencia de 2

(check-expect (es-potencia-de-dos? 0) #true)
(check-expect (es-potencia-de-dos? 256) #true)
(check-expect (es-potencia-de-dos? 1) #true)
(check-expect (es-potencia-de-dos? 5) #false)
(check-expect (es-potencia-de-dos? 6) #false)

(define (es-potencia-de-dos? n)
  (cond
    [(zero? n) #true]
    [(= n 1) #true]
    [(odd? n) #false]
    [else (es-potencia-de-dos? (/ n 2))]
    )
  )

; Ejercicio 8

; ubicar-mensaje : List(Number) List(Number) -> List(Number)
; ubica los bits del mensaje en las posiciones no reservadas

(check-expect (ubicar-mensaje (list 1 0 1 0 0 1 0 1 0 0 1) (range 0 16 1)) (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1))

(define (ubicar-mensaje m p)
  (cond
    [(empty? p) empty]
    [(es-potencia-de-dos? (first p)) (cons 0 (ubicar-mensaje m (rest p)))]
    [else (cons (first m) (ubicar-mensaje (rest m) (rest p)))]
    )
  )

; Ejercicio 9

; calcular-paridades : List(List(Number)) List(Number) -> List(Number)
; recibe una lista de grupos de posiciones y un bloque,
; y devuelve la lista de paridades calculadas sobre cada grupo

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

(define (calcular-paridades grupos bloque)
  (local ((define (paridad-grupo grupo)
            (paridad (valores-en-posiciones bloque grupo))))
    (map paridad-grupo grupos))
  )

; Ejercicio 10

; emparejar : List(X) List(Y) -> List(Par[X,Y])
; recibe dos listas y devuelve una lista de pares donde
; la primera componente de cada par es un elemento de la
; primera lista y la segunda componente es el elemento correspondiente de la segunda lista

(check-expect (emparejar (list 1 2 3)
                         (list "a" "b" "c"))
              (list (make-Par 1 "a")
                    (make-Par 2 "b")
                    (make-Par 3 "c")))

(check-expect (emparejar (list 1 2)
                         (list "a" "b" "c"))
              (list (make-Par 1 "a")
                    (make-Par 2 "b")))

(check-expect (emparejar empty
                         (list "a" "b"))
              empty)

(define (emparejar l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) empty]
    [else (cons (make-Par (first l1) (first l2)) (emparejar (rest l1) (rest l2)))]
    )
  )

; Ejercicico 11

; ubicar-paridades : List(Number) List(Number) -> List(Number)
; recibe una lista de paridades (incluyendo la de la posición 0)
; y un bloque con mensaje, y devuelve el bloque con las
; paridades ubicadas en las posiciones potencia de 2 y en 0

(define (ubicar-paridades paridades bloque)
  (local
    ((define (aux pares pars)
       (cond
         [(empty? pares) empty]
         [(es-potencia-de-dos? (Par-s (first pares)))
          (cons (first pars) (aux (rest pares) (rest pars)))]
         [else (cons (Par-p (first pares)) (aux (rest pares) pars))])))
    (aux (emparejar bloque (range 0 (length bloque) 1))
         paridades))
  )

; Ejercicio 12

; mensaje-a-bloque : List(Number) -> List(Number)
; recibe un mensaje y devuelve el bloque correspondiente
; con los bits de corrección de Hamming calculados

(check-expect (mensaje-a-bloque (list 0
                                      0 1 1
                                      0 0 0
                                      1 1 1 0))
              (list 0 0 1 0
                    1 0 1 1
                    1 0 0 0
                    1 1 1 0))

(define (mensaje-a-bloque mensaje)
  (local
    ((define bloque-con-mensaje
       (ubicar-mensaje mensaje (range 0 (tamaño-bloque (length mensaje)) 1)))

     (define paridades
       (calcular-paridades (obtener-grupos (length bloque-con-mensaje)) bloque-con-mensaje))

     (define bloque-sin-p0
       (ubicar-paridades (cons 0 paridades) bloque-con-mensaje))

     (define paridad-global
       (paridad bloque-sin-p0)))

    (ubicar-paridades
     (cons paridad-global paridades) bloque-con-mensaje))
  )


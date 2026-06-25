;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname DanaCejas-Final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Dana Cejas
(require racket/bool)

; Representación de datos.
; Representamos mensajes o bloques de información con listas de ceros y unos.


; Constantes para casos de prueba
(define MENSAJE1 (list 0 0 1 1 0 0 0 1 1 1 0))
(define MENSAJE2 (list 1 0 1 0 0 1 0 1 0 0 1))
(define MENSAJE3 (list 1 0 1 0 0 1 0 1 0 0 0))

;---------------- 1 ----------------------;

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
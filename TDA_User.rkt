#lang racket
(require "TDA_docs.rkt")
(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")

; Implementación TDA Usuario

; Representación [name, password, connected]

; Nivel 1 CONSTRUCTORES

; REGISTER:
; Descripción: Permite crear una cuenta mediante usuario, contraseña
; El parametro cte sirve para almacenar los usuarios registrados
; paradigmadocs X register X string X string

(define (register pDocs date user pass)
        (list pDocs date user pass))

; LOGIN:

(define (login pDocs date user pass)
  (define (register pDocs date user active)
    (list pDocs date user active))
  (register pDocs date user 1))

(define gDocs1 (login "Word" (date 5 10 2021) "US1" "PASS"))
  
; Nivel 2 registrado? true false

; Nivel 3 Getters y Setters:

; GET_USERNAME
; Dominio: Lista y Entero positivo
; Recorrido: String
; Descripción: ; Funcion que retorna el username de el usuario 'n'
; Representación/uso: (get_username cte n)
; cte XX n°user XX
; Recursión de cola

(define (get_username lista n)
  (if (= n 1)
      (list-ref lista 2)
  (get_username (list-ref lista 0) (- n 1))))

; EJEMPLOS:

(define emptyGDocs (paradigmadocs  "gDocs" (date 25 10 2021) "encrypt" "decrypt"))
(define gDocs (register (register (register emptyGDocs (date 05 10 2021) "US1" "PASS") (date 05 10 2021) "US2""PASS") (date 05 10 2021) "US3" "a"))

(get_username gDocs 3)
(get_username gDocs 2)
(get_username gDocs 1)
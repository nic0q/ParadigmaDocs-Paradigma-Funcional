#lang racket
(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")
(provide (all-defined-out))

; Implementación TDA Usuario

; PERTENENCIA:

; REGISTRADO_ANTES?: Función que retorna un tipo de dato booleano si el usuario se ha registrado previamente (Esta en la lista de registrados)
; Dominio: paradigma_docs (lista) usuario (string)
; Recorrido: booleano (#t o #f)

(define (registrado_antes? f user)
  (if (not(pair? f))
      #f
      (if (eq? user (second f))
          #t
          (registrado_antes? (first f) user))))

; LOGEADO?: Función que retorna un tipo de dato booleano si el usuario se ha logeado previamente
; Dominio: paradigma_docs (lista) usuario (string)
; Recorrido: booleano (#t o #f)

(define (logeado? f user)
  (if (list?(member user (get_lista_logeados f)))
      #t
      #f))

; REGISTRADO_FOR_SHARE?: Funcion que retorna una lista de los usuarios habilitados para que puedan aceptar la solicitud de compartimiento del propietario del doc,
; Dominio: paradigma_docs, lista accesos, usuario
; Recorrido: lista
; Tipo de Recursividad: Recursividad Natural

(define (registrado_for_share? f user lista)
  (if (empty? lista)
      null
      (if (and (registrado_antes? f (car(car lista))) (not(eq? (car(car lista))user)) (or (eq? (car(cdr(car lista))) #\r) (eq? (car(cdr(car lista))) #\w) (eq? (car(cdr(car lista))) #\c)))
          (cons (list (car(car lista))(cdr(car lista)))(registrado_for_share? f user (cdr lista)))
          (registrado_for_share? f user (cdr lista)))))

; SELECTORES:

; GET_PASSWORD: Función que retorna la contraseña de un usuario en paradigmadocs usuarios registrados
; Dominio: Paradigmadocs, string
; Recorrido: string

(define (get_password f user)
  (if (not(pair? f))
      null
      (if (eq? (second f) user)
          (third f)
          (get_password (first f )user))))

; MODIFICADORES:

; LOGEAR: Funcion que añade al usuario a la lista de activos cuando se usa la funcion login
; Dominio: string (usuario)
; Recorrido: paradigmadocs (lista)

(define (logear f user pass)
  (if(and(eq? pass (get_password f user))(not(logeado? f user))) ; Para que pueda logearse verifica si la contraseña es correcta y no esta en la lista de logeados
     (list (first f) (append (list user)(second f))(third f))
     f))

; DESLOGEAR: Funcion que elimina la sesion del usuario (elimina de la lista de activos) cuando realiza una operation
; Dominio: string (usuario)
; Recorrido: paradigmadocs (lista)

(define (deslogear f user)
  (list (first f) (append (remove user (get_lista_logeados f)))(third f)))

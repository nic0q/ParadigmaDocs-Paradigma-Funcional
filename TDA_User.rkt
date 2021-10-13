#lang racket
(require "TDA_doc.rkt")
(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")
(provide get_n_users)
(provide get_password)
(provide get_state)
(provide registrado_antes?)
(provide logeado?)

; Implementación TDA Usuario

; Nivel 1 CONSTRUCTORES:

; funcion login
; funcion register

; Nivel 2 PERTENENCIA:

; REGISTRADO_ANTES?: Función que retorna un tipo de dato booleano si el usuario se ha registrado previamente

(define (registrado_antes? f user)
  (if (not(pair? f))
      #f
      (if (eq? user (get_second f))
          #t
          (registrado_antes? (get_first f) user))))

; LOGEADO?: Función que retorna un tipo de dato booleano si el usuario se ha logeado previamente

(define (logeado? f user)
  (if ( eq? (get_state f user ) 1)
      #t
      #f))

; Nivel 3 Getters y Setters:

; GET_USERNAME funcion encapsulada que obtiene el nombre de usuario por cada register hecho

(define (get_second f)
  (cadr f))

; GET_FIRST: funcion encapsulada que obtiene el complemento de cada usuario por cada register hecho

(define (get_first f)
  (car f))

; GET SECOND: Segundo elemento de la lista

(define (get_third f)
  (caddr f))

(define (get_qr f)
  (cadddr f))

; GET STATE: En caso que el usuario este logeado, retorna el estado ( 0 - 1 ) deslogeado, logeado

(define (get_state f user)
  (if (not(pair? f))
      null
      (if (eq? (get_second f) user)
          (get_qr f)
          (get_state (first f )user))))

; NUMBER_OF_USERS: Función que retorna el numero de usuario  registrados hasta el momento en paradigmadocs

(define (get_n_users lista [n -1])
    (if (not(pair? lista ))
       n
      (get_n_users (first lista)( + n 1))))

; GET_PASSWORD: Función que retorna la contraseña de un usuario en paradigmadocs usuarios registrados
; Dominio, paradigma docs, nombre de usuario (String)
; Recorrido (String) contraseña del usuario

(define (get_password f user)
  (if (not(pair? f))
      null
      (if (eq? (get_second f) user)
          (get_third f)
          (get_password (first f )user))))

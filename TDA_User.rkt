#lang racket
(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")
(provide get_n_users)
(provide get_password)
(provide registrado_antes?)
(provide crear_listas)
(provide logear)
(provide deslogear)
(provide add_doc)
(provide get_lista_registrados)
(provide get_lista_documentos)
(provide creador_doc)
(provide registrado_for_share?)
(provide access)

; Implementación TDA Usuario

; Nivel 1 CONSTRUCTORES:

; funcion login
; funcion register

; Nivel 2 PERTENENCIA:

; REGISTRADO_ANTES?: Función que retorna un tipo de dato booleano si el usuario se ha registrado previamente

(define (registrado_antes? f user)
  (if (not(pair? f))
      #f
      (if (eq? user (second f))
          #t
          (registrado_antes? (first f) user))))

; REGISTER FOR SHARE: Funcion que recibe una lista de los usuarios que se quiere compartir el documento y retorna solamente los que estan registrados

(define (registrado_for_share? f lista)
  (if (empty? lista)
      null
      (if (registrado_antes? f (car(car lista)))
          (cons (list (car(car lista))(cdr(car lista)))(registrado_for_share? f (cdr lista)))
          (registrado_for_share? f (cdr lista)))))

; LOGEADO?: Función que retorna un tipo de dato booleano si el usuario se ha logeado previamente

(define (get_lista_registrados f)
  (first f))

(define (get_lista_logeados f)
  (second f))

(define (get_lista_documentos f)
  (third f))

(define (logeado? f user)
  (if (list?(member user (get_lista_logeados f)))
      #t
      #f))

; Nivel 3 Getters y Setters:

; GET_PASSWORD: Función que retorna la contraseña de un usuario en paradigmadocs usuarios registrados

(define (get_password f user)
  (if (not(pair? f))
      null
      (if (eq? (second f) user)
          (third f)
          (get_password (first f )user))))

; GET_NUMBER_OF_USERS: Función que retorna el numero de usuario  registrados hasta el momento en paradigmadocs

(define (get_n_users lista [n -1])
    (if (not(pair? lista ))
       n
      (get_n_users (first lista)( + n 1))))

(define (logear f user pass)
  (if(eq? pass (get_password f user))
     (list (first f) (append (list user)(second f))(third f))
     f))

(define (add_doc f x)
  (list (first f) (second f) (append (third f)(list x))))

(define (deslogear f user)
  (list (first f) (append (remove user (get_lista_logeados f)))(third f)))

(define (create_lista_logeados f)
 (append (list f) (list null)))

(define (create_lista_operaciones f)
 (append (list (first f))(list (second f)) (list null)))

(define(crear_listas f)(create_lista_operaciones(create_lista_logeados f)))

(define (access . x ) x )

; CREADOR, funcion que retorna el creador del documento, que esta situado al principio de la lista
; si el documento no existe, no fue creado, retorna un string vacio
(define (creador_doc f)
  (if (string?  (first(get_lista_documentos f)))
      (first(get_lista_documentos f))
      ""))
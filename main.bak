#lang racket
(require "TDA_User.rkt")
(require "TDA_Docs.rkt")
(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")

(define p_docs (paradigmadocs  "gDocs" (date 25 10 2021) "encrypt" "decrypt"))

; FUNCIÓN REGISTER:

; Dominio: Archivo de tipo paradigma_docs, una fecha (TDA Date), username (String), password (string)
; Recorrido: Versión actualizada de paradigma docs, con los usuarios con username únicos registrados
; Representación: (paradigmadocs X string X string X date)
; Descripción: Funcion que mediante una composición de funciones permite registrar un grupo de usuarios únicos mediante la fecha username y
; contraseña del registro, siendo el username un identificador único, en caso existan duplicados, permite solo 1 con ese username
; Tipo de recursión: Recursión Natural

(define (register pDocs user pass date  )
      (if (registrado_antes? pDocs user)   ; Si el usuario esta registrado, entonces no crear la lista, solamente llamar a la función pDocs
          pDocs
         (list  pDocs user pass date )))

;----------------------------- EJEMPLOS ----------------------------------------------------------------------------------------------------

(define g_Docs1 (register(register(register p_docs "user1" "pass1"(date 15 10 2021))"user2""pass2"(date 15 10 2021))"user3""pass3"(date 16 10 2021)))
(define g_Docs2 (register(register(register p_docs "user1" "pass1"(date 15 10 2021))"user1""pass2"(date 26 10 2021))"user3""pass3"(date 27 10 2021)))
(define g_Docs3 (register(register(register p_docs "user1" "pass1"(date 15 10 2021))"user1""pass2"(date 29 10 2021))"user1""pass3"(date 30 10 2021)))

; FUNCIÓN LOGIN:

; Dominio: Archivo de tipo paradigma_docs donde se encuentran los usuarios registrados
; Recorrido: Versión actualizada de paradigma docs, con los usuarios logeados o autenticados
; Representación: (paradigmadocs X string X string X function)
; Funcion que si el usuario existe, (previamente registrado en paradigma_docs), la constraseña que ingresa es igual a la registrada, si esto se cumple se
; una version actualizada de paradigma docs, con el usuario logeado (autenticado) añadido a la version anterior de paradigma docs con un parametro nuevo
; "state" con un 1 ahora registrado
; Tipo de Recursión: Recursión Natural

(define (login pDocs user pass f)
    (if (and (registrado_antes? pDocs user) (eq? pass (get_password pDocs user ))) ; Si el usuario esta en paradigma docs
        (list pDocs user 1 pass f)
        pDocs))
         ;Aca esta creando una nueva lista y la mete en paradigma_docs, debo crear algo que meta el 1 como autenticado

(define(f) null) ; Funcion de ejemplo, no hace nada

;----------------------------- EJEMPLOS ---------------------------------------------------------------------------------------------------------
; Ejemplo 1: Aqui los 3 existen y tienen la contraseña correcta
(define g_Docs4 (login(login(login g_Docs1 "user1" "pass1" f)"user2" "pass2" f)"user3" "pass3" f))

; Ejemplo 2: Aqui los 2 existen y solo 1 tiene la contraseña correcta
(define g_Docs5 (login(login g_Docs2 "user1" "pass1" f)"user3" "incorrecta" f))

; Ejemplo 3: Aqui existe y primero se intento logear con una contraseña incorrecta y al último si dio la clave correcta
(define g_Docs6 (login(login g_Docs3 "user1" "incorrecta" f)"user1" "pass1" f))

; Comprobacion mediante el verificador logeado? que retorna #t si se logeo correctamente

(logeado?  g_Docs4 "user1")  ; -> #t
(logeado?  g_Docs5 "user3")  ; -> #f
(logeado?  g_Docs6 "user1")  ; -> #t

;---------------------------------------------------------------------

; FUNCIÓN CREATE:
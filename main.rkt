#lang racket
(require "TDA_User.rkt")
(require "TDA_doc.rkt")
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

(define (register pDocs user pass date)
      (if (registrado_antes? pDocs user)   ; Si el usuario esta registrado, entonces no crear la lista, solamente llamar a la función pDocs
          pDocs
         (list pDocs user pass date)))

;----------------------------- EJEMPLOS --------------------------------------------------------------------------------------------------------------

(define g_Docs1 (register(register(register p_docs "user1" "pass1" (date 15 10 2021))"user2""pass2"(date 15 10 2021))"user3""pass3"(date 16 10 2021)))
(define g_Docs2 (register(register(register p_docs "user1" "pass1" (date 15 10 2021))"user1""pass2"(date 26 10 2021))"user3""pass3"(date 27 10 2021)))
(define g_Docs3 (register(register(register p_docs "user1" "pass1" (date 15 10 2021))"user1""pass2"(date 29 10 2021))"user1""pass3"(date 30 10 2021)))


; FUNCIÓN LOGIN:

; Dominio: Archivo de tipo paradigma_docs donde se encuentran los usuarios registrados
; Recorrido: Versión actualizada de paradigma docs, con los usuarios logeados o autenticados
; Representación: (paradigmadocs X string X string X function)
; Descripción:Funcion que si el usuario existe, (previamente registrado en paradigma_docs), la constraseña que ingresa es igual a la registrada, si esto se cumple se
; retorna una version actualizada de paradigma docs evaluada en operation y el usuario pasa a desconectado o deslogeado
; Tipo de Recursión: Recursión Natural en la función (registrado_antes?)

(define (login pDocs user pass f)
    (if (and (registrado_antes? pDocs user) (eq? pass (get_password pDocs user))) ; Si el usuario esta (registrado) en paradigma docs y tiene la contraseña correcta
        (cond
          [(eq? f create) (lambda(date name content)(f pDocs date name content user pass))])
        ;else
        (cond
          [(eq? f create)(lambda(date name content)pDocs)]))) ;Se crea una version de cond en 'else' para manejar los 6 parametros ingresados
          
         ;Aca esta creando una nueva lista y la mete en paradigma_docs, debo crear algo que meta el 1 como autenticado

;----------------------------- EJEMPLOS ---------------------------------------------------------------------
; Los ejemplos de login vienen incluidos en cada función posterior, ya que create no funcion sin operation
;------------------------------------------------------------------------------------------------------------

; FUNCIÓN CREATE:

; Descripción: Permite al usuario de paradigmadocs crear un documento en una fecha determinada, un nombre y su contenido
; Representación: (paradigmadocs X string X string X function)
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con el documento creado por el usuario y su desconección de la sesión

(define (create pDocs date nombre_doc texto user pass) ; Si el usuario esta en paradigma docs
    (list pDocs user pass 0 date nombre_doc texto ))

;----------------------------- EJEMPLOS ---------------------------------------------------------------------
(define gDocs_4 ((login g_Docs1 "user1" "pass1" create) (date 30 08 2021) "doc1" "Contenido del Documento"))
; El usuario existe, por lo tanto se crea el documento

(define gDocs_5 ((login g_Docs2 "user99" "pass1" create) (date 30 09 2021) "doc2" "Esto no sera mostrado"))
; El usuario no existe, por lo tanto no crea el documento y retorna la version no actualizada de paradigma_docs

(define gDocs_6 ((login g_Docs3 "user1" "incorrecta" create) (date 31 08 2021) "doc3" "Contenido"))
; El usuario existe pero tiene la contraseña incorrecta, por lo tanto no crea el documento y retorna la version no actualizada de paradigma_docs



#lang racket
; TDA PARADIGMADOCS
(provide (all-defined-out))

; Implementación del TDA paradigmadocs

; CONSTRUCTORES

; PARADIGMADOCS
; Descripción: Función que recibe un nombre que se le pondra a la plataforma, una fecha y 2 funciones de encryptación y desencriptación
; Dominio: string X date X funcion1 X funcion2
; Recorrido: lista
(define (paradigmadocs name date encrypt decrypt)
  (define (crear_lista_registrados)
        null)
  (define (crear_lista_logeados)
        null)
  (define (crear_lista_documentos)
        null)
  (list name date encryptFn decryptFn (crear_lista_registrados) (crear_lista_logeados) (crear_lista_documentos)))

; SELECTORES

; GET_NOMBRE_PLATAFORMA: Función que retorna el nombre de la plataforma paradigmadocs
; Dominio: paradigmadocs
; Recorrido: string
(define (get_nombre_plataforma pDocs)
  (first pDocs))

; GET_FECHA_CREACION_PLATAFORMA: Funcion que retorna la fecha de creación 'date' de la plataforma
; Dominio: paradigmadocs
; Recorrido: date
(define (get_fecha_creacion_plataforma pDocs)
  (second pDocs))

; GET_FUNCTION1: Función que retorna la funcion 1 construida en paradigmadocs en este caso encrypt
; Dominio: paradigmadocs
; Recorrido: procedure
(define (get_function1 pDocs) ;ENCRYPT
  (third pDocs))

; GET_FUNCTION2: Función que retorna la funcion 2 construida en paradigmadocs en este caso decrypt
; Dominio: paradigmadocs
; Recorrido: procedure
(define (get_function2 pDocs) ; DECRYPT
  (fourth pDocs))

; GET_LISTA_REGISTRADOS: Función que retorna la lista de usuarios registrados de paradigmadocs
; Dominio: paradigmadocs
; Recorrido: lista
(define (get_lista_registrados pDocs)
  (fifth pDocs))

; GET_LISTA_LOGEADOS: Función que retorna la lista de usuarios logeados de paradigmadocs
; Dominio: paradigmadocs
; Recorrido: lista
(define (get_lista_logeados pDocs)
  (sixth pDocs))

; GET_DOCUMENTOS: Función que retorna la lista de documentos de paradigmadocs
; Dominio: paradigmadocs
; Recorrido: lista
(define (get_documentos pDocs)
  (seventh pDocs))

; SET_DOCUMENTO: Funcion encargada de crear una versión de paradigmadocs con un nuevo documento, las constantes son:
; nombre_plataforma, fecha_creación, function1, function2, lista_registrados, lista_logeados
; Dominio: paradigmadocs, Contenido (lista)
; Recorrido paradigmadocs
(define (set_documento pDocs contenido)
  (list (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(get_function1 pDocs)(get_function2 pDocs)(get_lista_registrados pDocs)(get_lista_logeados pDocs) contenido))

; ANIADIR_USUARIO_REGISTRADO: Función que crea una nueva versión de paradigmadocs, añade un usuario nuevo a la lista de registrados
; Dominio: paradigmadocs X lista
; Recorrido: paradigmadocs
(define (aniadir_usuario_registrado paradigmadocs lista_registrados)
    (list (get_nombre_plataforma paradigmadocs)(get_fecha_creacion_plataforma paradigmadocs)(get_function1 paradigmadocs)(get_function2 paradigmadocs) lista_registrados (get_lista_logeados paradigmadocs) (get_documentos paradigmadocs)))

; OTRAS FUNCIONES

; ENCRYPTFN: Funcion que encrypta texto
; Dominio: string
; Recorrido string
(define encryptFn (λ (s) (list->string (reverse (string->list s)))))

; DECRYPTFN: Funcion que desencrypta texto
; Dominio: string
; Recorrido string
(define decryptFn (λ (s) (list->string (reverse (string->list s)))))
#lang racket
; TDA PARADIGMADOCS
(provide (all-defined-out))

; CONSTRUCTOR:
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

; CONTRUSTOR DE ESTILOS:

; STYLES
; Descripción: Funcion que retorna los elementos ingresados (cdr) por una access list, (car) es la cabeza "styles", es usada para almacenar los estilos aplicados a un documento
; Dominio: char . . .
; Recorrido: Access list (cdr) . . .
(define (styles . x ) x )

; SELECTORES:

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

; MODIFICADORES

; SET_DOCUMENTO: Funcion encargada de crear una versión de paradigmadocs con un nuevo documento, las constantes son:
; nombre_plataforma, fecha_creación, function1, function2, lista_registrados, lista_logeados
; Dominio: lista paradigmadocs, Contenido (lista)
; Recorrido lista paradigmadocs
(define (set_documento pDocs contenido)
  (list (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(get_function1 pDocs)(get_function2 pDocs)(get_lista_registrados pDocs)(get_lista_logeados pDocs) contenido))

; OTRAS FUNCIONES

; ENCRYPTFN: Funcion que encrypta texto
; Dominio: texto (string)
; Recorrido texto encriptado (string)
(define encryptFn (λ (s) (list->string (reverse (string->list s)))))

; DECRYPTFN: Funcion que desencrypta texto
; Dominio: texto (string)
; Recorrido texto desencriptado (string)
(define decryptFn (λ (s) (list->string (reverse (string->list s)))))
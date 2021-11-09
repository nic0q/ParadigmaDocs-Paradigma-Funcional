#lang racket
; TDA PARADIGMADOCS
(provide (all-defined-out))
(require "TDA_fecha_27391503_FarfanCheneaux.rkt")
; Implementación del TDA paradigmadocs

; CONSTRUCTORES

; PARADIGMADOCS
; Descripción: Función que recibe un nombre que se le pondra a la plataforma, una fecha y 2 funciones de encryptación y desencriptación
; Este constructor es usado por 'main' y todas las funciones de paradigmadocs, register, etc
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

; PARADIGMADOCS_CONSTRUCTOR: Versión de el constructor de paradigmadocs con todas las componentes requeridas para la representación escogida
; Dominio: string X date X funcion1 X funcion2 X lista X lista X lista X lista
; Recorrido: lista
(define (paradigmadocs_constructor name date encrypt decrypt registrados logeados documentos)
  (list name date encrypt decrypt registrados logeados documentos))

; DE PERTENENCIA

; ES_NOMBRE?
; Descripción: Función que retorna una tipo de dato booleano si el nombre de paradigmadocs es válido
; Dominio: X
; Recorrido: boolean
(define (es_nombre_pdocs? nombre)
  (string? nombre))

; PARADIGMADOCS?
; Descripción: Función que retorna un tipo de dato booleano si paradigmadocs es válido
; Dominio: X
; Recorrido: boolean
(define (paradigmadocs? paradigmadocs)
  (if(list? paradigmadocs)
     (if(and(es_nombre_pdocs?(get_nombre_plataforma paradigmadocs))(date?(get_fecha_creacion_plataforma paradigmadocs)))
        #t #f)
     #f))

; SELECTORES

; GET_NOMBRE_PLATAFORMA: Función que retorna el nombre de la plataforma paradigmadocs
; Dominio: paradigmadocs
; Recorrido: string
(define (get_nombre_plataforma paradigmadocs)
  (first paradigmadocs))

; GET_FECHA_CREACION_PLATAFORMA: Funcion que retorna la fecha de creación 'date' de la plataforma
; Dominio: paradigmadocs
; Recorrido: date
(define (get_fecha_creacion_plataforma paradigmadocs)
  (second paradigmadocs))

; GET_FUNCTION1: Función que retorna la funcion 1 construida en paradigmadocs en este caso encrypt
; Dominio: paradigmadocs
; Recorrido: procedure
(define (encrypt paradigmadocs) ;ENCRYPT
  (third paradigmadocs))

; GET_FUNCTION2: Función que retorna la funcion 2 construida en paradigmadocs en este caso decrypt
; Dominio: paradigmadocs
; Recorrido: procedure
(define (decrypt paradigmadocs) ; DECRYPT
  (fourth paradigmadocs))

; GET_LISTA_REGISTRADOS: Función que retorna la lista de usuarios registrados de paradigmadocs
; Dominio: paradigmadocs
; Recorrido: lista
(define (get_lista_registrados paradigmadocs)
  (fifth paradigmadocs))

; GET_LISTA_LOGEADOS: Función que retorna la lista de usuarios logeados de paradigmadocs
; Dominio: paradigmadocs
; Recorrido: lista
(define (get_lista_logeados paradigmadocs)
  (sixth paradigmadocs))

; GET_DOCUMENTOS: Función que retorna la lista de documentos de paradigmadocs
; Dominio: paradigmadocs
; Recorrido: lista
(define (get_documentos paradigmadocs)
  (seventh paradigmadocs))

; ANIADIR_USUARIO_REGISTRADO: Función que crea una nueva versión de paradigmadocs, añade un usuario nuevo a la lista de registrados
; Dominio: paradigmadocs X lista
; Recorrido: paradigmadocs
(define (aniadir_usuario_registrado paradigmadocs lista_registrados)
  (paradigmadocs_constructor (get_nombre_plataforma paradigmadocs)(get_fecha_creacion_plataforma paradigmadocs)(encrypt paradigmadocs)(decrypt paradigmadocs) lista_registrados (get_lista_logeados paradigmadocs) (get_documentos paradigmadocs)))

; SET_DOCUMENTO: Funcion encargada de crear una versión de paradigmadocs con un nuevo documento, las constantes son:
; nombre_plataforma, fecha_creación, function1, function2, lista_registrados, lista_logeados
; Dominio: paradigmadocs, Contenido (lista)
; Recorrido paradigmadocs
(define (set_documento pDocs documentos)
  (paradigmadocs_constructor (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(encrypt pDocs)(decrypt pDocs)(get_lista_registrados pDocs)(get_lista_logeados pDocs) documentos))

; OTRAS FUNCIONES

; ENCRYPTFN: Funcion que encrypta texto
; Dominio: string
; Recorrido string
(define encryptFn (λ (s) (list->string (reverse (string->list s)))))

; DECRYPTFN: Funcion que desencrypta texto
; Dominio: string
; Recorrido string
(define decryptFn (λ (s) (list->string (reverse (string->list s)))))
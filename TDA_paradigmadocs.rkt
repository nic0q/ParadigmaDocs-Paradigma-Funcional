#lang racket
; TDA PARADIGMADOCS
(provide (all-defined-out))

(define (paradigmadocs name date encrypt decrypt)
  (define (crear_lista_registrados)
        null)
  (define (crear_lista_logeados)
        null)
  (define (crear_lista_documentos)
        null)
  (list name date encryptFn decryptFn (crear_lista_registrados) (crear_lista_logeados) (crear_lista_documentos)))

; SELECTORES
(define (get_nombre_plataforma pDocs)
  (first pDocs))

(define (get_fecha_creacion_plataforma pDocs)
  (second pDocs))

(define (get_function1 pDocs) ;ENCRYPT
  (third pDocs))

(define (get_function2 pDocs) ; DECRYPT
  (fourth pDocs))

(define (get_lista_registrados pDocs)
  (fifth pDocs))

(define (get_lista_logeados pDocs)
  (sixth pDocs))

(define (get_lista_documentos pDocs)
  (seventh pDocs))

; MODIFICADORES

;MODIFICAR_DOCUMENTO: Funcion encargada de crear una versión de paradigmadocs con los cambios respectivos en el documento, las constantes son:
; nombre_plataforma, fecha_creación, function1, function2, lista_registrados, lista_logeados
; Dominio: lista paradigmadocs, Contenido (lista)
; Recorrido lista paradigmadocs
(define (set_documento pDocs contenido)
  (list (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(get_function1 pDocs)(get_function2 pDocs)(get_lista_registrados pDocs)(get_lista_logeados pDocs) contenido))

; OTRAS FUNCIONES

; ENCRYPTFN: Funcion que encrypta texto
; Dominio: texto (string)
; Recorrido texto encriptado (string)
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

; DECRYPTFN: Funcion que desencrypta texto
; Dominio: texto (string)
; Recorrido texto desencriptado (string)
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))

; ACCESS: Funcion que retorna una lista con los elementos ingresados (cdr) por una access list, (car) es la cabeza "access", es usada para almacenar los permisos a un documento
; Dominio: char
; Recorrido: Access list (cdr)
(define (access . x ) x )

; STYLES: Funcion que retorna los elementos ingresados (cdr) por una access list, (car) es la cabeza "styles", es usada para almacenar los estilos aplicados a un documento
; Dominio: char
; Recorrido: Access list (cdr)
(define (styles . x ) x )
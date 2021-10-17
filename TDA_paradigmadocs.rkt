#lang racket
; TDA Paradigma Docs
(require "TDA_fecha.rkt")
(provide paradigmadocs)
(provide encryptFn)
(provide decryptFn)
(provide get_lista_logeados)
(provide get_lista_registrados)
(provide get_lista_docs)
(provide create_listas)
(provide access)
(provide crear_pdocs_docs)

; REPRESENTACIÓN:

;(string X list X function X function)

; CONSTRUCTOR:
; CREATE_LISTA_LOGEADOS: Funcion que crea una lista vacia para los usuarios que en un futuro se logearon
; Dominio: paradigma_docs (lista)
; Recorrido: lista vacia

(define (create_lista_logeados f)
 (append (list f) (list null)))

; CREATE_LISTA_DOCS: Funcion que crea una lista donde se colocaran los documentos en un futuro
; Dominio: paradigma_docs (lista)
; Recorrido: lista vacia

(define (create_lista_docs f)
 (append (list (first f))(list (second f)) (list null)))

; CREATE_LISTAS: Funcion que junta ambas listas para darle forma a paradigmadocs
; Dominio: paradigma_docs (lista)
; Recorrido: lista vacia

(define(create_listas f)(create_lista_docs(create_lista_logeados f)))

; Descripción: Funcion que hace referencia a crear un documento con su nombre, fecha de creación, funcion encriptar y desencriptar
; Representacion (nombre_doc, (date), encrypt, decrypt) 
; dom: String XX date XX encrypt XX decrypt
; rec: Lista

(define (paradigmadocs name date encrypt decrypt)
  (list name date encrypt decrypt))
; CREAR_PARADIGMADOCS:

(define (crear_pdocs_docs pDocs x)
  (list (get_lista_registrados pDocs) (get_lista_logeados pDocs) x)
  )
; PERTENENCIA:

; Descripción: Verifica si el nombre es de tipo string, true, false
; dom: texto
; rec: boolean

(define (esnombre? name)
  (if (string? name)
      #t #f))

; SELECTORES:

; GET_LISTA_REGISTRADOS: Funcion que retorna la lista de los usuarios registrados en paradigmadocs
; Dominio: Paradigmadocs
; Recorrido: lista

(define (get_lista_registrados f)
  (first f))

; Descripción: Función que obtiene el nombre del espacio de trabajo
; dom: funcion
; rec: string

(define (get_nombre_pdocs f)
  (car f))

; GET_FECHA
; Descripción: Función que obtiene la fecha del espacio de trabajo
; dom: funcion
; rec: fecha

(define (get_fecha f)
  (cadr f))

; GET_LISTA_LOGEADOS: Funcion que retorna la lista de los usuarios logeados en paradigmadocs
; Dominio: Paradigmadocs
; Recorrido: lista

(define (get_lista_logeados f)
  (second f))

; GET_LISTA_DOCS: Funcion que retorna la lista de los documentos de paradigmadocs
; Dominio: Paradigmadocs
; Recorrido: lista

(define (get_lista_docs f)
  (third f))

; MODIFICADORES:
; Descripción: Retorna una version actualizada con el nombre cambiado de paradigmadocs
; dom: funcion, nombre
; rec: lista, funcion

(define (cambiar_nombre f nombre)
  (if (esnombre? nombre)
  (list nombre (get_fecha f) (caddr f) (cadddr f)) null))


; Descripción: Retorna una version actualizada con la fecha cambiada de paradigmadocs
; dom: funcion, fecha
; rec: lista, funcion

(define (cambiar_fecha f fecha)
  (if (date? fecha)
      (list (get_nombre_pdocs f) fecha (caddr f) (cadddr f))f))


; ACCESS: Funcion que retorna los elementos ingresados por una access list
; Dominio: Cualquier tipo
; Recorrido: Access list (cdr)

(define (access . x ) x )

; ENCRYPT

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))

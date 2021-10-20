#lang racket
; TDA Paradigma Docs
(provide (all-defined-out))

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
  (list (get_lista_registrados pDocs) (get_lista_logeados pDocs) x))
  
; PERTENENCIA:

; Descripción: Verifica si el nombre es de tipo string, true, false
; Dominio texto
; Recorrido: boolean

(define (esnombre? name)
  (if (string? name)
      #t #f))

; SELECTORES:

; GET_LISTA_REGISTRADOS: Funcion que retorna la lista de los usuarios registrados en paradigmadocs
; Dominio: Paradigmadocs
; Recorrido: Lista

(define (get_lista_registrados f)
  (first f))

; GET_LISTA_LOGEADOS: Funcion que retorna la lista de los usuarios logeados en paradigmadocs
; Dominio: Paradigmadocs
; Recorrido: Lista

(define (get_lista_logeados f)
  (second f))

; GET_LISTA_DOCS: Funcion que retorna la lista de los documentos de paradigmadocs
; Dominio: Paradigmadocs
; Recorrido: Lista

(define (get_lista_docs f)
  (third f))

; ACCESS: Funcion que retorna los elementos ingresados (cdr) por una access list, (car) es la cabeza "access"
; Dominio: Cualquier tipo de dato (string, int, etc)
; Recorrido: Access list (cdr)

(define (access . x ) x )

; ENCRYPTFN: Funcion que encrypta texto
; Dominio: texto (string)
; Recorrido texto encriptado (string)

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

; DECRYPTFN: Funcion que desencrypta texto
; Dominio: texto (string)
; Recorrido texto desencriptado (string)
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))

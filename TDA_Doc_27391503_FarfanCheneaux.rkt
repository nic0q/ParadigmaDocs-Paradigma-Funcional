#lang racket
(require "TDA_paradigmadocs_27391503_FarfanCheneaux.rkt")
(provide (all-defined-out))

; Implementación del TDA Doc

; CONSTRUCTORES

; DOCUMENTO: Crea un documento de la forma (title X creador X id X historial X shares)
; Dominio: string X string X integer X lista X lista
; Recorrido: lista
(define (documento title creador idDoc historial shares)
  (list title creador idDoc historial shares))

; PERTENENCIA

; ES_TEXTO?: Funcion que comprueba si es texto válido
; Dominio: x
; Recorrido: boolean
(define (es_texto? texto)
  (string? texto))

; ES_NOMBRE?: Funcion que comprueba si es un nombre válido
; Dominio: x
; Recorrido: boolean
(define (es_nombre? nombre)
  (string? nombre))

; ES_ID?: Funcion que comprueba si es un id válido
; Dominio: x
; Recorrido: boolean
(define (es_id? id)
  (integer? id))

; SELECTORES

; Getters para los documentos

; GET_NOMBRE_DOCUMENTO: Obtiene el nombre del documento
; Dominio: lista (lista documento)
; Recorrido: string
(define (get_nombre_documento documento)
  (car documento))

; GET_ID_DOCUMENTO: Función que obtiene el id del documento
; Dominio: lista
; Recorrido: int (idDoc)
(define (get_id_documento documento)
  (caddr documento))

; GET_AUTOR_DOCUMENTO: Función que obtiene el texto de un documento
; Dominio: lista
; Recorrido: string
(define (get_autor documento)
  (cadr documento))

; GET_HISTORIAL_DOCUMENTO: Función que obtiene el historial de versiones de un documento
; Dominio: lista (documento)
; Recorrido: lista (versiones)
(define (get_historial_documento documento)
  (cadddr documento))

; GET_LISTA_COMPARTIDOS: Función que obtiene la lista de usuarios con los que se comparte el documento
; Dominio: lista (documento)
; Recorrido: 
(define (get_lista_accesos documento)
  (car(cddddr documento)))

; GET_DOC_BYID: Obtiene el documento completo mediante su id
; Dominio: paradigma_docs X int
; Recorrido: documento (lista)
(define (get_doc_byId pDocs idDoc)
  (if (null? (filter (λ (documento) (equal? idDoc (get_id_documento documento)))(get_documentos pDocs)))
      null
      (car(filter (λ (documento) (equal? idDoc (get_id_documento documento)))(get_documentos pDocs)))))

; GET_TITULO_DOC: Función que retorna el título del documento mediante su id
; Dominio: paradigma_docs X int
; Recorrido: string
(define (get_tituloDoc_byid pDocs idDoc)
  (if (null? (get_doc_byId pDocs idDoc))
      null
      (car(get_doc_byId pDocs idDoc))))

; GET_CREADOR_DOC_BYID: Retorna el creador del documento mediante su id
; Dominio: paradigma_docs X int
; Recorrido: string
(define (get_creadorDoc_byid pDocs idDoc)
  (if (null? (get_doc_byId pDocs idDoc))
      null
      (cadr(get_doc_byId pDocs idDoc))))

; GET_HISTORIAL_BYID: Retorna historial de versiones, mediante su id
; Dominio: paradigma_docs X int
; Recorrido: lista
(define (get_historialDoc_byid pDocs idDoc)
  (if (null? (get_doc_byId pDocs idDoc))
      null
      (cadddr(get_doc_byId pDocs idDoc))))

; GET_COMPARTIDOS_DOC_BYID: Retorna la lista de los usuarios con los que se comparte el documento mediante su id
; Dominio: paradigma_docs X int
; Recorrido: lista
(define (get_compartidosDoc_byid pDocs idDoc)
  (if (null? (get_doc_byId pDocs idDoc))
      null
      (car(cddddr(get_doc_byId pDocs idDoc)))))

; SET_ID_DOC: Determina el id que le corresponde a un nuevo documento creado
; Dominio: paradigmadocs
; Recorrido: int
(define(set_id_doc paradigmadocs)
  (length (get_documentos paradigmadocs)))

; SET_DOC: Añade a la lista de documentos un nuevo documento
; Dominio: paradigma_docs X int X lista(contenido)
; Recorrido: lista (Documentos actualizados)
(define (set_doc pDocs idDoc documento)
  (append (filter(λ(documento)(not(eqv? idDoc (get_id_documento documento))) ) (get_documentos pDocs)) documento))

; MODIFICADORES

; UPDATE_DOC: Función que actualiza la versión sin crear una nueva (mismo id)
; Dominio: paradigma_docs X int X lista(version)
; Recorrido: lista (Documentos actualizados)
(define (update_doc pDocs idDoc new_version)
  (list (get_tituloDoc_byid pDocs idDoc) (get_creadorDoc_byid pDocs idDoc) idDoc (append (list new_version) (cdr(get_historialDoc_byid pDocs idDoc))) (get_compartidosDoc_byid pDocs idDoc)))
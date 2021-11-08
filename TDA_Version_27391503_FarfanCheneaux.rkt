#lang racket
(require "TDA_paradigmadocs_27391503_FarfanCheneaux.rkt")
(require "TDA_Doc_27391503_FarfanCheneaux.rkt")
(provide (all-defined-out))

; VERSION: Crea una version de historial de la forma (id X date X content)
; Dominio: integer X date X string
; Recorrido: lista
(define (version idVr date content)
  (list idVr date content))

; GET_ID_VERSION: Función que retorna el id de una versión
; Dominio: lista
; Recorrido: int
(define (get_id_version version)
  (car version))

; GET_DATE_VERSION: Función que retorna la fecha de una versión
; Dominio: lista
; Recorrido: date
(define (get_date_version version)
  (cadr version))

; GET_TEXTO_VERSION: Función que retorna el contenido "texto" de una versión
; Dominio: lista
; Recorrido: string
(define (get_texto_version version)
  (caddr version))

; GET_ID_OCURRENCIAS: Funcion que obtiene los documentos en donde se encontro el texto buscado de la forma: ( (1) (2) (...) ) )
; Dominio: lista X paradigmadocs X string
; Recorrido: lista
; Tipo de Recursividad: Recursividad Natural
(define (get_id_ocurrencias lista_versiones paradigmadocs texto )
  (if (empty? lista_versiones)
      null 
      (if (not(empty? (filter (λ (x) (string-contains? (get_texto_version x) texto))(get_historialDoc_byid paradigmadocs (car lista_versiones)))))
          (cons (car lista_versiones) (get_id_ocurrencias (cdr lista_versiones) paradigmadocs texto))
          (cons (filter (λ (x) (string-contains? (get_texto_version x) texto))(get_historialDoc_byid paradigmadocs (car lista_versiones)))(get_id_ocurrencias (cdr lista_versiones) paradigmadocs texto)))))

; GET_VERSION_SIN_COMMENT: Función que retorna la ultima version sin comentarios del documento para que pueda ser nuevamente comentada, ya que no es posible comentar 2 veces sobre la misma versión
; Dominio: paradigmadocs X int
; Recorrido: lista
; Se debe crear una nueva versión con 1 solo comentario
(define (get_version_sin_comment pDocs idDoc)
  (if(string-contains? (get_contenido_active_version pDocs idDoc) "c%>-") ; Si la versión actual del documento tiene comentarios, se retorna la versión anterior (La cual no tiene comentarios)
     (get_texto_version(get_segunda_version_byid pDocs idDoc)                     )
     (get_contenido_active_version pDocs idDoc)))

; GET_ACTIVE_VERSION_BYID: Obtiene la version activa del documento (La primera en el historial)
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_active_version_byid pDocs idDoc)
  (car (get_historialDoc_byid pDocs idDoc)))

; GET_SEGUNDA_VERSION: Obtiene la versión anterior a la activa
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_segunda_version_byid pDocs idDoc)
  (cadr (get_historialDoc_byid pDocs idDoc)))

; GET_CONTENIDO_ACTIVE_ACTIVE_VERSION: Retorna el "texto" o contenido de la version actual para su modificacion en posteriores versiones (Funcion ADD y Delete)
; Dominio: paradigma_docs X int
; Recorrido: string
(define (get_contenido_active_version pDocs idDoc)
  (caddr (get_active_version_byid pDocs idDoc)))

; GET_N_VERSIONS: Funcion que retorna el numero de versiones creadas en un documento mediante su id
; Dominio: paradigmadocs X int
; Recorrido: int
(define (get_n_versions pDocs idDoc)
  (length(get_historialDoc_byid pDocs idDoc)))

; GET_PRIMERA_VERSION: Función que retorna la primera versión creada en el documento
(define (get_primera_version pDocs idDoc)
  (car(filter (lambda(version)(eqv? 0 (get_id_version version)))(get_historialDoc_byid pDocs idDoc))))

; SET_ID_VR: Determina el id que le corresponde a la nueva versión creda
; Dominio: paradigma_docs X int
; Recorrido: int
(define (set_id_vr paradigmadocs idDoc)
  (length (get_historialDoc_byid paradigmadocs idDoc)))

; SET_VERSION_TO_DOC: Añade una nueva version "activa" al documento, crea un documento actualizado con la nueva version
; Dominio: paradigma_docs (lista) idDoc (int) newversion(lista) 
; Recorrido: lista (Documento actualizado)
(define (set_version_to_doc pDocs idDoc new_version)
  (list (get_tituloDoc_byid pDocs idDoc) (get_creadorDoc_byid pDocs idDoc) idDoc (append (list new_version) (get_historialDoc_byid pDocs idDoc)) (get_compartidosDoc_byid pDocs idDoc)))

; SET_VERSION: Añade una nueva version "activa" al paradigmadocs mediante la funcion de paradigmadocs set_documento
; Dominio: paradigma_docs X int X lista(contenido)
; Recorrido: lista (Documento actualizado)
; Tipo de Recursión: implicita en funciones declarativas: filter
(define (set_version pDocs idDoc x)
  (set_documento pDocs (append (filter(λ (x) (not(eqv? idDoc (get_id_documento x))))(get_documentos pDocs))(list(set_version_to_doc pDocs idDoc x)))))

; SET_HISTORIAL: Función que retorna un historial de la forma version1. . . resto_versiones
; Dominio: lista X lista
; Recorrido lista
(define (set_historial version resto_versiones)
  (append version resto_versiones))

; MODIFICADORES:

; RESTAURA_VERSION: Función que obtiene la versión buscada al aplicar "n" veces CtrlZ o CtrlY, desde la versión antigua
; Dominio: lista (historial_doc) X int X int
; Recorrido: string
; Tipo de Recursividad: Recursividad Natural
(define (get_n_version lista n)
  (define (get_n_version_encap lista x n)
  (if (empty? lista)
      null
      (if (eqv? x n)
          (car lista)
          (get_n_version_encap (cdr lista) (+ x 1) n))))
  (get_n_version_encap lista 0 n))
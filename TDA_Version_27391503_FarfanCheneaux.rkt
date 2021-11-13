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

; GET_ACTIVE_VERSION_BYID: Obtiene la version activa del documento (La primera en el historial)
; NOTA: Una versión activa para ser usada con cualquier 'operation' debe  contener solo strings de texto, por lo tanto no debe tener STYLES NI COMENTARIOS
; Para no tener conflictos con la función seach o search and replace, por lo tanto se filtra hasta que la versión no tenga "\\#" (STYLES) o "c%>-" (COMENTARIOS)
; Dominio: paradigmadocs X int
; Recorrido: lista

(define (get_active_version_byid pDocs idDoc)
  (car(get_historialDoc_byid pDocs idDoc)))

; GET_ACTIVE_FILTERVERSION: ACA LOS COMENTASO APPYLYSYLES
(define (get_active_filterversion_byid pDocs idDoc)
  (car(filter (λ (version) (and (not(string-contains? (get_texto_version version) "\\#"))(not(string-contains? (get_texto_version version) "c%>-"))))(get_historialDoc_byid pDocs idDoc))))

; GET_N_VERSIONS: Funcion que retorna el numero de versiones creadas en un documento mediante su id
; Dominio: paradigmadocs X int
; Recorrido: int
(define (get_n_versions pDocs idDoc)
  (length(get_historialDoc_byid pDocs idDoc)))

; GET_PRIMERA_VERSION: Función que retorna la primera versión creada en el documento
(define (get_primera_version pDocs idDoc)
  (car(filter (λ (version)(eqv? 0 (get_id_version version)))(get_historialDoc_byid pDocs idDoc))))

; SET_ID_VR: Determina el id que le corresponde a la nueva versión creda
; Dominio: paradigma_docs X int
; Recorrido: int
(define (set_id_vr paradigmadocs idDoc)
  (length (get_historialDoc_byid paradigmadocs idDoc)))

; SET_VERSION_TO_DOC: Añade una nueva version "activa" al documento, crea un documento actualizado con la nueva version
; Dominio: paradigma_docs (lista) idDoc (int) newversion(lista) 
; Recorrido: lista (Documento actualizado)
(define (set_version_to_doc pDocs idDoc new_version)
  (documento (get_tituloDoc_byid pDocs idDoc) (get_creadorDoc_byid pDocs idDoc) idDoc (append (list new_version) (get_historialDoc_byid pDocs idDoc)) (get_compartidosDoc_byid pDocs idDoc)))

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

; GET_N_VERSION: Función que obtiene la versión buscada al aplicar "n" veces CtrlZ o CtrlY, desde la versión antigua
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


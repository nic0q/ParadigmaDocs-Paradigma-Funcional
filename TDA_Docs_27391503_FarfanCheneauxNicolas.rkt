#lang racket
(require "TDA_paradigmadocs_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_fecha_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_User_27391503_FarfanCheneauxNicolas.rkt")

(provide (all-defined-out))

; Implementación del TDA Docs

; Constructor
; DOCUMENTO: Crea un documento de la forma (title X creador X id X historial X shares)
(define (documento title creador idDoc historial shares)
  (list title creador idDoc historial shares))

; VERSION: Crea una version de historial de la forma (id X date X content)
(define (version idVr date content)
  (list idVr date content))
; NUEVO_DOCUMENTO: Añaade un documento a los documentos creados en paradigmadocs
(define (nuevo_documento pDocs date title contenido creador)
    (set_documento pDocs (append (list(documento title creador (set_id_doc pDocs) (list (version 0 date contenido))null))(get_documentos pDocs))))

; PERTENENCIA:

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

; SELECTORES:

; Getters para los documentos

; GET_NOMBRE_DOCUMENTO: Obtiene el nombre del documento
; Dominio: lista (lista documento)
; Recorrido: string
(define (get_nombre_documento documento)
  (car documento))

; GET_ID_DOCUMENTO: Función que obtiene el id del documento
; Get Id Documento mediante el documento: Solo es usada por filter, ya que tenemos que recorrer la lista mediante su id
; Funcion encapsulada propia del TDA Documento que obtiene el 3er elemento de un documento, usada por la funcion (get_doc_byid) y (filter para agregar elementos)
; Dominio: lista (lista documento)
; Recorrido: int (idDoc)
(define (get_id_documento documento)
  (caddr documento))

; GET_AUTOR_DOCUMENTO: Función que obtiene el texto de un documento
; Dominio: lista (documento)
; Recorrido: string (autor)
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

; Getters para las versiones

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

; GETTERS/SETTERS del documento mediante su id

; GET_DOC_BYID: Obtiene el documento completo mediante su id
; Dominio: paradigma_docs X int
; Recorrido: documento (lista)
(define (get_doc_byId pDocs idDoc)
  (if (null? (filter (λ (x) (equal? idDoc (get_id_documento x)))(get_documentos pDocs)))
      null
      (car(filter (λ (x) (equal? idDoc (get_id_documento x)))(get_documentos pDocs)))))

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

; GET_N_VERSIONS: Funcion que retorna el numero de versiones creadas en un documento mediante su id
; Dominio: paradigmadocs X int
; Recorrido: int
(define (get_n_versions pDocs idDoc)
  (length(get_historialDoc_byid pDocs idDoc)))

; GET_ACTIVE_VERSION_BYID: Obtiene la version activa del documento (La primera en el historial)
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_active_version_byid pDocs idDoc)
  (car (get_historialDoc_byid pDocs idDoc)))

; GET_SEGUNDA_VERSION: Obtien la anterior versión a la activa
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_segunda_version_byid pDocs idDoc)
  (cadr (get_historialDoc_byid pDocs idDoc)))

; GET_CONTENIDO_ACTIVE_ACTIVE_VERSION: Retorna el "texto" o contenido de la version actual para su modificacion en posteriores versiones (Funcion ADD y Delete)
; Dominio: paradigma_docs X int
; Recorrido: string
(define (get_contenido_active_version pDocs idDoc)
  (caddr (get_active_version_byid pDocs idDoc)))

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

; Setters id de versión y documentos

; SET_ID_DOC: Determina el id que le corresponde a un nuevo documento creado
; Dominio: paradigmadocs
; Recorrido: int
(define(set_id_doc paradigmadocs)
  (length (get_documentos paradigmadocs)))

; SET_ID_VR: Determina el id que le corresponde a la nueva versión creda
; Dominio: paradigma_docs X int
; Recorrido: int
(define (set_id_vr paradigmadocs idDoc)
  (length (get_historialDoc_byid paradigmadocs idDoc)))

; SET_VERSION_TO_DOC: Añade una nueva version "activa" al documento, crea un documento actualizado con la nueva version
; Dominio: paradigma_docs (lista) idDoc (int) newversion(lista) 
; Recorrido: lista (Documento actualizado)
; Importante: parametro new version
(define (set_version_to_doc pDocs idDoc new_version)
  (list (get_tituloDoc_byid pDocs idDoc) (get_creadorDoc_byid pDocs idDoc) idDoc (append (list new_version) (get_historialDoc_byid pDocs idDoc)) (get_compartidosDoc_byid pDocs idDoc)))

; SET_VERSION: Añade una nueva version "activa" al paradigmadocs mediante la funcion de paradigmadocs set_documento
; Dominio: paradigma_docs X int X lista(contenido)
; Recorrido: lista (Documento actualizado)
(define (set_version pDocs idDoc x)
  (set_documento pDocs (append (filter(λ (x) (not(eqv? idDoc (get_id_documento x))))(get_documentos pDocs))(list(set_version_to_doc pDocs idDoc x)))))

; SET_DOC: Añade a la lista de documentos un nuevo documento
; Dominio: paradigma_docs X int X lista(contenido)
; Recorrido: lista (Documentos actualizados)
(define (set_doc pDocs idDoc x)
  (append (filter(λ(x)(not(eqv? idDoc (get_id_documento x))) ) (get_documentos pDocs)) x))

; SET_STYLES: Función que crea un string con los estilos para agregarlo al documento, ademas se filtra la acceslist style para que los estilos solo sean italic, bold, underlined(#\i #\b #\u)
; Dominio: access list (styles)
; Recorrido: string
; Tipo de Recursión: implicita en funciones declarativas: filter
(define (set_styles styles)
  (string-join(map(λ(x)(string-append "#\\" x ))(reverse(cdr(reverse(cdr(string-split(list->string (filter (λ(x)(member x (list #\i #\b #\u)))styles))""))))))))

; GET_VERSION_SIN_COMMENT: Función que retorna la ultima version sin comentarios del documento para que pueda ser nuevamente comentada, ya que no es posible comentar 2 veces sobre la misma versión
; Se debe crear una nueva versión con 1 solo comentario
(define (get_version_sin_comment pDocs idDoc)
  (if(string-contains? (get_contenido_active_version pDocs idDoc) "c%>-") ; Si la versión actual del documento tiene comentarios, se retorna la versión anterior (La cual no tiene comentarios)
     (get_texto_version(get_segunda_version_byid pDocs idDoc)                     )
     (get_contenido_active_version pDocs idDoc)))

; UPDATE_DOC: Función que actualiza la versión sin crear una nueva
(define (update_doc pDocs idDoc new_version)
  (list (get_tituloDoc_byid pDocs idDoc) (get_creadorDoc_byid pDocs idDoc) idDoc (append (list new_version) (cdr(get_historialDoc_byid pDocs idDoc))) (get_compartidosDoc_byid pDocs idDoc)))

; Funciones usadas para paradigmadocs->string:
; REGISTRADOS->STRING: Función que obtiene una versión 'string' de la lista de usuarios registrados en la plataforma paradigmadocs
; Dominio: paradigmadocs (lista)
; Recorrido: string
; Tipo de Recursión: Solo funciones declarativas
(define (registrados->string pDocs)
      (string-join(map (λ (username pass date)
      (string-append "\nUsuario: "username"\nContraseña: "pass"\nRegistrado el "(date->string date)"\n"(make-string 35 #\-)))
           (map (λ (x) (get_username x)) (get_lista_registrados pDocs))
           (map (λ (x) (get_password x)) (get_lista_registrados pDocs))
           (map (λ (x) (get_fecha_creacion x)) (get_lista_registrados pDocs)))))

; HISTORIAL->STRING: Función que convierte el historial en una versión string
; Dominio: lista (historial_doc)
; Recorrido: string
; Tipo de Recursión: Solo funciones declarativas
(define (historial->string historial_doc)
  (string-join(map (λ (idVer date texto)
                     (string-append "\n\t" "Versión n° "(number->string idVer)"\n\tUltima Modificación el "(date->string date)"\n\tContenido: "(decryptFn texto)"\n\t* * * * * * * * * * * *"))
                   (map (λ (x) (get_id_version x)) historial_doc)
                   (map (λ (x) (get_date_version x))historial_doc)
                   (map (λ (x) (get_texto_version x))historial_doc))))

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

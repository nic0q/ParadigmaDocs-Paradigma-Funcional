#lang racket
(require "TDA_paradigmadocs.rkt")
(require "TDA_User.rkt")
(require "TDA_fecha.rkt")
(provide (all-defined-out))

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
(define (get_lista_compartidos documento)
  (car(cddddr documento)))

; GET_TITULO_DOC: Función que retorna el título del documento mediante su id
; Dominio: paradigma_docs X int
; Recorrido: string
(define (get_tituloDoc_byid pDocs idDoc)
  (if (null? (get_doc_byId pDocs idDoc))
      null
      (car(get_doc_byId pDocs idDoc))))

; GET_TEXTO_VERSION: Función que retorna el contenido "texto" de una versión
; Dominio: lista
; Recorrido: string
(define (get_texto_version version)
  (caddr version))

; GET_DATE_VERSION: Función que retorna la fecha de una versión
; Dominio: lista
; Recorrido: date
(define (get_date_version version)
  (cadr version))

; GET_ID_VERSION: Función que retorna el id de una versión
; Dominio: lista
; Recorrido: int
(define (get_id_version version)
  (car version))

; GET_DOC_BYID: Obtiene el documento completo mediante su id
; Dominio: paradigma_docs X int
; Recorrido: documento (lista)
(define (get_doc_byId pDocs idDoc)
  (if (null? (filter (lambda (x) (equal? idDoc (get_id_documento x)))(get_lista_documentos pDocs)))
      null
      (car(filter (lambda (x) (equal? idDoc (get_id_documento x)))(get_lista_documentos pDocs)))))

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

; GET_ACTIVE_VERSION_BYID: Obtiene la version activa del documento (La primera en el historial)
; Dominio: paradigma_docs X int
; Recorrido: lista
(define (get_active_version_byid pDocs idDoc)
  (car (get_historialDoc_byid pDocs idDoc)))

; GET_CONTENIDO_ACTIVE_ACTIVE_VERSION: Retorna el "texto" o contenido de la version actual para su modificacion en posteriores versiones (Funcion ADD y Delete)
; Dominio: paradigma_docs X int
; Recorrido: string
(define (get_contenido_active_version pDocs idDoc)
  (caddr (get_active_version_byid pDocs idDoc)))

; SET_ID_VR: Determina el id que le corresponde a un nuevo documento creado
; Dominio: paradigmadocs
; Recorrido: int
(define(set_id_doc paradigmadocs)
  (length (get_lista_documentos paradigmadocs)))

; SET_ID_VR: Determina el id que le corresponde a la nueva versión creda
; Dominio: paradigma_docs X int
; Recorrido: int
(define (set_id_vr paradigmadocs idDoc)
  (length (get_historialDoc_byid paradigmadocs idDoc)))

; DELETE Y ADD

; ANIADIR_VERSION_TO_DOC: Añade una nueva version "activa" al documento, crea un documento actualizado con la nueva version
; Dominio: paradigma_docs (lista) idDoc (int) newversion(lista) 
; Recorrido: lista (Documento actualizado)
; Importante: parametro new version

(define (set_version_to_doc pDocs idDoc new_version)
  (list (get_tituloDoc_byid pDocs idDoc) (get_creadorDoc_byid pDocs idDoc) idDoc (append (list new_version) (get_historialDoc_byid pDocs idDoc)) (get_compartidosDoc_byid pDocs idDoc)))

; SET_VERSION: Añade una nueva version "activa" al paradigmadocs
; Dominio: paradigma_docs X int X lista(contenido)
; Recorrido: lista (Documento actualizado)
(define (set_version pDocs idDoc x)
  (set_documento pDocs (append (filter(lambda (x) (not(eqv? idDoc (get_id_documento x))))(get_lista_documentos pDocs))(list(set_version_to_doc pDocs idDoc x)))))

; GET_USUARIO_SHARE: Obtiene el usuario de lista de accesos
; Dominio: lista access (user X permiso)
; Recorrido: string
(define (get_usuario_share par_share)
    (car par_share))

; GET_PERMISO_SHARE: Obtiene el tipo de permiso de la lista de accesos 
; Dominio: lista access -> (user X permiso)
; Recorrido: char
(define (get_permiso_share par_share)
    (cadr par_share))
  (define (get_usernames lista)
    (if (empty? lista)
        null
        (cons (get_usuario_share (car lista))(get_usernames (cdr lista)))))

; USERS_WITH_ALL_ACCESS: Función que obtiene una lista con los usuarios que tienen acceso de edicion
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_editor_users pDocs idDoc)
  (append (list (get_creadorDoc_byid pDocs idDoc)) (get_usernames (filter (lambda (x) (eqv? #\w (get_permiso_share x))) (get_compartidosDoc_byid pDocs idDoc)))))

; GET_EDITOR_AND_COMMENT_USERS: Función que obtiene los usuarios que tienen permiso de editar el documento
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_editor_and_comment_users pDocs idDoc)
  (append (list (get_creadorDoc_byid pDocs idDoc))(get_usernames (filter (lambda (x) (or (eqv? #\c (get_permiso_share x))(eqv? #\w (get_permiso_share x)))) (get_compartidosDoc_byid pDocs idDoc)))))

; NUEVO_HISTORIAL: Crea un nuevo historial de la forma:
; Dominio: paradigmadocs X int X lista
; Recorrido: lista
(define (nuevo_historial pDocs idDoc new_historial)
  (list (get_tituloDoc_byid pDocs idDoc) (get_creadorDoc_byid pDocs idDoc) idDoc new_historial (get_compartidosDoc_byid pDocs idDoc)))

; GET_USERS_WITH_ACCESS: Función que obtiene los usernames de los usuarios que tienen acceso al documento (lista de accesos), recorriendo recursivamente
; Dominio: lista (documento)
; Recorrido: lista
; Tipo de Recursividad: Recursividad Natural
(define (get_users_with_access documento)
  (define (recorrer_lista lista_accesos)
    (if (empty? lista_accesos)
        null
        (cons (get_usuario_share(car lista_accesos))(recorrer_lista (cdr lista_accesos)))))
  (recorrer_lista (get_lista_compartidos documento)))


; GET_DOCUMENTOS COMPARTIDOS: Obtiene los ids de todos los documentos a los que al usuario le fueron compartidos
; Dominio: paradigmadocs X string(user)
; Recorrido: lista
(define (get_id_documentos_compartidos pDocs user )
    (map (lambda(x)(get_id_documento x)) (filter (lambda (x) (member user (get_users_with_access x))) (get_lista_documentos pDocs))))

; GET_DOCUMENTOS_CREADOS: Obtiene los ids de todos los documentos a los que el usuario ha creado
; Dominio: paradigmadocs(lista) x user (string)
; Recorrido: lista
(define (get_id_documentos_creados pDocs user)
  (map (lambda(x)(get_id_documento x))(filter (lambda (x) (equal? user (get_autor x))) (get_lista_documentos pDocs))))

; GET_DOCUMENTOS_ACCESO: Función que combina las 2 funciones anteriores (Obtiene los ids  de todos los documentos a los que el usuario tiene acceso)
; Dominio: paradigma_docs X int X string (usuario)
; Recorrido: lista
(define (get_id_documentos_acceso pDocs user)
  (append (get_id_documentos_creados pDocs user)  ; Aquí obtiene los ids de los documentos que el usuario es creador
          (get_id_documentos_compartidos pDocs user))) ; Aquí obtiene los documentos de los que el usuario tiene acceso

; GET OCURRENCIAS: Funcion que obtiene los documentos en donde se encontro el texto buscado de la forma: ( (1) (2) (...) ) )
; Dominio: lista X paradigmadocs X string
; Recorrido: lista
; Tipo de Recursividad: Recursividad Natural
(define (get_id_ocurrencias lista_versiones paradigmadocs texto )
  (if (empty? lista_versiones)
      null 
      (if (not(empty? (filter (lambda (x) (string-contains? (get_texto_version x) texto))(get_historialDoc_byid paradigmadocs (car lista_versiones)))))
          (cons (car lista_versiones) (get_id_ocurrencias (cdr lista_versiones) paradigmadocs texto))
          (cons (filter (lambda (x) (string-contains? (get_texto_version x) texto))(get_historialDoc_byid paradigmadocs (car lista_versiones)))(get_id_ocurrencias (cdr lista_versiones) paradigmadocs texto)))))

; FILTRA PERMISOS: Filtra permisos bajo tales condiciones, obtiene solamente el username de la lista de permisos, en este  caso de la version anterior a paradigmadocs
; 1) El usuario debe estar previamente registrado
; 2) Unicos_permisos: Filtra los permisos duplicados ingresados por el usuario, mantien los ultimos ingresados
; 3) Actualizar_permisos: Combina todos los permisos anteriores dados por la version anterior de paradigma docs y los actuales, si ambos existen en ambas listas, mantiene el de la ultima
; , si se crea uno nuevo en la lista actual, se añade automaticamente
; Dominio: paradigma_docs (lista) idDoc (int)
; Recorrido: access list (X user X permiso) ...
; Tipo de Recursividad: Recursividad Natural, funcion unicos permisos
(define (filtrar_permisos permisos_antiguos permisos_actuales pDocs idDoc)
  (define (unicos_permisos lista) ; unicos permisos: Funcion que filtra los permisos duplicados ingresados por el usuario
    (define (encap lista lista_unicos)
      (if (null? lista)
          null
          (if (and (es_usuario? (get_usuario_share (car lista)))(member (get_permiso_share (car lista))(list #\w #\c #\r))) ; Solo si el permiso tiene la sintaxis correcta: (#\w or #\c or #\r)
              (if (list? (member (get_usuario_share (car lista)) lista_unicos)) 
                  (encap (cdr lista) lista_unicos)
                  (cons (car lista) (encap (cdr lista) (append (list(get_usuario_share(car lista))) lista_unicos))))
              (encap (cdr lista) lista_unicos))))
    (encap (reverse lista) '())) ; Una lista vacia para comparar permisos
  (define (actualizar_permisos lista_arrastre lista_actual) ; actualizar permisos: Funcion recursiva que actualiza los nuevos y antiguos permisos, NO ELIMINA PERMISOS
    (define (lista_usuarios_compartidos lista)
      (if (empty? lista)
          null
          (cons (car (car lista )) (lista_usuarios_compartidos (cdr lista)))))
    (if (empty? lista_arrastre)
        lista_actual
        (if (not(list?(member (get_usuario_share(car lista_arrastre)) (lista_usuarios_compartidos lista_actual))))
            (actualizar_permisos (cdr lista_arrastre) (append (list(car lista_arrastre)) lista_actual))
            (actualizar_permisos (cdr lista_arrastre) lista_actual))))
  (if (null? permisos_antiguos) ; Si la lista de permisos esta vacia, solo filtra esa lista, sino aplica la funcion actualizar_permisos para actualizar los permisos de la lista antigua y la actual
      (filter (lambda (x) (and (not(equal? (get_usuario_share x) (get_creadorDoc_byid pDocs idDoc)))(registrado_antes? pDocs (get_usuario_share x)))) (unicos_permisos permisos_actuales))
      (filter (lambda (x) (and (not(equal? (get_usuario_share x) (get_creadorDoc_byid pDocs idDoc)))(registrado_antes? pDocs (get_usuario_share x)))) (actualizar_permisos permisos_antiguos (unicos_permisos permisos_actuales)))))

; ANIADIR_PERMISOS: Añade permisos a la lista de usuarios con permiso del documento
; Dominio: paradigmadocs(lista) idDoc(int) permisos(lista)
; Recorrido: lista_permisos (lista)
(define (aniadir_permisos pDocs idDoc permisos)
  (append (list(reverse(append (list permisos) (cdr (reverse(get_doc_byId pDocs idDoc))))))(filter (lambda (x) (not(equal? (get_id_documento x) idDoc)))(get_lista_documentos pDocs))))

; Funciones usadas para paradigmadocs->string:
; REGISTRADOS->STRING: Función que obtiene una versión 'string' de la lista de usuarios registrados en la plataforma paradigmadocs
; Dominio: paradigmadocs (lista)
; Recorrido: string
; Tipo de Recursión: Solo funciones declarativas
(define (registrados->string pDocs)
      (string-join(map (lambda (username pass date)
      (string-append "\nUsuario: "username"\nContraseña: "pass"\nFecha Registro: "(date->string date)"\n"(make-string 35 #\-)))
           (map (lambda (x) (get_username x)) (get_lista_registrados pDocs))
           (map (lambda (x) (get_password x)) (get_lista_registrados pDocs))
           (map (lambda (x) (get_fecha_creacion x)) (get_lista_registrados pDocs)))))

; ACCESSES->STRING: Función que convierte a string los accessos que tienen los usuarios al documento,
; además la función encapsulada access_name convierte a string el char, entendible por el usuario
; Dominio: access list (lista_accesses)
; Recorrido: string
; Tipo de Recursión: Solo funciones declarativas
(define (accesses->string lista_accesses)
  (define (access_name pr)
    (cond 
      [(eqv? pr #\w) "escritura"]
      [(eqv? pr #\c) "comentarios"]
      [(eqv? pr #\r) "lectura"]))
  (string-join(map (lambda (user permiso)
                     (string-append"\n"user" ➞ permiso de "(access_name permiso)))
                   (map (lambda (x) (get_usuario_share x))lista_accesses)
                   (map (lambda (x) (get_permiso_share x))lista_accesses))))

; HISTORIAL->STRING: Función que convierte el historial en una versión string
; Dominio: lista (historial_doc)
; Recorrido: string
; Tipo de Recursión: Solo funciones declarativas
(define (historial->string historial_doc)
  (string-join(map (lambda (idVer date texto)
                     (string-append "\n\tid versión: "(number->string idVer)"\n\tCreada el "(date->string date)"\n\tContenido: "(decryptFn texto)"\n\t* * * * * * * * *"))
                   (map (lambda (x) (get_id_version x)) historial_doc)
                   (map (lambda (x) (get_date_version x))historial_doc)
                   (map (lambda (x) (get_texto_version x))historial_doc))))

; DOCUMENTOS->STRING: Función que obtiene una versión 'string' de toda la información de los documentos de paradigmadocs,
; Utiliza las funciones accesses->string, historial->string
; Componentes Documentos: NombreDoc, Autor, id, versión activa, historial, Usuarios con acceso
; Dominio: lista X paradigmadocs
; Recorrido: string
(define (documentos->string lista_documentos)
  (string-join(map (lambda (nombre_doc creador id historial shares)
                     (string-append "\n"nombre_doc" creado por "creador"\nid: "(number->string id)"\nHistorial de Documentos:\n\tVersión Activa:" (historial->string historial)
                                    "\nUsuarios con acceso:"(accesses->string (filter (lambda (x)(not(null? x)))shares))"\n"))
                   (map (lambda (x) (get_nombre_documento x))lista_documentos)
                   (map (lambda (x) (get_autor x))lista_documentos)
                   (map (lambda (x) (get_id_documento x)) lista_documentos)
                   (map (lambda (x) (get_historial_documento x))lista_documentos)
                   (map (lambda (x) (get_lista_compartidos x))lista_documentos))))

; SET_STYLES: Función que crea un string con los estilos para agregarlo al documento, ademas se filtra la acceslist style para que los estilos solo sean italic, bold, underlined(#\i #\b #\u)
; Dominio: access list (styles)
; Recorrido: string
; Tipo de Recursión: implicita en funciones declarativas: map
(define (set_styles styles)
  (string-join(map(lambda(x)(string-append "#/" x ))(reverse(cdr(reverse(cdr(string-split(list->string (filter (lambda(x)(member x (list #\i #\b #\u)))styles))""))))))))
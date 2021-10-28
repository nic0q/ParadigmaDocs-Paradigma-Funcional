#lang racket
(require "TDA_Docs.rkt")
(require "TDA_User.rkt")
(require "TDA_paradigmadocs.rkt")
(provide (all-defined-out))

; CONSTRUCTOR DE ACCESOS:
; ACCESS: Funcion que retorna una lista con los elementos ingresados (cdr) por una access list, (car) es la cabeza "access", es usada para almacenar los permisos a un documento
; Dominio: char
; Recorrido: Access list (cdr)
(define (access . accesses ) accesses )

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

; GET_USERNAMES: Obtiene solo los usernames de la lista de accesos
; Dominio: lista access -> (user X permiso)
; Recorrido: lista
(define (get_usernames lista)
  (if (empty? lista)
      null
      (cons (get_usuario_share (car lista))(get_usernames (cdr lista)))))

; GET_EDITOR_USERS: Función que obtiene una lista con los usuarios que tienen acceso de edicion
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_editor_users pDocs idDoc)
  (append (list (get_creadorDoc_byid pDocs idDoc)) (get_usernames (filter (lambda (x) (eqv? #\w (get_permiso_share x))) (get_compartidosDoc_byid pDocs idDoc)))))

; GET_EDITOR_AND_COMMENT_USERS: Función que obtiene los usuarios que tienen permiso de editar el documento
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_editor_and_comment_users pDocs idDoc)
  (append (list (get_creadorDoc_byid pDocs idDoc))(get_usernames (filter (lambda (x) (or (eqv? #\c (get_permiso_share x))(eqv? #\w (get_permiso_share x)))) (get_compartidosDoc_byid pDocs idDoc)))))

; GET_DOCUMENTOS COMPARTIDOS: Obtiene los ids de todos los documentos a los que al usuario le fueron compartidos
; Dominio: paradigmadocs X string(user)
; Recorrido: lista
(define (get_id_documentos_compartidos pDocs user )
    (map (lambda(x)(get_id_documento x)) (filter (lambda (x) (member user (get_users_with_access x))) (get_documentos pDocs))))

; GET_DOCUMENTOS_CREADOS: Obtiene los ids de todos los documentos a los que el usuario ha creado
; Dominio: paradigmadocs(lista) x user (string)
; Recorrido: lista
(define (get_id_documentos_creados pDocs user)
  (map (lambda(x)(get_id_documento x))(filter (lambda (x) (equal? user (get_autor x))) (get_documentos pDocs))))

; GET_DOCUMENTOS_ACCESO: Función que combina las 2 funciones anteriores (Obtiene los ids  de todos los documentos a los que el usuario tiene acceso)
; Dominio: paradigma_docs X int X string (usuario)
; Recorrido: lista
(define (get_id_documentos_acceso pDocs user)
  (append (get_id_documentos_creados pDocs user)  ; Aquí obtiene los ids de los documentos que el usuario es creador
          (get_id_documentos_compartidos pDocs user))) ; Aquí obtiene los documentos de los que el usuario tiene acceso

; GET_USERS_WITH_ACCESS: Función que obtiene los usernames de los usuarios que tienen acceso al documento (lista de accesos), recorriendo recursivamente
; Dominio: lista (documento)
; Recorrido: lista
; Tipo de Recursividad: Recursividad Natural
(define (get_users_with_access documento)
  (define (recorrer_lista lista_accesos)
    (if (empty? lista_accesos)
        null
        (cons (get_usuario_share(car lista_accesos))(recorrer_lista (cdr lista_accesos)))))
  (recorrer_lista (get_lista_accesos documento)))

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

; Revoke all accesses, eliminar los permisos

(define (eliminar_permisos documento)
  (if (not (null? (get_lista_accesos documento)))                          ; Si la lista de compartidos no es nula:
      (append (remove (get_lista_accesos documento)documento) (list null)) ; Se agrega reemplaza por una lista vacía
      documento))                                                          ; Si no solo devuelve el documento

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

; Funciones usadas para paradigmadocs->string:





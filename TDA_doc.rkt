#lang racket
(require "TDA_paradigmadocs.rkt")
(provide (all-defined-out))

; Implementación TDA DOC:

; SELECTORES

(define (get_id x)
  (fourth x))

; GET_CREADOR_DOC: Funcion que retorna al creador del documento
; Dominio: int, lista
; Recorrido: string

(define (get_creador_doc id lista)
  (if (empty? lista) 
    "" ;devuelve un string vacio
    (if (eq? id (fourth(car lista)))
        (car (car lista))
        (get_creador_doc id (cdr lista)))))

(define (add_id id f lista content)
  (if (empty? lista) 
    null
    (if (eq? id (fourth(car lista)))
        (cons(append (car lista) (list content))lista)
        (add_id id f (cdr lista)content))))

(define (borrar_by_id id lista)
  (if (empty? lista)
      null
      (if (not(eq? id (fourth(car lista))))
          (cons (car lista)(borrar_by_id id (cdr lista)))
          (borrar_by_id id (cdr lista)))))

; ADD_DOC: Añade un documento nuevo a la lista de documentos
; Descripción
; Dominio: paradigma_docs (lista) y doc los elementos a ingresar
; Recorrido: Una version actualizada de paradigmadocs con el documento ingresado

(define (add_doc f doc)
  (list (get_lista_registrados f) (get_lista_logeados f) (append (get_lista_docs f)(list doc))))

; ADD_BY_ID: añade operaciones a un documento creado anteriormente
; Dominio: paradigma_docs (lista) y el contenido a agregar
; Recorrido: 

(define (add_by_id id pdocs content)
  (crear_pdocs_docs pdocs (append (borrar_by_id id (get_lista_docs pdocs))(list(car(add_id id pdocs (get_lista_docs pdocs)content))))))

;ULTIMO_PERMISO: Elimina los duplicados (si un usuario es ingresado 2 veces) y retorna una lista con usuarios unicos (se considera el ultimo ingresado para esto se usa "reverse")
; NOTA: El parametro lista unicos es una lista vacia para almacenar los usuarios que son unicos
; Dominio: Lista y lista vacia
; Recorrido: Lista con usuarios unicos

(define (ultimo_permiso lista_mix)
  (define (encap lista_mix lista_unicos)
  (if (null? lista_mix)
      null
      (if (list?(member (car(car lista_mix)) lista_unicos))
          (encap (cdr lista_mix) lista_unicos)
          (cons (car lista_mix) (encap (cdr lista_mix) (append (list(car(car lista_mix))) lista_unicos))))))
  (encap (reverse lista_mix) '()))

; GET_DOCUMENT_BYID: Retorna el documento y todas sus componentes mediante su ID
; Dominio: int (id) Lista(lista de documentos)
; Recorrido: Lista

(define (get_doc_byid id lista_docs)
  (if (empty? lista_docs)
      null
      (if (eq? id (get_id(car lista_docs)))
          (car lista_docs)
          (get_doc_byid id (cdr lista_docs)))))

; USUARIOS_SHARE:
; Funcion que retorna la lista de los usuarios con los que se comparte el documento
; Dominio: Documento
; Recorrido: Lista de permisos

(define (usuarios_share documento)
  (if (empty? documento)
      null
      (if(eq?(car(car documento))"s")
         (cdr(car documento))
         (usuarios_share (cdr documento)))))

(define (filtrar_ids id lista)
  (if (empty? lista)
      null
      (if (not(eq? id (get_id (car lista))))
          (cons (car lista)(filtrar_ids id (cdr lista)))
          (filtrar_ids id (cdr lista)))))

; MERGE_PERMISOS: Combina todos los permisos anteriores dados por la version anterior de paradigma docs y los actuales,
; Dominio: Lista Lista
; Recorrido: Lista
; ENCAPSULADA: Lista_usuarios_compartidos
; Obtiene solamente el username de la lista de permisos, en este  caso de la version anterior a paradigmadocs

(define (merge_permisos lista_arrastre lista_actual)
  (define (lista_usuarios_compartidos lista)
  (if (empty? lista)
      null
      (cons (car (car lista )) (lista_usuarios_compartidos (cdr lista)))))
  (if (empty? lista_arrastre)
      lista_actual
      (if (not(list?(member (car(car lista_arrastre)) (lista_usuarios_compartidos lista_actual))))
          (merge_permisos (cdr lista_arrastre) (append (list(car lista_arrastre))lista_actual))
          (merge_permisos (cdr lista_arrastre) lista_actual))))

; VERSIONES: Versiones para compartir archivo add

; GET_ALL_VERSIONS: Función que retorna toda la lista de versiones de un documento mediante su id
; Dominio: id(int) paradigmadocs(lista)
; Recorrido: lista de versiones (lista)

(define (get_all_versions_byid id pdocs)
  (caddr(get_doc_byid id (get_lista_docs pdocs))))

; GET_VERSION_BYID: Retorna una versión en específico de un documento, mediante una id y la la lista de versiones
; Dominio:  id(int) lista_all_versions(lista)
; Recorrido: version activa (lista)

(define (get_version_byid id list_all_versions)
  (if (null? list_all_versions)
      null
      (if (eq? id (third(car list_all_versions)))
          (car list_all_versions)
          (get_version_byid id (cdr list_all_versions)))))

; GET_ACTIVE_VERSION: Retorna la lista de la sesion actual (activa) dado un id de documento
; Dominio:  id(int) paradigmadocs(lista)
; Recorrido: lista de version activa (lista)

(define (get_active_vr_byid idDoc pdocs)
  (last(get_all_versions_byid idDoc pdocs)))

; GET_INACTIVE_VERSIONS: Retorna una lista de las sesiones anteriores (no activas), dado un id de documento
; Dominio: id(int) paradigmadocs(lista)
; Recorrido: lista de versiones inactivas (lista)

(define (get_inactive_vers_byid id pdocs)
  (cdr(reverse(get_all_versions_byid id pdocs))))

; FUNCION ADD
; ADD_TO_VER (encapsulada) Funcion que añade text a la version mediante su id

(define (add2 pdocs id date text)
  (define(add_vr pdocs id date text)
  (append (get_all_versions_byid id pdocs) (list(list date (string-join (list(cadr(get_active_vr_byid id pdocs))(encryptFn text))) (length (get_all_versions_byid id pdocs))))))

  (define (add_to_ver lista id date text)
    (if (empty? lista)
        null
        (if (and (not(list? (car lista))))
            (cons (car lista) (add_to_ver (cdr lista) id date text))
            (if (not(list? (car (car lista))))
                (cons (car lista)(add_to_ver (cdr lista)id date text))
                (cons (add_vr pdocs id date text)(add_to_ver (cdr lista)id date text))))))
  (add_to_ver (get_doc_byid id (get_lista_docs pdocs)) id date text))

(define (aniadir_ver_to_doc id lista_docs content)
  (if (empty? lista_docs)
      null
      (if (not(eq? (get_id (car lista_docs)) id))
          (cons (car lista_docs) (aniadir_ver_to_doc id (cdr lista_docs) content))
          (cons content (aniadir_ver_to_doc id (cdr lista_docs)content)))))

; RESTORE VERSION:

; ADD_VERSION: Funcion que crea una version actualizada del documento en el que se quiere restaurar una version

(define (add_version lista id x)
  (if (empty? lista)
      null
      (if (not(eq? (get_id (car lista)) id))
          (cons (car lista)(add_version (cdr lista) id x))
          (if (list? (last (car lista)))
              (cons (list (car(car lista)) (cadr (car lista))x (cadddr (car lista)) (last (car lista))) (add_version (cdr lista) id x))
              (cons (list (car(car lista)) (cadr (car lista))x (cadddr (car lista))) (add_version (cdr lista) id x))))))

; FUNCION REVOKEALLACCESES:

; GET_DOCS_BYUSER: Obtiene una lista de todos los documentos de un usuario, mediante su username
; Dominio: user (string) Lista de documentos(lista)
; Recorrido: Lista de documentos (lista)

(define (get_docs_byuser user lista_docs)
  (if (empty? lista_docs)
      null
      (if (and (eq?(car(car lista_docs))user))
          (cons (car lista_docs)(get_docs_byuser user (cdr lista_docs)))
          (get_docs_byuser user (cdr lista_docs)))))

; ACTUALIZAR_PERMISOS: Funcion que actualiza la lista de documentos ya que se removieron accesos y devuelve la lista completa de documentos de paradigmadocs
; Dominio:
; Recorrido:

(define (actualizar_permisos lista_docs lista_act user)
  (if (null? lista_docs)
      null
      (if (not(eq? (car (car lista_docs))user))
          (cons (car lista_docs) (actualizar_permisos (cdr lista_docs) lista_act user))
          (cons (car lista_act) (actualizar_permisos (cdr lista_docs) (cdr lista_act)user)))))

; ELIMINAR_PERMISOS: Funcion que elimina los permisos de un documento
; Dominio:
; Recorrido:

(define (eliminar_permisos lista_docs)
  (if (empty? lista_docs)
      null
      (if (> (length lista_docs) 4)
          (remove (fifth lista_docs)lista_docs)
          lista_docs)))

; GET_ACCESSES_SHARED: Funcion que retorna la lista de todos los accesos que tiene un documento (user XX permiso XX)
; Dominio: El id del documento (int), paradigmadocs (lista)
; Recorrido: Lista con los usuarios que se comparte el documento

(define (get_accesses_shared id pdocs)
  (if (< (length (get_doc_byid id (get_lista_docs pdocs))) 5)
      null
       (cdr(fifth (get_doc_byid id (get_lista_docs pdocs))))))

; GET_ALL_ACCESSES_USERS: Funcion que retorna todos los usuarios que tienen acceso a tal documento, sin importar que permiso tenga
; NOTA: Se incluye el creador
; Dominio: id (int) pdocs(Lista)
; Recorrido: Lista
; ENCAPSULADA:
; GET_ALL_USERS_SHARED: Retorna una lista con todos los "usernames con los que se ha compartido un documento
; Dominio: Lista
; Recorrido Lista

(define (get_all_users_accesses id pdocs)
  (define (get_all_users_shared lista_shares)
  (if (empty? lista_shares)
      null
      (cons (car(car lista_shares)) (get_all_users_shared (cdr lista_shares)))))
  (append (list (get_creador_doc id (get_lista_docs pdocs))) (get_all_users_shared (get_accesses_shared id pdocs))))

; GET_USERS_WRITE
; Funcion que retorna una lista con los usuarios que pueden editar "escribir" el documento
; Dominio: Funcion con todos los usuarios que tienen acceso al documento (funcion ususarios_share)
; Recorrido: Lista con los usuarios que tienen acceso a la edición (#\w)

(define (get_users_write lista_shares)
  (if (empty? lista_shares)
      null
      (if (eq? (car(car(cdr(car lista_shares)))) #\w )
      (cons (car(car lista_shares)) (get_users_write (cdr lista_shares)))
      (get_users_write (cdr lista_shares)))))

; GET_VERSIONS_WITH_ACCESS: Obtiene una lista de todas las versiones (id) que un usuario tiene acceso
; Dominio: pdocs(lista) user(string)
; Recorrido: Lista con los id de los documentos que user tiene acceso

(define (get_versions_with_access pdocs user)
  (define (get_all_access_docs2 lista_docs user)
  (if (empty? lista_docs)
      null
      (if (list?(member user (get_all_users_accesses (get_id (car lista_docs)) pdocs)))
          (cons (fourth (car lista_docs)) (get_all_access_docs2 (cdr lista_docs) user) )
          (get_all_access_docs2 (cdr lista_docs) user))))
  (get_all_access_docs2 (get_lista_docs pdocs) user))

(define (get_text_byid lista)
  (if (empty? lista)
      null
      (cons (decryptFn(second(car lista)))(get_text_byid (cdr lista)))))

(define (encuentra_texto? lista_versions text pdocs)
  (if (empty? lista_versions)
      null
      (cons (list (car lista_versions) (filter (lambda (x) (string-contains? x text)) (get_text_byid (get_all_versions_byid (car lista_versions) pdocs))))(encuentra_texto? (cdr lista_versions) text pdocs))))

(define (id_list lista_texto)
    (if (empty? lista_texto)
        null
        (if (not(null?(second(car lista_texto))))
            (cons (first (car lista_texto))(id_list (cdr lista_texto)))
            (id_list (cdr lista_texto)))))

(define (imprimir_versiones pdocs lista_v)
  (if (empty? lista_v)
      null
      (cons (get_doc_byid (car lista_v)(get_lista_docs pdocs)) (imprimir_versiones pdocs (cdr lista_v)))))

; LISTA-CARACTERES: Crea una lista con los todos los caracteres del texto de la ultima version del documento, mediante su id
; get_vr_text: Obtiene el texto de una version y lo desencrypta
; Dominio:
; Recorrido:

(define (lista_caracteres id pdocs)
  (define (get_vr_text vr)
    (decryptFn(second vr)))
  (map string (string->list (get_vr_text(get_active_vr_byid id pdocs)))))

; DELETE_CHARACTERS: Elimina los ultimos caracteres "x" un string y retorna una lista con los caracteres eliminados
; Dominio: paradigmadocs (lista) idDoc (int) "x" (int)
; Recorrido: Lista

(define (delete_characters pDocs idDoc x)
  (define (recorrer_lista lista i n)
  (if (equal? i n)
      null
      (cons (car lista)(recorrer_lista (cdr lista)(+ i 1)n))))
  (recorrer_lista (lista_caracteres idDoc pDocs) 0 (- (length (lista_caracteres idDoc pDocs)) x)))
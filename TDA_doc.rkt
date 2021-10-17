#lang racket

(require "TDA_User.rkt")
(require "TDA_paradigmadocs.rkt")

(provide get_creador_doc)
(provide add_by_id)
(provide add_doc)
(provide ultimo_permiso)
(provide get_id)
(provide get_listas)
(provide get_doc_byid)
(provide tiene_shares?)
(provide usuarios_share)
(provide filtrar_ids)
(provide lista_usuarios_compartidos)
(provide mix)
(provide crear_permisos)
(provide filtrar_shares)
(provide get_all_versions_byid)
(provide get_version_byid)
(provide get_active_vr_byid)
(provide get_inactive_vers_byid)
(provide get_date_active_vr)
(provide get_id_active_vr)


(define (crear_permisos pdocs id x)
  (append (filtrar_ids id (get_lista_docs pdocs))(list(remove(filtrar_shares (filter list? (get_doc_byid id (get_lista_docs pdocs))))(append(get_doc_byid id (get_lista_docs pdocs))(list x))))))

(define(filtrar_shares lista)
  (if (empty? lista)
      null
  (if (eq? (car(car lista))"s")
           (car lista)
           (filtrar_shares (cdr lista)))))

; Implementación TDA DOC:

; Constructor:

(define (get_create x)
  (fourth x))
  
; Pertenencia

; ES_TITULO?

; ES_CONTENIDO?

; FUNCION GET_OPERATION:
; Funcion que retorna la operacion realizada por el usuario logeado, si el segundo elemento no es una lista, no hizo ninguna operación (retorna null)

(define (get_operation f)
           (if(operacion? f)
              (get_lista_logeados f)
              null))

; OPERACION?:
; Funcion que retorna #t si se hicieron operaciones en el documento de paradigmadocs

(define (operacion? f)
  (if (list?(get_lista_logeados f))
           #t
             #f))

(define (get_id x)
  (fourth x))

; GET_NAME_DOCUMENT:
; Funcion que retorna el nombre del documento creado por el usuario

(define (get_name_document f)
  (if (operacion? f) 
      (cadddr(get_operation f))
       null))

; GET_CREADOR_DOC: Funcion que retorna al creador del documento
; Dominio: int, lista
; Recorrido: string

(define (get_creador_doc id lista)
  (if (empty? lista) 
    ""
    (if (eq? id (get_create(car lista)))
        (car (car lista))
        (get_creador_doc id (cdr lista)))))

(define (add_id id f lista content)
  (if (empty? lista) 
    null
    (if (eq? id (get_create(car lista)))
        (cons(append (car lista) (list content))lista)
        (add_id id f (cdr lista)content))))

(define (borrar_by_id id lista)
  (if (empty? lista)
      null
      (if (not(eq? id (get_create(car lista))))
          (cons (car lista)(borrar_by_id id (cdr lista)))
          (borrar_by_id id (cdr lista)))))

; ADD_DOC: Funcion que añade un documento nuevo a la lista de documentos
; Dominio: paradigma_docs (lista) y X los elementos a ingresar
; Recorrido: Una version actualizada de paradigmadocs con el documento ingresado

(define (add_doc f x)
  (list (get_lista_registrados f) (get_lista_logeados f) (append (get_lista_docs f)(list x))))

; ADD_BY_ID: Funcion que añade operaciones a un documento creado anteriormente
; Dominio: paradigma_docs (lista) y el contenido a agregar
; Recorrido: 

(define (add_by_id id f content)
  (append (list(get_lista_registrados f))(list(get_lista_logeados f))(list(append (borrar_by_id id (get_lista_docs f))(list(car(add_id id f (get_lista_docs f)content)))))))

(define (ultimo_permiso lista_mix lista_unicos)
  (define (encap lista_mix lista_unicos)
  (if (null? lista_mix)
      null
  (if (list?(member (car(car lista_mix)) lista_unicos))
      (encap (cdr lista_mix) lista_unicos)
      (cons (car lista_mix) (encap (cdr lista_mix) (append (list(car(car lista_mix))) lista_unicos))))))
  (encap (reverse lista_mix) lista_unicos))

; GET_DOCUMENT_BYID: Funcion que retorna el documento y todas sus operaciones mediante su ID

(define (get_doc_byid id lista_docs)
  (if (empty? lista_docs)
      null
      (if (eq? id (get_id(car lista_docs)))
          (car lista_docs)
          (get_doc_byid id (cdr lista_docs)))))
      

; FILTRAR SOLO LAS QUE SON LISTAS
; SI ENCUENTRA UNA LISTA QUE TENGA un "s" al principio

(define (get_listas documento)
  (filter list? documento))
  

(define (tiene_shares? lista)
  (if (empty? lista)
      #f
      (if (eq? (car(car lista))"s")
          #t
          (tiene_shares? (cdr lista)))))

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

(define (lista_usuarios_compartidos lista)
  (if (empty? lista)
      null
      (cons (car (car lista )) (lista_usuarios_compartidos (cdr lista)))))

(define (mix lista_arrastre lista_actual)
  (if (empty? lista_arrastre)
      lista_actual
      (if (not(list?(member (car(car lista_arrastre)) (lista_usuarios_compartidos lista_actual))))
          (mix (cdr lista_arrastre) (append (list(car lista_arrastre))lista_actual))
          (mix (cdr lista_arrastre) lista_actual))))


; VERSIONES

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

(define (get_active_vr_byid id pdocs)
  (last(get_all_versions_byid id pdocs)))

; GET_INACTIVE_VERSIONS: Retorna una lista de las sesiones anteriores (no activas), dado un id de documento
; Dominio: id(int) paradigmadocs(lista)
; Recorrido: lista de versiones inactivas (lista)

(define (get_inactive_vers_byid id pdocs)
  (cdr(reverse(get_all_versions_byid id pdocs))))

; GET_DATE_ACTIVE_VERSION: Retorna la fecha(TDA Date) de la version activa
; Dominio: id(int) paradigmadocs(lista)
; Recorrido: lista (date)

(define (get_date_active_vr id pdocs)
  (car (get_active_vr_byid id pdocs)))

; GET_ID_ACTIVE_VERSION: Retorna el id de la versión activa
; Dominio: id(int) paradigmadocs(lista)
; Recorrido: id (int)

(define (get_id_active_vr id pdocs)
  (caddr (get_active_vr_byid id pdocs)))


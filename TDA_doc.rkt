#lang racket
(require "TDA_User.rkt")
(provide get_creador_doc)
(provide add_by_id)
(provide get_lista_docs)
(provide add_doc)

; Implementación TDA DOC:

; Constructor:

; Pertenencia

; ES_TITULO?

; ES_CONTENIDO?

; FUNCION GET_OPERATION:
; Funcion que retorna la operacion realizada por el usuario logeado, si el segundo elemento no es una lista, no hizo ninguna operación (retorna null)

(define (get_operation f)
           (if(operacion? f)
              (second f)
              null))

; OPERACION?:
; Funcion que retorna #t si se hicieron operaciones en el documento de paradigmadocs

(define (operacion? f)
  (if (list?(second f))
           #t
             #f))

; GET_LISTA_DOCS: Funcion que retorna la lista de los documentos de paradigmadocs
; Dominio: Paradigmadocs
; Recorrido: lista

(define (get_lista_docs f)
  (third f))

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
    (if (eq? id (fifth(car lista)))
        (car (car lista))
        (get_creador_doc id (cdr lista)))))


(define (add_id id f lista content)
  (if (empty? lista) 
    null
    (if (eq? id (fifth(car lista)))
        (cons(append (car lista) (list content))lista)
        (add_id id f (cdr lista)content))))

(define (borrar_by_id id lista)
  (if (empty? lista)
      null
      (if (not(eq? id (fifth(car lista))))
          (cons (car lista)(borrar_by_id id (cdr lista)))
          (borrar_by_id id (cdr lista)))))

; ADD_DOC: Funcion que añade un documento nuevo a la lista de documentos
; Dominio: paradigma_docs (lista) y X los elementos a ingresar
; Recorrido: Una version actualizada de paradigmadocs con el documento ingresado

(define (add_doc f x)
  (list (first f) (second f) (append (third f)(list x))))

; ADD_BY_ID: Funcion que añade operaciones a un documento creado anteriormente
; Dominio: paradigma_docs (lista) y el contenido a agregar
; Recorrido: 

(define (add_by_id id f content)
  (append (list(first f))(list(second f))(list(append (borrar_by_id id (get_lista_docs f))(list (car(add_id id f (get_lista_docs f) content)))))))

; funcion en duda
(define (shared user content f lista)
  (if (empty? lista)
      null
      (if (eq?(car(car lista)) user)
          (cons (append (car lista) (list content)) (get_lista_docs  f))
          (shared user content f (cdr lista)))))

; Funcion pendiente para probar si un permiso se duplica

(define (permiso_otorgado? f user lista)
  (if (list?(member user (lista user)))
      #t #f))

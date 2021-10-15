#lang racket
(require "TDA_User.rkt")

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

; GET_NAME_DOCUMENT:
; Funcion que retorna el nombre del documento creado por el usuario

(define (get_name_document f)
  (if (operacion? f) 
      (cadddr(get_operation f))
       null))
  
; GET_ID: Funcion que retorna la id del documento de paradigmadocs

(define (get_id_document f)
  (if (operacion? f) 
    (caddr(get_operation f))
      null))
  
(define (add_lista_operaciones f n)
  (list (first f)(second f)(append (list n)(third f))))

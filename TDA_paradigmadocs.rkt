#lang racket
; TDA Paradigma Docs
(require "TDA_fecha.rkt")
(provide paradigmadocs)

; CONSTRUCTOR:
; Funcion que hace referencia a crear un documento con su nombre, fecha de creación, funcion encriptar y desencriptar
; Representacion (nombre_doc, (date), encrypt, decrypt) 
; Dominio: String XX date XX encrypt XX decrypt
; Recorrido: Lista

(define (paradigmadocs name date encrypt decrypt)
  (list name date encrypt decrypt))

; TDA DATE

; TDA USER

; Constructor
; Crear Documento (TDA Documento)
; Crear Usuario (TDA Usuario)


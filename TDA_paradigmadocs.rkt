#lang racket
; TDA Paradigma Docs
(require "TDA_fecha.rkt")
(provide paradigmadocs)
(provide get_nombre_pdocs)

; REPRESENTACIÓN:

;(string X list X function X function)

; CONSTRUCTOR:

; Descripción: Funcion que hace referencia a crear un documento con su nombre, fecha de creación, funcion encriptar y desencriptar
; Representacion (nombre_doc, (date), encrypt, decrypt) 
; dom: String XX date XX encrypt XX decrypt
; rec: Lista

(define (paradigmadocs name date encrypt decrypt)
  (list name date encrypt decrypt))

; PERTENENCIA:

; Descripción: Verifica si el nombre es de tipo string, true, false
; dom: texto
; rec: boolean

(define (esnombre? name)
  (if (string? name)
      #t #f))

; SELECTORES:

; Descripción: Función que obtiene el nombre del espacio de trabajo
; dom: funcion
; rec: string

(define (get_nombre_pdocs f)
  (car f))

; Descripción: Función que obtiene la fecha del espacio de trabajo
; dom: funcion
; rec: fecha

(define (get_fecha f)
  (cadr f))

; MODIFICADORES:
; Descripción: Retorna una version actualizada con el nombre cambiado de paradigmadocs
; dom: funcion, nombre
; rec: lista, funcion

(define (cambiar_nombre f nombre)
  (if (esnombre? nombre)
  (list nombre (get_fecha f) (caddr f) (cadddr f)) null))

; Descripción: Retorna una version actualizada con la fecha cambiada de paradigmadocs
; dom: funcion, fecha
; rec: lista, funcion

(define (cambiar_fecha f fecha)
  (if (date? fecha)
      (list (get_nombre_pdocs f) fecha (caddr f) (cadddr f))f))

;EJEMPLO

(define pdocs (paradigmadocs "Word" (date 12 04 2001) "en" "vry"))
#lang racket
(require "TDA_Doc_27391503_FarfanCheneaux.rkt")
(require "TDA_Version_27391503_FarfanCheneaux.rkt")
(provide (all-defined-out))

; Implementación del TDA Memoria

; CONSTRUCTORES
; VERSION_MEMORIA: Constructor que crea una versión nueva al usar la función CTRLZ que crea una memoria en la version, quedando de la forma id date texto memoria
; Dominio: int X date X string X lista
; Recorrido: lista
(define (version_memoria id date texto memoria)
  (list id date texto memoria))

; CREAR_MEMORIA: Función que crea una lista (memoria) donde se almacena el "texto" de la versión que se quiere rehacer o deshacer cambios y "n" versiones restauradas o deshechas
; Dominio: paradigmadocs X int x int
; Recorrido: lista
(define (memoria pDocs idDoc n_versiones)
  (list (get_texto_version (get_active_version_byid pDocs idDoc)) n_versiones))

; DE PERTENENCIA

; TIENE_MEMORIA?: Funcion que obtiene la memoria de una version de un documento (si no tiene retorna una lista vacia)
; Dominio: paradigmadocs X int
; Recorrido: boolean
(define (tiene_memoria? pDocs idDoc)
  (if (eqv? (length (get_active_version_byid pDocs idDoc))4)
      #t
      #f))

; SELECTORES

; GET_MEMORIA: Función que obtiene la memoria de la versión activa de un documento
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_memoria pDocs idDoc )
  (if (tiene_memoria? pDocs idDoc)
      (cadddr (get_active_version_byid pDocs idDoc))        
      null))

; GET_MEMORY_STATE: Función que obtiene el estado de la memoria o de versiones restauradas y deshechas con CtrlZ y CtrlY
; Dominio: paradigmadocs X int
; Recorrido: int
(define (get_memory_state paradigmadocs idDoc)
  (if (null? (get_memoria paradigmadocs idDoc))
      null
      (cadr (get_memoria paradigmadocs idDoc))))

; GET_TEXT_MEMORY: Función que obtiene el texto original de la  versión donde se hizo CtrlZ por primera vez
; Dominio: paradigmadocs X int
; Recorrido: string
(define (get_text_memory paradigmadocs idDoc)
  (if (null? (get_memoria paradigmadocs idDoc))
      null
      (car (get_memoria paradigmadocs idDoc))))

; MODIFICADORES

; ACTUALIZAR_VZ: Función que actualiza la versión actual donde se hace  el cambio  de control Z
; Dominio: paradigmadocs X intX int
; Recorrido: lista
(define (actualizar_vz pDocs idDoc number_of_undo)
  (if (tiene_memoria? pDocs idDoc)
      (version_memoria (get_id_version (get_active_version_byid pDocs idDoc))(get_date_version (get_active_version_byid pDocs idDoc)) (get_texto_version (get_n_version (get_historialDoc_byid pDocs idDoc)  ( + (get_memory_state pDocs idDoc)number_of_undo))) (list (get_text_memory pDocs idDoc)(+ (get_memory_state pDocs idDoc)number_of_undo)))
      (version_memoria (get_id_version (get_active_version_byid pDocs idDoc))(get_date_version (get_active_version_byid pDocs idDoc)) (get_texto_version (get_n_version (get_historialDoc_byid pDocs idDoc)  number_of_undo))(memoria pDocs idDoc number_of_undo) )))

; ACTUALIZAR_VY: Al igual que "Actualizar vz", esta funcion retorna una versiona actualizada de la versión activa  (sin crear una nueva version) con los cambios
; en la memoria y en el contenido correspondientes
; Dominio: paradigmadocs X intX int
; Recorrido: lista
; Tipo de Recursividad: Recursividad Natural (restaura version)
(define (actualizar_vy pDocs idDoc number_of_redo)
  (if (>= number_of_redo (get_memory_state pDocs idDoc)) ; Si total CtrlZ's,(tambien CtrlY's) hechos y de CtrY que se quiere hacer son iguales, se restaura la versión original, como si no se hubiera hecho algun Ctrl
      (version (get_id_version (get_active_version_byid pDocs idDoc))(get_date_version (get_active_version_byid pDocs idDoc)) (get_text_memory pDocs idDoc))
      (version_memoria (get_id_version (get_active_version_byid pDocs idDoc))(get_date_version (get_active_version_byid pDocs idDoc)) (get_texto_version (get_n_version (get_historialDoc_byid pDocs idDoc)  ( - (get_memory_state pDocs idDoc)number_of_redo))) (list (get_text_memory pDocs idDoc)(- (get_memory_state pDocs idDoc)number_of_redo)))))
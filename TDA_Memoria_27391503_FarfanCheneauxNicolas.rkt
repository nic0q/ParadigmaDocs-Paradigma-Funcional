#lang racket
(require "TDA_paradigmadocs_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_Docs_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_fecha_27391503_FarfanCheneauxNicolas.rkt")
(provide (all-defined-out))

; Implementación del TDA Memoria

; CONSTRUCTORES

; CREAR_MEMORIA: Función que crea una lista (memoria) donde se almacena el "texto" de la versión que se quiere rehacer o deshacer cambios y "n" versiones restauradas o deshechas
; Dominio: paradigmadocs X int x int
; Recorrido: lista
(define (crear_memoria pDocs idDoc n_versiones)
  (list (get_texto_version (get_active_version_byid pDocs idDoc)) n_versiones))

; DE PERTENENCIA

; TIENE_MEMORIA?: Funcion que obtiene la memoria de una version de un documento (si no tiene retorna una lista vacia)
; Dominio: paradigmadocs X int
; Recorrido: boolean
(define (tiene_memoria? pDocs idDoc)
  (if (equal? (length (get_active_version_byid pDocs idDoc))4)
      #t
      #f))

; SELECTORES

; GET_MEMORIA: Función que obtiene la memoria de la versión activa de un documento
; Dominio: paradigmadocs X int
; Recorrido: lista
(define (get_memoria pDocs idDoc )
  (if (tiene_memoria? pDocs idDoc) ; Si el  largo es 4
      (cadddr (get_active_version_byid pDocs idDoc))         ; Tiene memoria
      null)) ;Si no, retorna null (NO hay lista de memoria)

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
(define (actualizar_vz pDocs idDoc number_of_undo)  ;  BUG: Si  number_of_undo   =   0
  (if (tiene_memoria? pDocs idDoc)
      (list (get_id_version (get_active_version_byid pDocs idDoc))(get_date_version (get_active_version_byid pDocs idDoc)) (get_texto_version (get_n_version (get_historialDoc_byid pDocs idDoc)  ( + (get_memory_state pDocs idDoc)number_of_undo))) (list (get_text_memory pDocs idDoc)(+ (get_memory_state pDocs idDoc)number_of_undo)))
      (list (get_id_version (get_active_version_byid pDocs idDoc))(get_date_version (get_active_version_byid pDocs idDoc)) (get_texto_version (get_n_version (get_historialDoc_byid pDocs idDoc)  number_of_undo))(crear_memoria pDocs idDoc number_of_undo) )))

; ACTUALIZAR_VY: Al igual que "Actualizar vz", esta funcion retorna una versiona actualizada de la versión activa  (sin crear una nueva version) con los cambios
; en la memoria y en el contenido correspondientes
; Dominio: paradigmadocs X intX int
; Recorrido: lista
; Tipo de Recursividad: Recursividad Natural (restaura version)
(define (actualizar_vy pDocs idDoc number_of_redo)
  (if (tiene_memoria? pDocs idDoc); Se comprueba que se haya hecho CtrlZ antes
      (if (eqv? number_of_redo (get_memory_state pDocs idDoc)) ; Si total CtrlZ's,(tambien CtrlY's) hechos y de CtrY que se quiere hacer son iguales, se restaura la versión original, como si no se hubiera hecho algun Ctrl
          (list (get_id_version (get_active_version_byid pDocs idDoc))(get_date_version (get_active_version_byid pDocs idDoc)) (get_text_memory pDocs idDoc))
      (list (get_id_version (get_active_version_byid pDocs idDoc))(get_date_version (get_active_version_byid pDocs idDoc)) (get_texto_version (get_n_version (get_historialDoc_byid pDocs idDoc)  ( - (get_memory_state pDocs idDoc)number_of_redo))) (list (get_text_memory pDocs idDoc)(- (get_memory_state pDocs idDoc)number_of_redo))))
      pDocs))
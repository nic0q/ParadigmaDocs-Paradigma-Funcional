#lang racket
(require "TDA_paradigmadocs.rkt")
(provide (all-defined-out))

; REGISTRADO_ANTES?: Retorna un dato booleano segun, el usurio esta registrado o no
; Dominio: paradigma_docs (lista), string
; Recorrido: boolean

(define (registrado_antes? pDocs user)
  (define (recorrer_lista lista_registrados)
  (if (empty? lista_registrados)
      #f
      (if (eqv? user (car(car lista_registrados)))
          #t
          (recorrer_lista (cdr lista_registrados)))))
  (recorrer_lista (get_lista_registrados pDocs)))

; GET_USERNAME: Obtiene el username de la lista del usuario
; Dominio: lista
; Recorrido: string

(define (get_username lista_usuario)
  (first lista_usuario))

; GET_PASSWORD: Obtiene la contraseña desencriptada de la lista del usuario
; Dominio: lista
; Recorrido: string

(define (get_password lista_usuario)
 (decryptFn (second lista_usuario)))

; TIENE_CUENTA?: Retorna un tipo de dato booleano si las credenciales son correctas a las registradas en paradigmadocs
; Dominio paradigma_docs (lista), string, string
; Recorrido: boolean
; Tipo de Recursividad: Recursividad Natural
; NOTA: La funcion recorrer lista registrados solo puede ser usada internamente por login

(define (tiene_cuenta? pDocs user pass)
  (define (recorrer_lista_registrados lista_registrados)
  (if (empty? lista_registrados)
      #f
      (if (and (equal? user (get_username (car lista_registrados)))(equal? pass (get_password (car lista_registrados))))
          #t
          (recorrer_lista_registrados (cdr lista_registrados)))))
  (recorrer_lista_registrados (get_lista_registrados pDocs)))

; Version declarativa:
; (filter (lambda (x) (and (equal? user (get_username x))(equal? pass (get_password x))))(get_lista_registrados pDocs)))

; LOGEAR: Funcion que añade al usuario a la lista de activos cuando se usa la funcion login
; Dominio: string (usuario)
; Recorrido: paradigmadocs (lista)
; Condiciones de su funcionamiento:
; 1) El usuario no puede logearse 2 veces
; 2) El usuario debe estar previamente registrado (la contraseña y el username deben coincidir), funcion encargada: "tiene_cuenta?"

(define (logear pDocs user pass)
  (define (aniadir_usuario_logeado pDocs user)
    (list (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(get_function1 pDocs)(get_function2 pDocs) (get_lista_registrados pDocs) (append (list user)(get_lista_logeados pDocs)) (get_lista_documentos pDocs)))
    (if (and (not(member user (get_lista_logeados pDocs)))(tiene_cuenta? pDocs user pass))
        (aniadir_usuario_logeado pDocs user)
        pDocs))

; LOGEADO?: Función que retorna un tipo de dato booleano si el usuario se ha logeado previamente
; Dominio: paradigma_docs (lista) usuario (string)
; Recorrido: booleano (#t o #f)

(define (logeado? pDocs user)
  (if (not(member user (get_lista_logeados pDocs)))
      #f
      #t))

; DESLOGEAR: Retorna una version actualizada de paradigmadocs con el usuario deslogeado
; Dominio: paradigma_docs (lista)
; Recorrido: paradigma_docs (lista)

(define (deslogear pDocs)
  (list (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(get_function1 pDocs)(get_function2 pDocs) (get_lista_registrados pDocs) null (get_lista_documentos pDocs)))


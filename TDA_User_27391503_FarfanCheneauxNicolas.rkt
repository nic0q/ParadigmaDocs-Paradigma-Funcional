#lang racket
(require "TDA_paradigmadocs_27391503_FarfanCheneauxNicolas.rkt")
(provide (all-defined-out))

; Implementación del TDA User

; PERTENENCIA:

; ES_USUARIO?
; Dominio: x
; Recorrido: boolean
(define (es_usuario? user)
  (string? user))

; ES_PASSWORD?: Funcion que comprueba si una contraseña válida
; Dominio: x
; Recorrido: boolean
(define (es_password? password)
  (string? password))

; REGISTRADO_ANTES?: Retorna un dato booleano segun, el usurio esta registrado o no
; Dominio: paradigma_docs (lista), string
; Recorrido: boolean
; Tipo de Recursividad: Recursividad de Cola
(define (registrado_antes? pDocs user)
  (define (recorrer_lista lista_registrados)
    (if (empty? lista_registrados)
        #f
        (if (eqv? user (car(get_username lista_registrados)))
            #t
            (recorrer_lista (cdr lista_registrados)))))
(recorrer_lista (get_lista_registrados pDocs)))

; TIENE_CUENTA?: Retorna un tipo de dato booleano si las credenciales son correctas a las registradas en paradigmadocs (username y password)
; Dominio paradigma_docs (lista), string, string
; Recorrido: boolean
; Tipo de Recursividad: Recursividad Natural
; NOTA: La funcion recorrer lista registrados solo puede ser usada internamente por login
(define (tiene_cuenta? pDocs user pass)
  (define (recorrer_lista_registrados lista_registrados)
  (if (empty? lista_registrados)
      #f
      (if (and (equal? user (get_username (car lista_registrados)))(equal? pass (get_password_desencrypt (car lista_registrados))))
          #t
          (recorrer_lista_registrados (cdr lista_registrados)))))
  (recorrer_lista_registrados (get_lista_registrados pDocs)))

; LOGEADO?: Función que retorna un tipo de dato booleano si el usuario se ha logeado previamente
; Dominio: paradigma_docs X user
; Recorrido: booleano (#t o #f)
(define (logeado? pDocs)
  (if (empty? (get_lista_logeados pDocs))
      #f
      #t))

; GET_CUENTA: Función que busca e imprime la cuenta mediante su user name de la forma (user X password X date )
; Dominio: paradigmadocs (lista) X user (string)
; Recorrido: lista
(define (get_cuenta pDocs user)
  (car(filter (λ (x) (equal? user (get_username x)))(get_lista_registrados pDocs))))
; La utilizacion de car aqui es por que filter crea una lista encima de la lista del usuario ya que siempre encontrará 1 elemento ya que los usuarios son unicos

; GET_USERNAME: Función que obtiene el username de una cuenta creada 
; Dominio: lista (username X pass X date)
; Recorrido: string 
(define (get_username cuenta)
  (car cuenta))

; GET_PASSWORD_CUENTA: Función que obtiene el username de una cuenta creada 
; Dominio: lista (user X password X date)
; Recorrido: password (string)
(define (get_password cuenta)
  (cadr cuenta))

; GET_FECHA_CREACION: Función que obtiene la fecha de creación de cuenta del usuario
; Dominio: lista (user X password X date)
; Recorrido: date (lista)
(define (get_fecha_creacion cuenta)
  (caddr cuenta))

; GET_FECHA_CREACION: Funcion que obtiene la fecha de la cuenta creada de la forma (user X pass X fecha)
; Dominio: paradigmadocs X user
; Recorrido: date
(define (get_fecha_creacion_cuenta pDocs user)
  (get_fecha_creacion (get_cuenta pDocs user)))

; GET_PASSWORD: Obtiene la contraseña desencriptada de la lista del usuario
; Dominio: lista (user X password X date)
; Recorrido: string
(define (get_password_desencrypt lista_usuario)
 (decryptFn (second lista_usuario)))

; GET_LOGEADO: Función que retorna al unico usuario logeado en paradigmadocs
; Dominio: lista
; Recorrido: string
(define (get_logeado pDocs)
  (car (get_lista_logeados pDocs)))

; MODIFICADORES

; REGISTRAR_USUARIO: Función que añade un elemento al final de la lista
; Dominio: lista x elemento
; Recorrido: lista
; Tipo de Recursividad: Recursividad Natural
(define (registrar_usuario lista n)
  (if (empty? lista)
      (cons n null)
      (cons (car lista) (registrar_usuario (cdr lista )n))))

; LOGEAR: Funcion que añade al usuario a la lista de activos cuando se usa la funcion login
; Dominio: paradigmadocs X user X pass 
; Recorrido: paradigmadocs (lista)
; Condiciones de su funcionamiento:
; 1) El usuario no puede logearse 2 veces
; 2) El usuario debe estar previamente registrado (la contraseña y el username deben coincidir), funcion encargada: "tiene_cuenta?"
; Tipo de Recursividad: Recursividad de cola (tiene_cuenta?)
(define (logear pDocs user pass)
  (define (aniadir_usuario_logeado pDocs user)
    (list (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(get_function1 pDocs)(get_function2 pDocs) (get_lista_registrados pDocs) (append (list user)(get_lista_logeados pDocs)) (get_documentos pDocs)))
  (if (and (not(member user (get_lista_logeados pDocs)))(tiene_cuenta? pDocs user pass))
      (aniadir_usuario_logeado pDocs user)
      pDocs))

; DESLOGEAR: Retorna una version actualizada de paradigmadocs con el usuario deslogeado
; Dominio: paradigma_docs (lista)
; Recorrido: paradigma_docs (lista)
(define (deslogear pDocs)
  (list (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(get_function1 pDocs)(get_function2 pDocs) (get_lista_registrados pDocs) null (get_documentos pDocs)))
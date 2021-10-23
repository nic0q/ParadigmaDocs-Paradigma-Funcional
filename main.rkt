#lang racket
(require "TDA_User.rkt")
(require "TDA_Docs.rkt")
(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")

(define emptyGDocs (paradigmadocs "NICO DOCS" (date 25 10 2021) encryptFn decryptFn))

; REGISTER
; Dominio: Archivo de tipo paradigma_docs, una fecha (TDA Date), username (String), password (string)
; Recorrido: Versión actualizada de paradigma docs, con los usuarios con username únicos registrados
; Representación: (paradigmadocs X string X string X date)
; Descripción: Funcion que mediante una composición de funciones permite registrar un grupo de usuarios únicos mediante la fecha username y
; contraseña del registro, siendo el username un identificador único, en caso existan duplicados, permite solo 1 con ese username
; Tipo de recursión: Recursión Natural (función registrado_antes?)

(define (register pDocs date user pass)
  (define (aniadir_usuario_registrado pDocs user pass date)
    (list (get_nombre_plataforma pDocs)(get_fecha_creacion_plataforma pDocs)(get_function1 pDocs)(get_function2 pDocs)(append (list (list user pass date)) (get_lista_registrados pDocs))(get_lista_logeados pDocs) (get_lista_documentos pDocs)))
    (if (registrado_antes? pDocs user)
        pDocs
        (aniadir_usuario_registrado pDocs user (encryptFn pass) date)))

;---------------------------------------------------------------------EJEMPLOS------------------------------------------------------------------------------------
(define gDocs1 (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 26 10 2021) "user2" "pass2") (date 27 10 2021) "user3" "pass3"))

; LOGIN:
; Dominio: Archivo de tipo paradigma_docs donde se encuentran los usuarios registrados
; Recorrido: Versión actualizada de paradigma docs, con los usuarios logeados o autenticados
; Representación: (paradigmadocs X string X string X function)
; Descripción:Funcion que si el usuario existe, (previamente registrado en paradigma_docs), la constraseña que ingresa es igual a la registrada, si esto se cumple se
; retorna una version actualizada de paradigma docs evaluada en operation y el usuario pasa a desconectado o deslogeado
; Tipo de Recursión: Recursión Natural (unción tiene_cuenta?, función registrado_antes?)

(define (login paradigmadocs username password operation)
    (if (tiene_cuenta? paradigmadocs username password) ; Si el usuario esta registrado en paradigma docs y tiene la contraseña correcta
        (cond
          [(eq? operation create) (lambda(date nombre contenido) (operation (logear paradigmadocs username password)date nombre contenido))]
          [(eq? operation share)  (lambda(idDoc access . accesses) (operation (logear paradigmadocs username password) idDoc (cons access accesses)))]
          [(eq? operation add)    (lambda(idDoc date content) (operation (logear paradigmadocs username password) idDoc date content))]
          [(eq? operation restoreVersion) (lambda(idDoc idVersion) (operation (logear paradigmadocs username password) idDoc idVersion))]
          [(eq? operation revokeAllAccesses)(operation (logear paradigmadocs username password))]
          [(eq? operation search) (lambda(searchText)  (operation (logear paradigmadocs username password) searchText))]
          [(eq? operation paradigmadocs->string)(operation (logear paradigmadocs username password))])
        (cond
          [(eq? operation create) (lambda(date name content)paradigmadocs)]
          [(eq? operation share)  (lambda(idDoc access . accesses)paradigmadocs)]
          [(eq? operation add)    (lambda(idDoc date content) paradigmadocs)]
          [(eq? operation restoreVersion) (lambda(idDoc idVersion) paradigmadocs)]
          [(eq? operation revokeAllAccesses) paradigmadocs]
          [(eq? operation search) (lambda(searchText) paradigmadocs)]
          [(eq? operation paradigmadocs->string)paradigmadocs]
          [(eq? operation search) (lambda(searchText) paradigmadocs)]
          [(eq? operation paradigmadocs->string)paradigmadocs])))

; CREATE
; TITUTO PROPIETARIO (VERSION ACTIVA (FECHA CONTENIDO) ID) (HISTORIAL DE VERSIONES)
; Descripción: Permite al usuario de paradigmadocs crear un documento en una fecha determinada, un nombre y su contenido
; NOTA: La función encapsulada "nuevo_documento" solo puede ser llamada por esta función
; Representación: (paradigmadocs X string X string X function)
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con el documento creado por el usuario y su desconección de la sesión
  
(define (create paradigmadocs date nombre contenido)
  (define (nuevo_documento pDocs date title contenido creador)
    (modificar_documento paradigmadocs (append (list(list title creador (length(get_lista_documentos pDocs))(list(list (length(get_historialDoc_byid pDocs (length(get_lista_documentos pDocs)))) date contenido))null))(get_lista_documentos pDocs))))
  (if (and (logeado? paradigmadocs)(string? nombre)(string? contenido))
      (deslogear (nuevo_documento paradigmadocs date nombre (encryptFn contenido) (get_logeado paradigmadocs)))
      paradigmadocs))

;------------------------------------------EJEMPLOS-----------------------------------------------------
(define gDocs2 ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc1" "DOC1"))
(define gDocs3 ((login gDocs2 "user2" "pass2" create) (date 30 09 2021) "doc2" "DOC2"))
(define gDocs4 ((login gDocs3 "user3" "pass3" create) (date 30 10 2021) "doc3" "DOC3"))

;gDocs2
;gDocs3
;gDocs4

; SHARE
; Descripción: Permite al usuario (propietario de un documento) en paradigmadocs, compartir un documento mediante su id con otros usuarios, otorgando permisos de lectura/escritura/comentarios
; Condiciones para su funcionamiento:
; 1) ¿El creador del documento es el mismo que desea compartir? -> Si, se ejecuta la funcion compartir; No, la funcion no se ejecuta
; 2) ¿Los usuarios con los que sea quiere compartir, estan registrados ? -> Si, se agrega a la lista de compartidos; No, no se agrega a la lista de compartidos
; 3) El usuario no puede compartir un documento consigo mismo
; 4) Los permisos otorgados solo pueden ser (#\r / #\w / #\c)
; 5) El usuario puede quitar y colocar distintos permisos, el ultimo dado a tal usuario es el activo
; Representación: (paradigmadocs XX int XX access [string char])
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con los documentos compartidos por este, id y lista de usuarios con los que se comparte
; Tipo de Recursividad: Recursividad Natural (Funciones encapsuladas de filtrar_permisos)

(define (share paradigmadocs idDoc accesses)
  (if (logeado? paradigmadocs)
      (if (equal? (get_logeado paradigmadocs) (get_creadorDoc_byid paradigmadocs idDoc))
          (deslogear (modificar_documento paradigmadocs (aniadir_permisos paradigmadocs idDoc (filtrar_permisos (get_compartidosDoc_byid paradigmadocs idDoc) accesses paradigmadocs idDoc))))
          (deslogear paradigmadocs))
      paradigmadocs))

;---------------------------------------------------------------------EJEMPLOS------------------------------------------------------------------------------------
(define gDocs5 ((login gDocs4 "user1" "pass1" share) 0 (access "user1" #\y )(access "user1" #\y)(access "user3" #\r) (access "user2" #\w)))
(define gDocs6 ((login gDocs5 "user2" "pass2" share) 1 (access "user1" #\w )(access "user2" #\c)(access "user3" #\w)))
(define gDocs7 ((login gDocs6 "user3" "pass3" share) 2 (access "user1" #\r )(access "user1" #\w)(access "user2" #\c)(access "user2" #\c)(access "user5" #\c)))

;gDocs5
;gDocs6
;gDocs7

;ADD
; Representación: (paradigmadocs XX int XX date X string XX)
; Descripción: Funcion que añade texto al final de la ultima versión y crea una nueva version
; Condiciones para su funcionamiento:
; 1) Solo el usuario y los usuarios con los que el creador del documento haya compartido el documento pueden agregar texto a la ultima version (Funcion users_with_all_access)
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con los documentos compartidos por este, id y lista de usuarios con los que se comparte
; Tipo de Recursividad: Recursividad Natural (Funciones encapsuladas users_with_all_access)

(define (add paradigmadocs idDoc date contenidoTexto)
  (define (add_texto_nueva_vr pDocs idDoc date texto) ;Funcion propia de la Funcion ADD que añade texto al FINAL, nota: para fnciones como apply styles, comment, podría resultar util aplicar otras funciones
    (list (length(get_historialDoc_byid pDocs idDoc)) date(encryptFn(string-join (list(decryptFn(get_contenido_active_version pDocs idDoc)) texto)))))
  (define (actualizar pDocs idDoc)
    (modificar_documento paradigmadocs (append (list(aniadir_version_activa paradigmadocs idDoc (add_texto_nueva_vr pDocs idDoc date  contenidoTexto)))
     (filter (lambda (x) (not(equal? idDoc (get_id_documento x))))(get_lista_documentos pDocs)))))
      (if (logeado? paradigmadocs)
          (if (member (get_logeado paradigmadocs) (users_with_all_access paradigmadocs idDoc))
              (deslogear (actualizar paradigmadocs idDoc))
              (deslogear paradigmadocs))
          paradigmadocs))

;--------------------------------EJEMPLOS-----------------------------------------
(define gDocs8 ((login gDocs7 "user2" "pass2" add) 0 (date 20 10 2021) "DDA"))
(define gDocs9 ((login gDocs8 "user1" "pass1" add) 0 (date 22 10 2021) "DA"))

;gDocs8
;gDocs9

; RESTORE VERSION
; Representación: (paradigmadocs XX int XX int XX)
; Descripción: Función que 'restaura' un versión, es decir que la versión escogida pasa a ser la versión activa en el documento
; NOTA: La ultima version actual es la ultima en la lista de versiones creadas, es posible obtenerla mediante un selector llamado (get_active_vr_byid idDoc pdocs)
; Condiciones para su funcionamiento:
; 1) La versión debe existir, de lo contrario retorna paradigmadocs sin modificaciones
; Dominio: Archivo de paradigmadocs
; Recorrido: Archivo actualizado de paradigmadocs con la ultima version activa
; Tipo de Recursividad: Recursividad Natural

(define (restoreVersion paradigmadocs idDoc idVersion)
  (define (actualizar_documento pDocs idDoc)
    (modificar_documento paradigmadocs (append (list(nuevo_historial paradigmadocs idDoc
    (append (filter (lambda (x) (equal? idVersion (get_id_version x))) (get_historialDoc_byid paradigmadocs idDoc))
            (filter (lambda (x) (not(equal? idVersion (get_id_version x)))) (get_historialDoc_byid paradigmadocs idDoc)))))
     (filter (lambda (x)(not(equal? idDoc (get_id_documento x))))(get_lista_documentos paradigmadocs)))))
  (if (logeado? paradigmadocs)
      (if (equal? (get_logeado paradigmadocs) (get_creadorDoc_byid paradigmadocs idDoc))
          (deslogear (actualizar_documento paradigmadocs idDoc))
          (deslogear paradigmadocs))
      paradigmadocs))

;--------------------------------EJEMPLOS-----------------------------------
(define gDocs10 ((login gDocs9 "user1" "pass1" restoreVersion) 0 1))

; FUNCION REVOKE ALL ACCESSES
; Representación: (paradigmadocs XX)
; Descripción: Funcion que elimina los permisos que el usuario ha otorgado a otros usuarios de todos sus documentos
; Dominio: Archivo de paradigmadocs
; Recorrido: Archivo de paradigmadocs con todos los permisos de los documentos del usuario eliminados
; Tipo de Recursividad: Solo las utilizadas por las funciones declarativas (filter, map)

(define (eliminar_permisos documento)
  (if (not (null? (last documento)))
      (append (remove (last documento)documento) (list null)) ; Si tiene usuarios con permisos, se agrega reemplaza por una lista vacía
      documento)) ; Si no solo devuelve el documento

(define (revokeAllAccesses paradigmadocs)
  (define (actualizar_documento pDocs)
    (if (empty? (filter (lambda (x) (equal? (get_logeado paradigmadocs) (get_autor x))) (get_lista_documentos paradigmadocs)))
        pDocs 
        (modificar_documento paradigmadocs (append (filter (lambda (x) (not(equal? (get_logeado paradigmadocs) (get_autor x)))) (get_lista_documentos paradigmadocs)) 
                                                   (map eliminar_permisos (filter (lambda (x) (equal? (get_logeado paradigmadocs) (get_autor x))) (get_lista_documentos paradigmadocs)))))))
  (if (logeado? paradigmadocs)                ;Elimina los permisos de los documentos que es usuario y no hace nada a los documentos que no es creador mediante filter
      (deslogear(actualizar_documento paradigmadocs))
      paradigmadocs))

;--------------------------EJEMPLOS--------------------------------
(define gDocs11  (login gDocs10 "user1" "pass1" revokeAllAccesses))
(define gDocs011 (login gDocs11 "user2" "pass2" revokeAllAccesses))
(define gDocs0011 (login gDocs011 "user7" "pass3" revokeAllAccesses))

; SEARCH
; Representación: (paradigmadocs X string X)
; Descripción: Permite al usuario buscar texto en sus documentos (propios o compartidos por otros usuarios) tanto en las versiones actuales como en las antiguas
; en caso el texto exista, retornará todos los documentos donde se encuentra, sino retornara un string vació (no hay documentos)
; NOTA: De manera declarativa se utilizo la funcion filter en la funcion encuentra_texto?
; Dominio: Archivo de paradigmadocs
; Recorrido: Lista con los documentos que contienen al texto buscado
; Tipo de Recursividad: Recursividad Natural
; Funciones Declarativas: Filter / map en get_ocurrencias y documentos_con_acceso

(define (search paradigmadocs texto)
  (if (logeado? paradigmadocs (get_logeado paradigmadocs))
      (map (lambda (x) (get_doc_byId paradigmadocs x))
           (filter (lambda (x) (not(null? x))) (get_ocurrencias (get_id_documentos_acceso paradigmadocs (get_logeado paradigmadocs)) paradigmadocs (decryptFn texto))))
      null))

;-------------------EJEMPLOS----------------------
;((login gDocs11 "user1" "pass1" search) "DOC")

;gDocs11

; PARADIGMADOCS->STRING
; Función que crea una versión de 'string' para que el contenido de la plataforma paradigmadocs sea entendible por el usuario
; Dominio: paradigmadocs (lista)
; Recorrido: string
; Tipo de Recursión: Solo funciones de forma declarativa: documentos->string, registrados->string, accesses->string, historial->string
(define (paradigmadocs->string paradigmadocs) 
  (if (logeado? paradigmadocs)
      (string-append (make-string 15 #\*)" "(get_logeado paradigmadocs)" "(make-string 15 #\*)"\nCuenta creada el "(date->string (get_fecha_creacion_cuenta paradigmadocs (get_logeado paradigmadocs)))"\n\nEs creador de:"
    (documentos->string (map (lambda(x)(get_doc_byId paradigmadocs x)) (get_id_documentos_creados paradigmadocs (get_logeado paradigmadocs))))"\n"(make-string 15 #\-) "\nTiene acceso a: " (documentos->string (map (lambda(x)(get_doc_byId paradigmadocs x)) (get_id_documentos_compartidos paradigmadocs (get_logeado paradigmadocs)))))
      (string-append (make-string 15 #\*)" "(get_nombre_plataforma paradigmadocs)" "(make-string 15 #\*)"\nFecha de creación: "(date->string (get_fecha_creacion_plataforma paradigmadocs))
                 "\n\n➤USUARIOS REGISTRADOS:"(registrados->string paradigmadocs)"\n\n➤DOCUMENTOS:"(documentos->string (get_lista_documentos paradigmadocs)))))

;-----------------------------EJEMPLOS--------------------------------
(define gDocs12 (login gDocs11 "user3" "pass3" paradigmadocs->string)) ;(display gDocs12)
(define gDocs012 (login gDocs11 "user1" "pass3" paradigmadocs->string)) ;(display gDocs012)
(define gDocs0012 (login gDocs11 "user2" "pass2" paradigmadocs->string)) ;(display gDocs0012)
(define gDocs00012 (login gDocs11 "noregistrado" "pass2" paradigmadocs->string)) ; Si aplicamos display aparecerá una versión sin tabulador de paradigmadocs, no creará el string
(define gDocs000012 (paradigmadocs->string gDocs11)) ;(display gDocs000012)
(define gDocs13 (paradigmadocs->string gDocs11))     ;(display gDocs13)

; CAMBIAR EL PASO DE PARAMETRO DEL USUARIO (ESTA MAL) ; GET USUARIO LOGEADO Y ERA

; ENCRYPT
; Encrypt: Funcion que encrypta texto de una forma distinta al laboratorio
; Dominio: str (texto)
; Recorrido: bytes (encryptado)
(define (encrypt str)
  (bytes->list (string->bytes/utf-8 str)))

; DECRYPT
; Decrypt: Función que desencrypt texto de una forma distinta al laboratorio
; Dominio: bytes (encryptado)
; Recorrido: str (texto desencryptado)
(define (decrypt str)
  (bytes->string/utf-8 (list->bytes str)))

;-------------------------------EJEMPLOS---------------------------------
;(encrypt "contraseña1") ; (99 111 110 116 114 97 115 101 195 177 97 49)
;(decrypt (encrypt "contraseña1")) ; "contraseña1"



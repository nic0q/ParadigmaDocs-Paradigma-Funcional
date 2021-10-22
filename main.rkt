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
(define gDocs1 (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 25 10 2021) "user2" "pass2") (date 25 10 2021) "user3" "pass3"))

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
          [(eq? operation create) (lambda(date nombre contenido) (operation (logear paradigmadocs username password)date nombre contenido username))]
          [(eq? operation share)  (lambda(idDoc access . accesses) (operation (logear paradigmadocs username password) idDoc (cons access accesses) username))]
          [(eq? operation add)    (lambda(idDoc date content) (operation (logear paradigmadocs username password) idDoc date content username))]
          [(eq? operation restoreVersion) (lambda(idDoc idVersion) (operation (logear paradigmadocs username password) idDoc idVersion username))]
          [(eq? operation revokeAllAccesses)(operation (logear paradigmadocs username password) username)]
          [(eq? operation search) (lambda(searchText)  (operation (logear paradigmadocs username password) searchText username))])
        (cond
          [(eq? operation create) (lambda(date name content)paradigmadocs)]
          [(eq? operation share)  (lambda(idDoc access . accesses)paradigmadocs)]
          [(eq? operation add)    (lambda(idDoc date content) paradigmadocs)]
          [(eq? operation restoreVersion) (lambda(idDoc idVersion) paradigmadocs)]
          [(eq? operation revokeAllAccesses) paradigmadocs])))

; CREATE:
; TITUTO PROPIETARIO (VERSION ACTIVA (FECHA CONTENIDO) ID) (HISTORIAL DE VERSIONES)
; Descripción: Permite al usuario de paradigmadocs crear un documento en una fecha determinada, un nombre y su contenido
; NOTA: La función encapsulada "nuevo_documento" solo puede ser llamada por esta función
; Representación: (paradigmadocs X string X string X function)
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con el documento creado por el usuario y su desconección de la sesión
  
(define (create paradigmadocs date nombre contenido user)
  (define (nuevo_documento pDocs date title contenido creador)
    (modificar_documento paradigmadocs (append (list(list title creador (length(get_lista_documentos pDocs))(list(list (length(get_historialDoc_byid pDocs (length(get_lista_documentos pDocs)))) date contenido))null))(get_lista_documentos pDocs))))
  (if (and (logeado? paradigmadocs user)(string? nombre)(string? contenido))
      (deslogear(nuevo_documento paradigmadocs date nombre (encryptFn contenido) user))
      paradigmadocs))

;------------------------------------------EJEMPLOS-----------------------------------------------------
(define gDocs2 ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc1" "DOC1"))
(define gDocs3 ((login gDocs2 "user2" "pass2" create) (date 30 09 2021) "doc2" "DOC2"))
(define gDocs4 ((login gDocs3 "user3" "pass3" create) (date 30 10 2021) "doc3" "DOC3"))

;gDocs2
;gDocs3
;gDocs4

; SHARE:
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

(define (share paradigmadocs idDoc accesses user)
  (if (logeado? paradigmadocs user)
      (if (equal? user (get_creadorDoc_byid paradigmadocs idDoc))
          (deslogear (modificar_documento paradigmadocs (aniadir_permisos paradigmadocs idDoc (filtrar_permisos (get_compartidosDoc_byid paradigmadocs idDoc) accesses paradigmadocs idDoc))))
          (deslogear paradigmadocs))
      paradigmadocs))

;---------------------------------------------------------------------EJEMPLOS------------------------------------------------------------------------------------
(define gDocs5 ((login gDocs4 "user1" "pass1" share) 0 (access "user1" #\r )(access "user1" #\c)(access "user1" #\c) (access "user2" #\w)))
(define gDocs6 ((login gDocs5 "user2" "pass2" share) 1 (access "user1" #\w )(access "user2" #\c)(access "user3" #\w)))
(define gDocs7 ((login gDocs6 "user3" "pass3" share) 2 (access "user1" #\r )(access "user1" #\w)(access "user2" #\c)(access "user2" #\c)(access "user5" #\c)))

;gDocs5
;gDocs6
;gDocs7

;ADD:
; Representación: (paradigmadocs XX int XX date X string XX)
; Descripción: Funcion que añade texto al final de la ultima versión y crea una nueva version
; Condiciones para su funcionamiento:
; 1) Solo el usuario y los usuarios con los que el creador del documento haya compartido el documento pueden agregar texto a la ultima version (Funcion users_with_all_access)
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con los documentos compartidos por este, id y lista de usuarios con los que se comparte
; Tipo de Recursividad: Recursividad Natural (Funciones encapsuladas users_with_all_access)

(define (add paradigmadocs idDoc date contenidoTexto user)
  (define (add_texto_nueva_vr pDocs idDoc date texto) ;Funcion propia de la Funcion ADD que añade texto al FINAL, nota: para fnciones como apply styles, comment, podría resultar util aplicar otras funciones
    (list (length(get_historialDoc_byid pDocs idDoc)) date(encryptFn(string-join (list(decryptFn(get_contenido_active_version pDocs idDoc)) texto)))))
  (define (actualizar pDocs idDoc)
    (modificar_documento paradigmadocs (append (list(aniadir_version_activa paradigmadocs idDoc (add_texto_nueva_vr pDocs idDoc date  contenidoTexto)))
     (filter (lambda (x) (not(equal? idDoc (get_id_documento x))))(get_lista_documentos pDocs)))))
      (if (logeado? paradigmadocs user)
          (if (member user (users_with_all_access paradigmadocs idDoc))
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

; GET_ID_VERSION: Funcion interna de restoreversion

(define (restoreVersion paradigmadocs idDoc idVersion user)
  (define (actualizar_documento pDocs idDoc)
    (modificar_documento paradigmadocs (append (list(nuevo_historial paradigmadocs idDoc
    (append (filter (lambda (x) (equal? idVersion (get_id_version x))) (get_historialDoc_byid paradigmadocs idDoc))
            (filter (lambda (x) (not(equal? idVersion (get_id_version x)))) (get_historialDoc_byid paradigmadocs idDoc)))))
     (filter (lambda (x)(not(equal? idDoc (get_id_documento x))))(get_lista_documentos paradigmadocs)))))
  (if (logeado? paradigmadocs user)
      (if (equal? user (get_creadorDoc_byid paradigmadocs idDoc))
          (deslogear (actualizar_documento paradigmadocs idDoc))
          (deslogear paradigmadocs))
      paradigmadocs))

;--------------------------------EJEMPLOS-----------------------------------
(define gDocs10 ((login gDocs9 "user1" "pass1" restoreVersion) 0 1))

;gDocs10

; FUNCION REVOKE ALL ACCESSES
; Representación: (paradigmadocs XX)
; Descripción: Funcion que elimina los permisos que el usuario ha otorgado a otros usuarios de todos sus documentos
; Dominio: Archivo de paradigmadocs
; Recorrido: Archivo de paradigmadocs con todos los permisos de los documentos del usuario eliminados
; Tipo de Recursividad: Recursividad Natural
; Función Declarativa: crear_documento (map eliminar_permisos)

(define (eliminar_permisos documento)
  (if (not (null? (last documento)))
      (append (remove (last documento)documento) (list null))
      documento))

(define (revokeAllAccesses paradigmadocs user)
  (define (actualizar_documento pDocs)
    (if (empty? (filter (lambda (x) (equal? user (get_autor x))) (get_lista_documentos paradigmadocs)))
        pDocs
        (modificar_documento paradigmadocs (map eliminar_permisos (filter (lambda (x) (equal? user (get_autor x))) (get_lista_documentos paradigmadocs))))))
  (if (logeado? paradigmadocs user)
      (deslogear(actualizar_documento paradigmadocs))
      paradigmadocs))

;--------------------------EJEMPLOS-------------------------------
(define gDocs11 (login gDocs10 "user1" "pass1" revokeAllAccesses))

;gDocs11

; FUNCIÓN SEARCH
; Representación: (paradigmadocs XX string XX)
; Descripción: Permite al usuario buscar texto en sus documentos (propios o compartidos por otros usuarios) tanto en las versiones actuales como en las antiguas
; en caso el texto exista, retornará todos los documentos donde se encuentra, sino retornara un string vació (no hay documentos)
; NOTA: De manera declarativa se utilizo la funcion filter en la funcion encuentra_texto?
; Dominio: Archivo de paradigmadocs
; Recorrido: Lista con los documentos que contienen al texto buscado
; Tipo de Recursividad: Recursividad Natural
; Funciones Declarativas: Filter / map en get_ocurrencias y documentos_con_acceso

(define (search paradigmadocs texto user)
  (if (logeado? paradigmadocs user)
      (map (lambda (x) (get_doc_byId paradigmadocs x))
           (filter (lambda (x) (not(null? x)))(get_ocurrencias (get_id_versiones_acceso paradigmadocs user) paradigmadocs (decryptFn texto))))
      null))

;-------------------EJEMPLOS----------------------
;((login gDocs11 "user1" "pass1" search) "DOC")

;gDocs11

; FUNCIÓN PARADIGMADOCS->STRING
; 2 casos: Al usarlo sin login y con login
;(get_lista_registrados gDocs11)



; REGISTRADOS->STRING: Función que obtiene una versión 'string' de la lista de usuarios registrados en la plataforma paradigmadocs
; Dominio: paradigmadocs (lista)
; Recorrido: string
; Tipo de Recursión: Solo funciones declarativas
(define (registrados->string pDocs)
      (string-join(map (lambda (username pass date)
      (string-append "\nUsuario: "username"\nContraseña: "pass"\nFecha Registro: "(date->string date)"\n"(make-string 35 #\-)))
           (map (lambda (x) (get_username x)) (get_lista_registrados pDocs))
           (map (lambda (x) (get_password x)) (get_lista_registrados pDocs))
           (map (lambda (x) (get_fecha_creacion x)) (get_lista_registrados pDocs)))))

; ACCESSES->STRING: Función que convierte a string los accessos que tienen los usuarios al documento,
; además la función encapsulada access_name convierte a string el char, entendible por el usuario
; Dominio: access list (lista_accesses)
; Recorrido: string
; Tipo de Recursión: Solo funciones declarativas
(define (accesses->string lista_accesses)
  (define (access_name pr)
    (cond 
      [(eqv? pr #\w) "escritura"]
      [(eqv? pr #\c) "comentarios"]
      [(eqv? pr #\r) "lectura"]))
  (string-join(map (lambda (user permiso)
                     (string-append"\n"user" ➞ permiso de "(access_name permiso)))
                   (map (lambda (x) (get_usuario_share x))lista_accesses)
                   (map (lambda (x) (get_permiso_share x))lista_accesses))))

; HISTORIAL->STRING: Función que convierte el historial en una versión string
; Dominio: lista (historial_doc)
; Recorrido: string
; Tipo de Recursión: Solo funciones declarativas
(define (historial->string historial_doc)
  (string-join(map (lambda (idVer date texto)
                     (string-append "\nid_versión: "(number->string idVer)"\nCreada el "(date->string date)"\nContenido: "texto"\n* * * * * * * * *\n"))
                   (map (lambda (x) (get_id_version x)) historial_doc)
                   (map (lambda (x) (get_date_version x))historial_doc)
                   (map (lambda (x) (get_texto_version x))historial_doc))))

; DOCUMENTOS->STRING: Función que obtiene una versión 'string' de toda la información de los documentos de paradigmadocs,
; Componentes Documentos: NombreDoc, Autor, id, versión activa, historial, Usuarios con acceso
; Dominio: lista X paradigmadocs
; Recorrido: string
(define (documentos->string pDocs)
  (string-join(map (lambda (nombre_doc autor id historial shares)
                     (string-append "\n"nombre_doc"\nAutor: "autor"\nid: "(number->string id)"\n\nHistorial de Documentos:\n☞Versión Activa:" (historial->string historial)
                                    "\nUsuarios con acceso:"(accesses->string (filter (lambda (x)(not(null? x)))shares))"\n\n"))
                   (map (lambda (x) (get_nombre_documento x))(get_lista_documentos pDocs))
                   (map (lambda (x) (get_autor x))(get_lista_documentos pDocs))
                   (map (lambda (x) (get_id_documento x))(get_lista_documentos pDocs))
                   (map (lambda (x) (get_historial_documento x))(get_lista_documentos pDocs))
                   (map (lambda (x) (get_lista_compartidos x))(get_lista_documentos pDocs)))))

; PARADIGMADOCS->STRING: Función que crea una versión de 'string' para que el contenido de la plataforma paradigmadocs sea entendible por el usuario
; Dominio: paradigmadocs (lista)
; Recorrido: string
; Tipo de Recursión: Solo funciones de forma declarativa: documentos->string, registrados->string, accesses->string, historial->string
(define (paradigmadocs->string pDocs)
  (string-append (make-string 15 #\*)" "(get_nombre_plataforma pDocs)" "(make-string 15 #\*)"\nFecha de creación: "(date->string (get_fecha_creacion_plataforma pDocs))
                 "\n\n➤USUARIOS REGISTRADOS:"(registrados->string pDocs)"\n\n➤DOCUMENTOS:"(documentos->string pDocs)))
gDocs10

(display(paradigmadocs->string gDocs10))

; Función Encrypt y Decrypt

; Encrypt: Funcion que encrypta texto de una forma distinta al laboratorio
; Dominio: str (texto)
; Recorrido: bytes (encryptado)
(define (encrypt str)
  (bytes->list (string->bytes/utf-8 str)))

; Decrypt: Función que desencrypt texto de una forma distinta al laboratorio
; Dominio: bytes (encryptado)
; Recorrido: str (texto desencryptado)
(define (decrypt str)
  (bytes->string/utf-8 (list->bytes str)))

;--------------------------EJEMPLOS-------------------------------
(encrypt "contraseña1") ; (99 111 110 116 114 97 115 101 195 177 97 49)
(decrypt (encrypt "contraseña1")) ; "contraseña1"

  
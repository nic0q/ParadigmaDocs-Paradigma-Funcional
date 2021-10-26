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
; Tipo de Recursividad: Recursión Natural (función registrar_usuario)

(define (register paradigmadocs date username password)
  (define (aniadir_usuario_registrado paradigmadocs user pass date) ;(modificar_documento pDocs contenido) ; (registrar (get_lista_registrados paradigmadocs) (list(list user pass date))
    (list (get_nombre_plataforma paradigmadocs)(get_fecha_creacion_plataforma paradigmadocs)(get_function1 paradigmadocs)(get_function2 paradigmadocs) (registrar_usuario (get_lista_registrados paradigmadocs) (list user pass date)) (get_lista_logeados paradigmadocs) (get_lista_documentos paradigmadocs)))
    (if (and (es_usuario? username)(es_password? password)(date? date)(not(registrado_antes? paradigmadocs username)))
        (aniadir_usuario_registrado paradigmadocs username ((get_function1 paradigmadocs) password) date)
        paradigmadocs))
        

;---------------------------------------------------------------------EJEMPLOS------------------------------------------------------------------------------------
(define gDocs1 (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 26 10 2021) "user2" "pass2") (date 27 10 2021) "user3" "pass3"))
(define gDocs01 (register (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 26 10 2021) "user3" "pass3") (date 27 10 2021) "user1" "passxx")(date 27 10 2021) "user4" "pass4"))
(define gDocs001 (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 26 10 2021) "user1" "pass2") (date 27 10 2021) "user1" "pass3"))
;gDocs1
;gDocs01
;gDocs001

; LOGIN:
; Dominio: Archivo de tipo paradigma_docs donde se encuentran los usuarios registrados
; Recorrido: Versión actualizada de paradigma docs, con los usuarios logeados o autenticados
; Representación: (paradigmadocs X string X string X function)
; Descripción:Funcion que si el usuario existe, (previamente registrado en paradigma_docs), la constraseña que ingresa es igual a la registrada, si esto se cumple se
; retorna una version actualizada de paradigma docs evaluada en operation y el usuario pasa a desconectado o deslogeado
; Tipo de Recursión: Recursión Natural (unción tiene_cuenta?, función registrado_antes?)

(define (login paradigmadocs username password operation)
        (cond
          [(eq? operation create) (lambda(date nombre contenido) (operation (logear paradigmadocs username password)date nombre contenido))]
          [(eq? operation share)  (lambda(idDoc accesslist . accesses) (operation (logear paradigmadocs username password) idDoc (cons accesslist accesses)))]
          [(eq? operation add)    (lambda(idDoc date content) (operation (logear paradigmadocs username password) idDoc date content))]
          [(eq? operation restoreVersion) (lambda(idDoc idVersion) (operation (logear paradigmadocs username password) idDoc idVersion))]
          [(eq? operation revokeAllAccesses)(operation (logear paradigmadocs username password))]
          [(eq? operation search) (lambda(searchText)  (operation (logear paradigmadocs username password) searchText))]
          [(eq? operation paradigmadocs->string) (operation (logear paradigmadocs username password))]
          [(eq? operation delete) (lambda (idDoc date numberOfCharacters)(operation (logear paradigmadocs username password) idDoc date numberOfCharacters))]
          [(eq? operation searchAndReplace) (lambda (idDoc date searchText replaceText)(operation (logear paradigmadocs username password) idDoc date searchText replaceText))]
          [(eq? operation comment) (lambda (idDoc date selectedText commenText)(operation (logear paradigmadocs username password) idDoc date selectedText commenText))]
          [(eq? operation applyStyles) (lambda (idDoc date searchText accesslist . styles)(operation (logear paradigmadocs username password) idDoc date searchText (cons accesslist styles)))]
          [else null]))

; CREATE
; TITUTO PROPIETARIO (VERSION ACTIVA (FECHA CONTENIDO) ID) (HISTORIAL DE VERSIONES)
; Descripción: Permite al usuario de paradigmadocs crear un documento en una fecha determinada, un nombre y su contenido
; NOTA: La función encapsulada "nuevo_documento" solo puede ser llamada por esta función
; Representación: (paradigmadocs X string X string X function)
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con el documento creado por el usuario y su desconección de la sesión
  
(define (create paradigmadocs date nombre contenido)
  (define (nuevo_documento pDocs date title contenido creador)
    (set_documento paradigmadocs (append (list(list title creador (length(get_lista_documentos pDocs))(list(list (length(get_historialDoc_byid pDocs (length(get_lista_documentos pDocs)))) date contenido))null))(get_lista_documentos pDocs))))
  (if (logeado? paradigmadocs)
      (if (and (date? date)(es_nombre? nombre)(es_texto? contenido))
          (deslogear (nuevo_documento paradigmadocs date nombre ((get_function1 paradigmadocs) contenido) (get_logeado paradigmadocs)))
          (deslogear paradigmadocs))
      paradigmadocs))

;------------------------------------------EJEMPLOS-----------------------------------------------------
(define gDocs2  ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc1" "Documento1 creado por user1"))
(define gDocs3  ((login gDocs2 "user2" "pass2" create) (date 30 09 2021) "doc2" "Documento1 creado por user2"))
(define gDocs04 ((login gDocs3 "user3" "pass3" create) (date 30 10 2021) "doc3" "Documento1 creado por user3"))
(define gDocs4  ((login gDocs04 "user1" "pass1" create) (date 30 10 2021) "doc4" "Documento2 creado por user1"))
(define gDocs02 ((login gDocs4 "user3" "pass3" create) (date 30 10 2021) "doc4" "Documento2 creado por user3")) ;Contraseña incorrecta-> no logeado -> doc no creado

;gDocs2
;gDocs3
;gDocs4
;gDocs02

; SHARE
; Descripción: Permite al usuario (propietario de un documento) en paradigmadocs, compartir un documento mediante su id con otros usuarios, otorgando permisos de lectura/escritura/comentarios
; Condiciones para su funcionamiento:
; 1) ¿El creador del documento es el mismo que desea compartir? -> Si, se ejecuta la funcion compartir; No, la funcion no se ejecuta
; 2) ¿Los usuarios con los que sea quiere compartir, estan registrados ? -> Si, se agrega a la lista de compartidos; No, no se agrega a la lista de compartidos
; 3) El usuario no puede compartir un documento consigo mismo
; 4) Los permisos otorgados solo pueden ser (#\r || #\w || #\c)
; 5) El usuario puede quitar y colocar distintos permisos, el ultimo dado a tal usuario es el activo
; Representación: (paradigmadocs XX int XX access [string char])
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con los documentos compartidos por este, id y lista de usuarios con los que se comparte
; Tipo de Recursividad: Recursividad Natural (Funciones encapsuladas de filtrar_permisos)

(define (share paradigmadocs idDoc accesses)
  (if (logeado? paradigmadocs)
      (if (and (es_id? idDoc)(equal? (get_logeado paradigmadocs) (get_creadorDoc_byid paradigmadocs idDoc)))
          (deslogear (set_documento paradigmadocs (aniadir_permisos paradigmadocs idDoc (filtrar_permisos (get_compartidosDoc_byid paradigmadocs idDoc) accesses paradigmadocs idDoc))))
          (deslogear paradigmadocs))
      paradigmadocs))

;---------------------------------------------------------------------EJEMPLOS------------------------------------------------------------------------------------
(define gDocs5 ((login gDocs4 "user1" "pass1" share) 0 (access "user1" #\y )(access "user1" #\t)(access "user3" #\r) (access "user2" #\c)(access "user2" #\o)(access "user10" #\c)))
(define gDocs05 ((login gDocs5 "user1" "pass1" share) 0 (access "user1" #\y )(access "user1" #\t)(access "user3" #\r) (access "user2" #\w)(access "user2" #\o)(access "user10" #\c))) ;se comparte nuevamente el doc:id:0 cambiando el permiso de user2#\c a user2 #\w, los demas permisos se mantienen intactos
(define gDocs6 ((login gDocs05 "user2" "pass2" share) 1 (access "user1" #\i )(access "user2" #\c)(access "user3" #\w)(access "user3" #\q)))
(define gDocs07 ((login gDocs6 "user3" "pass3" share) 2 (access "user1" #\r )(access "user1" #\w)(access "user2" #\c)(access "user2" #\c)(access "user5" #\c)))
(define gDocs7 ((login gDocs07 "user1" "pass1" share) 3 (access "user1" #\r )(access "user2" #\w)(access "user2" #\c)(access "user3" #\w)(access "user9" #\c)))

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
    (list (length(get_historialDoc_byid pDocs idDoc)) date ((get_function1 paradigmadocs) (string-join (list(decryptFn(get_contenido_active_version pDocs idDoc)) texto)))))
      (if (logeado? paradigmadocs)
          (if (and (es_id? idDoc)(es_texto? contenidoTexto)(date? date)(member (get_logeado paradigmadocs) (get_editor_users paradigmadocs idDoc)))
              (deslogear (set_version paradigmadocs idDoc (add_texto_nueva_vr paradigmadocs idDoc date contenidoTexto)))
              (deslogear paradigmadocs))
          paradigmadocs))

;--------------------------------EJEMPLOS-----------------------------------------
(define gDocs8 ((login gDocs7 "user2" "pass2" add) 0 (date 20 10 2021) "Add")) ;user2 tiene permiso de escritura en doc1
(define gDocsej9 ((login gDocs8 "user1" "pass1" add) 0 (date 21 10 2021) "Palabra"))  ;el creador agrega texto nuevamente en doc1
(define gDocs9 ((login gDocsej9 "user1" "pass1" add) 2 (date 21 10 2021) "Word2"))  ;user2 tiene permiso en escritura en doc3, añade texto
(define gDocs09 ((login gDocs9 "user1" "pass1" add) 2 (date 21 10 2021) "Añado en doc3")) ;user1 tiene permiso de escritura en doc3
(define gDocs009 ((login gDocs09 "user1" "pass1" add) 1 (date 21 10 2021) "No añado en doc2")) ;user1 NO tiene permiso de escritura en doc2, no añade texto
(define gDocs0009 ((login gDocs009 "user3" "pass3" add) 1 (date 21 10 2021) "No Añado doc2")) ;user3 NO tiene permiso de escritura en doc2, solo lectura, no añade texto

;gDocs8
;gDocs9
;gDocs0009

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
    (set_documento paradigmadocs (append (list(nuevo_historial paradigmadocs idDoc
    (append (filter (lambda (x) (equal? idVersion (get_id_version x))) (get_historialDoc_byid paradigmadocs idDoc))
            (filter (lambda (x) (not(equal? idVersion (get_id_version x)))) (get_historialDoc_byid paradigmadocs idDoc)))))
     (filter (lambda (x)(not(equal? idDoc (get_id_documento x))))(get_lista_documentos paradigmadocs)))))
  (if (logeado? paradigmadocs)
      (if (and (es_id? idDoc)(es_id? idVersion)(equal? (get_logeado paradigmadocs) (get_creadorDoc_byid paradigmadocs idDoc)))
          (deslogear (actualizar_documento paradigmadocs idDoc))
          (deslogear paradigmadocs))
      paradigmadocs))

;--------------------------------EJEMPLOS-----------------------------------
(define gDocs010 ((login gDocs9 "user1" "pass1" restoreVersion) 0 1))     ;user1 creador de doc1, se restaura versión 1, se verifica mediante: (get_id_version(get_active_version_byid gDocs010 0)) -> 1
(define gDocs0ex10 ((login gDocs010 "user2" "pass2" restoreVersion) 1 1)) ;user2 creador de doc, pero la version 1 no existe, no se restura:   (get_id_version(get_active_version_byid gDocs0ex10 1))
(define gDocs0010 ((login gDocs010 "user1" "pass1" restoreVersion) 2 0))  ;user1 no es el creador de doc2, no se restaura versión:             (get_id_version(get_active_version_byid gDocs0010 2)) -> 1
(define gDocs10 ((login gDocs0010 "user3" "pass3" restoreVersion) 2 0))   ;user3 es el creador de doc2, se restaura versión 0:                 (get_id_version(get_active_version_byid gDocs10 2)) -> 0

;gDocs10

; REVOKE ALL ACCESSES
; Representación: (paradigmadocs XX)
; Descripción: Funcion que elimina los permisos que el usuario ha otorgado a otros usuarios de todos sus documentos
; Dominio: Archivo de paradigmadocs
; Recorrido: Archivo de paradigmadocs con todos los permisos de los documentos del usuario eliminados
; Tipo de Recursividad: Solo las utilizadas por las funciones declarativas (filter, map)

(define (eliminar_permisos documento)
  (if (not (null? (get_lista_compartidos documento))) ; Si la lista de compartidos no es nula:
      (append (remove (get_lista_compartidos documento)documento) (list null)) ; Se agrega reemplaza por una lista vacía
      documento))                                             ; Si no solo devuelve el documento

(define (revokeAllAccesses paradigmadocs)
  (define (actualizar_documento)
    (if (empty? (filter (lambda (x) (equal? (get_logeado paradigmadocs) (get_autor x))) (get_lista_documentos paradigmadocs)))
        paradigmadocs 
        (set_documento paradigmadocs (append (filter (lambda (x) (not(equal? (get_logeado paradigmadocs) (get_autor x)))) (get_lista_documentos paradigmadocs)) 
                                                   (map eliminar_permisos (filter (lambda (x) (equal? (get_logeado paradigmadocs) (get_autor x))) (get_lista_documentos paradigmadocs)))))))
  (if (logeado? paradigmadocs)                ;Elimina los permisos de los documentos que es usuario y no hace nada a los documentos que no es creador mediante filter
      (deslogear(actualizar_documento))
      paradigmadocs))

;--------------------------EJEMPLOS--------------------------------
(define gDocs11  (login gDocs10 "user1" "pass1" revokeAllAccesses)) ;user1 es propietario de doc4 y doc1, se eliminan todos los permisos para acceder a estos documentos
(define gDocs011 (login gDocs11 "user2" "pass2" revokeAllAccesses)) ;user2 es propietario de doc2, se eliminan todos los permisos para acceder a estos documentos
(define gDocs0011 (login gDocs011 "user7" "passxyz" revokeAllAccesses)) ;usuario no esta registrado
(define gDocs00011 (login gDocs0011 "user3" "pass3" revokeAllAccesses)) ;user2 es propietario de doc3, se eliminan todos los permisos para acceder a estos documentos

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
  (if (and (logeado? paradigmadocs)(es_texto? texto))
      (map (lambda (x) (get_doc_byId paradigmadocs x))
           (filter (lambda (x) (not(null? x))) (get_id_ocurrencias (get_id_documentos_acceso paradigmadocs (get_logeado paradigmadocs)) paradigmadocs (decryptFn texto))))
      null))

;-------------------EJEMPLOS----------------------
;((login gDocs11 "user1" "pass1" search) "por")     ;encuentra "por" en: doc1 (creador), doc4(creador), doc3(le fue compartido)
;((login gDocs11 "user2" "pass2" search) "Doc")     ;encuentra "Doc" em doc2 (creador), doc3(le fue compartido)
;((login gDocs11 "user1" "pass1" search) "ejemplo") ;no encuentra "ejemplo" en ningún documento
;((login gDocs11 "user6" "pass6" search) "Doc")      ;no busca texto, el usuario no está registrado
;((login gDocs11 "user3" "pass123" search) "por")      ;no busca texto, el "user" está registrado, pero Contraseña incorrecta

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

;-----------------------------EJEMPLOS-------------------------------------------
(define gDocs12 (login gDocs11 "user3" "pass3" paradigmadocs->string))           ;(display gDocs12)     -> "user3" registrado *versión LOGIN "user3"*
(define gDocs012 (login gDocs11 "user1" "pass3" paradigmadocs->string))          ;(display gDocs012)    -> user1 no registrado (constraseña incorrecta) versión NO LOGIN
(define gDocs0012 (login gDocs11 "user2" "pass2" paradigmadocs->string))         ;(display gDocs0012)   -> "user2" registrado *versión LOGIN "user2"*
(define gDocs00012 (login gDocs11 "user10" "pass2" paradigmadocs->string))       ;(display gDocs00012)  -> user10 no registrado, versión NO LOGIN
(define gDocs13 (paradigmadocs->string gDocs11))                                 ;(display gDocs13)     -> versión NO LOGIN

; DELETE

(define (delete paradigmadocs idDoc date numberOfCharacters)
  (define (nueva_ver) ;Funcion propia (encapsulada) de la Funcion DELETE que elimina texto del final
    (if (>= numberOfCharacters (string-length(get_contenido_active_version paradigmadocs idDoc)))
        (list (length(get_historialDoc_byid paradigmadocs idDoc)) date "") ;; RETORNA CADENA VACIA, ya que se borro todo el texto ((get_function2 pDocs)get_contenido_active_version)
        (list (length(get_historialDoc_byid paradigmadocs idDoc)) date ((get_function1 paradigmadocs) (substring ((get_function2 paradigmadocs)(get_contenido_active_version paradigmadocs idDoc))0(-(string-length (get_contenido_active_version paradigmadocs idDoc)) numberOfCharacters)))))) ; Se elimina el texto
  (if (logeado? paradigmadocs)
      (if (and (es_id? idDoc)(date? date)(integer? numberOfCharacters)(not (eqv? 0 numberOfCharacters))(member (get_logeado paradigmadocs) (get_editor_users paradigmadocs idDoc))) ;si pertenece a los editores, si el numero de caracteres a eliminar es distinto de 0 ya que no borraria nada
          (deslogear (set_version paradigmadocs idDoc(nueva_ver))) ; Se crea una nueva versión con el texto eliminado
          (deslogear paradigmadocs)) ; Si no no se hace cambios al documento
      paradigmadocs))

;-----------------------------EJEMPLOS-------------------------------------------
(define gDocs0140 ((login gDocs11 "user1" "pass1" delete) 0 (date 30 11 2021) 0))         ; No se elimina nada, no se crea una nueva versión
(define gDocs014 ((login gDocs11 "user1" "pass1" delete) 0 (date 30 11 2021) 1))          ; Se eliminan los 10 ultimos caracteres de la versión activa de doc0 y se actualiza la ultima versión activa
(define gDocs0014 ((login gDocs014 "user2" "pass2" delete) 1 (date 30 11 2021) 20))       ; Se eliminan los 20 ultimos caracteres de la versión activa de doc2 y se actualiza la ultima versión activa
(define gDocs00014 ((login gDocs0014 "user3" "pass3" delete) 1 (date 30 11 2021) 20))     ; Se eliminan todo el contenido de la ultima version de doc2, ya que user3 tiene permiso de escritura en este documento
(define gDocs000014 ((login gDocs00014 "user2" "pass2" delete) 2 (date 30 11 2021) 20))   ; Usuario no tiene permiso de escritura, no se hace ningun cambio a doc3
(define gDocs15 ((login gDocs000014 "user5" "pass1" delete) 1 (date 30 11 2021) 20))      ; Usuario no registrado

(define (searchAndReplace paradigmadocs idDoc date searchText replaceText)
  (if (logeado? paradigmadocs)
      (if (and (member (get_logeado paradigmadocs)(get_editor_users paradigmadocs idDoc))(not(eqv? searchText replaceText))(es_id? idDoc)(date? date)(es_texto? searchText)(es_texto? replaceText))
          (if (string-contains? (get_contenido_active_version paradigmadocs idDoc) ((get_function2 paradigmadocs)searchText)) ; Si encuentra texto
              (deslogear (set_version paradigmadocs idDoc (list(set_id_vr paradigmadocs idDoc) date (string-replace (get_contenido_active_version paradigmadocs idDoc) ((get_function2 paradigmadocs)searchText) ((get_function2 paradigmadocs)replaceText)))))
              (deslogear paradigmadocs))
          (deslogear paradigmadocs))
      paradigmadocs))

;----------------------------------------EJEMPLOS-------------------------------------------------
(define gDocs16 ((login gDocs15 "user1" "pass1" searchAndReplace ) 0 (date 20 10 2021) "por" "change"))  ; Se crea una nueva versión con "por" reemplazado por "change"
(define gDocs016 ((login gDocs15 "user1" "pass1" searchAndReplace ) 0 (date 20 10 2021) "por" "por"))         ; Se pretende cambiar el texto por el mismo texto, no se crea una nueva version
gDocs16
; COMMENT: Función que permite comentar texto seleccionado de la ultima versión de el documento, solo los usuarios con acceso a comentarios, escritura o creadores pueden comentar
; Dominio: paradigmadocs X int X date X String X String
; Recorrido: paradigmadocs
; Tipo de Recursión: Solo en funciones declarativas (map)
(define (comment paradigmadocs idDoc date selectedText commenText)
  (if  (logeado? paradigmadocs)
      (if (and(member (get_logeado paradigmadocs)(get_editor_and_comment_users paradigmadocs idDoc))(es_id? idDoc)(date? date)(es_texto? selectedText)(es_texto? commenText))
          (if (string-contains? (get_contenido_active_version paradigmadocs idDoc) ((get_function2 paradigmadocs)selectedText)) ; Si encuentra texto
              (deslogear (set_version paradigmadocs idDoc (list(set_id_vr paradigmadocs idDoc) date (string-replace (get_contenido_active_version paradigmadocs idDoc)((get_function2 paradigmadocs)selectedText)(string-append ((get_function2 paradigmadocs)selectedText) " ->%c "((get_function2 paradigmadocs)commenText) " c% ")))))
              (deslogear paradigmadocs))
          (deslogear paradigmadocs))
      paradigmadocs))


(define gDocs17 ((login gDocs16 "user2" "pass2" comment ) 2 (date 20 10 2021)  "1"  "comentario"))

;gDocs17

; APPLYSTYLES:
; Dominio: paradigmadocs X int X date X String X char List
; Recorrido: paradigmadocs

(define (applyStyles paradigmadocs idDoc date searchText styles)
  (if (logeado? paradigmadocs)
      (if (and (es_id? idDoc)(date? date)(es_texto? searchText)(member (get_logeado paradigmadocs )(get_editor_users paradigmadocs idDoc)))
          (if (string-contains? (get_contenido_active_version paradigmadocs idDoc) ((get_function2 paradigmadocs)searchText)) ; Si encuentra texto
              (deslogear (set_version paradigmadocs idDoc (list(set_id_vr paradigmadocs idDoc) date (string-replace (get_contenido_active_version paradigmadocs idDoc) ((get_function2 paradigmadocs)searchText) (string-append " "(set_styles styles)" "((get_function2 paradigmadocs)searchText)" "(set_styles styles)" ")))))
              ;(filter (lambda(x)(member x (list #\i #\b #\u)))styles)
              ; por algun motivo, crea 2 "" al principio y al final, por eso reverse y cdr)
              (deslogear paradigmadocs))
          (deslogear paradigmadocs))
      paradigmadocs))

;----------------------------------------EJEMPLOS-------------------------------------------------
(define gDocs18 ((login gDocs15 "user1" "pass1" applyStyles) 0 (date 20 10 2021) "por"  #\b #\u #\i #\y))

;gDocs18

; ENCRYPT: Función que encripta texto de forma distinta a la propuesta
; Dominio: string
; Recorrido: string
; Tipo de Recursividad: Solo en funciones declarativas (map)
(define (encrypt texto)
  (list->string (map(lambda(x)(integer->char x)) (map(lambda(x)(+ x 5))(map (lambda(x) (char->integer x)) (string->list texto))))))

; ENCRYPT: Función que desencripta texto de forma distinta a la propuesta
; Dominio: string
; Recorrido: string
; Tipo de Recursividad: Solo en funciones declarativas (map)
(define (decrypt texto)
  (list->string(map(lambda(x)(integer->char x)) (map(lambda(x)(- x 5))(map (lambda(x) (char->integer x))(string->list texto))))))

;-------------------------------EJEMPLOS---------------------------------
;(decrypt (encrypt "contraseña1234"))
;(decrypt (encrypt "laboratorio1"))
;(decrypt (encrypt "paradigma_funcional"))
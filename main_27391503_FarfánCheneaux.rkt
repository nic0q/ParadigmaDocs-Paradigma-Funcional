#lang racket
(require "TDA_paradigmadocs_27391503_FarfanCheneaux.rkt")
(require "TDA_Memoria_27391503_FarfanCheneaux.rkt")
(require "TDA_Version_27391503_FarfanCheneaux.rkt")
(require "TDA_Access_27391503_FarfanCheneaux.rkt")
(require "TDA_fecha_27391503_FarfanCheneaux.rkt")
(require "TDA_User_27391503_FarfanCheneaux.rkt")
(require "TDA_Doc_27391503_FarfanCheneaux.rkt")

; REGISTER
; Descripción: Funcion que mediante una composición de funciones permite registrar un grupo de usuarios únicos mediante la fecha, username y
; contraseña del registro, siendo el username un identificador único, no se permiten duplicados
; Dominio: Archivo de tipo paradigma_docs, una fecha (date), username (string), password (string)
; Recorrido: Versión actualizada de paradigma docs, con los usuarios con username únicos registrados
; Tipo de Recursividad: Recursión Natural (función registrar_usuario)
(define (register paradigmadocs date username password)
  (define (aniadir_usuario_registrado paradigmadocs user pass date) ;(modificar_documento pDocs contenido) ; (registrar (get_lista_registrados paradigmadocs) (list(list user pass date))
    (list (get_nombre_plataforma paradigmadocs)(get_fecha_creacion_plataforma paradigmadocs)(get_function1 paradigmadocs)(get_function2 paradigmadocs) (registrar_usuario (get_lista_registrados paradigmadocs) (usuario user pass date)) (get_lista_logeados paradigmadocs) (get_documentos paradigmadocs)))
  (if (and (es_usuario? username)(es_password? password)(date? date)(not(registrado_antes? paradigmadocs username)))
      (aniadir_usuario_registrado paradigmadocs username ((get_function1 paradigmadocs) password) date)
      paradigmadocs))

; LOGIN:
; Descripción: Funcion que inicia sesion en paradigmadocs e hace un llamado a una 'operation' disponible en paradigmadocs, dentro de cada una de estas se comprobará si el usuario que logeo correctamente
; Dominio: paradigmadocs X string X string X operation
; Recorrido: operation -> paradigmadocs
; Tipo de Recursión: Recursión de cola (función logear -> función tiene_cuenta?)
(define (login paradigmadocs username password operation)
        (cond 
          [(eqv? operation create) (λ (date nombre contenido) ((operation (logear paradigmadocs username password))date nombre contenido))]
          [(eqv? operation share)  (λ (idDoc access . accesses) ((operation (logear paradigmadocs username password)) idDoc access accesses))]
          [(eqv? operation add)    (λ (idDoc date content) ((operation (logear paradigmadocs username password)) idDoc date content))]
          [(eqv? operation restoreVersion) (λ (idDoc idVersion) ((operation (logear paradigmadocs username password)) idDoc idVersion))]
          [(eqv? operation revokeAllAccesses)(operation (logear paradigmadocs username password))]
          [(eqv? operation search) (λ (searchText)  ((operation (logear paradigmadocs username password)) searchText))]
          [(eqv? operation paradigmadocs->string) (operation (logear paradigmadocs username password))]
          [(eqv? operation delete) (λ (idDoc date numberOfCharacters)((operation (logear paradigmadocs username password)) idDoc date numberOfCharacters))]
          [(eqv? operation searchAndReplace) (λ (idDoc date searchText replaceText)((operation (logear paradigmadocs username password)) idDoc date searchText replaceText))]
          [(eqv? operation comment) (λ (idDoc date selectedText commenText)((operation (logear paradigmadocs username password)) idDoc date selectedText commenText))]
          [(eqv? operation applyStyles) (λ (idDoc date searchText accesslist . styles)((operation (logear paradigmadocs username password)) idDoc date searchText (cons accesslist styles)))]
          [(eqv? operation ctrlZ) (λ (idDoc numberOfUndo)((operation (logear paradigmadocs username password)) idDoc numberOfUndo))]
          [(eqv? operation ctrlY) (λ (idDoc numberOfRedo)((operation (logear paradigmadocs username password)) idDoc numberOfRedo))]
          [else null]))

; CREATE
; Descripción: Permite al usuario de paradigmadocs crear un documento en una fecha determinada, un nombre y su contenido encryptado por la función propia de paradigmadocs
; Función encapsulada: Nuevo documento
; Dominio: paradigmadocs X date X string X string
; Recorrido: paradigmadocs
; Tipo de Recursividad: funciones declarativas en set_documento

 ; Función Encapsulada:
 ; PRIMER_DOCUMENTO: Añade un nuevo documento, con una primera versión
 ; Dominio: paradigmadocs X date X string X string X string
 ; Recorrido: paradigmadocs

(define (create paradigmadocs)
  (λ(date nombre contenido)
    (define (primer_documento pDocs date title contenido creador)
      (set_documento pDocs (append (list(documento title creador (set_id_doc pDocs) (list (version 0 date contenido))null))(get_documentos pDocs))))
    (if (logeado? paradigmadocs)
        (if (and (date? date)(es_nombre? nombre)(es_texto? contenido))
            (deslogear (primer_documento paradigmadocs date nombre ((get_function1 paradigmadocs) contenido) (get_logeado paradigmadocs)))
            (deslogear paradigmadocs))
        paradigmadocs)))

; SHARE
; Descripción: Permite al usuario (propietario de un documento) en paradigmadocs, compartir un documento mediante su id con otros usuarios, otorgando permisos de lectura/escritura/comentarios
; Condiciones para su funcionamiento:
; 1) ¿El creador del documento es el mismo que desea compartir? -> Si, se ejecuta la funcion compartir; No, la funcion no se ejecuta
; 2) ¿Los usuarios con los que sea quiere compartir, estan registrados ? -> Si, se agrega a la lista de compartidos; No, no se agrega a la lista de compartidos
; 3) El usuario no puede compartir un documento consigo mismo
; 4) Los permisos otorgados solo pueden ser los characters (#\r || #\w || #\c)
; 5) El usuario puede quitar y colocar distintos permisos, el ultimo dado a tal usuario es el activo
; Dominio: paradigmadocs
; Recorrido: paradigmadocs X int X access list
; Tipo de Recursividad: Recursividad Natural (función filtrar_permisos)
(define (share paradigmadocs)
  (λ(idDoc access . accesses)
    (if (logeado? paradigmadocs)
        (if (and (es_id? idDoc)(eqv? (get_logeado paradigmadocs) (get_creadorDoc_byid paradigmadocs idDoc)))
            (deslogear (set_documento paradigmadocs (set_permisos paradigmadocs idDoc (filtrar_permisos (get_compartidosDoc_byid paradigmadocs idDoc) (get_accesses accesses) paradigmadocs idDoc))))
            (deslogear paradigmadocs))
        paradigmadocs)))

; ADD
; Descripción: Funcion que añade texto al final de la ultima versión, creando una nueva version, solo los usuarios con permiso de edición (propietarios o les fue compatido),pueden usar esta herramienta
; Dominio: paradigmadocs X int X date X string
; Recorrido: paradigmadocs
; Tipo de Recursividad: Recursividad (Funciones declarativas en get_editor_users, set_version)
(define (add paradigmadocs)
  (λ (idDoc date contenidoTexto)
   (if (logeado? paradigmadocs)
       (if (and (es_id? idDoc)(es_texto? contenidoTexto)(date? date)(member (get_logeado paradigmadocs) (get_editor_users paradigmadocs idDoc)))
           (deslogear (set_version paradigmadocs idDoc  (version (set_id_vr paradigmadocs idDoc)date ((get_function1 paradigmadocs) (string-join(list(decryptFn(get_contenido_active_version paradigmadocs idDoc)) contenidoTexto))))))
           (deslogear paradigmadocs))
       paradigmadocs)))

; RESTORE VERSION
; Descripción: Función que 'restaura' un versión, es decir que la versión escogida pasa a ser la versión activa en el documento, si la versión no existe se retorna p.docs sin
; modificar, además solo el propietario de los documentos puede restaurar una versión
; NOTA: La ultima version actual es la ultima en la lista de versiones creadas, es posible obtenerla mediante un selector llamado (get_active_version_byid idDoc paradigmadocs)
; Condiciones para su funcionamiento:
; Dominio: paradigmadocs X int X int
; Recorrido: paradigmadocs
; Tipo de Recursividad: funciones declarativas (filter, map)
(define (restoreVersion paradigmadocs)
 (λ (idDoc idVersion)
  (if (logeado? paradigmadocs)
      (if (and (es_id? idDoc)(es_id? idVersion)(eqv? (get_logeado paradigmadocs)(get_creadorDoc_byid paradigmadocs idDoc)))
          (deslogear (set_documento paradigmadocs (append (list (list (get_tituloDoc_byid paradigmadocs idDoc) (get_creadorDoc_byid paradigmadocs idDoc) idDoc
                                                                      (append (filter (λ (version) (eqv? idVersion (get_id_version version))) (get_historialDoc_byid paradigmadocs idDoc))
                                                                              (filter (λ (version) (not(eqv? idVersion (get_id_version version)))) (get_historialDoc_byid paradigmadocs idDoc))) (get_compartidosDoc_byid paradigmadocs idDoc)))
                                                          (filter (λ (documento)(not(eqv? idDoc (get_id_documento documento))))(get_documentos paradigmadocs)))))
          (deslogear paradigmadocs))
      paradigmadocs)))

; REVOKE ALL ACCESSES
; Descripción: Funcion que elimina los permisos que el usuario ha otorgado a otros usuarios de todos sus documentos, solo el creador de un documento puede usar esta función
; Dominio: paradigmadocs
; Recorrido: paradigmadocs
; Tipo de Recursividad: funciones declarativas (filter, map)
(define (revokeAllAccesses paradigmadocs)
  (if (logeado? paradigmadocs)                     
      (if (empty? (filter (λ (documento)(eqv? (get_logeado paradigmadocs)(get_autor documento)))(get_documentos paradigmadocs)))
          (deslogear paradigmadocs )
          (deslogear(set_documento paradigmadocs(append(filter (λ (documento) (not(eqv? (get_logeado paradigmadocs) (get_autor documento)))) (get_documentos paradigmadocs)) 
                                                         (map eliminar_permisos (filter (λ (documento) (eqv? (get_logeado paradigmadocs) (get_autor documento))) (get_documentos paradigmadocs)))))))
      paradigmadocs))

; SEARCH
; Descripción: Función  que permite al usuario buscar texto en sus documentos (propios o compartidos por otros usuarios) tanto en las versiones actuales como en las antiguas
; en caso el texto exista, retornará todos los documentos donde se encuentra, sino retornara un string vacío (no hay documentos)
; Dominio: paradigmadocs
; Recorrido: lista
; Tipo de Recursividad: Recursivida Natural (get_id_ocurrencias) y funciones declarativas (map, filter)
(define (search paradigmadocs)
 (λ (searchText)
  (if (and (logeado? paradigmadocs)(es_texto? searchText))
      (map (λ (id) (get_doc_byId paradigmadocs id))
           (filter (λ (id) (not(null? id))) (get_id_ocurrencias (get_id_documentos_acceso paradigmadocs (get_logeado paradigmadocs)) paradigmadocs ((get_function2 paradigmadocs)searchText))))
      null)))

; PARADIGMADOCS->STRING
; Descripción: Función que permite crear una versión de 'string' organizado del documento para que el contenido de la plataforma paradigmadocs sea entendible por el usuario;
; si el usuario se logea y usa esta función le muestra toda la información sobre sus documentos, sino muestra información general de la plataforma
; Dominio: paradigmadocs
; Recorrido: string
; Tipo de Recursividad: Solo funciones de forma declarativa (map) en documentos->string, registrados->string, accesses->string, historial->string

 ; Funciones encapsuladas:
 ; REGISTRADOS->STRING: Función que obtiene una versión 'string' de la lista de usuarios registrados en la plataforma paradigmadocs
 ; Dominio: paradigmadocs (lista)
 ; Recorrido: string
 ; Tipo de Recursión: Solo funciones declarativas

 ; DOCUMENTOS->STRING: Función que obtiene una versión 'string' de toda la información de los documentos de paradigmadocs, esta funcion recibe datos de
 ; las funciones accesses->string, historial->string (TDA DOCUMENTOS)
 ; Componentes Documentos: NombreDoc, Autor, id, versión activa, historial, Usuarios con acceso
 ; Dominio: lista
 ; Recorrido: string

 ; HISTORIAL->STRING: Función que convierte el historial en una versión string
 ; Dominio: lista
 ; Recorrido: string
 ; Tipo de Recursión: Solo funciones declarativas

 ; ACCESSES->STRING: Función que convierte a string los accessos que tienen los usuarios al documento,
 ; además la función encapsulada access_name convierte a string el char, entendible por el usuario
 ; Dominio: access list
 ; Recorrido: string
 ; Tipo de Recursión: Solo funciones declarativas
(define (paradigmadocs->string paradigmadocs)
  (define (registrados->string pDocs)
    (string-join(map (λ (username pass date)
      (string-append "\nUsuario: "username"\nContraseña: "pass"\nRegistrado el "(date->string date)"\n"(make-string 35 #\-)))
                     (map (λ (x) (get_username x)) (get_lista_registrados pDocs))
                     (map (λ (x) (get_password x)) (get_lista_registrados pDocs))
                     (map (λ (x) (get_fecha_creacion x)) (get_lista_registrados pDocs)))))
  (define (historial->string historial_doc)
  (string-join(map (λ (idVer date texto)
                     (string-append "\n\t" "Versión n° "(number->string idVer)"\n\tUltima Modificación el "(date->string date)"\n\tContenido: "(decryptFn texto)"\n\t* * * * * * * * * * * *"))
                   (map (λ (x) (get_id_version x)) historial_doc)
                   (map (λ (x) (get_date_version x))historial_doc)
                   (map (λ (x) (get_texto_version x))historial_doc))))
  (define (accesses->string lista_accesses)
    (define (access_name pr)
      (cond 
        [(eqv? pr #\w) "escritura"]
        [(eqv? pr #\c) "comentarios"]
        [(eqv? pr #\r) "lectura"]))
    (string-join(map (λ (user permiso)
                       (string-append"\n"user" ➞ permiso de "(access_name permiso)))
                     (map (λ (x) (get_usuario_share x))lista_accesses)
                     (map (λ (x) (get_permiso_share x))lista_accesses))))
  (define (documentos->string lista_documentos)
  (string-join(map (λ (nombre_doc creador id historial shares)
                     (string-append "\n"nombre_doc"\nCreado el "(date->string(get_date_version(get_primera_version paradigmadocs id)))"\nEl propietario es "creador"\nid: "(number->string id)"\nHistorial de Documentos:\n\t* * Versión Activa * *" (historial->string historial)
                                    "\nUsuarios con acceso:"(accesses->string (filter (λ (access)(not(null? access)))shares))"\n* * * * * * * * * * * *\n"))
                   (map (λ (x) (get_nombre_documento x))lista_documentos)
                   (map (λ (x) (get_autor x))lista_documentos)
                   (map (λ (x) (get_id_documento x)) lista_documentos)
                   (map (λ (x) (get_historial_documento x))lista_documentos)
                   (map (λ (x) (get_lista_accesos x))lista_documentos))))
  (if (logeado? paradigmadocs)
      (string-append (make-string 15 #\*)" "(get_logeado paradigmadocs)" "(make-string 15 #\*)"\nCuenta creada el "(date->string (get_fecha_creacion_cuenta paradigmadocs (get_logeado paradigmadocs)))"\n\nEs creador de:"
    (documentos->string (map (λ(id)(get_doc_byId paradigmadocs id)) (get_id_documentos_creados paradigmadocs (get_logeado paradigmadocs))))"\n"(make-string 15 #\-) "\nTiene acceso a: " (documentos->string (map (λ(id)(get_doc_byId paradigmadocs id)) (get_id_documentos_compartidos paradigmadocs (get_logeado paradigmadocs)))))
      (string-append (make-string 15 #\*)" "(get_nombre_plataforma paradigmadocs)" "(make-string 15 #\*)"\nCreado el "(date->string (get_fecha_creacion_plataforma paradigmadocs))
                 "\n\n➤USUARIOS REGISTRADOS:"(registrados->string paradigmadocs)"\n\n➤DOCUMENTOS:"(documentos->string (get_documentos paradigmadocs)))))

; DELETE
; Descripción: Función que permite eliminar los 'n' ultimos caracteres de la versión actual del documento, solo los usuarios con permiso de edición pueden usar esta función,
; si el número de caracteres a borrar es superior al largo del texto se elimina todo, quedando  "", nueva_ver:Funcion propia (encapsulada) de la Funcion DELETE que elimina texto del final
; Dominio: paradigmadocs X int X date X int
; Recorrido: paradigmadocs
; Tipo de Recursividad: funciones declarativas (get_editor_users, set_version)
(define (delete paradigmadocs)
 (λ (idDoc date numberOfCharacters)    
  (if (logeado? paradigmadocs)
      (if (and (es_id? idDoc)(date? date)(integer? numberOfCharacters)(not (eqv? 0 numberOfCharacters))(member (get_logeado paradigmadocs) (get_editor_users paradigmadocs idDoc))) ;si pertenece a los editores, si el numero de caracteres a eliminar es distinto de 0 ya que no borraria nada
          (if (>= numberOfCharacters (string-length(get_contenido_active_version paradigmadocs idDoc)))
              (deslogear (set_version paradigmadocs idDoc (version (set_id_vr paradigmadocs idDoc) date "")))
              (deslogear (set_version paradigmadocs idDoc (version (set_id_vr paradigmadocs idDoc) date ((get_function1 paradigmadocs) (substring ((get_function2 paradigmadocs)(get_contenido_active_version paradigmadocs idDoc))0(-(string-length (get_contenido_active_version paradigmadocs idDoc)) numberOfCharacters))))))) ; Se elimina el texto
          (deslogear paradigmadocs))
      paradigmadocs))) 

; SEARCH AND REPLACE
; Descripción: Función que permite buscar y reemplazar texto de la versión activa del documento, solo los  usuarios con permiso de edición pueden usar esta herramiento
; Si lo  que se quiere reemplazar es lo que se ingresa, no se hacen cambios sobre el documento
; Dominio: paradigmadocs X int X date X string X string
; Recorrido: paradigmadocs
; Tipo de Recursividad: funciones declarativas (get_editor_users, set_version)
(define (searchAndReplace paradigmadocs)
 (λ (idDoc date searchText replaceText)
  (if (logeado? paradigmadocs)
      (if (and (member (get_logeado paradigmadocs)(get_editor_users paradigmadocs idDoc))(not(eqv? searchText replaceText))(es_id? idDoc)(date? date)(es_texto? searchText)(es_texto? replaceText))
          (if (string-contains? (get_contenido_active_version paradigmadocs idDoc) ((get_function2 paradigmadocs)searchText)) ; Si encuentra texto
              (deslogear (set_version paradigmadocs idDoc (version (set_id_vr paradigmadocs idDoc) date (string-replace (get_contenido_active_version paradigmadocs idDoc)((get_function2 paradigmadocs)searchText)((get_function2 paradigmadocs)replaceText)))))
              (deslogear paradigmadocs))
          (deslogear paradigmadocs))
      paradigmadocs)))

; APPLYSTYLES
; Descripción: Función que permite aplicar los estilos [#\i, #\b, #\u] a un texto seleccionado del documento, solo los usuarios con permiso de edición pueden usar esta herramienta
; Dominio: paradigmadocs X int X date X String X accessList
; Recorrido: paradigmadocs
; Tipo de Recursión: funciones declarativas (get_editor_users, set_version)
; Función encapsulada:
 ; SET_STYLES: Función que crea un string con los estilos para agregarlo al documento, ademas se filtra la acceslist style para que los estilos solo sean italic, bold, underlined(#\i #\b #\u)
 ; Dominio: access list (styles)
 ; Recorrido: string
 ; Tipo de Recursión: implicita en funciones declarativas: filter
(define (applyStyles paradigmadocs)
 (λ (idDoc date searchText . styles)
  (define (set_styles styles)
    (string-join(map(λ(style)(string-append "#\\" style ))(reverse(cdr(reverse(cdr(string-split(list->string (filter (λ(style)(member style (list #\i #\b #\u)))styles))""))))))))
  (if (logeado? paradigmadocs)
      (if (and (es_id? idDoc)(not(eqv? searchText ""))(date? date)(es_texto? searchText)(member (get_logeado paradigmadocs )(get_editor_users paradigmadocs idDoc)))
          (if (string-contains? (get_contenido_active_version paradigmadocs idDoc) ((get_function2 paradigmadocs)searchText)) ; Si encuentra texto
              (deslogear (set_version paradigmadocs idDoc (version(set_id_vr paradigmadocs idDoc) date (string-replace (get_contenido_active_version paradigmadocs idDoc) ((get_function2 paradigmadocs)searchText) (string-append " "((get_function2 paradigmadocs)(set_styles (get_accesses styles)))" "((get_function2 paradigmadocs)searchText)" "((get_function2 paradigmadocs)(set_styles (get_accesses styles)))" ")))))
              (deslogear paradigmadocs))
          (deslogear paradigmadocs))
      paradigmadocs)))

; COMMENT
; Descripción: Función que permite comentar texto seleccionado de la ultima versión de el documento, solo los usuarios con acceso a comentarios, escritura o creadores pueden comentar
; Condiciones de funcionamiento:
; -  En caso la version actual del documento ya está comentada, la funcion (get_version_sin_comment) retorna la ultima version sin comentar, esta será la que se comente ahora, eliminando el anterior comentario
; Dominio: paradigmadocs X int X date X String X String
; Recorrido: paradigmadocs
; Tipo de Recursión: funciones declarativas (get_editor_and_comment_users, set_version)
(define (comment paradigmadocs)
 (λ (idDoc date selectedText commenText)
  (if  (logeado? paradigmadocs)
      (if (and(member (get_logeado paradigmadocs)(get_editor_and_comment_users paradigmadocs idDoc))(es_id? idDoc)(date? date)(es_texto? selectedText)(es_texto? commenText))
          (if (string-contains? (get_version_sin_comment paradigmadocs idDoc) ((get_function2 paradigmadocs)selectedText)) ; Si encuentra texto
              (deslogear (set_version paradigmadocs idDoc (version(set_id_vr paradigmadocs idDoc) date (string-replace (get_version_sin_comment paradigmadocs idDoc)((get_function2 paradigmadocs)selectedText) ((get_function2 paradigmadocs)(string-append selectedText "->%c["commenText "]c% "))))))
              (deslogear paradigmadocs))
          (deslogear paradigmadocs))
      paradigmadocs)))

; ENCRYPT
; Descripción: Función que encripta texto de forma distinta a la propuesta
; Dominio: string
; Recorrido: string
; Tipo de Recursividad: Ninguna
(define (encrypt text)
  (list->string (map(λ(number)(integer->char number)) (map(λ(number)(+ number 5)) (map (λ(char) (char->integer char)) (string->list text))))))

; DECRYPT
; Descripción: Función que desencripta texto de forma distinta a la propuesta
; Dominio: string
; Recorrido: string
; Tipo de Recursividad: Ninguna
(define (decrypt text)
  (list->string(map(λ(number)(integer->char number)) (map(λ(number)(- number 5))(map (λ(char) (char->integer char))(string->list text))))))

; CtrlZ
; Descripción: Función que deshace los cambios hechos en  la versión activa del documento, esta función crea una memoria  que guarda los datos de la versión donde se deshaceran
; y rehaceran los cambios, la cual contiene el texto de esta y el numero undos, el documento queda  de la forma: ; ID DATE CONTENT MEMORIA -> (ULTIMA VERSION N°RESTAURADOS)
; Dominio paradigmadocs X int X int
; Recorrido: paradigmadocs
; Tipo de Recursividad: Recursividad de cola (Función actualizarvz->restaura_version)
(define (ctrlZ paradigmadocs)
  (λ (idDoc numberOfUndo)
  (if (logeado? paradigmadocs)
      (if (>= (get_n_versions paradigmadocs idDoc)numberOfUndo)
          (deslogear(set_documento paradigmadocs (set_doc paradigmadocs idDoc (list(update_doc paradigmadocs idDoc (actualizar_vz paradigmadocs idDoc numberOfUndo))))))
          (deslogear paradigmadocs))
      paradigmadocs)))

; CtrlY
; Descripción: Función que rehace los cambios hechos por CtrlZ, para su funcionamiento se debe hacer CtrlZ antes
; Si el número CtrlY excede al número de versiones, numberOfRedo es el numero de versiones
; Dominio paradigmadocs X int X int
; Recorrido: paradigmadocs
; Tipo de Recursividad: Recursividad de cola (Función actualizarvy->restaura_version)
(define (ctrlY paradigmadocs)
               (λ (idDoc numberOfRedo)
  (if (logeado? paradigmadocs)
      (if (>= (get_n_versions paradigmadocs idDoc)numberOfRedo)
          (deslogear(set_documento paradigmadocs (set_doc paradigmadocs idDoc (list(update_doc paradigmadocs idDoc (actualizar_vy paradigmadocs idDoc numberOfRedo))))))
          (deslogear paradigmadocs))
      paradigmadocs)))
      
;---------------------------------------------------------------------EJEMPLOS------------------------------------------------------------------------------------
; CONSTRUCTOR PARADIGMADOCS
(define emptyGDocs (paradigmadocs "NICO DOCS" (date 25 10 2021) encryptFn decryptFn))
; REGISTER
(define gDocs1    (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 26 10 2021) "user2" "pass2") (date 27 10 2021) "user3" "pass3"))
(define gDocs01   (register (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 26 10 2021) "user3" "pass3") (date 27 10 2021) "user1" "passxx")(date 27 10 2021) "user4" "pass4"))
(define gDocs001  (register (register (register emptyGDocs (date 25 10 2021) "user1" "pass1") (date 26 10 2021) "user1" "pass2") (date 27 10 2021) "user1" "pass3"))
; CREATE
(define gDocs2     ((login gDocs1 "user1" "pass1" create) (date 30 08 2021) "doc1" "Documento1 creado por user1"))
(define gDocs3     ((login gDocs2 "user2" "pass2" create) (date 30 09 2021) "doc2" "Documento1 creado por user2"))
(define gDocs04    ((login gDocs3 "user3" "pass3" create) (date 30 10 2021) "doc3" "Documento1 creado por user3"))
(define gDocs4     ((login gDocs04 "user1" "pass1" create)(date 30 10 2021) "doc4" "Documento2 creado por user1"))
(define gDocs02    ((login gDocs4 "user3" "pass34" create) (date 30 10 2021) "doc4" "Documento2 creado por user3")) ;Contraseña incorrecta-> no logeado -> doc no creado
; SHARE
(define gDocs5     ((login gDocs4 "user1" "pass1" share)  0 (access "user1" #\y )(access "user1" #\t)(access "user3" #\r) (access "user2" #\c)(access "user2" #\o)(access "user10" #\c)))
(define gDocs05    ((login gDocs5 "user1" "pass1" share)  0 (access "user1" #\y )(access "user1" #\t)(access "user3" #\r) (access "user2" #\w)(access "user2" #\o)(access "user10" #\c))) ;se comparte nuevamente el doc:id:0 cambiando el permiso de user2#\c a user2 #\w, los demas permisos se mantienen intactos
(define gDocs6     ((login gDocs05 "user2" "pass2" share) 1 (access "user1" #\i )(access "user2" #\c)(access "user3" #\w)(access "user3" #\q)))
(define gDocs07    ((login gDocs6 "user3" "pass3" share)  2 (access "user1" #\r )(access "user1" #\w)(access "user2" #\c)(access "user2" #\c)(access "user5" #\c)))
(define gDocs7     ((login gDocs07 "user1" "pass1" share) 3 (access "user1" #\r )(access "user2" #\w)(access "user2" #\c)(access "user3" #\w)(access "user9" #\c)))
; ADD
(define gDocs8     ((login gDocs7 "user2" "pass2" add)   0 (date 20 10 2021) "Add"))             ;user2 tiene permiso de escritura en doc1
(define gDocsej9   ((login gDocs8 "user1" "pass1" add)   0 (date 21 10 2021) "Palabra"))         ;el creador agrega texto nuevamente en doc1
(define gDocs9     ((login gDocsej9 "user1" "pass1" add) 2 (date 21 10 2021) "Word2"))           ;user2 tiene permiso en escritura en doc3, añade texto
(define gDocs09    ((login gDocs9 "user1" "pass1" add)   2 (date 21 10 2021) "Añado en doc3"))   ;user1 tiene permiso de escritura en doc3
(define gDocs009   ((login gDocs09 "user1" "pass1" add)  1 (date 21 10 2021) "No añado en doc2"));user1 NO tiene permiso de escritura en doc2, no añade texto
(define gDocs0009  ((login gDocs009 "user3" "pass3" add) 1 (date 21 10 2021) "No Añado doc2"))   ;user3 NO tiene permiso de escritura en doc2, solo lectura, no añade texto
; RESTORE VERSION
(define gDocs010   ((login gDocs9 "user1" "pass1" restoreVersion)    0 1)) ;user1 creador de doc1, se restaura versión 1, se verifica mediante: (get_id_version(get_active_version_byid gDocs010 0)) -> 1
(define gDocs0ex10 ((login gDocs010 "user2" "pass2" restoreVersion)  1 1)) ;user2 creador de doc2, pero la version 1 no existe, no se restura:   (get_id_version(get_active_version_byid gDocs0ex10 1)) -> 0
(define gDocs0010  ((login gDocs010 "user1" "pass1" restoreVersion)  2 0)) ;user1 no es el creador de doc2, no se restaura versión:             (get_id_version(get_active_version_byid gDocs0010 2)) -> 1
(define gDocs10    ((login gDocs0010 "user3" "pass3" restoreVersion) 2 0)) ;user3 es el creador de doc3, se restaura versión 0:                 (get_id_version(get_active_version_byid gDocs10 2)) -> 0
; REVOKE ALL ACCESSES
(define gDocs11     (login gDocs10   "user1" "pass1"   revokeAllAccesses)) ;user1 es propietario de doc4 y doc1, se eliminan todos los permisos para acceder a estos documentos
(define gDocs011    (login gDocs11   "user2" "pass2"   revokeAllAccesses)) ;user2 es propietario de doc2, se eliminan todos los permisos para acceder a estos documentos
(define gDocs0011   (login gDocs011  "user7" "passxyz" revokeAllAccesses)) ;usuario no esta registrado
(define gDocs00011  (login gDocs0011 "user3" "pass3"   revokeAllAccesses)) ;user2 es propietario de doc3, se eliminan todos los permisos para acceder a estos documentos
; SEARCH
((login gDocs11 "user1" "pass1" search) "por")     ;encuentra "por" en: doc1 (creador), doc4(creador), doc3(le fue compartido)
((login gDocs11 "user2" "pass2" search) "Doc")     ;encuentra "Doc" em doc2 (creador), doc3(le fue compartido)
((login gDocs11 "user1" "pass1" search) "ejemplo") ;no encuentra "ejemplo" en ningún documento
((login gDocs11 "user6" "pass6" search) "Doc")     ;no busca texto, el usuario no está registrado
((login gDocs11 "user3" "pass123" search) "por")   ;no busca texto, el "user" está registrado, pero Contraseña incorrecta
; PARADIGMADOCS->STRING
(login gDocs11 "user3" "pass3" paradigmadocs->string)           ;-> "user3" registrado *versión LOGIN "user3"*
(login gDocs11 "user1" "pass3" paradigmadocs->string)           ;-> user1 no registrado (constraseña incorrecta) versión NO LOGIN
(login gDocs11 "user2" "pass2" paradigmadocs->string)           ;-> "user2" registrado *versión LOGIN "user2"*
(login gDocs11 "user10" "pass2" paradigmadocs->string)          ;-> user10 no registrado, versión NO LOGIN
(paradigmadocs->string gDocs11)                                 ;-> versión NO LOGIN
; DELETE
(define gDocs14     ((login gDocs11 "user1" "pass1" delete) 0 (date 30 11 2021) 0))          ; No se elimina nada, no se crea una nueva versión
(define gDocs014    ((login gDocs11 "user1" "pass1" delete) 0 (date 30 11 2021) 10))         ; Se eliminan los 10 ultimos caracteres de la versión activa de doc0 y se actualiza la ultima versión activa
(define gDocs0014   ((login gDocs14 "user2" "pass2" delete) 1 (date 30 11 2021) 20))         ; Se eliminan los 20 ultimos caracteres de la versión activa de doc2 y se actualiza la ultima versión activa
(define gDocs00014  ((login gDocs0014 "user3" "pass3" delete) 1 (date 30 11 2021) 20))       ; Se eliminan todo el contenido de la ultima version de doc2, ya que user3 tiene permiso de escritura en este documento
(define gDocs000014 ((login gDocs00014 "user2" "pass2" delete) 2 (date 30 11 2021) 20))      ; Usuario no tiene permiso de escritura, no se hace ningun cambio a doc3
(define gDocs15     ((login gDocs000014 "user5" "pass1" delete) 1 (date 30 11 2021) 20))     ; Usuario no registrado
; SEARCH AND REPLACE
(define gDocs16     ((login gDocs15 "user1" "pass1" searchAndReplace ) 0 (date 20 10 2021) "po" "CHANGE"))     ; Se crea una nueva versión con "po" reemplazado por "CHANGE"
(define gDocs016    ((login gDocs16 "user1" "pass1" searchAndReplace ) 0 (date 20 10 2021) "CHANGE" "CAMBIO")) ; Se hace un cambio en cadena probando su funcionamiento, quedando una version  con CHANGE y una version con CAMBIO
(define gDocs0016   ((login gDocs016 "user1" "pass1" searchAndReplace ) 0 (date 20 10 2021) "CAMBIO" "CAMBIO")); Se pretende cambiar el texto por el mismo texto, no se crea una nueva version
; APPLYSTYLES
(define gDocs18     ((login gDocs15 "user1" "pass1" applyStyles) 0 (date 20 10 2021) "por"  #\b #\u #\i #\y))  ; No se acepta el estilo #\y pero si los demás
(define gDocs018    ((login gDocs15 "user1" "pass1" applyStyles) 0 (date 20 10 2021) "Documento1 creado"  #\u #\i #\b )) ; Se aplican todos los estilos
(define gDocs0018   ((login gDocs15 "user1" "pass1" applyStyles) 2 (date 20 10 2021) "Documento1"  #\u #\i #\b )); User1 no es propietario  pero tiene permiso de escritura sobre doc3
; COMMENT
(define gDocs17     ((login gDocs16 "user2" "pass2" comment ) 2 (date 20 10 2021)  "1"  "comentario"))         ; User2 tiene permisos de comentarios sobre doc3
(define gDocs017    ((login gDocs17 "user2" "pass2" comment ) 2 (date 22 10 2021)  "Documento"  "comentarioooo"))     ; En que caso se quiera comentar una version ya antes comentada, se comentara la ultima version sin comentar del documento
(define gDoocs0017  ((login gDocs017"user2" "pass2" comment ) 1 (date 20 10 2021)  ""  "comentarioo"))         ; Se comenta un string vacio
; ENCRYPT - DECRYPT
(decrypt (encrypt "contraseña1234"))
(decrypt (encrypt "laboratorio1"))
(decrypt (encrypt "paradigma_funcional"))
; CTRLZ &  CTRLY
(define  gDocs019  ((login gDocs18 "user1" "pass1" ctrlZ)0 2))    ; En  este ejemplo se aplica  ctrlZ  1 vez sobre el documento id:0
(define  gDocs0019 ((login gDocs019 "user1" "pass1" ctrlZ)0 1))   ; Luego, se aplica  ctrlZ  nuevamente ctrlZ sobre el documento id:0, los ctrlZ se acumulan en la memoria Ahora 3
(define  gDocs20   ((login ((login gDocs18 "user1" "pass1" ctrlZ)0 3) "user1" "pass1" ctrlY)0 3))    ; Ejemplo compuesto donde se aplica CtrlZ 3 veces y CtrlY 3 veces, obteniendo el estado inicial sin ninguna modificación
(define  gDocs020  ((login gDocs0019 "user1" "pass1" ctrlZ)0 10))    ; Se aplica  ctrlY  3 restaurando la versiono original como si nunca se  hubiera hecho ctrlZ


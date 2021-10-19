#lang racket
(require "TDA_User.rkt")
(require "TDA_Doc.rkt")
(require "TDA_fecha.rkt")
(require "TDA_paradigmadocs.rkt")

(define p_docs (paradigmadocs  "gDocs" (date 25 10 2021) encryptFn decryptFn))

; FUNCIÓN REGISTER:

; Dominio: Archivo de tipo paradigma_docs, una fecha (TDA Date), username (String), password (string)
; Recorrido: Versión actualizada de paradigma docs, con los usuarios con username únicos registrados
; Representación: (paradigmadocs X string X string X date)
; Descripción: Funcion que mediante una composición de funciones permite registrar un grupo de usuarios únicos mediante la fecha username y
; contraseña del registro, siendo el username un identificador único, en caso existan duplicados, permite solo 1 con ese username
; Tipo de recursión: Recursión Natural

(define (register pDocs user pass date)
      (if (registrado_antes? pDocs user)   ; Si el usuario esta registrado, entonces no crear la lista, solamente llamar a la función pDocs
          pDocs
         (list pDocs user pass date)))

;-----------------------------------------------------------------------------------EJEMPLOS--------------------------------------------------------------------
(define g_Docs1 (create_listas(register(register(register p_docs "user1" "pass1" (date 15 10 2021))"user2""pass2"(date 16 10 2021))"user3""pass3"(date 17 10 2021))))
; Los 3 usuarios son unicos
;(define g_Docs2 (crear_listas(register(register(register p_docs "user1" "pass1" (date 15 10 2021))"user1""pass2"(date 26 10 2021))"user3""pass3"(date 27 10 2021))))
; "user1" esta duplicado, por lo tanto, solo se mantiene la primera version registrada de "user1"
;(define g_Docs3 (crear_listas(register(register(register p_docs "user1" "pass1" (date 15 10 2021))"user1""pass2"(date 29 10 2021))"user1""pass3"(date 30 10 2021))))
; "user1" esta triplicado, por lo tanto, solo se mantiene la primera version registrada de "user1"
;-----------------------------------------------------------------------------------------------------------------------------------------------------

; FUNCIÓN LOGIN:

; Dominio: Archivo de tipo paradigma_docs donde se encuentran los usuarios registrados
; Recorrido: Versión actualizada de paradigma docs, con los usuarios logeados o autenticados
; Representación: (paradigmadocs X string X string X function)
; Descripción:Funcion que si el usuario existe, (previamente registrado en paradigma_docs), la constraseña que ingresa es igual a la registrada, si esto se cumple se
; retorna una version actualizada de paradigma docs evaluada en operation y el usuario pasa a desconectado o deslogeado
; Tipo de Recursión: Recursión Natural en la función (registrado_antes?)

(define (login pDocs user pass f)
    (if (and (registrado_antes? pDocs user)(eq? pass (get_password (get_lista_registrados pDocs) user))) ; Si el usuario esta (registrado) en paradigma docs y tiene la contraseña correcta
        (cond
          [(eq? f create) (lambda(date name content) (f (logear pDocs user pass)date name content user))]
          [(eq? f share) (lambda(idDoc access . accesses) (f (logear pDocs user pass) idDoc user (cons access accesses)))]
          [(eq? f add) (lambda(idDoc date content) (f (logear pDocs user pass) idDoc date content user ))]
          [(eq? f restoreVersion) (lambda(idDoc idVersion) (f (logear pDocs user pass) idDoc idVersion user))]
          [(eq? f revokeAllAccesses)(f (logear pDocs user pass) user)]
          [(eq? f search)(lambda(text)(f (logear pDocs user pass) text user))]
          )
        (cond
          [(eq? f create)(lambda(date name content)pDocs)]
          [(eq? f share) (lambda(idDoc access . accesses)pDocs)]
          [(eq? f add) (lambda(idDoc date content) pDocs)]
          [(eq? f revokeAllAccesses) pDocs]
          [(eq? f search)(lambda(text) pDocs)])))

;----------------------------------------------------EJEMPLOS--------------------------------------------------
; Los ejemplos de login vienen incluidos en cada función posterior, ya que create no funcion sin operation
;------------------------------------------------------------------------------------------------------------

; FUNCIÓN CREATE:

; Descripción: Permite al usuario de paradigmadocs crear un documento en una fecha determinada, un nombre y su contenido
; Representación: (paradigmadocs X string X string X function)
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con el documento creado por el usuario y su desconección de la sesión

(define (create pDocs date nombre_doc content user) ; Si el usuario esta en paradigma docs
      (if (logeado? pDocs user)
          (add_doc(deslogear pDocs user) (list user nombre_doc (list(list date content 0))(length(get_lista_docs pDocs))))
          pDocs))

;------------------------------------------------------------EJEMPLOS---------------------------------------------------------------------------------
(define g_Docs2 ((login g_Docs1 "user1" "pass1" create) (date 30 08 2021) "doc0" (encryptFn "DOC0 creado por user1")))
; El usuario existe, por lo tanto se crea el documento y retorna la version actualizada de paradigma_docs
(define g_Docs3 ((login g_Docs2 "user1" "pass1" create) (date 30 09 2021) "doc1" (encryptFn "2DO DOC1 creado por USER 1")))
; El usuario no existe, por lo tanto no crea el documento y retorna la version no actualizada de paradigma_docs
(define g_Docs4 ((login g_Docs3 "user2" "pass2" create) (date 31 08 2021) "doc2" (encryptFn "DOC2 creado por user2")))
; El usuario existe pero tiene la contraseña incorrecta, por lo tanto no crea el documento y retorna la version no actualizada de paradigma_docs
;----------------------------------------------------------------------------------------------------------------------------------------------------

;g_Docs2
;g_Docs3
;g_Docs4

; FUNCIÓN SHARE:

; Descripción: Permite al usuario (propietario de un documento) en paradigmadocs, compartir un documento mediante su id con otros usuarios, otorgando permisos de lectura o escritura
; Condiciones para su funcionamiento:
; 1) ¿El creador del documento es el mismo que desea compartir? -> Si, se ejecuta la funcion compartir; No, la funcion no se ejecuta
; 2) ¿Los usuarios con los que sea quiere compartir, estan registrados ? -> Si, se agrega a la lista de compartidos; No, no se agrega a la lista de compartidos
; 3) ¿El usuario creo el documento antes? -> Si, la funcion se ejecuta; No, la funcion no se ejecuta
; 4) El usuario no puede compartir un documento consigo mismo
; 5) Los permisos otorgados solo pueden ser (#\r / #\w / #\c)
; 6) El usuario puede quitar y colocar distintos permisos, el ultimo dado a tal usuario es el activo

; Representación: (paradigmadocs XX int XX access [string char])
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con los documentos compartidos por este, id y lista de usuarios con los que se comparte
; Tipo de Recursividad: Recursividad Natural

(define (share pDocs id user accesses)
  (define (tiene_shares? lista)
  (if (empty? lista)
      #f
      (if (eq? (car(car lista))"s")
          #t
          (tiene_shares? (cdr lista)))))
  
  (define (crear_permisos pdocs id x)
    (append (filtrar_ids id (get_lista_docs pdocs))(list(remove(filtrar_shares (filter list? (get_doc_byid id (get_lista_docs pdocs))))(append(get_doc_byid id (get_lista_docs pdocs))(list x))))))
  
  (define(filtrar_shares lista)
    (if (empty? lista)
        null
        (if (eq? (car(car lista))"s")
           (car lista)
           (filtrar_shares (cdr lista)))))
;----------------------------------------->
  (if (logeado? pDocs user)
      (if (and (eq? user (get_creador_doc id (get_lista_docs pDocs))) (not(null?(get_lista_docs pDocs))) (not(null?(registrado_for_share? pDocs user accesses)))) ; Si no se ha creado ningun documento -> Retorna paradigma_docs, con el usuario deslogeado
          (if (tiene_shares?(filter list? (get_doc_byid id (get_lista_docs pDocs))))
              (deslogear(crear_pdocs_docs pDocs (crear_permisos pDocs id (cons "s" (mix (usuarios_share (filter list? (get_doc_byid id (get_lista_docs pDocs))))(ultimo_permiso (registrado_for_share? pDocs user accesses) '())))))user)
              (deslogear (add_by_id id pDocs (cons  "s"  (ultimo_permiso (registrado_for_share? pDocs user accesses) '())))user))
              (deslogear pDocs user))
               pDocs))

;------------------------------------------------------------------EJEMPLOS-------------------------------------------------------------------------------------------
(define g_Docs5 ((login g_Docs4 "user1" "pass1" share) 0 (access "user1" #\r )(access "user2" #\c)(access "user3" #\c) (access "user1" #\c)))
(define g_Docs6 ((login g_Docs5 "user1" "pass1" share) 0 (access "user1" #\w )(access "user2" #\w)(access "user3" #\w)))
(define g_Docs7 ((login g_Docs6 "user2" "pass2" share) 2 (access "user1" #\r )(access "user1" #\w)(access "user2" #\c)(access "user3" #\c)(access "user5" #\c)))
;------------------------------------------------------------------------------------------------------------------------------------------------------------------

;g_Docs5
;g_Docs6
;g_Docs7

; FUNCIÓN ADD:
; Representación: (paradigmadocs XX int XX date X string)
; Descripción: Funcion que añade texto al final de la ultima versión y crea una nueva version
; Condiciones para su funcionamiento:
; 1) Solo el usuario y los usuarios con los que el creador del documento haya compartido el documento pueden agregar texto a la ultima version
; Dominio: Archivo de paradigmadocs con los usuarios registrados en la plataforma
; Recorrido: Archivo actualizado de paradigmadocs con los documentos compartidos por este, id y lista de usuarios con los que se comparte
; Tipo de Recursividad: Recursividad Natural

(define (add pDocs idDoc date text user)
  (if (logeado? pDocs user)
      (if (or (eq? user (get_creador_doc idDoc (get_lista_docs pDocs))) (list?(member user (get_users_write (get_accesses_shared idDoc pDocs)))))
          (deslogear (list (get_lista_registrados pDocs)(get_lista_logeados pDocs)(aniadir_ver_to_doc idDoc (get_lista_docs pDocs)(add2 pDocs idDoc date text)))user)
          (deslogear pDocs user))
           pDocs))
;------------------------------------------------------------------EJEMPLOS----------------------------------------------------------------------------------------------------------------------
(define g_Docs8 ((login g_Docs7 "user2" "pass2" add) 2 (date 25 11 2021) "D D A "))
(define g_Docs9 ((login g_Docs8 "user2" "pass2" add) 2 (date 10 11 2021) "D D A 2"))
;------------------------------------------------------------------------------------------------------------------------------------------------------------------

;g_Docs8
;g_Docs9

; FUNCIÓN RESTORE VERSION:
; Representación: (paradigmadocs XX int XX int)
; Descripción: Función que 'restaura' un versión, es decir que la versión escogida pasa a ser la versión activa en el documento
; NOTA: La ultima version actual es la ultima en la lista de versiones creadas, es posible obtenerla mediante un selector llamado (get_active_vr_byid idDoc pdocs)
; Condiciones para su funcionamiento:
; 1) La versión debe existir, de lo contrario retorna paradigmadocs sin modificaciones
; Dominio: Archivo de paradigmadocs
; Recorrido: Archivo actualizado de paradigmadocs con la ultima version activa
; Tipo de Recursividad: Recursividad Natural

(define (restoreVersion pdocs idDoc idVersion user)
  (if (logeado? pdocs user)
      (if(and (eq? user (get_creador_doc idDoc (get_lista_docs pdocs))) (< idVersion  (length (get_all_versions_byid idDoc pdocs))))
       (deslogear (crear_pdocs_docs pdocs(add_version (get_lista_docs pdocs) idDoc (append(remove (get_version_byid idVersion (get_all_versions_byid idDoc pdocs)) (get_all_versions_byid idDoc pdocs)) (list(get_version_byid idVersion (get_all_versions_byid idDoc pdocs))))))user)
          (deslogear pdocs))
          pdocs))

;---------------------------------------------------------------------EJEMPLOS---------------------------------------------------------------------------------
(define g_Docs10 ((login g_Docs9 "user2" "pass2" restoreVersion) 2 0))
;(get_active_vr_byid 2 g_Docs9)  ;-> '((10 11 2021) "2resu rop odaerc 2COD  A D D A D D 2" 2)
;(get_active_vr_byid 2 g_Docs10) ;-> '((31 8 2021) "2resu rop odaerc 2COD" 0)
;------------------------------------------------------------------------------------------------------------------------------------------------------------------
;g_Docs9
;g_Docs10

; FUNCION REVOKE ALL ACCESSES
; Representación: (paradigmadocs XX)
; Descripción: Funcion que elimina los permisos que el usuario ha otorgado a otros usuarios de todos sus documentos
; Dominio: Archivo de paradigmadocs
; Recorrido: Archivo de paradigmadocs con todos los permisos de los documentos del usuario eliminados
; Tipo de Recursividad: Recursividad Natural

(define (revokeAllAccesses pDocs user)
  (if (logeado? pDocs user)
      (deslogear(crear_pdocs_docs pDocs (actualizar_permisos (get_lista_docs pDocs)(map eliminar_permisos (get_docs_byuser user (get_lista_docs pDocs)))user))user)
      pDocs))
      
;---------------------------------------------------------------------EJEMPLOS---------------------------------------------------------------------------------
(define g_Docs11(login g_Docs10 "user2" "pass2" revokeAllAccesses))
(define g_Docs12(login g_Docs11 "user1" "pass1" revokeAllAccesses))
;------------------------------------------------------------------------------------------------------------------------------------------------------------------
;g_Docs11
;g_Docs12

; FUNCIÓN SEARCH
; Representación:
; Descripción: Permite al usuario buscar texto en sus documentos (propios o compartidos por otros usuarios) tanto en las versiones actuales como en las antiguas
; en caso el texto exista, retornará todos los documentos donde se encuentra, sino retornara un string vació (no hay documentos)
; Dominio: Archivo de paradigmadocs
; Recorrido: Lista con los documentos que contienen al texto buscado
; Tipo de Recursividad: Recursividad Natural

(define (search pDocs text user)
  (if (logeado? pDocs user)
      (if (empty? (id_list (encuentra_texto? (get_versions_with_access pDocs user) text pDocs)))
          null
      (imprimir_versiones (deslogear pDocs user) (id_list (encuentra_texto? (get_versions_with_access pDocs user) text pDocs))))
      null))

((login g_Docs9 "user3" "pass3" search) "A")
((login g_Docs11 "user2" "pass2" search) "A")
((login g_Docs11 "user3" "pass3" search) "A") ;usuario 3 revocado del acceso al documento 2


  


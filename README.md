# Laboratio1 G.Docs

#### Proyecto 1 Paradigmas de la Programación

**Lenguaje**
El presente proyecto pertence a la asignatura de Paradigmas de la Programación impartido en la universidad de Santiago de Chile y está codificado en Scheme/Racket versión 8.2.

**Acerca de las funciones**
La plataforma ParadigmaDocs es una plataforma de ofimática la cual permite añadir documentos, editar texto (añadir y eliminar) restaurar versiones de un documento, añadir estilos como **bold** **underlined** **italic**, tambien permite añadir comentarios a los documentos, realizar los comandos ControlZ y ControlY.

### Funciones de la plataforma (Para más detalles revisar el informe 😀)		
✅ Register<br/> 	 			
✅ Login		<br/>		
✅ Share	<br/>			
✅ Add 		<br/>				
✅ RestoreVersion 	<br/>		
✅ RevokeAllAccesses 	<br/>	
✅ Search				<br/>	
✅ paradigmadocs->string	<br/>
✅ Delete					<br/>
✅ SearchAndReplace <br/>		
✅ Comment		<br/>			
✅ ApplyStyles	<br/>			
✅ CtrlZ & CtrlY<br/>

**Ejecución**
Para la correcta ejecución el proyecto se compone de 7 archivos (exceptuando el Readme):

- TDA paradigmadocs_27391503_FarfanCheneauxNicolas.rkt
- TDA Version_27391503_FarfanCheneaux.rkt
- TDA Memoria_27391503_FarfanCheneaux.rkt
- TDA Access_27391503_FarfanCheneaux.rkt
- TDA fecha_27391503_FarfanCheneaux.rkt
- TDA Docs_27391503_FarfanCheneaux.rkt
- TDA_User_27391503_FarfanCheneaux.rkt
- main_27391503_FarfanCheneaux.rkt

Para ejectuarlo solamente compile el archivo** main_27391503_FarfanCheneauxNicolas**
Los demás archivos están implícitos en **main** al comienzo, de la siguiente forma, por lo tanto cada uno de estos son necesario para que el proyecto funcione correctamente.

```cpp
#lang racket
(require "TDA_paradigmadocs_27391503_FarfanCheneaux.rkt")
(require "TDA_Memoria_27391503_FarfanCheneaux.rkt")
(require "TDA_Version_27391503_FarfanCheneaux.rkt")
(require "TDA_Access_27391503_FarfanCheneaux.rkt")
(require "TDA_fecha_27391503_FarfanCheneaux.rkt")
(require "TDA_User_27391503_FarfanCheneaux.rkt")
(require "TDA_Doc_27391503_FarfanCheneaux.rkt")
```

**Nota:** Toda la documentación necesaria se encuentra dentro del cada TDA/main, cada función posee una descripción y en las últimas líneas de **main** se encuentran ejemplos de funcionamiento.

**Calificacion Docente (Escala del 1 al 7)**

Código: 7
Informe: 6.6

Made with <3 by Nícolas

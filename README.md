# Nico Docs
#### Proyecto 1 Paradigmas de la Programación
**Lenguaje**
El presente proyecto está codificado en Scheme/Racket versión 8.2.

**Ejecución**
Para la correcta ejecución el proyecto se compone de 7 archivos  (exceptuando el Readme):
- TDA paradigmadocs_27391503_FarfanCheneauxNicolas.rkt
- TDA Memoria_27391503_FarfanCheneauxNicolas.rkt
- TDA Access_27391503_FarfanCheneauxNicolas.rkt
- TDA fecha_27391503_FarfanCheneauxNicolas.rkt
- TDA Docs_27391503_FarfanCheneauxNicolas.rkt
- TDA_User_27391503_FarfanCheneauxNicolas.rkt
- main_27391503_FarfanCheneauxNicolas.rkt

Para ejectuarlo solamente compile el archivo** main_27391503_FarfanCheneauxNicolas**
Los demás archivos están implícitos en **main** al comienzo, de  la siguiente forma, por lo tanto cada uno de estos son necesario para que el proyecto funcione correctamente.
```cpp
#lang racket
(require "TDA_User_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_Docs_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_fecha_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_paradigmadocs_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_Access_27391503_FarfanCheneauxNicolas.rkt")
(require "TDA_Memoria_27391503_FarfanCheneauxNicolas.rkt")
```
**Nota:** Toda la documentación necesaria se encuentra dentro del cada TDA/main, cada función posee una descripción y en las últimas líneas de **main** se encuentran ejemplos de funcionamiento.

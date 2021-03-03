# TP Final para programación funcional: Anotador para "La Podrida"

Este es el trabajo final para la materia de programación funcional, realizado por Benjamín Ciancio y Santiago Tettamanti. 

Se trata de un anotador para el conocido juego de cartas llamado "la podrida".

El desarrollo está realizado en su totalidad en Haskell y se utilizó para la GUI el framework Threepenny (https://wiki.haskell.org/Threepenny-gui).

Todo el código que conforma el anotador se encuentra dentro de la carpet "app" de este proyecto.

## Instalación de dependencias

Se requiere la instalación de Stack, herramienta para el desarrollo de proyectos Haskell (https://docs.haskellstack.org/en/stable/README/).

En sistemas Linux:
  1) Ejecutar:
    `wget -qO- https://get.haskellstack.org/ | sh`
  2) Luego en la carpeta raíz del proyecto:
      `stack build`

## Ejecución

Simplemente ejecutar el script "startpodrida" que se encuentra en el directorio raíz del proyecto.

Luego en el navegador web estará disponible el anotador en `localhost` en el puerto `8023`.

O entrar en `http://127.0.0.1:8023/`

## Sobre Threepenny

Threepenny es un framework para la interfaz gráfica de usuario desarrollado en Haskell. Es de fácil instalación y utiliza el navegador web como pantalla.

Un programa escrito con Threepenny es esencialmente un pequeño servidor web que muestra la interfaz de usuario como una página web en cualquier navegador.
Permite a través de funciones y su Haskell API crear elementos html que luego se renderizarán en pantalla, y asociar esos elementos con una hoja de estilso css correspondiente. También posee una librería de Functional Reactive Programming, siendo su uso opcional.

Su API está documentada de manera muy completa (http://hackage.haskell.org/package/threepenny-gui) y a su vez el framework viene con algunos ejemplos para que se pueda ver algunos casos de uso de la librería ya implementados (https://github.com/HeinrichApfelmus/threepenny-gui/tree/master/samples#readme). Esto es útil y sirve cómo guía a la hora de comenzar un desarrollo sin conocimientos previos del framework.

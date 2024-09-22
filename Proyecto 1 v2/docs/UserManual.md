# Manual de Usuario: Analizador Léxico con Interfaz Tkinter

## 1. Introducción

El **Analizador Léxico** es una herramienta diseñada para identificar y categorizar los elementos léxicos (tokens) en un texto o código fuente. Utiliza una interfaz gráfica (GUI) construida con **Tkinter** que facilita la interacción del usuario con el software. Este documento describe el funcionamiento y las funcionalidades del programa.

## 2. Alcances del Software

El **Analizador Léxico** incluye las siguientes características:

- **Cargar archivos de texto o código fuente**.
- **Análisis léxico** para la identificación de tokens.
- **Visualización de resultados** a través de una tabla en la interfaz.
- **Soporte para un solo lenguaje**.
- **Interfaz gráfica**.
  
## 3. Requisitos del Sistema

### Requisitos de Hardware

- **Procesador**: 1 GHz o superior.
- **Memoria RAM**: 512 MB mínimo (1 GB recomendado).
- **Almacenamiento**: 100 MB de espacio libre.
- **Pantalla**: Resolución mínima de 1024x768.

### Requisitos de Software

- **Sistema Operativo**: Windows, macOS o Linux.
- **Python 3.6+**.
- **Librerías necesarias**:
  - `Tkinter`
  - `Pillow`
  - `subprocess`
  - `Fortran`

## ¿Cómo usarlo?

### 1. Descarga y ejecución del programa:
- Descarga el archivo del programa desde el repositorio correspondiente.
- Extrae el archivo en una carpeta de tu elección.
- Abre la carpeta en una terminal de linux, windows o McOs.
- Instala las dependencias necesarias mediante pip: `pip install subprocess`.
- Ejecuta el programa mediante Python: `python main.py`.
- La interfaz gráfica se abrirá automáticamente.

### 2. Uso del programa:
![Interfaz web](Proyecto1/images/UserManual.png)
- **Abrir**: Selecciona el archivo de texto o código fuente que deseas analizar.
- **Análisar**: Haz clic en el botón "Análisis" para iniciar el analisis.
- **Guardar**: Guarda el archivo que este en el editor de texto.
- **Acerca de**: Muestra la informacion del autor.

### 3. Analisis de datos
- El programa analiza el archivo seleccionado y muestra los tokens encontrados en un grafo si todos los token son aceptados.
![Programa sin errores](Proyecto1/images/programaBien.png)
- Si hay un token que no es aceptado, el programa muestra una tabla de errores que muestra el error y tambien la ubicacion del error.
![Interfaz web](Proyecto1/images/programaMal.png)

### 4. Muestra de datos
- Aparece un nuevo frame con una parte de seleccion con los paises detectados, si seleccionamos un pais nos mostrara su bandera y su poblacion.
![Interfaz web](Proyecto1/images/Banderas.png)



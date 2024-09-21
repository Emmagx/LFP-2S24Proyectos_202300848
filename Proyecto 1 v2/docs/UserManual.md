# Manual de Usuario: Analizador Léxico con Interfaz Tkinter

## 1. Introducción

El **Analizador Léxico** es una herramienta diseñada para identificar y categorizar los elementos léxicos (tokens) en un texto o código fuente. Utiliza una interfaz gráfica (GUI) construida con **Tkinter** que facilita la interacción del usuario con el software. Este documento describe el funcionamiento y las funcionalidades del programa.

## 2. Alcances del Software

El **Analizador Léxico** incluye las siguientes características:

- **Cargar archivos de texto o código fuente**.
- **Análisis léxico** para la identificación de tokens.
- **Visualización de resultados** a través de una tabla en la interfaz.
- **Soporte para un solo lenguaje** configurables.
- **Interfaz gráfica** amigable y fácil de usar.
  
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
  - `Tkinter` (incluido con Python).
  - `Ply` (para el análisis léxico).

### Instalación de Dependencias

Para instalar las dependencias necesarias, ejecutar el siguiente comando en la terminal:
```bash
pip install ply

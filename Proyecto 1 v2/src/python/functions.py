import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess
import os
import re
from PIL import Image, ImageTk
from pais import Pais
from continente import Continente
from graphviz import Digraph

def extraer_datos_tabla(html_content):
    filas = re.findall(r'<tr>(.*?)</tr>', html_content, re.DOTALL)
    datos = []
    for fila in filas:
        celdas = re.findall(r'<td>(.*?)</td>', fila, re.DOTALL)
        celdas_limpias = [re.sub(r'<.*?>', '', celda).strip() for celda in celdas]
        if celdas_limpias:
            datos.append(celdas_limpias)
    return datos

def analizar(text_input, graph_label, tree):
    texto_entrada = text_input.get("1.0", tk.END).strip()

    if not texto_entrada:
        messagebox.showwarning("Advertencia", "El cuadro de entrada está vacío. Por favor, ingrese texto para analizar.")
        return

    ruta_entrada = "../fortran/entradaEjemplo.org"
    ruta_reporte_errores = "reporte_errores.html"
    ruta_reporte_tokens = "reporte_tokens.html"
    ruta_grafico = "grafo.png"

    eliminar_archivos(ruta_reporte_errores, ruta_reporte_tokens, ruta_grafico)

    try:
        escribir_archivo(ruta_entrada, texto_entrada)
        os.chdir("../fortran")
        subprocess.run(["main.exe"], check=True)

        limpiar_salida(tree, graph_label)

        if os.path.exists(ruta_reporte_errores) and os.path.getsize(ruta_reporte_errores) > 0:
            mostrar_errores(ruta_reporte_errores, tree)
        else:
            if os.path.exists(ruta_reporte_tokens):
                datos = extraer_datos_tabla(abrir_archivo_html(ruta_reporte_tokens))
                continentes, tituloGrafica = construir_estructura_datos(datos)
                generar_grafico(continentes, tituloGrafica)
                mostrar_imagen(ruta_grafico, graph_label)
            else:
                messagebox.showerror("Error", "No se encontró el archivo de tokens.")

    except subprocess.CalledProcessError as e:
        messagebox.showerror("Error", f"Hubo un error al ejecutar el analizador léxico: {e}")
    except FileNotFoundError:
        messagebox.showerror("Error", "No se encontró el archivo de salida generado por el analizador léxico.")
    except Exception as e:
        messagebox.showerror("Error", f"Se produjo un error inesperado: {e}")

def construir_estructura_datos(datos):
    continentes = []
    continente_actual = None
    pais_actual = None
    estado = None
    estadoNombre = None
    tituloGrafica = "No titulo"

    for fila in datos:
        lexema = fila[0]
        tipo = fila[1]

        if tipo == "PALABRA_RESERVADA":
            if lexema.lower() == "grafica":
                estado = "GRAFICA"
            elif lexema.lower() == "continente":
                continente_actual = Continente(nombre="")
                continentes.append(continente_actual)
                estado = 'NOMBRE_CONTINENTE'
            elif lexema.lower() == "pais":
                pais_actual = Pais(nombre="", poblacion=0, saturacion="", bandera="")
                if continente_actual:
                    continente_actual.agregar_pais(pais_actual)
                estado = 'PAIS'
                estadoNombre = "PAIS"
            elif lexema.lower() == "nombre" and estadoNombre == "PAIS":
                estado = 'NOMBRE_PAIS'
            elif lexema.lower() == "saturacion":
                estado = 'SATURACION'
            elif lexema.lower() == "poblacion":
                estado = 'POBLACION'
            elif lexema.lower() == "bandera":
                estado = 'BANDERA'
            continue

        elif tipo == "CADENA":
            if estado == 'NOMBRE_CONTINENTE':
                continente_actual.nombre = lexema.strip('"')
                estado = None
            elif estado == "GRAFICA":
                tituloGrafica = lexema
                estado = None
            elif estado == 'NOMBRE_PAIS' or estadoNombre == 'NOMBRE_PAIS':
                pais_actual.nombre = lexema.strip('"')
                estado = None
                estadoNombre = None
            elif estado == 'BANDERA' and pais_actual:
                pais_actual.bandera = 'images/' + pais_actual.nombre + '.png'
                estado = None
        
        elif tipo == "NUMERO_ENTERO":
            if estado == 'POBLACION' and pais_actual:
                pais_actual.poblacion = int(lexema.strip('"'))
                estado = None

        elif tipo == "PORCENTAJE":
            if estado == 'SATURACION' and pais_actual:
                pais_actual.saturacion = int(lexema.strip('"'))
                estado = None

    return continentes, tituloGrafica

def eliminar_archivos(*rutas):
    for ruta in rutas:
        if os.path.exists(ruta):
            os.remove(ruta)

def escribir_archivo(ruta, contenido):
    with open(ruta, "w") as file:
        file.write(contenido)

def abrir_archivo_html(ruta):
    with open(ruta, "r") as file:
        return file.read()

def limpiar_salida(tree, graph_label):
    for row in tree.get_children():
        tree.delete(row)
    tree.pack_forget()
    graph_label.config(image='')

def mostrar_errores(ruta_reporte_errores, tree):
    contenido_html = abrir_archivo_html(ruta_reporte_errores)
    datos = extraer_datos_tabla(contenido_html)
    
    tree['columns'] = ('Caracter', 'Descripcion', 'Tipo', 'Fila', 'Columna')
    tree.heading('#0', text='ID')
    tree.heading('Caracter', text='Caracter')
    tree.heading('Descripcion', text='Descripcion')
    tree.heading('Tipo', text='Tipo')
    tree.heading('Fila', text='Fila')
    tree.heading('Columna', text='Columna')
    
    tree.column('#0', width=50)
    tree.column('Caracter', width=100)
    tree.column('Descripcion', width=200)
    tree.column('Tipo', width=100)
    tree.column('Fila', width=100)
    tree.column('Columna', width=100)
    
    for fila in datos:
        tree.insert('', 'end', values=fila)
    
    tree.pack(padx=10, pady=10, expand=True, fill='both')

def generar_grafico(continentes, nombre_grafica):
    grafo = Digraph(comment=nombre_grafica)
    grafo.node('grafico', nombre_grafica, shape='box', style='bold', fillcolor='lightblue', fontsize="16")

    for continente in continentes:
        grafo.node(continente.nombre, shape='ellipse', style='filled', fillcolor='lightgreen')
        grafo.edge('grafico', continente.nombre)
        
        for pais in continente.paises:
            grafo.node(pais.nombre, shape='ellipse', style='filled', fillcolor='lightyellow')
            grafo.edge(continente.nombre, pais.nombre)
            
            grafo.node(f"{pais.nombre}_saturacion", f"Saturación: {pais.saturacion}%", shape='box', style='filled', fillcolor=calcular_color(pais.saturacion))
            grafo.edge(pais.nombre, f"{pais.nombre}_saturacion")
    
    grafo.save('grafo.dot')
    grafo.render('grafo', format='png')

def mostrar_imagen(ruta_grafico, graph_label):
    if os.path.exists(ruta_grafico):
        img = Image.open(ruta_grafico)
        img = img.resize((1200, 800), Image.Resampling.LANCZOS)
        img_tk = ImageTk.PhotoImage(img)
        graph_label.config(image=img_tk)
        graph_label.image = img_tk
        graph_label.pack(fill="both", expand=True)
    else:
        messagebox.showerror("Error", "No se encontró el archivo de gráfico.")

def calcular_color(porcentaje):
    if 0 <= porcentaje <= 15:
        return "white"
    elif 16 <= porcentaje <= 30:
        return "blue"
    elif 31 <= porcentaje <= 45:
        return "green"
    elif 46 <= porcentaje <= 60:
        return "yellow"
    elif 61 <= porcentaje <= 75:
        return "orange"
    elif 76 <= porcentaje <= 100:
        return "red"
    return "white"

def abrir_archivo(text_input):
    file_path = filedialog.askopenfilename(filetypes=[("Archivos ORG", "*.org")])
    if file_path:
        with open(file_path, "r") as file:
            text_input.delete("1.0", tk.END)
            text_input.insert(tk.END, file.read())

def guardar(text_input):
    file_path = filedialog.asksaveasfilename(defaultextension=".org", filetypes=[("Archivos ORG", "*.org")])
    if file_path:
        with open(file_path, "w") as file:
            file.write(text_input.get("1.0", tk.END).strip())

def mostrar_acerca_de():
    messagebox.showinfo("Acerca de", "Proyecto de Lenguajes Formales y de Programación.\nUniversidad de San Carlos de Guatemala.\nEstudiante: Brayan Emanuel Garcia\nCarnet: 202300848")

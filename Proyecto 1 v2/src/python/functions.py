import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess
import os
import re
from PIL import Image, ImageTk
import graphviz

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
    ruta_grafico = "grafico.png"

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
                generar_grafico(datos, graph_label)
            else:
                messagebox.showerror("Error", "No se encontró el archivo de tokens.")

    except subprocess.CalledProcessError as e:
        messagebox.showerror("Error", f"Hubo un error al ejecutar el analizador léxico: {e}")
    except FileNotFoundError:
        messagebox.showerror("Error", "No se encontró el archivo de salida generado por el analizador léxico.")
    except Exception as e:
        messagebox.showerror("Error", f"Se produjo un error inesperado: {e}")

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
    tree.pack_forget()  # Oculta la tabla si estaba visible
    graph_label.config(image='')  # Limpiar la imagen si existe

def mostrar_errores(ruta_reporte_errores, tree):
    contenido_html = abrir_archivo_html(ruta_reporte_errores)
    datos = extraer_datos_tabla(contenido_html)
    for fila in datos:
        tree.insert('', 'end', values=fila)
    tree.pack(padx=10, pady=10, expand=True, fill='both')

def generar_grafico(datos, graph_label):
    dot = graphviz.Digraph(format='png')
    dot.attr(rankdir='TB', size='12,8', dpi='300')  # Ajusta el tamaño y la resolución

    # Variables para almacenar el estado actual de los nodos
    nodo_actual = None
    nodo_padre = None
    nombre_grafico = "Grafica"  # Valor por defecto
    es_continente = False
    primer_continente = True  # Controla si es el primer continente procesado
    saturacion_pendiente = None  # Almacena la saturación pendiente

    for fila in datos:
        lexema = fila[0]
        tipo = fila[1]

        if tipo == "PALABRA_RESERVADA":
            if lexema == "Grafica":
                nombre_grafico = "Grafica"  # Valor por defecto, se actualizará si hay un nombre
            elif lexema == "Continente":
                es_continente = True
                if not primer_continente and nodo_padre:
                    dot.edge(nombre_grafico, nodo_padre)  # Conecta el continente al título del grafo
                nodo_padre = None
                nodo_actual = None
                primer_continente = False
            elif lexema == "Pais":
                es_continente = False
                nodo_actual = None
            elif lexema == "Saturacion":
                saturacion_en_proceso = True
            elif lexema == "Nombre":
                nombre_en_proceso = True
                continue


        elif tipo == "CADENA":
            if nombre_en_proceso:
                if es_continente:
                    nodo_padre = lexema.strip('"')
                    dot.node(nodo_padre, label=nodo_padre)
                    if nodo_padre:
                        dot.edge(nombre_grafico, nodo_padre)  # Conecta el continente al título del grafo
                else:
                    nodo_actual = lexema.strip('"')
                    dot.node(nodo_actual, label=nodo_actual)
                    if nodo_padre:
                        dot.edge(nodo_padre, nodo_actual)

                # Si había saturación pendiente, ahora la procesamos
                if saturacion_pendiente is not None:
                    porcentaje = saturacion_pendiente
                    color = calcular_color(porcentaje)
                    dot.node(f"{nodo_actual}_saturacion", label=f"{porcentaje}%", style="filled", fillcolor=color)
                    dot.edge(nodo_actual, f"{nodo_actual}_saturacion")
                    saturacion_pendiente = None
                
                nombre_en_proceso = False

            elif saturacion_en_proceso:
                saturacion_pendiente = int(lexema.strip('"').strip('%'))  # Almacena el porcentaje de saturación pendiente
                saturacion_en_proceso = False

        elif tipo == "PORCENTAJE":
            # Solo procesamos porcentajes si estamos en la fase de saturación
            if saturacion_en_proceso:
                if nodo_actual:
                    porcentaje = int(lexema.strip('%'))  # Asegúrate de eliminar el símbolo de porcentaje
                    color = calcular_color(porcentaje)
                    dot.node(f"{nodo_actual}_saturacion", label=f"{porcentaje}%", style="filled", fillcolor=color)
                    dot.edge(nodo_actual, f"{nodo_actual}_saturacion")
                saturacion_en_proceso = False

    # Conectar el último continente al título del grafo si es necesario
    if nodo_padre and nombre_grafico != "Grafica":
        dot.edge(nombre_grafico, nodo_padre)

    ruta_grafico = nombre_grafico
    dot.render(filename=ruta_grafico, format='png', cleanup=True)
    mostrar_imagen(f"{ruta_grafico}.png", graph_label)


def mostrar_imagen(ruta_grafico, graph_label):
    if os.path.exists(ruta_grafico):
        img = Image.open(ruta_grafico)
        # Asegúrate de que el tamaño sea adecuado para la visualización sin pérdida de calidad
        img = img.resize((1200, 800), Image.Resampling.LANCZOS)
        img_tk = ImageTk.PhotoImage(img)
        graph_label.config(image=img_tk)
        graph_label.image = img_tk
        graph_label.pack(fill="both", expand=True)  # Muestra la imagen si estaba oculta
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

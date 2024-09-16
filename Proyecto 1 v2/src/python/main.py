import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import subprocess
import os
import re

# Función para extraer los datos de una tabla HTML simple
def extraer_datos_tabla(html_content):
    # Usar una expresión regular para encontrar todas las filas de la tabla
    filas = re.findall(r'<tr>(.*?)</tr>', html_content, re.DOTALL)
    datos = []
    
    for fila in filas:
        # Encontrar todas las celdas dentro de una fila
        celdas = re.findall(r'<td>(.*?)</td>', fila, re.DOTALL)
        # Limpiar los datos, eliminando etiquetas HTML restantes
        celdas_limpias = [re.sub(r'<.*?>', '', celda).strip() for celda in celdas]
        if celdas_limpias:
            datos.append(celdas_limpias)
    
    return datos

# Función para ejecutar el análisis léxico
def analizar():
    # Obtener el texto del cuadro de entrada
    texto_entrada = text_input.get("1.0", tk.END).strip()
    
    # Verificar si el cuadro de entrada está vacío
    if not texto_entrada:
        messagebox.showwarning("Advertencia", "El cuadro de entrada está vacío. Por favor, ingrese texto para analizar.")
        return

    # Ruta al archivo que será modificado por el texto de entrada
    ruta_entrada = "../fortran/entradaEjemplo.org"
    
    # Sobrescribir el archivo 'entradaEjemplo.org' con el contenido del cuadro de entrada
    try:
        with open(ruta_entrada, "w") as file:
            file.write(texto_entrada)
        
        # Ejecutar el programa Fortran con subprocess
        subprocess.run(["../fortran/main.exe"], check=True)
        
        # Variables para las rutas de los reportes
        ruta_reporte_errores = "../fortran/reporte_errores.html"
        ruta_reporte_tokens = "../fortran/reporte_tokens.html"
        
        # Limpiar la tabla existente
        for row in tree.get_children():
            tree.delete(row)

        if os.path.exists(ruta_reporte_errores) and os.path.getsize(ruta_reporte_errores) > 0:
            # Leer y procesar el contenido de 'reporte_errores.html'
            with open(ruta_reporte_errores, "r") as file:
                contenido_html = file.read()
                datos = extraer_datos_tabla(contenido_html)
                # Insertar datos en el Treeview
                for fila in datos:
                    tree.insert('', 'end', values=fila)
        else:
            # Leer y procesar el contenido de 'reporte_tokens.html'
            if os.path.exists(ruta_reporte_tokens):
                with open(ruta_reporte_tokens, "r") as file:
                    contenido_html = file.read()
                    datos = extraer_datos_tabla(contenido_html)
                    # Insertar datos en el Treeview
                    for fila in datos:
                        tree.insert('', 'end', values=fila)
            else:
                messagebox.showinfo("Información", "No se generó ningún reporte de tokens ni de errores.")
        
        # Solo mostrar la tabla si se encontraron errores
        if os.path.exists(ruta_reporte_errores) and os.path.getsize(ruta_reporte_errores) > 0:
            tree.pack(padx=10, pady=10, expand=True, fill='both')
        else:
            tree.pack_forget()

    except subprocess.CalledProcessError as e:
        messagebox.showerror("Error", f"Hubo un error al ejecutar el analizador léxico: {e}")
    except FileNotFoundError:
        messagebox.showerror("Error", "No se encontró el archivo de salida generado por el analizador léxico.")
    except Exception as e:
        messagebox.showerror("Error", f"Se produjo un error inesperado: {e}")

# Crear la ventana principal
root = tk.Tk()
root.title("Interfaz de Analizador Léxico")
root.geometry("800x600")

# Crear cuadro de texto para la entrada
text_input = tk.Text(root, wrap='word', height=15)
text_input.pack(padx=10, pady=10, expand=True, fill='both')

# Crear botón para iniciar el análisis
analyze_button = tk.Button(root, text="Analizar", command=analizar)
analyze_button.pack(pady=5)

# Crear Treeview para mostrar los datos como una tabla
tree = ttk.Treeview(root, columns=("Col1", "Col2", "Col3", "Col4"), show="headings")
tree.heading("Col1", text="Error/Tokens")
tree.heading("Col2", text="Detalle")
tree.heading("Col3", text="Línea")
tree.heading("Col4", text="Columna")
tree.pack_forget()  # Inicialmente, no mostrar la tabla

# Ejecutar la aplicación
root.mainloop()

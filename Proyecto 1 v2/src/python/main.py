import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import subprocess
import os
import re

def extraer_datos_tabla(html_content):
    filas = re.findall(r'<tr>(.*?)</tr>', html_content, re.DOTALL)
    datos = []
    
    for fila in filas:
        celdas = re.findall(r'<td>(.*?)</td>', fila, re.DOTALL)
        celdas_limpias = [re.sub(r'<.*?>', '', celda).strip() for celda in celdas]
        if celdas_limpias:
            datos.append(celdas_limpias)
    
    return datos

def analizar():
    texto_entrada = text_input.get("1.0", tk.END).strip()

    if not texto_entrada:
        messagebox.showwarning("Advertencia", "El cuadro de entrada está vacío. Por favor, ingrese texto para analizar.")
        return

    ruta_entrada = "../fortran/entradaEjemplo.org"
    ruta_reporte_errores = "reporte_errores.html"
    ruta_reporte_tokens = "reporte_tokens.html"
    if os.path.exists(ruta_reporte_errores):
        os.remove(ruta_reporte_errores)
    if os.path.exists(ruta_reporte_tokens):
        os.remove(ruta_reporte_tokens)
    try:
        with open(ruta_entrada, "w") as file:
            file.write(texto_entrada)
        os.chdir("../fortran") 
        subprocess.run(["main.exe"], check=True)
        
        for row in tree.get_children():
            tree.delete(row)

        if os.path.exists(ruta_reporte_errores) and os.path.getsize(ruta_reporte_errores) > 0:
            with open(ruta_reporte_errores, "r") as file:
                contenido_html = file.read()
                datos = extraer_datos_tabla(contenido_html)
                for fila in datos:
                    tree.insert('', 'end', values=fila)
        else:
            if os.path.exists(ruta_reporte_tokens):
                with open(ruta_reporte_tokens, "r") as file:
                    contenido_html = file.read()
                    datos = extraer_datos_tabla(contenido_html)
                    for fila in datos:
                        tree.insert('', 'end', values=fila)
            else:
                messagebox.showinfo("Información", "No se generó ningún reporte de tokens ni de errores.")
        
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

root = tk.Tk()
root.title("Interfaz de Analizador Léxico")
root.geometry("800x600")

text_input = tk.Text(root, wrap='word', height=15)
text_input.pack(padx=10, pady=10, expand=True, fill='both')

analyze_button = tk.Button(root, text="Analizar", command=analizar)
analyze_button.pack(pady=5)

tree = ttk.Treeview(root, columns=("Col1", "Col2", "Col3", "Col4"), show="headings")
tree.heading("Col1", text="Error/Tokens")
tree.heading("Col2", text="Detalle")
tree.heading("Col3", text="Línea")
tree.heading("Col4", text="Columna")
tree.pack_forget()  
root.mainloop()

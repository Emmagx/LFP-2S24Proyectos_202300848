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

def abrir_archivo():
    file_path = filedialog.askopenfilename(filetypes=[("Archivos ORG", "*.org")])
    if file_path:
        with open(file_path, "r") as file:
            text_input.delete("1.0", tk.END)
            text_input.insert(tk.END, file.read())

def guardar():
    file_path = filedialog.asksaveasfilename(defaultextension=".org", filetypes=[("Archivos ORG", "*.org")])
    if file_path:
        with open(file_path, "w") as file:
            file.write(text_input.get("1.0", tk.END).strip())

def guardar_como():
    guardar()

def mostrar_acerca_de():
    messagebox.showinfo("Acerca de", "Proyecto de Lenguajes Formales y de Programación.\nUniversidad de San Carlos de Guatemala.\nEstudiante: [Tu nombre]\nCarnet: [Tu carnet]")

root = tk.Tk()
root.title("Interfaz de Analizador Léxico")
root.geometry("1000x600")
root.configure(bg="#2b2b2b")  # Color de fondo oscuro

style = ttk.Style()
style.theme_use("clam")  # Usar un tema que permita modificar colores
style.configure("Treeview", background="#333333", foreground="white", fieldbackground="#333333", rowheight=25)
style.configure("Treeview.Heading", background="#444444", foreground="white")

# Frame principal
main_frame = tk.Frame(root, bg="#2b2b2b")
main_frame.pack(expand=True, fill='both')

# Frame para entrada de texto
text_frame = tk.Frame(main_frame, bg="#2b2b2b")
text_frame.pack(side="left", padx=10, pady=10, fill="y")

text_input = tk.Text(text_frame, wrap='word', height=25, bg="#333333", fg="white", insertbackground="white")
text_input.pack(padx=10, pady=10, expand=True, fill='both')

# Frame para la gráfica y los resultados
output_frame = tk.Frame(main_frame, bg="#2b2b2b")
output_frame.pack(side="right", padx=10, pady=10, expand=True, fill='both')

tree = ttk.Treeview(output_frame, columns=("Col1", "Col2", "Col3", "Col4"), show="headings")
tree.heading("Col1", text="Error/Tokens")
tree.heading("Col2", text="Detalle")
tree.heading("Col3", text="Línea")
tree.heading("Col4", text="Columna")
tree.pack_forget()

# Menú de opciones
menu_bar = tk.Menu(root)
file_menu = tk.Menu(menu_bar, tearoff=0)
file_menu.add_command(label="Abrir", command=abrir_archivo)
file_menu.add_command(label="Guardar", command=guardar)
file_menu.add_command(label="Guardar como", command=guardar_como)
file_menu.add_separator()
file_menu.add_command(label="Salir", command=root.quit)
menu_bar.add_cascade(label="Archivo", menu=file_menu)

help_menu = tk.Menu(menu_bar, tearoff=0)
help_menu.add_command(label="Acerca de", command=mostrar_acerca_de)
menu_bar.add_cascade(label="Ayuda", menu=help_menu)

root.config(menu=menu_bar)

# Botones
button_frame = tk.Frame(text_frame, bg="#2b2b2b")
button_frame.pack(pady=5)

analyze_button = tk.Button(button_frame, text="Analizar", command=analizar, bg="#444444", fg="white")
analyze_button.pack(side="left", padx=5)

root.mainloop()

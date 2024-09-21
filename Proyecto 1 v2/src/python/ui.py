import tkinter as tk
from tkinter import ttk
from PIL import Image, ImageTk  # Necesitarás instalar Pillow para manejar las imágenes
from functions import analizar, abrir_archivo, guardar, mostrar_acerca_de, getListaPaises

def crear_ventana():
    root = tk.Tk()
    root.title("Interfaz de Analizador Léxico")
    root.geometry("1300x700")
    root.resizable(False, False)

    main_frame = tk.Frame(root)
    main_frame.pack(fill="both", expand=True)

    left_frame = tk.Frame(main_frame, bg="#f0f0f0", padx=5, pady=5)
    left_frame.grid(row=0, column=0, sticky="nsew")

    text_input = tk.Text(left_frame, height=7, width=7)
    text_input.pack(fill="both", expand=True)

    center_frame = tk.Frame(main_frame, bg="#e0e0e0", padx=20, pady=20)
    center_frame.grid(row=0, column=1, sticky="n")

    center_top_frame = tk.Frame(center_frame, bg="#e0e0e0", padx=5, pady=5)
    center_top_frame.pack(side="top", fill="x", expand=True)

    # Frame inferior donde irá el ComboBox, la imagen, y la población
    center_bottom_frame = tk.Frame(main_frame, bg="#d0d0d0", padx=5, pady=5)
    center_bottom_frame.grid(row=1, column=1, sticky="s")

    listaPaises = getListaPaises()  # Obtener la lista de países (aún vacío antes de analizar)

    def actualizar_combobox():
        nonlocal listaPaises
        listaPaises = getListaPaises()  # Obtener la lista de países
        nombres_paises = [pais.nombre for pais in listaPaises]  # Lista con nombres de los países
        
        # Limpiar el frame antes de añadir el nuevo ComboBox y otros widgets
        for widget in center_bottom_frame.winfo_children():
            widget.destroy()

        # Crear un nuevo ComboBox con los nombres de los países
        combo = ttk.Combobox(center_bottom_frame, state='readonly', values=nombres_paises)
        combo.pack(anchor='n', pady=10)

        # Función que se ejecuta cuando se selecciona un país
        def mostrar_info_pais(event):
            
            # Obtener el índice del país seleccionado
            seleccion = combo.current()
            if seleccion >= 0:
                pais_seleccionado = listaPaises[seleccion]

                # Mostrar la imagen de la bandera
                imagen_bandera = Image.open(pais_seleccionado.bandera)
                imagen_bandera = imagen_bandera.resize((100, 60), Image.LANCZOS)
                imagen_tk = ImageTk.PhotoImage(imagen_bandera)

                # Limpiar cualquier imagen o label previo
                for widget in center_bottom_frame.winfo_children():
                    if isinstance(widget, tk.Label) and widget not in [combo]:
                        widget.destroy()

                label_imagen = tk.Label(center_bottom_frame, image=imagen_tk)
                label_imagen.image = imagen_tk  # Guardar una referencia de la imagen para que no se recolecte
                label_imagen.pack(anchor='n', pady=5)

                # Mostrar la población
                label_poblacion = tk.Label(center_bottom_frame, text=f"Población: {pais_seleccionado.poblacion}")
                label_poblacion.pack(anchor='n', pady=5)

        combo.bind("<<ComboboxSelected>>", mostrar_info_pais)

    # Botón "Analizar" que llama a la función de análisis y actualiza el ComboBox
    tk.Button(center_top_frame, text="Analizar", command=lambda: [analizar(text_input, graph_label, tree), actualizar_combobox()]).pack(anchor="w", pady=5)

    tk.Button(center_top_frame, text="Abrir", command=lambda: abrir_archivo(text_input)).pack(anchor="w", pady=5)
    tk.Button(center_top_frame, text="Guardar", command=lambda: guardar(text_input)).pack(anchor="w", pady=5)
    tk.Button(center_top_frame, text="Acerca de", command=mostrar_acerca_de).pack(anchor="w", pady=5)

    tk.Label(center_bottom_frame, text="Este es el nuevo frame inferior", bg="#d0d0d0").pack(anchor="center", pady=10)

    right_frame_container = tk.Frame(main_frame)
    right_frame_container.grid(row=0, column=2, sticky="nsew")

    right_canvas = tk.Canvas(right_frame_container)
    right_canvas.pack(side="left", fill="both", expand=True)

    right_scroll_y = tk.Scrollbar(right_frame_container, orient="vertical", command=right_canvas.yview)
    right_scroll_y.pack(side="right", fill="y")

    right_scroll_x = tk.Scrollbar(right_frame_container, orient="horizontal", command=right_canvas.xview)
    right_scroll_x.pack(side="bottom", fill="x")

    right_frame = tk.Frame(right_canvas, bg="#ffffff", padx=5, pady=5)
    right_canvas.create_window((0, 0), window=right_frame, anchor="nw")

    right_canvas.configure(yscrollcommand=right_scroll_y.set, xscrollcommand=right_scroll_x.set)

    def on_frame_configure(event):
        right_canvas.configure(scrollregion=right_canvas.bbox("all"))

    right_frame.bind("<Configure>", on_frame_configure)

    graph_label = tk.Label(right_frame)
    tree = ttk.Treeview(right_frame, columns=("Error", "Descripción"), show='headings')
    tree.heading("Error", text="Error")
    tree.heading("Descripción", text="Descripción")
    tree.pack_forget()

    main_frame.grid_columnconfigure(0, weight=1)
    main_frame.grid_columnconfigure(1, weight=0)
    main_frame.grid_columnconfigure(2, weight=1)
    main_frame.grid_rowconfigure(0, weight=1)

    return root, text_input, graph_label, tree

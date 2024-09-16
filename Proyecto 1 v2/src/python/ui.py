import tkinter as tk
from tkinter import ttk
from functions import analizar, abrir_archivo, guardar, mostrar_acerca_de

def crear_ventana():
    # Crear la ventana principal
    root = tk.Tk()
    root.title("Interfaz de Analizador Léxico")
    root.geometry("1200x600")  # Ajustar el tamaño de la ventana
    root.resizable(False, False)

    # Crear el frame principal con tres columnas
    main_frame = tk.Frame(root)
    main_frame.pack(fill="both", expand=True)

    # --- Panel izquierdo (Editor de texto) ---
    left_frame = tk.Frame(main_frame, bg="#f0f0f0", padx=5, pady=5)
    left_frame.grid(row=0, column=0, sticky="nsew")

    text_input = tk.Text(left_frame, height=9, width=9)  # Reducir el ancho a 40
    text_input.pack(fill="both", expand=True)

    # --- Panel central (Botones de acciones) ---
    center_frame = tk.Frame(main_frame, bg="#e0e0e0", padx=5, pady=5)
    center_frame.grid(row=0, column=1, sticky="n")

    tk.Button(center_frame, text="Analizar", command=lambda: analizar(text_input, graph_label, tree)).pack(anchor="w", pady=5)
    tk.Button(center_frame, text="Abrir", command=lambda: abrir_archivo(text_input)).pack(anchor="w", pady=5)
    tk.Button(center_frame, text="Guardar", command=lambda: guardar(text_input)).pack(anchor="w", pady=5)
    tk.Button(center_frame, text="Acerca de", command=mostrar_acerca_de).pack(anchor="w", pady=5)

    # --- Panel derecho (Resultados) ---
    right_frame_container = tk.Frame(main_frame)
    right_frame_container.grid(row=0, column=2, sticky="nsew")

    # Crear Canvas para el right_frame
    right_canvas = tk.Canvas(right_frame_container)
    right_canvas.pack(side="left", fill="both", expand=True)

    # Crear las barras de desplazamiento para el right_frame
    right_scroll_y = tk.Scrollbar(right_frame_container, orient="vertical", command=right_canvas.yview)
    right_scroll_y.pack(side="right", fill="y")

    right_scroll_x = tk.Scrollbar(right_frame_container, orient="horizontal", command=right_canvas.xview)
    right_scroll_x.pack(side="bottom", fill="x")

    # Crear el right_frame dentro del canvas
    right_frame = tk.Frame(right_canvas, bg="#ffffff", padx=5, pady=5)
    right_canvas.create_window((0, 0), window=right_frame, anchor="nw")

    # Evitar que el frame se ajuste automáticamente al tamaño de su contenido
    right_frame_container.pack_propagate(False)

    # Configurar el canvas para scrollbars
    right_canvas.configure(yscrollcommand=right_scroll_y.set, xscrollcommand=right_scroll_x.set)

    # Configurar el área visible del canvas para mostrar scrollbars cuando sea necesario
    def on_frame_configure(event):
        right_canvas.configure(scrollregion=right_canvas.bbox("all"))
        # Forzar que el contenido sea al menos del tamaño del frame
        min_width = right_frame_container.winfo_width()
        min_height = right_frame_container.winfo_height()
        right_canvas.itemconfig("right_frame", width=max(event.width, min_width), height=max(event.height, min_height))

    right_frame.bind("<Configure>", on_frame_configure)

    # Widgets dentro del right_frame
    graph_label = tk.Label(right_frame)
    tree = ttk.Treeview(right_frame, columns=("Error", "Descripción"), show='headings')
    tree.heading("Error", text="Error")
    tree.heading("Descripción", text="Descripción")
    tree.pack_forget()  # Inicialmente oculto

    # Configuración de las columnas y filas del main_frame
    # Ajustar las columnas 0 y 2 para que tengan menos peso y así sean más pequeñas
    main_frame.grid_columnconfigure(0, weight=1)  # Columna izquierda más pequeña
    main_frame.grid_columnconfigure(1, weight=0)  # Columna central
    main_frame.grid_columnconfigure(2, weight=1)  # Columna derecha más pequeña
    main_frame.grid_rowconfigure(0, weight=1)

    return root, text_input, graph_label, tree

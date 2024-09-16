import tkinter as tk
from tkinter import ttk
from functions import analizar, abrir_archivo, guardar, mostrar_acerca_de

def crear_ventana():
    # Crear la ventana principal
    root = tk.Tk()
    root.title("Interfaz de Analizador Léxico")
    root.geometry("1000x600")

    # Crear el frame principal con tres columnas
    main_frame = tk.Frame(root)
    main_frame.pack(fill="both", expand=True)

    # --- Panel izquierdo (Editor de texto) ---
    left_frame = tk.Frame(main_frame, bg="#f0f0f0", padx=5, pady=5)
    left_frame.grid(row=0, column=0, sticky="nsew")

    text_input = tk.Text(left_frame, height=30, width=60)
    text_input.pack(fill="both", expand=True)

    # --- Panel central (Botones de acciones) ---
    center_frame = tk.Frame(main_frame, bg="#e0e0e0", padx=5, pady=5)
    center_frame.grid(row=0, column=1, sticky="nsew")

    tk.Button(center_frame, text="Analizar", command=lambda: analizar(text_input, graph_label, tree)).pack(anchor="w", pady=5)
    tk.Button(center_frame, text="Abrir", command=lambda: abrir_archivo(text_input)).pack(anchor="w", pady=5)
    tk.Button(center_frame, text="Guardar", command=lambda: guardar(text_input)).pack(anchor="w", pady=5)
    tk.Button(center_frame, text="Acerca de", command=mostrar_acerca_de).pack(anchor="w", pady=5)

    # --- Panel derecho (Resultados) ---
    right_frame = tk.Frame(main_frame, bg="#ffffff", padx=5, pady=5)
    right_frame.grid(row=0, column=2, sticky="nsew")

    graph_label = tk.Label(right_frame)
    tree = ttk.Treeview(right_frame, columns=("Error", "Descripción"), show='headings')
    tree.heading("Error", text="Error")
    tree.heading("Descripción", text="Descripción")
    tree.pack_forget()  # Inicialmente oculto

    main_frame.grid_columnconfigure(0, weight=1)
    main_frame.grid_columnconfigure(1, weight=0)
    main_frame.grid_columnconfigure(2, weight=1)
    main_frame.grid_rowconfigure(0, weight=1)

    return root, text_input, graph_label, tree

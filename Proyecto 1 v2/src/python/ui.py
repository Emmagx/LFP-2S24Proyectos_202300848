import tkinter as tk
from tkinter import ttk
from functions import analizar, abrir_archivo, guardar, mostrar_acerca_de

def crear_ventana():
    root = tk.Tk()
    root.title("Interfaz de Analizador Léxico")
    root.geometry("1300x700")
    root.resizable(False, False)

    main_frame = tk.Frame(root)
    main_frame.pack(fill="both", expand=True)

    left_frame = tk.Frame(main_frame, bg="#f0f0f0", padx=5, pady=5)
    left_frame.grid(row=0, column=0, sticky="nsew")

    text_input = tk.Text(left_frame, height=7, width=7)  # Ajustado a un ancho mayor
    text_input.pack(fill="both", expand=True)

    center_frame = tk.Frame(main_frame, bg="#e0e0e0", padx=20, pady=20)
    center_frame.grid(row=0, column=1, sticky="n")

    center_top_frame = tk.Frame(center_frame, bg="#e0e0e0", padx=5, pady=5)
    center_top_frame.pack(side="top", fill="x", expand=True)

    tk.Button(center_top_frame, text="Analizar", command=lambda: analizar(text_input, graph_label, tree)).pack(anchor="w", pady=5)
    tk.Button(center_top_frame, text="Abrir", command=lambda: abrir_archivo(text_input)).pack(anchor="w", pady=5)
    tk.Button(center_top_frame, text="Guardar", command=lambda: guardar(text_input)).pack(anchor="w", pady=5)
    tk.Button(center_top_frame, text="Acerca de", command=mostrar_acerca_de).pack(anchor="w", pady=5)

    center_bottom_frame = tk.Frame(center_frame, bg="#d0d0d0", padx=5, pady=5)
    center_bottom_frame.pack(side="top", fill="x", expand=True)
    
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

    right_frame_container.pack_propagate(False)

    right_canvas.configure(yscrollcommand=right_scroll_y.set, xscrollcommand=right_scroll_x.set)

    def on_frame_configure(event):
        right_canvas.configure(scrollregion=right_canvas.bbox("all"))
        min_width = right_frame_container.winfo_width()
        min_height = right_frame_container.winfo_height()
        right_canvas.itemconfig("right_frame", width=max(event.width, min_width), height=max(event.height, min_height))

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

from ui import crear_ventana
from functions import analizar

if __name__ == "__main__":
    app, text_input, graph_label, tree = crear_ventana()

    from functions import analizar
    analizar(text_input, graph_label, tree)
    
    app.mainloop()

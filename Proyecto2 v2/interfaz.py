import tkinter as tk
from tkinter import filedialog
from tkinter import messagebox
from tkinter import ttk as tkk
import webbrowser
import subprocess
import copy



#Declarando unas variables globales
errores=[]
erroresLexicos=[]
analizado=False


# Función para analizar el texto
def analizar():
    global analizado

    #limpio la tabla de errores
    for i in arbol.get_children():
        arbol.delete(i)

    cuerpo=entrada.get("1.0", tk.END) #Obtengo el contenido del editor de texto
    lineas=cuerpo.splitlines() #Divido el contenido del editor de texto por líneas
    todasVacias=all(linea=="" or linea.isspace() for linea in lineas) #Verifico si todas las líneas estan vacias
    if todasVacias:
        messagebox.showerror("Error", "No se ha ingresado información.") #Mensaje de error
        info.config(text="Ingrese Información en el editor de texto", foreground="red") #Mensaje de error
    else:
        info.config(text="Analizando...", foreground="black") #Mensaje que dice el proceso

        datos=entrada.get("1.0","end-1c")
        resultado=subprocess.run(['./main.exe'],input=datos, stdout=subprocess.PIPE,text=True)
        salida=resultado.stdout.strip() #Aquí obtengo la salida del análisis
        print(salida)
        print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
        #Aquí coloco todo lo demás del análisis
        if (salida=="Bien"):
            messagebox.showinfo("Análisis", "El análisis se realizó correctamente.")
            info.config(text="Análisis correcto", foreground="green") #Mensaje de éxito
            analizado=True
            
            #limpio la tabla de errores
            for i in arbol.get_children():
                arbol.delete(i)

        else:
            messagebox.showerror("Error", "Error durante el análisis")
            info.config(text="Análisis Fallido", foreground="red")
            analizado=False
            partes=salida.split("\n")
            print(len(partes))
            print("+++++++++++++++++++++++++++++++++++++++++++++++++++++")
            temporadita=0
            otro=0
            v=0
            while temporadita<len(partes)/5:
                fila=[]
                v=0
                fila.append(temporadita+1)
                while v<5:
                    fila.append(partes[otro])
                    otro=otro+1
                    v+=1
                temporadita+=1
                arbol.insert("", "end", values=fila)
            

def insertarDatos(tabla):
    for indice, row in tabla.iterrows():
        arbol.insert("", tk.END, values=(row["Tipo"], row["Linea"], row["Columna"], row["Token"], row["Descripción"]))

def tokens(): #Función para mostrar los tokens
    global analizado
    if analizado:

        webbrowser.open("tablaTokens.html")
        info.config(text="Mostrando tokens", foreground="green")
    else:
        messagebox.showerror("Error", "Primero se debe ejecutar el análisis exitosamente")
        info.config(text="Falta de análisis exitoso", foreground="red")
        
        

#Todo lo relacionado a la interfaz gráfica ----------------------------------------------

#Declaro e inicializo las variables globales para la interfaz
guardado=False
ruta=""
preguntar=True

# Función para la posición del mouse
def posicion(event):
    x, y = event.x, event.y
    pos.config(text=f"x={x}, y={y}")

def abro(): #Función para abrir un archivo
    global ruta, guardado,preguntar #Uso las variables globales

    #limpio la tabla de errores
    for i in arbol.get_children():
        arbol.delete(i)

    info.config(text="Abriendo archivo...", foreground="black") #Mensaje que dice el proceso
    ruta=filedialog.askopenfilename(title="Abrir archivo",filetypes=(("Archivos .LFP", "*.LFP"),)) #Solo acepta archivos .LFP
    if ruta != "": #Si la ruta no está vacía
        guardado=True #Cambio el estado de guardado
        preguntar=True
        try:
            with open(ruta, "r", encoding="utf-8") as archivo: #Abro el archivo
                cuerpo=archivo.read() #Leo el archivo
                entrada.delete("1.0", tk.END) #Borro el contenido del editor de texto
                entrada.insert("1.0", cuerpo) #Inserto el contenido del archivo en el editor de texto
            info.config(text="Se abrió el archivo correctamente", foreground="green")
        except FileNotFoundError:
            messagebox.showerror("Error", "El archivo no se encontró o no se pudo abrir.")
        except Exception as e:
            messagebox.showerror("Error", f"No se pudo abrir el archivo: {str(e)}")
    else:
        info.config(text="No se seleccionó ningún archivo", foreground="red") #Mensaje de error

def guardar(): #Función para guardar el archivo
    global ruta, guardado, preguntar #Uso las variables globales
    info.config(text="Guardando archivo...", foreground="black") #Mensaje que dice el proceso
    if guardado:
        try:
            preguntar=True
            with open(ruta,"w", encoding="utf-8") as archivo: #Abro el archivo
                archivo.write(entrada.get("1.0", tk.END)) #Escribo el contenido del editor de texto en el archivo
            info.config(text="Se guardó el archivo correctamente", foreground="green") #Mensaje de éxito
        except Exception as e:
            messagebox.showerror("Error", f"No se pudo guardar el archivo: {str(e)}")
            info.config(text="No se pudo guardar el archivo", foreground="red") #Mensaje de error
    else:
        guardarComo() #Si no se ha guardado, llamo a la función guardarComo

def guardarComo(): #Función para guardar el archivo como
    global ruta, guardado,preguntar #Uso las variables globales
    info.config(text="Guardando archivo como...", foreground="black") #Mensaje que dice el proceso
    temprutra=copy.deepcopy(ruta) #Guardo la ruta actual
    ruta=filedialog.asksaveasfilename(title="Guardar archivo",filetypes=(("Archivos .LFP", "*.LFP"),))
    if ruta != "": #Si la ruta no está vacía
        guardado=True #Cambio el estado de guardado
        guardar() #Llamo a la función guardar
    else:
        info.config(text="No se guardó el archivo", foreground="red") #Mensaje de error
        ruta=copy.deepcopy(temprutra) #Recupero la ruta anterior

def pregunta(): #Función para preguntar si se quiere guardar
    global guardado, ruta, preguntar #Uso las variables globales
    preguntando=messagebox.askyesno("Guardar", "¿Desea guardar antes de abrir un nuevo archivo?") #Pregunta si se quiere guardar
    if preguntando: #Si se quiere guardar
        guardar() #Llamo a la función guardar
    else: #Si no se quiere guardar
        info.config(text="No se guardó el archivo", foreground="red") #Mensaje de error

def nuevar(): #Función para empezar un archivo nuevo
    global ruta, guardado,preguntar #Uso las variables globales
    if preguntar:
        pregunta()

    preguntar=True
    info.config(text="Creando nuevo archivo...", foreground="black") #Mensaje que dice el proceso
    ruta="" #La ruta se vuelve vacía
    guardado=False #El estado de guardado se vuelve falso
    entrada.delete("1.0", tk.END) #Borro el contenido del editor de texto
    info.config(text="Nuevo archivo creado", foreground="green") #Mensaje de éxito

    #limpio la tabla de errores
    for i in arbol.get_children():
        arbol.delete(i)

# Creo la ventana principal
ventana = tk.Tk()
ventana.title("Proyecto 1 LFP")

# Creación de los frames para la interfaz
frame1 = tk.Frame(ventana)
frame1.pack(side=tk.TOP)
frame2 = tk.Frame(ventana)
frame2.pack(side=tk.BOTTOM, expand=True, fill="both")
frame3 = tk.Frame(frame1)
frame3.pack(side=tk.LEFT)
frame4 = tk.Frame(frame1)
frame4.pack(side=tk.RIGHT)

# Creo el editor de texto ----------------------------------------------
#Creación de la información de la posición del mouse
mouse = tk.Label(frame3, text="Posición del mouse: ", font=("Arial", 12))
mouse.pack()
ventana.bind("<Motion>", posicion) # Vincular el movimiento del mouse a la función posicion
pos=tk.Label(frame3, text="x=0, y=0", font=("Arial", 12, "bold"))
pos.pack()
# Frame para tener un scrollbar
frameIzq = tk.Frame(frame3)
frameIzq.pack()

# Textbox
entrada = tk.Text(frameIzq, height=26, width=75)
entrada.pack(side="left")

# Scrollbar
scroll = tk.Scrollbar(frameIzq, orient="vertical", command=entrada.yview)
entrada.config(yscrollcommand=scroll.set)
scroll.pack(side="right", fill="y")

# Label que irá diciendo que está pasando en el programa
info=tk.Label(frame4,text=" ")
info.pack()
info.config(foreground="red", font=("Arial", 9, "italic"))

# Creación de los botones y las cosas que se mostrarán en el lado derecho ----------------------------------------------



#Creación del botón para el análisis




#Creación del contenedor donde meta lo demás
contenedor = tk.Frame(frame4)
contenedor.pack(ipady=5,ipadx=5, padx=5, pady=5)
contenedor.config(border=2, relief="groove")

#Creación de la etiqueta para el título
titulo = tk.Label(contenedor, text="ARCHIVO", font=("Arial", 15, "bold"))
titulo.pack(side="top")

#Creación del botón para nuevo
nuevo = tk.Button(contenedor, text="Nuevo", height="1", width="11", command=nuevar)
nuevo.pack(side="left")
nuevo.config(background="white", foreground="black", font=("Arial", 10, "bold"))

#Creación del botón para abrir
abrir = tk.Button(contenedor, text="Abrir", height="1", width="11", command=abro)
abrir.pack(side="right")
abrir.config(background="white", foreground="black", font=("Arial", 10, "bold"))

#Creación del botón para guardar
guardo = tk.Button(contenedor, text="Guardar", height="1", width="11", command=guardar)
guardo.pack()
guardo.config(background="white", foreground="black", font=("Arial", 10, "bold"))

#Creación del botón para guardar como
guardoComo = tk.Button(contenedor, text="Guardar como", height="1", width="11", command=guardarComo)
guardoComo.pack()
guardoComo.config(background="white", foreground="black", font=("Arial", 10, "bold"))

#Creación del contenedor para el análisis y token ----------------------------------------------
contener= tk.Frame(frame4)
contener.pack(ipadx=2, padx=5, pady=5,ipady=5)


#Creación del botón para analizar
analisis = tk.Button(contener, text="Analizar", height="2", width="9", command=analizar)
analisis.pack(side="left")
analisis.config(background="white", foreground="green", font=("Arial", 15, "bold"))

#Creación del botón para ver los tokens
token= tk.Button(contener, text="Tokens", height="2", width="9", command=tokens)
token.pack(side="right")
token.config(background="white", foreground="orange", font=("Arial", 15, "bold"))

#Creación del botón para salir
salir= tk.Button(frame4, text="Salir", height="1", width="9", command=ventana.quit)
salir.pack(side="bottom")
salir.config(background="white", foreground="red", font=("Arial", 13, "bold"))


#Creación de la tabla de errores ----------------------------------------------
arbol=tkk.Treeview(frame2, columns=("No. Error","Tipo","Token/Token Esperado","Descripción","Linea","Columna"), show="headings")
arbol.pack(side="left") #Lo empaqueto en el frame2
#Encabezados
arbol.heading("No. Error", text="No. Error")
arbol.heading("Tipo", text="Tipo")
arbol.heading("Token/Token Esperado", text="Token/Lexema Esperado")
arbol.heading("Descripción", text="Descripción")
arbol.heading("Linea", text="Linea")
arbol.heading("Columna", text="Columna")


#Las scrollbar de la tabla
barraX=tk.Scrollbar(frame2, orient="vertical", command=arbol.yview)
barraX.pack(side="right", fill="y")
arbol.config(yscrollcommand=barraX.set)


# Ejecuta la ventana
ventana.mainloop()
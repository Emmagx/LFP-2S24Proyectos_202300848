# pais.py

class Pais:
    def __init__(self, nombre, poblacion, saturacion, bandera):
        self.nombre = nombre
        self.poblacion = poblacion
        self.saturacion = saturacion
        self.bandera = bandera
    
    def __str__(self):
        return f"País: {self.nombre}, Población: {self.poblacion}, Saturación: {self.saturacion}, Bandera: {self.bandera}"

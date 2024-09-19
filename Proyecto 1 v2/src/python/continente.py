class Continente:
    def __init__(self, nombre):
        self.nombre = nombre
        self.paises = []

    def agregar_pais(self, pais):
        self.paises.append(pais)
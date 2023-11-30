import pandas as pd

# Cargar datos desde el archivo CSV sin cabecera
datos = pd.read_csv("D:/Documentos/Universidad/TFG/resultado_medicamentos.csv", header=None, names=["palabras"])

print(datos)

def limpiar_palabras(palabra):
    if "/" in palabra:
        partes = palabra.split("/")
        print(partes)

        datos.drop(datos[datos["palabras"] == palabra].index, inplace=True)

        for i in partes:
            print(i)
            if i in datos["palabras"]:
                datos.append(i)
                datos.sort()


datos["palabras_limpias"] = datos["palabras"].apply(limpiar_palabras)

# Imprimir el resultado
pd.set_option("display.max_rows", None)
print(datos)

datos.to_csv("D:/Documentos/Universidad/TFG/datos_limpios.csv", index=False)

import pandas as pd

with open(r"D:\Documentos\Universidad\TFG\TFG-Data-Analisis\Analiticas.csv", 'r', encoding='utf-8') as file:
    for i in range(5):  # Lee las primeras 5 líneas del archivo
        line = file.readline()
        print(line)

# Lee los archivos CSV
ANALITIC = pd.read_csv(r"D:\Documentos\Universidad\TFG\TFG-Data-Analisis\Analiticas.csv", sep=";", header=0) # El argumento header=0 especifica que la primera fila es el encabezado
INFORM = pd.read_csv(r"D:\Documentos\Universidad\TFG\TFG-Data-Analisis\Datos_informe.csv", sep=";", header=0)

# Muestra los primeros registros para verificar si los datos se han leído correctamente
print("Primeros registros de ANALITIC:")
print(ANALITIC.head())

print("\nPrimeros registros de INFORM:")
print(INFORM.head())

# Suponiendo que 'INFORM' es tu data frame en Python y 'GIDENPAC' es la columna que contiene los ID de pacientes
NumXPacienteI = INFORM['GIDENPAC'].value_counts().reset_index()
NumXPacienteI.columns = ['ID_Pacientes', 'Freq']

# Imprimir el data frame resultante
print(NumXPacienteI)

#%%

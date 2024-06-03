import pandas as pd
from lifelines import AalenJohansenFitter
import matplotlib.pyplot as plt
import numpy as np

# Cargar los datos
baseurl = "D:/gugui/Documentos/Universidad/TFG/"
df = pd.read_csv(baseurl + "mice/ANALITIC_mice_1.csv", sep=",")

# Lista de todas las variables clínicas
variables_clinicas = [
    'Edad', 'Hemodialisis', 'Cociente.Album.Creat', 'Porcbasofilos', 'Porclinfocitos', 'Porcmonocitos', 'Acido.Folico',
    'ALAT.GPT', 'Albumina', 'Bilirrubina.directa', 'Bilirrubina.total', 'Calcio', 'CHCM', 'Cifra.de.Plaquetas',
    'CO2.suero', 'Colesterol.de.LDL.Formula.de.Friedewald', 'Creatinina', 'Creatinina.orina', 'Densidad',
    'Fosfatasa.alcalina', 'Gamma.GT', 'HDL.Colesterol', 'Hemoglobina.A1c', 'LDH', 'Linfocitos.V.Absoluto',
    'Monocitos.V.Absoluto', 'Parathormona.Intacta', 'Peso', 'Potasio', 'Potasio.en.orina', 'Proteina.C.reactiva',
    'Proteinas.totales', 'Sodio.orina', 'T4.libre', 'Talla', 'Temperatura.Axilar', 'TSH', 'Vitamina.B12',
    'Volumen.plaquetar.medio'
]

# Asegúrate de que la columna 'fechatoma' está en el formato correcto
df['fechatoma'] = pd.to_datetime(df['fechatoma'])

# Ordenar el DataFrame por 'id' y 'fechatoma'
df = df.sort_values(by=['ID', 'fechatoma'])

# Crear la variable de duración desde la primera toma
df['duration'] = (df['fechatoma'] - df.groupby('ID')['fechatoma'].transform('first')).dt.days

# Crear la variable de evento basada en los umbrales de FGE y el estado de fallecido
def categorize_event(row):
    if row['Fallecido'] == 1:
        return 'Fallecido'
    elif row['FGE'] > 90:
        return 'Hiperfiltración'
    elif 60 <= row['FGE'] <= 89:
        return 'Insuficiencia Temprana'
    elif 30 <= row['FGE'] <= 59:
        return 'Insuficiencia Moderada'
    elif 15 <= row['FGE'] <= 29:
        return 'Insuficiencia Severa'
    elif row['FGE'] < 15:
        return 'Terminal'
    else:
        return 'Sin evento'

df['event_type'] = df.apply(categorize_event, axis=1)

# Asignar un número entero a cada tipo de evento
event_mapping = {
    'Fallecido': 1,
    'Hiperfiltración': 2,
    'Insuficiencia Temprana': 3,
    'Insuficiencia Moderada': 4,
    'Insuficiencia Severa': 5,
    'Terminal': 6
}
df['event_type_code'] = df['event_type'].map(event_mapping)

# Filtrar filas con eventos relevantes
df_eventos = df[df['event_type'] != 'Sin evento'].copy()

# Verificar y eliminar filas con NaNs o valores infinitos en las columnas relevantes
df_eventos = df_eventos.replace([np.inf, -np.inf], np.nan).dropna(subset=['duration', 'event_type_code'])

# Verificar si todavía hay NaNs en el dataset y manejarlos
print("NaNs en el dataset después de eliminar filas con NaNs en las columnas relevantes:")
print(df_eventos.isnull().sum())

# Añadir un pequeño ruido aleatorio a los tiempos de duración para evitar empates
df_eventos.loc[:, 'duration'] = df_eventos['duration'] + np.random.uniform(-0.01, 0.01, size=df_eventos.shape[0])

# Ajustar el modelo Aalen-Johansen usando las columnas 'duration' y 'event_type_code'
ajf = AalenJohansenFitter()

# Ajustar el modelo a los datos para cada evento de interés
for event_name, event_code in event_mapping.items():
    print(f"Probabilidad acumulativa para {event_name}:")
    ajf.fit(df_eventos['duration'], df_eventos['event_type_code'], event_of_interest=event_code)
    # Imprimir la probabilidad acumulativa de los eventos
    print(ajf.cumulative_density_)
    # Graficar las curvas de probabilidad acumulativa de los eventos
    ajf.plot()
    plt.title(f'Curva de probabilidad acumulativa de {event_name} (Aalen-Johansen)')
    plt.xlabel('Duración (días)')
    plt.ylabel('Probabilidad acumulativa')
    plt.savefig(baseurl + f'Graficas/python/AalenJohansen/AJ_Curv_prob_acum_en_{event_name}.png')
    plt.show()

print('fin')

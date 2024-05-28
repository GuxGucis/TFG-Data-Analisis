import pandas as pd
import numpy as np
from lifelines import CoxTimeVaryingFitter

# Cargar los datos
# Set the base URL for your data
baseurl = "D:/gugui/Documentos/Universidad/TFG/"
df = pd.read_csv(baseurl + "mice/ANALITIC_mice_1.csv", sep=",")


# Convertir 'fechatoma' a datetime
df['fechatoma'] = pd.to_datetime(df['fechatoma'])

# Ordenar por 'id' y 'fechatoma'
df = df.sort_values(by=['ID', 'fechatoma'])

# Crear columnas 'start' y 'stop'
df['start'] = df.groupby('ID')['fechatoma'].shift(1)
df['start'] = df['start'].fillna(df['fechatoma'].min())
df['stop'] = df['fechatoma']

# Convertir 'start' y 'stop' a días desde una fecha de referencia
reference_date = df['fechatoma'].min()
df['start'] = (df['start'] - reference_date).dt.days
df['stop'] = (df['stop'] - reference_date).dt.days

# Eliminar filas con start y stop ambos en tiempo 0
df = df.loc[~((df['start'] == 0) & (df['stop'] == 0))]

# Crear una columna 'event' que indica si el FGE ha disminuido (empeoramiento)
df['event'] = df.groupby('ID')['FGE'].diff().fillna(0).apply(lambda x: 1 if x < 0 else 0)

# Combinar filas con start y stop iguales y un evento de muerte
df = df.sort_values(by=['ID', 'start', 'stop'])
df['next_start'] = df.groupby('ID')['start'].shift(-1)
df['next_event'] = df.groupby('ID')['event'].shift(-1)
df.loc[(df['start'] == df['stop']) & (df['event'] == 1), 'stop'] = df['next_start']
df = df.drop(columns=['next_start', 'next_event'])

# Verificar y eliminar cualquier fila con NaN o infinitos
df = df.replace([np.inf, -np.inf], np.nan).dropna()

# El mensaje indica que la columna 'Densidad' tiene una varianza muy baja,
# lo que puede afectar la convergencia del modelo.
# Si esta columna no proporciona información significativa,
# es recomendable eliminarla antes de ajustar el modelo.
# Seleccionar todas las columnas relevantes para el análisis
variables_clinicas = ['FGE', 'Edad', 'Hemodialisis', 'Cociente.Album.Creat', 'Porcbasofilos', 'Porclinfocitos',
                      'Porcmonocitos', 'Acido.Folico', 'ALAT.GPT', 'Albumina',
                      'Bilirrubina.directa', 'Bilirrubina.total', 'Calcio', 'CHCM',
                      'Cifra.de.Plaquetas', 'CO2.suero',
                      'Colesterol.de.LDL.Formula.de.Friedewald', 'Creatinina',
                      'Creatinina.orina', 'Fosfatasa.alcalina', 'Gamma.GT',
                      'HDL.Colesterol', 'Hemoglobina.A1c', 'LDH', 'Linfocitos.V.Absoluto',
                      'Monocitos.V.Absoluto', 'Parathormona.Intacta', 'Peso', 'Potasio',
                      'Potasio.en.orina', 'Proteina.C.reactiva', 'Proteinas.totales',
                      'Sodio.orina', 'T4.libre', 'Talla', 'Temperatura.Axilar', 'TSH',
                      'Vitamina.B12', 'Volumen.plaquetar.medio']

# Crear el DataFrame con las columnas necesarias para el modelo de Cox
df_cox = df[['ID', 'start', 'stop', 'event'] + variables_clinicas]

# Crear el modelo de CoxTimeVarying
ctv = CoxTimeVaryingFitter()

# Ajustar el modelo
ctv.fit(df_cox, id_col='ID', event_col='event', start_col='start', stop_col='stop', show_progress=True)

# Mostrar el resumen del modelo
print(ctv.summary)

#%%
import pandas as pd
import numpy as np
from lifelines import CoxTimeVaryingFitter, KaplanMeierFitter
import matplotlib.pyplot as plt

# IMPORTANCIA DE LAS VARIABLES

# Graficar la importancia de las variables (coeficientes del modelo de Cox)
plt.figure(figsize=(10, 12))
ctv.plot()
plt.title('Importancia de las Variables')
plt.xlabel('Coeficiente')
plt.ylabel('Variable')
plt.grid(True)
plt.savefig(baseurl+'Graficas/python/importancia_variables.png')
plt.show()

# CURVAS DE SUPERVIVICENCIA POR HEMIADÍALISIS

# Obtener la estimación del riesgo acumulado base
baseline_cumulative_hazard = ctv.baseline_cumulative_hazard_

# Crear el estimador de Kaplan-Meier para la supervivencia
kmf = KaplanMeierFitter()

# Separar los datos en función de la variable 'Hemodialisis'
hemodialisis_si = df_cox[df_cox['Hemodialisis'] == 1]
hemodialisis_no = df_cox[df_cox['Hemodialisis'] == 0]

# Graficar la función de supervivencia para ambos grupos
plt.figure(figsize=(10, 6))

# Supervivencia para pacientes con hemodiálisis
if not hemodialisis_si.empty:
    kmf.fit(durations=hemodialisis_si['stop'], event_observed=hemodialisis_si['event'], entry=hemodialisis_si['start'])
    kmf.plot_survival_function(ci_show=False, label='Hemodialisis SI')

# Supervivencia para pacientes sin hemodiálisis
if not hemodialisis_no.empty:
    kmf.fit(durations=hemodialisis_no['stop'], event_observed=hemodialisis_no['event'], entry=hemodialisis_no['start'])
    kmf.plot_survival_function(ci_show=False, label='Hemodialisis NO')


plt.title('Curva de Supervivencia Predicha por Hemodialisis')
plt.xlabel('Tiempo')
plt.ylabel('Probabilidad de Supervivencia')
plt.legend()
plt.grid(True)
plt.savefig(baseurl+'Graficas/python/Surv_HM.png')
plt.show()

# CURVAS DE SUPERVIVICENCIA POR GRUPOS DE EDAD

# Definir los grupos de edad
bins = [0, 30, 50, 70, 100]
labels = ['<30', '30-50', '50-70', '>70']
df_cox['age_group'] = pd.cut(df_cox['Edad'], bins=bins, labels=labels, right=False)

# Graficar la función de supervivencia para cada grupo de edad
plt.figure(figsize=(10, 6))

for label in labels:
    grupo_edad = df_cox[df_cox['age_group'] == label]
    if not grupo_edad.empty:
        kmf.fit(durations=grupo_edad['stop'], event_observed=grupo_edad['event'], entry=grupo_edad['start'])
        kmf.plot_survival_function(ci_show=False, label=f'Edad {label}')

plt.title('Curva de Supervivencia Predicha por Grupo de Edad')
plt.xlabel('Tiempo')
plt.ylabel('Probabilidad de Supervivencia')
plt.legend()
plt.grid(True)
plt.savefig(baseurl+'Graficas/python/Surv_edad.png')
plt.show()

print('fin')
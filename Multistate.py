import pandas as pd
import numpy as np
from lifelines import CoxTimeVaryingFitter, KaplanMeierFitter
import matplotlib.pyplot as plt

# Cargar los datos
baseurl = "D:/gugui/Documentos/Universidad/TFG/"
df_1 = pd.read_csv(baseurl + "mice/ANALITIC_mice_1.csv", sep=",")
df_2 = pd.read_csv(baseurl + "mice/ANALITIC_mice_2.csv", sep=",")
df_3 = pd.read_csv(baseurl + "mice/ANALITIC_mice_3.csv", sep=",")
df_4 = pd.read_csv(baseurl + "mice/ANALITIC_mice_4.csv", sep=",")
df_5 = pd.read_csv(baseurl + "mice/ANALITIC_mice_5.csv", sep=",")
df_6 = pd.read_csv(baseurl + "mice/ANALITIC_mice_6.csv", sep=",")
df_7 = pd.read_csv(baseurl + "mice/ANALITIC_mice_7.csv", sep=",")

dataframes = {
    1: df_1,
    2: df_2,
    3: df_3,
    4: df_4,
    5: df_5,
    6: df_6,
    7: df_7
}

mice = [1, 2, 3, 4, 5, 6, 7]

for i in mice:
    # Obtener el dataframe correspondiente
    df = dataframes[i]

    # Convertir 'fechatoma' a datetime
    df['fechatoma'] = pd.to_datetime(df['fechatoma'])

    # Ordenar por 'ID' y 'fechatoma'
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

    # Definir los estados basados en FGE y estado de fallecimiento
    definir_estado = lambda fge, fallecido: (
        6 if fallecido == 1 else
        1 if fge > 90 else
        2 if 60 <= fge <= 89 else
        3 if 30 <= fge <= 59 else
        4 if 15 <= fge <= 29 else
        5 if fge < 15 else
        np.nan
    )

    df['estado_actual'] = df.apply(lambda row: definir_estado(row['FGE'], row['Fallecido']), axis=1)
    df['estado_siguiente'] = df.groupby('ID')['estado_actual'].shift(-1)

    # Eliminar filas sin transición
    df = df.dropna(subset=['estado_siguiente'])

    # Convertir las columnas de tiempo a numéricas
    df['start'] = pd.to_numeric(df['start'], errors='coerce')
    df['stop'] = pd.to_numeric(df['stop'], errors='coerce')
    df['estado_siguiente'] = pd.to_numeric(df['estado_siguiente'], errors='coerce')

    # Eliminar filas con valores nulos en 'start', 'stop', o 'estado_siguiente'
    df = df.dropna(subset=['start', 'stop', 'estado_siguiente'])

    # Colapsar filas con start y stop iguales y un evento de muerte
    df['next_start'] = df.groupby('ID')['start'].shift(-1)
    df['next_event'] = df.groupby('ID')['estado_siguiente'].shift(-1)
    df.loc[(df['start'] == df['stop']) & (df['estado_siguiente'] == 6), 'stop'] = df['next_start']
    df = df.drop(columns=['next_start', 'next_event'])

    # Verificar y eliminar cualquier fila con NaN o infinitos
    df = df.replace([np.inf, -np.inf], np.nan).dropna()

    # Eliminar filas donde 'start' no sea menor que 'stop'
    df = df[df['start'] < df['stop']]

    # Eliminar columnas con baja variabilidad
    low_variance_cols = ['Transplante', 'Densidad']
    df = df.drop(columns=low_variance_cols)

    # Eliminar columnas datetime y otras no numéricas
    df = df.select_dtypes(include=[np.number])

    # Asegurarse de que no hay columnas datetime residuales
    print(df.dtypes)

    # Crear el modelo de CoxTimeVarying
    ctv = CoxTimeVaryingFitter()

    # Ajustar el modelo para el conjunto de datos completo
    print("Ajustando el modelo general")
    try:
        ctv.fit(df, id_col='ID', event_col='estado_siguiente', start_col='start', stop_col='stop', show_progress=True)
    except Exception as e:
        print(f"Error ajustando el modelo: {e}")

    # Mostrar el resumen del modelo
    print(ctv.summary)

    # Graficar la Significancia de las variables
    plt.figure(figsize=(12, 10))
    ctv.plot()
    plt.title('Significancia de las Variables en el Modelo Multiestado General')
    plt.xlabel('Coeficiente')
    plt.ylabel('Variable')
    plt.grid(True)
    plt.savefig(baseurl + 'Graficas/python/MultiEstado/MS_significancia_variables_modelo_general_' + str(i) + '.png')
    plt.close()

    # CURVAS DE SUPERVIVENCIA POR HEMODIALISIS
    # Crear el estimador de Kaplan-Meier para la supervivencia
    kmf = KaplanMeierFitter()

    # Separar los datos en función de la variable 'Hemodialisis'
    hemodialisis_si = df[df['Hemodialisis'] == 1]
    hemodialisis_no = df[df['Hemodialisis'] == 0]

    print(f"Datos con hemodiálisis: {len(hemodialisis_si)} filas")
    print(f"Datos sin hemodiálisis: {len(hemodialisis_no)} filas")

    # Graficar la función de supervivencia para ambos grupos
    plt.figure(figsize=(10, 6))

    # Supervivencia para pacientes con hemodiálisis
    if not hemodialisis_si.empty:
        print("Ajustando el modelo Kaplan-Meier para pacientes con hemodiálisis")
        kmf.fit(durations=hemodialisis_si['stop'], event_observed=hemodialisis_si['estado_siguiente'] == 6, entry=hemodialisis_si['start'])
        kmf.plot_survival_function(ci_show=False, label='Hemodialisis SI')

    # Supervivencia para pacientes sin hemodiálisis
    if not hemodialisis_no.empty:
        print("Ajustando el modelo Kaplan-Meier para pacientes sin hemodiálisis")
        kmf.fit(durations=hemodialisis_no['stop'], event_observed=hemodialisis_no['estado_siguiente'] == 6, entry=hemodialisis_no['start'])
        kmf.plot_survival_function(ci_show=False, label='Hemodialisis NO')

    plt.title('Curva de Supervivencia Predicha por Hemodialisis')
    plt.xlabel('Tiempo')
    plt.ylabel('Probabilidad de Supervivencia')
    plt.legend()
    plt.grid(True)
    plt.savefig(baseurl + 'Graficas/python/MultiEstado/MS_Surv_HM_' + str(i) + '.png')
    plt.close()

    # CURVAS DE SUPERVIVENCIA POR GRUPOS DE EDAD

    # Definir los grupos de edad
    bins = [0, 30, 50, 70, 100]
    labels = ['<30', '30-50', '50-70', '>70']
    df['age_group'] = pd.cut(df['Edad'], bins=bins, labels=labels, right=False)

    # Graficar la función de supervivencia para cada grupo de edad
    plt.figure(figsize=(10, 6))

    for label in labels:
        grupo_edad = df[df['age_group'] == label]
        if not grupo_edad.empty:
            kmf.fit(durations=grupo_edad['stop'], event_observed=grupo_edad['estado_siguiente'] == 6, entry=grupo_edad['start'])
            kmf.plot_survival_function(ci_show=False, label=f'Edad {label}')

    plt.title('Curva de Supervivencia Predicha por Grupo de Edad')
    plt.xlabel('Tiempo')
    plt.ylabel('Probabilidad de Supervivencia')
    plt.legend()
    plt.grid(True)
    plt.savefig(baseurl + 'Graficas/python/MultiEstado/MS_Surv_edad_' + str(i) + '.png')
    plt.close()

    print('fin de la iteración: ' + str(i))

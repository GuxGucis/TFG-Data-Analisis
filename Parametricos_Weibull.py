import pandas as pd
import matplotlib.pyplot as plt
from math import log2
from lifelines import WeibullAFTFitter, KaplanMeierFitter

# Cargar los datos
baseurl = "D:/gugui/Documentos/Universidad/TFG/"
df_1 = pd.read_csv(baseurl + "Mice/ANALITIC_mice_1.csv", sep=",")
df_2 = pd.read_csv(baseurl + "Mice/ANALITIC_mice_2.csv", sep=",")
df_3 = pd.read_csv(baseurl + "Mice/ANALITIC_mice_3.csv", sep=",")
df_4 = pd.read_csv(baseurl + "Mice/ANALITIC_mice_4.csv", sep=",")
df_5 = pd.read_csv(baseurl + "Mice/ANALITIC_mice_5.csv", sep=",")
df_6 = pd.read_csv(baseurl + "Mice/ANALITIC_mice_6.csv", sep=",")
df_7 = pd.read_csv(baseurl + "Mice/ANALITIC_mice_7.csv", sep=",")

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
umbral = [90, 60, 30, 17]

for i in range(len(mice)):
    for j in range(len(umbral)):
        # Obtener el dataframe correspondiente
        df = dataframes[mice[i]]

        # Aseguramos que 'fechatoma' sea de tipo datetime
        df['fechatoma'] = pd.to_datetime(df['fechatoma'])

        # Ordenamos por id de paciente y fecha de toma de datos
        df = df.sort_values(by=['ID', 'fechatoma'])

        # Definimos el umbral de FGE que indica el evento (ajusta según tus necesidades)
        threshold_value = umbral[j]# Ejemplo: cualquier valor de FGE por debajo de 50 indica un evento

        # Calculamos la duración en días hasta el evento observado o el último seguimiento
        df['max_fechatoma'] = df.groupby('ID')['fechatoma'].transform('max')
        df['duration'] = (df['max_fechatoma'] - df['fechatoma']).dt.days
        df['duration'] = df['duration'] + 1  # Agregamos 1 día a todas las duraciones para asegurarnos de que sean positivas
        df['event'] = df['FGE'] < threshold_value

        # Obtener la edad mínima de cada paciente
        df_min_age = df.groupby('ID')['Edad'].min().reset_index()
        df_min_age.columns = ['ID', 'min_age']

        # Merge con el dataframe de supervivencia
        df_survival = pd.merge(df, df_min_age, on='ID')

        # Añadimos las covariables a df_survival
        covariates = [
                        'Hemodialisis', 'Edad', 'ITIPSEXO', 'Cociente.Album.Creat', 'Porclinfocitos',
                        'Porcmonocitos', 'ALAT.GPT', 'Bilirrubina.directa', 'Bilirrubina.total',
                        'Calcio', 'CHCM', 'Cifra.de.Plaquetas', 'Colesterol.de.LDL.Formula.de.Friedewald',
                        'Creatinina', 'Creatinina.orina', 'Densidad', 'Fosfatasa.alcalina', 'Gamma.GT',
                        'HDL.Colesterol', 'LDH', 'Peso', 'Potasio', 'Potasio.en.orina',
                        'Proteina.C.reactiva', 'Proteinas.Orina', 'Proteinas.totales', 'Sodio.orina',
                        'T4.libre', 'Talla', 'Temperatura.Axilar', 'TSH', 'Vitamina.B12',
                        'Volumen.plaquetar.medio', 'renta2021'
                    ]

        for covariate in covariates:
            df_survival[covariate] = df[covariate]

        # Asegurémonos de que la columna 'event' esté presente
        df_survival['event'] = df['event']

        # Ajustamos el modelo WeibullAFTFitter
        aft = WeibullAFTFitter()
        aft.fit(df_survival[['duration', 'event'] + covariates], 'duration', event_col='event')

        # Mostramos los resultados
        print(aft.summary)

        # Verificar las columnas en aft.summary
        print("Columnas disponibles en aft.summary:", aft.summary.columns)
        print("Contenido de aft.summary:", aft.summary)

        # Gráfica de supervivencia por franjas de edades
        kmf = KaplanMeierFitter()

        df_survival['age_group'] = pd.cut(df_survival['min_age'], bins=[0, 30, 40, 50, 70, 100], labels=['<30', '30-40', '40-50', '50-70', '>70'])

        plt.figure(figsize=(10, 6))
        for name, grouped_df in df_survival.groupby('age_group'):
            kmf.fit(grouped_df['duration'], grouped_df['event'], label=name)
            kmf.plot_survival_function()

        plt.title('Curvas de Supervivencia por Franjas de Edades con umbral FGE' + str(threshold_value))
        plt.xlabel('Tiempo (días)')
        plt.ylabel('Probabilidad de Supervivencia')
        plt.legend(title='Grupo de Edad')
        plt.savefig(baseurl+'Graficas/python/ParamWeibull/PW_Surv_edad_' + str(mice[i]) + '_umbral_' + str(threshold_value) + '.png')
        plt.close()

        # Gráfica de supervivencia por tratamiento de hemodiálisis
        plt.figure(figsize=(10, 6))
        for name, grouped_df in df_survival.groupby('Hemodialisis'):
            kmf.fit(grouped_df['duration'], grouped_df['event'], label='Hemodialisis' if name == 1 else 'No Hemodialisis')
            kmf.plot_survival_function()

        plt.title('Curvas de Supervivencia por Tratamiento de Hemodialisis con umbral FGE' + str(threshold_value))
        plt.xlabel('Tiempo (días)')
        plt.ylabel('Probabilidad de Supervivencia')
        plt.legend(title='Tratamiento de Hemodialisis')
        plt.savefig(baseurl+'Graficas/python/ParamWeibull/PW_Surv_HM_' + str(mice[i]) + '_umbral_' + str(threshold_value) + '.png')
        plt.close()

        # Verificar las columnas y el contenido de aft.summary
        summary_df = aft.summary.reset_index()  # Aplanar el índice
        print("Contenido de summary_df después de reset_index:", summary_df)

        # Filtrar los coeficientes relevantes
        summary_df = summary_df[summary_df['param'] == 'lambda_']
        print("Contenido de summary_df después del filtrado:", summary_df)

        # Graficar la importancia de las variables
        plt.figure(figsize=(12, 12))
        summary_df.set_index('covariate')['coef'].plot(kind='barh', color='skyblue', edgecolor='black')
        plt.title('Importancia de las Variables en el Modelo Weibull con umbral FGE' + str(threshold_value))
        plt.xlabel('Coeficiente')
        plt.ylabel('Variable')
        plt.axvline(x=0, color='black', linestyle='--')
        plt.subplots_adjust(left=0.3)  # Aumentar el margen izquierdo
        plt.savefig(baseurl+'Graficas/python/ParamWeibull/PW_Importancia_Variables_' + str(mice[i]) + '_umbral_' + str(threshold_value) + '.png')
        plt.close()

        # Mostrar la significancia de las variables
        plt.figure(figsize=(12, 12))
        summary_df.set_index('covariate')['-log2(p)'].plot(kind='barh', color='lightcoral', edgecolor='black')
        plt.title('Significancia de las Variables en el Modelo Weibull con umbral FGE' + str(threshold_value))
        plt.xlabel('-log2(p)')
        plt.ylabel('Variable')
        plt.axvline(x=-log2(0.05), color='black', linestyle='--', label='p=0.05')
        plt.legend()
        plt.subplots_adjust(left=0.3)  # Aumentar el margen izquierdo
        plt.savefig(baseurl+'Graficas/python/ParamWeibull/PW_Significancia_Variables_' + str(mice[i]) + '_umbral_' + str(threshold_value) + '.png')
        plt.close()

        print('fin de la iteración: ' + str(mice[i]) + '. En el umbral: ' + str(threshold_value))

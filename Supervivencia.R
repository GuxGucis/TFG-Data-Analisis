
library(survival)
library(dplyr)
library(tidyr)

# ------------------- MODELO DE COX -------------------
print('------------------- MODELO DE COX -------------------')

# Como preparación al modelo de COX, usamos como evento la comparativa entre el primero y el ultimo valor que se tenga de FGE
#   Si el valor ha crecido (que es mejor en teoria par la enfermedad) 0 y 1 si el valor es menor y a descendido y por tanto la enfermeda se considera que ha empeorado
#   El tiempo en dias que lleva en el estudio (es decir el tiempo en el que ha empeorado o mejorado)
#   La edad con la que empieza el estudio


df_datos <- ANALITIC %>%
  arrange(ID, fechatoma) %>%
  group_by(ID) %>%
  summarise(
    # tendencia_FGE = if_else(last(FGE, na_rm = TRUE) < first(FGE, na_rm = TRUE), 1, 0), # 1 si ha descendido, 0 en caso contrario
    tiempo_total = as.integer(last(fechatoma) - first(fechatoma)), # Días totales
    edad_inicio = first(Edad), # Usar la primera Edad registrada por ID
    .groups = "drop" # Asegurar que el resultado final no esté agrupado
  )

# Se observa que hay pacientes que solo tienen un registro y por tanto no hay valor informativo en esto puesto que no se sabe y ha habido evolución o no
# Se eliminan dichas filas

df_datos <- df_datos %>%
  filter(tiempo_total > 0)

# Inicializar una lista para almacenar los resultados de cada columna
resultados <- list()

for(i in 11:ncol(ANALITIC)) {
  # Obtener el nombre de la columna actual
  nombre_columna <- names(ANALITIC)[i]

  df_tendencia <- ANALITIC %>%
    arrange(ID, fechatoma) %>%
    group_by(ID) %>%
    # Filtrar solo los valores no nulos para la columna actual antes de aplicar last() y first()
    mutate(temp_col = !!sym(nombre_columna),
           temp_col = ifelse(is.na(temp_col), NA_real_, temp_col)) %>%
    # Asegurarse de que el grupo tiene más de un valor no nulo para comparar
    filter(!is.na(temp_col)) %>%
    summarise(
      tendencia = if_else(last(temp_col) < first(temp_col), 1, 0), #1 Si desciende 0 si asciende
      .groups = 'drop'
    ) %>%
    # Si el grupo fue filtrado completamente (todos eran NA), este paso evita un error
    # 1 si ha descendido, 0 en caso contrario
    mutate(tendencia = ifelse(is.na(tendencia), NA_integer_, tendencia)) %>%
    # Renombrar dinámicamente la columna de tendencia
    rename(!!nombre_columna := tendencia)

  # Agregar el dataframe de tendencia a la lista
  resultados[[nombre_columna]] <- df_tendencia
}

# Unir todos los dataframes de tendencia por ID
df_resultados <- reduce(resultados, full_join, by = "ID")
df_cox <- left_join(df_datos, df_resultados, by = "ID") #Las columnas de resultados se unen a datos

# REORDENAR PORQUE TOC

df_cox <- df_cox %>%
  select(
    1:3, # Selecciona las primeras tres columnas para mantenerlas en su lugar
    ncol(.)-1, ncol(.), # Selecciona las últimas dos columnas para moverlas
    4:(ncol(.)-2) # Selecciona el resto de las columnas para moverlas después de las últimas dos
  )

# ------------------------------------------------------------------
# ---------------------- MODELO DE COX -----------------------------
# ------------------------------------------------------------------

# Preparar la fórmula del modelo de Cox incluyendo todas las columnas desde la 5ª en adelante como covariables
covariables <- names(df_cox)[5:ncol(df_cox)] # Asume que las columnas de interés empiezan en la 5ª posición

# Imputar valores faltantes para las covariables numéricas con la media de cada columna
# Asume que 'edad_inicio' y todas las covariables desde la 5ª columna hacia adelante son numéricas
df_cox <- df_cox %>%
  mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

formula_cox <- as.formula(paste("Surv(tiempo_total, FGE) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

# Ajustar el modelo de Cox
modelo_cox <- coxph(formula_cox, data = df_cox)

# Ver el resumen del modelo
summary(modelo_cox)

# ------------------------------------------------------------------
# ---------------------- GRAFICA DE COX -----------------------------
# ------------------------------------------------------------------

library(survminer)
library(ggplot2)
library(broom)

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox, conf.int = TRUE, conf.level = 0.95)

# Crear el gráfico de hazard ratios con ggplot2
ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() + # Pone los términos en el eje Y para mejor legibilidad
  labs(x = "Covariables", y = "Log(Hazard Ratio)", title = "Efecto de las Covariables en el Riesgo Relativo") +
  theme_minimal()


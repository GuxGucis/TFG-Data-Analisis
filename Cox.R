
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

# Inicializar una lista para almacenar los resultados de cada columna
resultados <- list()

# Columnas a excluir para calcular las tendencias
indices <- setdiff(11:ncol(ANALITIC), match(c("Transplante", "Hemodialisis", "Fallecido"), names(ANALITIC)))

for(i in indices) {
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

# Adapatamos los datos extra que no se incluyen en ninguno de ambos apartados
# Transplante, Hemodialisis y Fallecido
df_transplante_hemodialisis <- ANALITIC %>%
  group_by(ID) %>%
  summarise(
    Transplante = max(Transplante, na.rm = TRUE),
    Hemodialisis = max(Hemodialisis, na.rm = TRUE),
    Fallecido = max(Fallecido, na.rm = TRUE),
    .groups = 'drop'
  )

# Unir todos los dataframes de tendencia por ID
df_resultados <- reduce(resultados, full_join, by = "ID")
df_cox <- left_join(df_datos, df_resultados, by = "ID") #Las columnas de resultados se unen a datos

# REORDENAR PORQUE TOC (muevo FGE y cociente mas adelante)
# df_cox <- df_cox %>%
#   select(
#     1:3, # Selecciona las primeras tres columnas para mantenerlas en su lugar
#     ncol(.)-1, ncol(.), # Selecciona las últimas dos columnas para moverlas
#     4:(ncol(.)-2) # Selecciona el resto de las columnas para moverlas después de las últimas dos
#   )
df_cox <- df_cox %>%
  relocate("FGE", .after = "edad_inicio")
df_cox <- df_cox %>%
  relocate("Cociente.Album.Creat", .after="FGE")

df_cox <- left_join(df_cox, df_transplante_hemodialisis, by = "ID")

# Se observa que hay pacientes que solo tienen un registro y por tanto no hay valor informativo en esto puesto que no se sabe y ha habido evolución o no
# Se eliminan dichas filas

df_cox <- df_cox %>%
  filter(tiempo_total > 0)

# ------------------- GENERAL -------------------
print('------------------- GENERAL -------------------')

# ------------------------------------------------------------------
# ---------------------- MODELO DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------
# ------------------------------------------------------------------

# Preparar la fórmula del modelo de Cox incluyendo todas las columnas desde la 5ª en adelante como covariables
covariables <- names(df_cox)[5:ncol(df_cox)] # Asume que las columnas de interés empiezan en la 5ª posición

# Imputar valores faltantes para las covariables numéricas con la media de cada columna
# Asume que 'edad_inicio' y todas las covariables desde la 5ª columna hacia adelante son numéricas
df_cox_fill <- df_cox %>%
  mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

formula_cox <- as.formula(paste("Surv(tiempo_total, FGE) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

# Ajustar el modelo de Cox
modelo_cox_FGE <- coxph(formula_cox, data = df_cox_fill)

# Ver el resumen del modelo
# summary(modelo_cox)

# ------------------------------------------------------------------
# ---------------------- MODELO DE COX -----------------------------
# -------------------- (sobre Fallecido) ---------------------------
# ------------------------------------------------------------------

formula_cox <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

# Ajustar el modelo de Cox
modelo_cox_Fallecido <- coxph(formula_cox, data = df_cox_fill)

# -------------------------------------------------------------------
# ---------------------- GRAFICA DE COX -----------------------------
# ------------------------ (sobre FGE) ------------------------------
# -------------------------------------------------------------------

library(survminer)
library(ggplot2)
library(broom)

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_FGE, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE con TODOS LOS PACIENTES") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))

print(g)

# -------------------------------------------------------------------
# ---------------------- GRAFICA DE COX -----------------------------
# --------------------- (sobre Fallecido) ---------------------------
# -------------------------------------------------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_Fallecido, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre Fallecimiento con TODOS LOS PACIENTES") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

g <- g + ylim(c(-3, 3))

print(g)

# =============== DIVIDIMOS DATAFRAMES =====================
# ------------------- EN HEMODIALISIS -------------------
print('------------------- EN HEMODIALISIS -------------------')

df_cox_hm <- df_cox %>%
  filter(Transplante == 0 & Hemodialisis == 1)

# ---------------------- NULOS -----------------------------

# Primero, calcula el porcentaje de valores NA por columna
porcentaje_nulos <- sapply(df_cox_hm, function(x) sum(is.na(x)) / length(x))

# Identifica las columnas con más del 90% de valores NA
columnas_a_eliminar <- names(porcentaje_nulos[porcentaje_nulos > 0.9])

# Elimina esas columnas del dataframe
df_cox_hm <- df_cox_hm[, !(names(df_cox_hm) %in% columnas_a_eliminar)]

# ---------------------- MODELO DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------

covariables <- names(df_cox_hm)[5:ncol(df_cox_hm)]

df_cox_hm_fill <- df_cox_hm %>%
  mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

formula_cox <- as.formula(paste("Surv(tiempo_total, FGE) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

modelo_cox_hm_FGE <- coxph(formula_cox, data = df_cox_hm_fill)

# Ver el resumen del modelo
# summary(modelo_cox_hm)

# -------------------- (sobre Fallecido) ---------------------------

formula_cox <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

modelo_cox_hm_Fallecido <- coxph(formula_cox, data = df_cox_hm_fill)

# ---------------------- GRAFICA DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_hm_FGE, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE CON HEMODIALISIS") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))
print(g)

# -------------------- (sobre Fallecido) ---------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_hm_Fallecido, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre Fallecido CON HEMODIALISIS") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# ------------------- CON TRANSPLANTE -------------------
print('------------------- CON TRANSPLANTE -------------------')

df_cox_tr <- df_cox %>%
  filter(Transplante == 1 & Hemodialisis == 0)

# ---------------------- NULOS -----------------------------

# Primero, calcula el porcentaje de valores NA por columna
porcentaje_nulos <- sapply(df_cox_tr, function(x) sum(is.na(x)) / length(x))

# Identifica las columnas con más del 90% de valores NA
columnas_a_eliminar <- names(porcentaje_nulos[porcentaje_nulos > 0.9])

# Elimina esas columnas del dataframe
df_cox_tr <- df_cox_tr[, !(names(df_cox_tr) %in% columnas_a_eliminar)]

# ---------------------- MODELO DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------

covariables <- names(df_cox_tr)[5:ncol(df_cox_tr)]

df_cox_tr_fill <- df_cox_tr %>%
  mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

formula_cox <- as.formula(paste("Surv(tiempo_total, FGE) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

modelo_cox_tr_FGE <- coxph(formula_cox, data = df_cox_tr_fill)

# Ver el resumen del modelo
# summary(modelo_cox_tr)

# -------------------- (sobre Fallecido) ---------------------------

formula_cox <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

modelo_cox_tr_Fallecido <- coxph(formula_cox, data = df_cox_tr_fill)

# ---------------------- GRAFICA DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_tr_FGE, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE CON TRANSPLANTE") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-1000, 1000))
print(g)

# -------------------- (sobre Fallecido) ---------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_tr_Fallecido, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre Fallecido CON TRANSPLANTE") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-1000, 1000))
print(g)

# ------------------- HEMODIALISIS Y TRANSPLANTE -------------------
print('------------------- HEMODIALISIS Y TRANSPLANTE -------------------')

df_cox_tr_hm <- df_cox %>%
  filter(Transplante == 1 & Hemodialisis == 1)

# ---------------------- NULOS -----------------------------

# Primero, calcula el porcentaje de valores NA por columna
porcentaje_nulos <- sapply(df_cox_tr_hm, function(x) sum(is.na(x)) / length(x))

# Identifica las columnas con más del 90% de valores NA
columnas_a_eliminar <- names(porcentaje_nulos[porcentaje_nulos > 0.9])

# Elimina esas columnas del dataframe
df_cox_tr_hm <- df_cox_tr_hm[, !(names(df_cox_tr_hm) %in% columnas_a_eliminar)]

# ---------------------- MODELO DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------

covariables <- names(df_cox_tr_hm)[5:ncol(df_cox_tr_hm)]

df_cox_tr_hm_fill <- df_cox_tr_hm %>%
  mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

formula_cox <- as.formula(paste("Surv(tiempo_total, FGE) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

modelo_cox_tr_hm_FGE <- coxph(formula_cox, data = df_cox_tr_hm_fill)

# Ver el resumen del modelo
# summary(modelo_cox_tr_hm)

# -------------------- (sobre Fallecido) ---------------------------

formula_cox <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

modelo_cox_tr_hm_Fallecido <- coxph(formula_cox, data = df_cox_tr_hm_fill)

# ---------------------- GRAFICA DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_tr_hm_FGE, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE CON HEMODIALISIS Y TRANSPLANTE") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-1000, 1000))
print(g)

# -------------------- (sobre Fallecido) ---------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_tr_hm_Fallecido, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre Fallecido CON HEMODIALISIS Y TRANSPLANTE") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-1000, 1000))
print(g)

# ------------------- NI HEMODIALISIS NI TRANSPLANTE -------------------
print('------------------- NI HEMODIALISIS NI TRANSPLANTE -------------------')

df_cox_NN <- df_cox %>%
  filter(Transplante == 0 & Hemodialisis == 0)

# ---------------------- NULOS -----------------------------

# Primero, calcula el porcentaje de valores NA por columna
porcentaje_nulos <- sapply(df_cox_NN, function(x) sum(is.na(x)) / length(x))

# Identifica las columnas con más del 90% de valores NA
columnas_a_eliminar <- names(porcentaje_nulos[porcentaje_nulos > 0.9])

# Elimina esas columnas del dataframe
df_cox_NN <- df_cox_NN[, !(names(df_cox_NN) %in% columnas_a_eliminar)]

# ---------------------- MODELO DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------

covariables <- names(df_cox_NN)[5:ncol(df_cox_NN)]

df_cox_NN_fill <- df_cox_NN %>%
  mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

formula_cox <- as.formula(paste("Surv(tiempo_total, FGE) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

modelo_cox_NN_FGE <- coxph(formula_cox, data = df_cox_NN_fill)

# Ver el resumen del modelo
# summary(modelo_cox_NN)

# -------------------- (sobre Fallecido) ---------------------------

formula_cox <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ edad_inicio + ", paste(covariables, collapse = " + ")))

modelo_cox_NN_Fallecido <- coxph(formula_cox, data = df_cox_NN_fill)

# ---------------------- GRAFICA DE COX -----------------------------
# ----------------------- (sobre FGE) ------------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_NN_FGE, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE SIN HEMODIALISIS Y TRANSPLANTE") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# -------------------- (sobre Fallecido) ---------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_NN_Fallecido, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre Fallecido SIN HEMODIALISIS Y TRANSPLANTE") +
  theme_minimal() +
  theme(legend.position = "right")

print(g)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))
print(g)


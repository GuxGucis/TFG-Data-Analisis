print('================================= MODELO DE COX =================================')

library(survival)
library(dplyr)
library(tidyr)
library(survminer)
library(ggplot2)
library(broom)

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
# baseurl <- "D:/Documentos/Universidad/TFG/"

# -------------------------------------------------------------------
# ------------------------ PREPARACIÓN ------------------------------
# -------------------------------------------------------------------
print('------------------- PREPARACIÓN -------------------')

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
df_cox <- df_cox %>%
  relocate("FGE", .after = "edad_inicio")
df_cox <- df_cox %>%
  relocate("Cociente.Album.Creat", .after="FGE")

df_cox <- left_join(df_cox, df_transplante_hemodialisis, by = "ID")

# Se observa que hay pacientes que solo tienen un registro y por tanto no hay valor informativo en esto puesto que no se sabe y ha habido evolución o no
# Se eliminan dichas filas

df_cox <- df_cox %>%
  filter(tiempo_total > 0)

# ------------------- MODELO DE COX GENERAL -------------------
print('------------------- MODELO DE COX GENERAL -------------------')

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
# ---------------------- GRAFICAS DE COX ----------------------------
# ------------------------ (sobre FGE) ------------------------------
# -------------------------------------------------------------------

# ------------------------ HAZARD RATIO -----------------------------

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_ALL.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_ALL_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

# ------------------- CURVAS DE SUPERVIVENCIA -----------------------
# ------------------- (WORKING) -------------------
# # Suponiendo que tienes una variable Estado con 4 niveles
# estados <- unique(df_cox_fill$Estado)
#
# # Crear un nuevo dataframe para las predicciones
# # En este caso, replicamos cada estado tantas veces como sea necesario para las predicciones
# newdata <- data.frame(Estado = rep(unique(df_cox$Estado), each = 1))
#
# # Calcular las curvas de supervivencia ajustadas por Estado
# surv_ajustado <- survfit(modelo_cox_FGE, newdata = newdata)
#
# # Gráfico de las curvas ajustadas por Estado
# g <- ggsurvplot(surv_ajustado, data = newdata,
#                 xlab = "Tiempo",
#                 ylab = "Probabilidad de Supervivencia",
#                 title = "Curvas de Supervivencia Ajustadas por Estado",
#                 ggtheme = theme_minimal() +
#                   theme(plot.background = element_rect(fill = "white", colour = "black"),
#                         panel.background = element_rect(fill = "white", colour = "black"),
#                         legend.background = element_rect(fill = "white", colour = "black"))
# )

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_ALL.png"), plot = g, width = 14, height = 10, dpi = 300)

g <- g + ylim(c(-3, 3))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_ALL_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_HM.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))
print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_HM_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_HM.png"), plot = g, width = 14, height = 10, dpi = 300)

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_TR.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-1000, 1000))
print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_TR_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_TR.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-1000, 1000))
print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_TR_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_HMTR.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-1000, 1000))
print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_HMTR_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_HMTR.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-1000, 1000))
print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_HMTR_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FGE_NN.png"), plot = g, width = 14, height = 10, dpi = 300)

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
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_NN.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))
print(g)
ggsave(paste0(baseurl, "Graficas/Cox1/COX_FLL_NN_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

print('================================= FIN MODELO DE COX =================================')

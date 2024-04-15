print('================================= MODELO DE COX 2 =================================')

library(survival)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
# baseurl <- "D:/Documentos/Universidad/TFG/"

# ------------------- Rellenado de Datos con MICE -------------------
print('------------------- Rellenado de Datos con MICE -------------------')
# computacionalmente imposible por ahora pero no estaria de mas la verdad

# Aplica mice con el método 'cart'
# mice_analitics <- mice(ANALITIC, m=3, method='cart', seed=123)

# # CONFIGURACIÓN PARA PODER USAR VARIOS HILOS
# # Detectar el número de núcleos lógicos
# num_cores <- detectCores(logical = TRUE)
#
# # Crear un clúster con un núcleo menos que el total para dejar recursos para el sistema
# cl <- makeCluster(num_cores - 2)
#
# # Usar el clúster para paralelizar mice
# df_cox_mice <- mice(df_cox, m=5, method='cart', seed=123, maxit=5, parallel = "snow", cluster=cl)
#
# # Detener el clúster una vez completada la imputación
# stopCluster(cl)
#
# # Selecciona un conjunto imputado
# df_cox_1 <- complete(df_cox_mice, 1)
# df_cox_2 <- complete(df_cox_mice, 2)
# df_cox_3 <- complete(df_cox_mice, 3)
# df_cox_4 <- complete(df_cox_mice, 4)
# df_cox_5 <- complete(df_cox_mice, 5)
#
# write.csv(df_cox_1, paste0(baseurl, "Mice/tend_mice_1.csv"), row.names = FALSE)
# write.csv(df_cox_2, paste0(baseurl, "Mice/tend_mice_2.csv"), row.names = FALSE)
# write.csv(df_cox_3, paste0(baseurl, "Mice/tend_mice_3.csv"), row.names = FALSE)
# write.csv(df_cox_4, paste0(baseurl, "Mice/tend_mice_4.csv"), row.names = FALSE)
# write.csv(df_cox_5, paste0(baseurl, "Mice/tend_mice_5.csv"), row.names = FALSE)

print('================================= MODELO DE COX =================================')

# -------------------------------------------------------------------
# ------------------------ PREPARACIÓN ------------------------------
# -------------------------------------------------------------------
print('------------------- PREPARACIÓN -------------------')

df_cox_1 <- read.csv(paste0(baseurl, "Mice/tend_mice_1.csv"), sep = ",", header = TRUE)
df_cox_2 <- read.csv(paste0(baseurl, "Mice/tend_mice_2.csv"), sep = ",", header = TRUE)
df_cox_3 <- read.csv(paste0(baseurl, "Mice/tend_mice_3.csv"), sep = ",", header = TRUE)
df_cox_4 <- read.csv(paste0(baseurl, "Mice/tend_mice_4.csv"), sep = ",", header = TRUE)
df_cox_5 <- read.csv(paste0(baseurl, "Mice/tend_mice_5.csv"), sep = ",", header = TRUE)

mice <- list(1, 2, 3, 4, 5)

for (i in mice){

  # Construir nombres dinámicamente
  df_nombre <- paste0("df_cox_", i)

  df_cox <- get(df_nombre)

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
    mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

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
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_ALL_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-3, 3))

  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_ALL_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_ALL_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  g <- g + ylim(c(-3, 3))

  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_ALL_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
    mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_HM_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-3, 3))
  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_HM_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_HM_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
    mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_TR_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-1000, 1000))
  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_TR_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_TR_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-1000, 1000))
  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_TR_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
    mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_HMTR_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-1000, 1000))
  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_HMTR_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_HMTR_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-1000, 1000))
  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_HMTR_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
    mutate(across(.cols = c(edad_inicio, all_of(covariables)), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FGE_NN_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

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
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_NN_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-3, 3))
  print(g)
  ggsave(paste0(baseurl, "Graficas/Cox2/COX_FLL_NN_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  dev.off()
}
print('================================= FIN MODELO DE COX 2 =================================')
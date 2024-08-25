print('================================= MODELO DE COX NEW 2 =================================')

library(survival)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)
library(survminer)
library(broom)
library(parallel)

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
# baseurl <- "D:/Documentos/Universidad/TFG/"

df_cox <- read.csv(paste0(baseurl, "data/ANALITIC_2.csv"), sep = ",", header = TRUE)

# ------------------- Rellenado de Datos con MICE -------------------
print('------------------- Rellenado de Datos con MICE -------------------')
# computacionalmente imposible por ahora pero no estaria de mas la verdad para ANALITIC como tal
# ANALITIC <- read.csv(paste0(baseurl, "data/ANALITIC_mi.csv"), sep = ",", header = TRUE)
# ANALITIC$FFECCITA <- as.Date(ANALITIC$FFECCITA)
# ANALITIC$fechatoma <- as.Date(ANALITIC$fechatoma)
#
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
# df_cox_mice <- mice(df_cox, m=7, method='cart', seed=123, parallel = "snow", maxit=7, cluster=cl)
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
# df_cox_6 <- complete(df_cox_mice, 6)
# df_cox_7 <- complete(df_cox_mice, 7)
#
# write.csv(df_cox_1, paste0(baseurl, "Mice/tend_mice_1.csv"), row.names = FALSE)
# write.csv(df_cox_2, paste0(baseurl, "Mice/tend_mice_2.csv"), row.names = FALSE)
# write.csv(df_cox_3, paste0(baseurl, "Mice/tend_mice_3.csv"), row.names = FALSE)
# write.csv(df_cox_4, paste0(baseurl, "Mice/tend_mice_4.csv"), row.names = FALSE)
# write.csv(df_cox_5, paste0(baseurl, "Mice/tend_mice_5.csv"), row.names = FALSE)
# write.csv(df_cox_6, paste0(baseurl, "Mice/tend_mice_6.csv"), row.names = FALSE)
# write.csv(df_cox_7, paste0(baseurl, "Mice/tend_mice_7.csv"), row.names = FALSE)

print('================================= MODELO DE COX =================================')

# -------------------------------------------------------------------
# ------------------------ PREPARACIÓN ------------------------------
# -------------------------------------------------------------------
print('------------------- PREPARACIÓN -------------------')

ANALITIC_1 <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_1.csv"), sep = ",", header = TRUE)
ANALITIC_2 <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_2.csv"), sep = ",", header = TRUE)
ANALITIC_3 <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_3.csv"), sep = ",", header = TRUE)
ANALITIC_4 <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_4.csv"), sep = ",", header = TRUE)
ANALITIC_5 <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_5.csv"), sep = ",", header = TRUE)
ANALITIC_6 <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_6.csv"), sep = ",", header = TRUE)
ANALITIC_7 <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_7.csv"), sep = ",", header = TRUE)

# Variable para poder ver como se lleva la significancia en los modelos y así descartar modelos
significancia <- data.frame(variable = names(ANALITIC_1)[8:ncol(ANALITIC_1)])
significancia$count_all_FGE <- 0
significancia$count_all_FLL <- 0
significancia$count_hm_FGE <- 0
significancia$count_hm_FLL <- 0
significancia$count_NN_FGE <- 0
significancia$count_NN_FLL <- 0

# Para el FGE calculado con EKFC
significancia2 <- data.frame(variable = names(ANALITIC_1)[8:ncol(ANALITIC_1)])
significancia2$count_all_FGE <- 0
significancia2$count_hm_FGE <- 0
significancia2$count_NN_FGE <- 0

mice <- list(1, 2, 3, 4, 5, 6, 7)

for (i in mice){

  # Construir nombres dinámicamente
  df_nombre <- paste0("ANALITIC_", i)

  ANALITIC <- get(df_nombre)
  # ------------------- MODELO DE COX GENERAL -------------------
  print('------------------- MODELO DE COX GENERAL -------------------')

  ANALITIC$fechatoma <- as.Date(ANALITIC$fechatoma)

  ANALITIC <- ANALITIC %>%
    arrange(ID, fechatoma) %>%
    group_by(ID) %>%
    mutate(tiempo_total = as.integer(last(fechatoma) - first(fechatoma))) %>% # Días totales
    mutate(edad_inicio = first(Edad)) %>% # Usar la primera Edad registrada por ID
    mutate(dias_transcurridos = round(as.numeric(difftime(fechatoma, lag(fechatoma, default = first(fechatoma)), units = "days")))) %>%
    mutate(FGE_microten = as.integer(lag(FGE, default = first(FGE)) > FGE)) %>% # 1 si el FGE desciende y 0 si asciende o se mantiene igual
    mutate(FGE2_microten = as.integer(lag(FGE2, default = first(FGE2)) > FGE2)) %>% # 1 si el FGE desciende y 0 si asciende o se mantiene igual
    ungroup()

  ANALITIC <- ANALITIC %>%
    relocate("FGE_microten", .after = "FGE")
  ANALITIC <- ANALITIC %>%
    relocate("FGE2_microten", .after = "FGE2")
  ANALITIC <- ANALITIC %>%
    relocate("edad_inicio", .after = "Edad")
  ANALITIC <- ANALITIC %>%
    relocate("dias_transcurridos", .after = "fechatoma")
  ANALITIC <- ANALITIC %>%
    relocate("tiempo_total", .after = "dias_transcurridos")

  # Columna Estado para crear los grupos de tratamientos importantes que han tenido
  ANALITIC <- ANALITIC %>%
    mutate(Estado = case_when(
      Hemodialisis == 1 & Transplante == 0 ~ "hemodialisis",
      Hemodialisis == 0 & Transplante == 1 ~ "transplante",
      Hemodialisis == 1 & Transplante == 1 ~ "ambas",
      Hemodialisis == 0 & Transplante == 0 ~ "nada"
    ))

  # Columnas no necesarias
  ANALITIC <- ANALITIC %>%
    select(-c(Hemodialisis, Transplante))

  # Se observa que hay pacientes que solo tienen un registro y por tanto no hay valor informativo en esto puesto que no se sabe y ha habido evolución o no
  # Se eliminan dichas filas
  ANALITIC <- ANALITIC %>%
    filter(tiempo_total > 0)

  ANALITIC <- ANALITIC %>%
    relocate("Estado", .after = "FGE_microten")

  # ------------------------------------------------------------------
  # ---------------------- MODELO DE COX -----------------------------
  # ----------------------- (sobre FGE) ------------------------------
  # ------------------------------------------------------------------

  # Preparar la fórmula del modelo de Cox incluyendo todas las columnas desde la 6ª en adelante como covariables
  covariables <- names(ANALITIC)[11:ncol(ANALITIC)] # Asume que las columnas de interés empiezan en la 6ª posición

  ANALITIC$Estado <- as.factor(ANALITIC$Estado)
  formula_cox_FGE <- as.formula(paste("Surv(dias_transcurridos, FGE_microten) ~ strata(Estado) + ", paste(covariables, collapse = " + ")))
  formula_cox_FLL <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ strata(Estado) + ", paste(covariables, collapse = " + ")))
  formula_cox_FGE2 <- as.formula(paste("Surv(dias_transcurridos, FGE2_microten) ~ strata(Estado) + ", paste(covariables, collapse = " + ")))

  # Ajustar el modelo de Cox
  modelo_cox_FGE <- coxph(formula_cox_FGE, data = ANALITIC)

  # Ver el resumen del modelo
  # summary(modelo_cox)

  # ------------------------------------------------------------------
  # ---------------------- MODELO DE COX -----------------------------
  # -------------------- (sobre Fallecido) ---------------------------
  # ------------------------------------------------------------------

  # Ajustar el modelo de Cox
  modelo_cox_Fallecido <- coxph(formula_cox_FLL, data = ANALITIC)

  # ------------------------------------------------------------------
  # ---------------------- MODELO DE COX -----------------------------
  # --------------------- (sobre FGE EKFC) ---------------------------
  # ------------------------------------------------------------------

  # Ajustar el modelo de Cox
  modelo_cox_FGE2 <- coxph(formula_cox_FGE2, data = ANALITIC)

  # -------------------------------------------------------------------
  # ---------------------- GRAFICAS DE COX ----------------------------
  # ------------------------ (sobre FGE) ------------------------------
  # -------------------------------------------------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_FGE, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia$count_all_FGE <- significancia$count_all_FGE + (significancia$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE con TODOS LOS PACIENTES") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE_ALL_harz_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-3, 3))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE_ALL_harz_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # ------------------- CURVAS DE SUPERVIVENCIA -----------------------

  # Calcular las curvas de supervivencia ajustadas por Estado
  surv_ajustado_FGE <- survfit(modelo_cox_FGE)

  # P-VALOR
  logrank_test <- survdiff(Surv(dias_transcurridos, FGE_microten) ~ Estado, data = ANALITIC)
  p_valor_logrank <- pchisq(logrank_test$chisq, length(logrank_test$n) - 1, lower.tail = FALSE)
  print(paste0('P-Valor sobre FGE: ', p_valor_logrank))

  # Gráfico de las curvas ajustadas por Estado
  g <- ggsurvplot(surv_ajustado_FGE, data = ANALITIC,
                  pval = FALSE, conf.int = TRUE,
                  xlab = "Tiempo",
                  ylab = "Probabilidad de Supervivencia",
                  title = "Curvas de Supervivencia Ajustadas por Estado sobre FGE",
                  ggtheme = theme_minimal() +
                    theme(plot.background = element_rect(fill = "white", colour = "black"),
                          panel.background = element_rect(fill = "white", colour = "black"),
                          legend.background = element_rect(fill = "white", colour = "black"))
  )

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE_ALL_curv_", i, ".png"), plot = g$plot, width = 18, height = 9, dpi = 300)

  # -------------------------------------------------------------------
  # ---------------------- GRAFICA DE COX -----------------------------
  # --------------------- (sobre Fallecido) ---------------------------
  # -------------------------------------------------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_Fallecido, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia$count_all_FLL <- significancia$count_all_FLL + (significancia$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre Fallecimiento con TODOS LOS PACIENTES") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FLL_ALL_harz_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  g <- g + ylim(c(-3, 3))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FLL_ALL_harz_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # ------------------- CURVAS DE SUPERVIVENCIA -----------------------

  # Calcular las curvas de supervivencia ajustadas por Estado
  surv_ajustado_FLL <- survfit(modelo_cox_Fallecido)

  # P-VALOR
  logrank_test <- survdiff(Surv(tiempo_total, Fallecido) ~ Estado, data = ANALITIC)
  p_valor_logrank <- pchisq(logrank_test$chisq, length(logrank_test$n) - 1, lower.tail = FALSE)
  print(paste0('P-Valor sobre Fallecido: ', p_valor_logrank))

  # Gráfico de las curvas ajustadas por Estado
  g <- ggsurvplot(surv_ajustado_FLL, data = ANALITIC,
                  strata = "Estado",
                  pval = FALSE, conf.int = TRUE,
                  xlab = "Tiempo",
                  ylab = "Probabilidad de Supervivencia",
                  title = "Curvas de Supervivencia Ajustadas por Estado por Fallecimiento",
                  ggtheme = theme_minimal() +
                    theme(plot.background = element_rect(fill = "white", colour = "black"),
                          panel.background = element_rect(fill = "white", colour = "black"),
                          legend.background = element_rect(fill = "white", colour = "black"))
  )

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FLL_ALL_curv_", i, ".png"), plot = g$plot, width = 18, height = 9, dpi = 300)

  # -------------------------------------------------------------------
  # ---------------------- GRAFICAS DE COX ----------------------------
  # ---------------------- (sobre FGE EKFC) ---------------------------
  # -------------------------------------------------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_FGE2, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia2$count_all_FGE <- significancia2$count_all_FGE + (significancia2$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE (EKFC) con TODOS LOS PACIENTES") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE2_ALL_harz", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-3, 3))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE2_ALL_harz_Sca", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # ------------------- CURVAS DE SUPERVIVENCIA -----------------------

  # Calcular las curvas de supervivencia ajustadas por Estado
  surv_ajustado_FGE2 <- survfit(modelo_cox_FGE2)

  # P-VALOR
  logrank_test <- survdiff(Surv(dias_transcurridos, FGE2_microten) ~ Estado, data = ANALITIC)
  p_valor_logrank <- pchisq(logrank_test$chisq, length(logrank_test$n) - 1, lower.tail = FALSE)
  print(paste0('P-Valor sobre FGE: ', p_valor_logrank))

  # Gráfico de las curvas ajustadas por Estado
  g <- ggsurvplot(surv_ajustado_FGE2, data = ANALITIC,
                  pval = FALSE, conf.int = TRUE,
                  xlab = "Tiempo",
                  ylab = "Probabilidad de Supervivencia",
                  title = "Curvas de Supervivencia Ajustadas por Estado sobre FGE (EKFC)",
                  ggtheme = theme_minimal() +
                    theme(plot.background = element_rect(fill = "white", colour = "black"),
                          panel.background = element_rect(fill = "white", colour = "black"),
                          legend.background = element_rect(fill = "white", colour = "black"))
  )

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE2_ALL_curv", i, ".png"), plot = g$plot, width = 18, height = 9, dpi = 300)

  # =============== DIVIDIMOS DATAFRAMES =====================
  # ------------------- EN HEMODIALISIS -------------------
  print('------------------- EN HEMODIALISIS -------------------')

  ANALITIC_hm <- ANALITIC %>%
    filter(Estado == 'hemodialisis')

  # ---------------------- MODELO DE COX -----------------------------
  # ----------------------- (sobre FGE) ------------------------------

  covariables <- names(ANALITIC_hm)[11:ncol(ANALITIC_hm)]

  modelo_cox_hm_FGE <- coxph(formula_cox_FGE, data = ANALITIC_hm)

  # Ver el resumen del modelo
  # summary(modelo_cox_hm)

  # -------------------- (sobre Fallecido) ---------------------------

  modelo_cox_hm_Fallecido <- coxph(formula_cox_FLL, data = ANALITIC_hm)

  # --------------------- (sobre FGE EKFC) ---------------------------

  modelo_cox_hm_FGE2 <- coxph(formula_cox_FGE2, data = ANALITIC_hm)

  # ---------------------- GRAFICA DE COX -----------------------------
  # ----------------------- (sobre FGE) ------------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_hm_FGE, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia$count_hm_FGE <- significancia$count_hm_FGE + (significancia$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE CON HEMODIALISIS") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE_HM_harz_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-3, 3))
  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE_HM_harz_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # -------------------- (sobre Fallecido) ---------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_hm_Fallecido, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia$count_hm_FLL <- significancia$count_hm_FLL + (significancia$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre Fallecido CON HEMODIALISIS") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FLL_HM_harz_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # --------------------- (sobre FGE EKFC) ---------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_hm_FGE2, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia2$count_all_FGE <- significancia2$count_all_FGE + (significancia2$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE (EKFC) CON HEMODIALISIS") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE2_HM_harz", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-3, 3))
  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE2_HM_harz_Sca", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # ------------------- NI HEMODIALISIS NI TRANSPLANTE -------------------
  print('------------------- NI HEMODIALISIS NI TRANSPLANTE -------------------')

  ANALITIC_NN <- ANALITIC %>%
    filter(Estado == 'nada')

  # ---------------------- MODELO DE COX -----------------------------
  # ----------------------- (sobre FGE) ------------------------------

  covariables <- names(ANALITIC_NN)[11:ncol(ANALITIC_NN)]

  modelo_cox_NN_FGE <- coxph(formula_cox_FGE, data = ANALITIC_NN)

  # Ver el resumen del modelo
  # summary(modelo_cox_NN)

  # -------------------- (sobre Fallecido) ---------------------------

  modelo_cox_NN_Fallecido <- coxph(formula_cox_FLL, data = ANALITIC_NN)

  # --------------------- (sobre FGE EKFC) ---------------------------

  modelo_cox_NN_FGE2 <- coxph(formula_cox_FGE2, data = ANALITIC_NN)

  # ---------------------- GRAFICA DE COX -----------------------------
  # ----------------------- (sobre FGE) -------------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_NN_FGE, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia$count_NN_FGE <- significancia$count_NN_FGE + (significancia$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE SIN HEMODIALISIS Y TRANSPLANTE") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE_NN_harz_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # -------------------- (sobre Fallecido) ---------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_NN_Fallecido, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia$count_NN_FLL <- significancia$count_NN_FLL + (significancia$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre Fallecido SIN HEMODIALISIS Y TRANSPLANTE") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FLL_NN_harz_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
  g <- g + ylim(c(-3, 3))
  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FLL_NN_harz_Sca_", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  # --------------------- (sobre FGE EKFC) ---------------------------

  # ------------------------ HAZARD RATIO -----------------------------

  # Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
  coeficientes_cox <- broom::tidy(modelo_cox_NN_FGE2, conf.int = TRUE, conf.level = 0.95)

  # Añadir una nueva columna al dataframe para la significancia basada en el p-valor
  coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

  # Extraer variables no significativas basadas en broom::tidy
  no_sig_vars <- coeficientes_cox$term[coeficientes_cox$significancia == "No significativo"]
  # Acumular conteo de no significancia en el registro de seguimiento
  significancia2$count_all_FGE <- significancia2$count_all_FGE + (significancia2$variable %in% no_sig_vars)

  # Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
  g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
    geom_point(aes(color = significancia), size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
    scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
    coord_flip() +
    labs(x = "Covariables", y = "Hazard Ratio (log(HR))", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE (EKFC) SIN HEMODIALISIS Y TRANSPLANTE") +
    theme_minimal() +
    theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

  print(g)
  ggsave(paste0(baseurl, "Graficas/CoxTest2/COX_FGE2_NN_harz", i, ".png"), plot = g, width = 14, height = 10, dpi = 300)

  dev.off()
}

# ------------------- EXPORTAR -------------------
print('------------------- EXPORTAR -------------------')
write.csv(significancia, paste0(baseurl, "data/significancia_ANALITICS_clean.csv"), row.names = FALSE)
write.csv(significancia2, paste0(baseurl, "data/significancia2_ANALITICS_clean.csv"), row.names = FALSE)

# ------------------- Significancia COX -------------------
print('------------------- Significancia COX -------------------')

significancia <- read.csv(paste0(baseurl, "data/significancia_ANALITICS_clean.csv"), sep = ",", header = TRUE)
# Sumar total de no significancia por modelo
modelo_frecuencias <- colSums(significancia[, -c(1, ncol(significancia))])

# Gráfico de barras
bar_data <- data.frame(Modelo = names(modelo_frecuencias), Total = modelo_frecuencias)
g <- ggplot(bar_data, aes(x = reorder(Modelo, Total), y = Total, fill = Total)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Modelo", y = "Total de No Significativa", title = "Frecuencia de la No Significancia por Modelo") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest2/Freq_Colum_Significancia.png"), plot = g, width = 10, height = 12, dpi = 300)

# POR FILA
significancia <- read.csv(paste0(baseurl, "data/significancia_ANALITICS_clean.csv"), sep = ",", header = TRUE)
# Sumar todas las columnas de conteo para cada variable
significancia$total_no_significativa <- rowSums(significancia[, -1])

# Ordenar para visualización
significancia <- significancia %>%
  arrange(desc(total_no_significativa))

# Gráfico de barras
g <- ggplot(significancia, aes(x = reorder(variable, total_no_significativa), y = total_no_significativa, fill = total_no_significativa)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Variable", y = "Total de No Significativa", title = "Frecuencia Total de la No Significancia por Variable") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest2/Freq_Fila_Significancia.png"), plot = g, width = 10, height = 12, dpi = 300)

# EN HEATMAP
significancia <- read.csv(paste0(baseurl, "data/significancia_ANALITICS_clean.csv"), sep = ",", header = TRUE)
# Convertir los datos a formato largo
long_data <- pivot_longer(significancia,
                          cols = -variable,
                          names_to = 'modelo',
                          values_to = 'conteo')

# Crear el heatmap
g <- ggplot(long_data, aes(x = modelo, y = variable, fill = conteo)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 5),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = "black")
  ) +
  labs(
    title = 'Heatmap de la No Significancia por Modelo',
    x = 'Modelo',
    y = 'Variable',
    fill = 'Conteo de No Significancia'
  )

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest2/HeatMap_Significancia.png"), plot = g, width = 10, height = 12, dpi = 300)

# DESVIACIÓN ESTANDAR
significancia <- read.csv(paste0(baseurl, "data/significancia_ANALITICS_clean.csv"), sep = ",", header = TRUE)
# Calcular desviación estándar
varianzas <- apply(significancia[, -c(1, ncol(significancia))], 2, sd)

# Gráfico de barras
var_data <- data.frame(Modelo = names(varianzas), SD = varianzas)
g <- ggplot(var_data, aes(x = reorder(Modelo, SD), y = SD, fill = SD)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Modelo", y = "Desviacion Estandar", title = "Desviacion Estandar de la No Significativa por Modelo") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest2/Desviacion_Estandar_Significancia.png"), plot = g, width = 10, height = 12, dpi = 300)

# ------------------- Significancia COX (FGE EKFC)-------------------
print('------------------- Significancia COX (FGE EKFC)-------------------')

significancia <- read.csv(paste0(baseurl, "data/significancia2_ANALITICS_clean.csv"), sep = ",", header = TRUE)
# Sumar total de no significancia por modelo
modelo_frecuencias <- colSums(significancia[, -c(1, ncol(significancia))])

# Gráfico de barras
bar_data <- data.frame(Modelo = names(modelo_frecuencias), Total = modelo_frecuencias)
g <- ggplot(bar_data, aes(x = reorder(Modelo, Total), y = Total, fill = Total)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Modelo", y = "Total de No Significativa", title = "Frecuencia de la No Significancia por Modelo (FGE EKFC)") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest2/Freq_Colum_Significancia2.png"), plot = g, width = 10, height = 12, dpi = 300)

# POR FILA
significancia <- read.csv(paste0(baseurl, "data/significancia2_ANALITICS_clean.csv"), sep = ",", header = TRUE)
# Sumar todas las columnas de conteo para cada variable
significancia$total_no_significativa <- rowSums(significancia[, -1])

# Ordenar para visualización
significancia <- significancia %>%
  arrange(desc(total_no_significativa))

# Gráfico de barras
g <- ggplot(significancia, aes(x = reorder(variable, total_no_significativa), y = total_no_significativa, fill = total_no_significativa)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Variable", y = "Total de No Significativa", title = "Frecuencia Total de la No Significancia por Variable (FGE EKFC)") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest2/Freq_Fila_Significancia2.png"), plot = g, width = 10, height = 12, dpi = 300)

# EN HEATMAP
significancia <- read.csv(paste0(baseurl, "data/significancia2_ANALITICS_clean.csv"), sep = ",", header = TRUE)
# Convertir los datos a formato largo
long_data <- pivot_longer(significancia,
                          cols = -variable,
                          names_to = 'modelo',
                          values_to = 'conteo')

# Crear el heatmap
g <- ggplot(long_data, aes(x = modelo, y = variable, fill = conteo)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 5),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = "black")
  ) +
  labs(
    title = 'Heatmap de la No Significancia por Modelo (FGE EKFC)',
    x = 'Modelo',
    y = 'Variable',
    fill = 'Conteo de No Significancia'
  )

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest2/HeatMap_Significancia2.png"), plot = g, width = 10, height = 12, dpi = 300)

# DESVIACIÓN ESTANDAR
significancia <- read.csv(paste0(baseurl, "data/significancia2_ANALITICS_clean.csv"), sep = ",", header = TRUE)
# Calcular desviación estándar
varianzas <- apply(significancia[, -c(1, ncol(significancia))], 2, sd)

# Gráfico de barras
var_data <- data.frame(Modelo = names(varianzas), SD = varianzas)
g <- ggplot(var_data, aes(x = reorder(Modelo, SD), y = SD, fill = SD)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Modelo", y = "Desviacion Estandar", title = "Desviacion Estandar de la No Significativa por Modelo (FGE EKFC)") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest2/Desviacion_Estandar_Significancia2.png"), plot = g, width = 10, height = 12, dpi = 300)
print('================================= FIN MODELO DE COX NEW 2 =================================')
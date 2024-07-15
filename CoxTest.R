print('================================= MODELO DE COX NEW =================================')

library(survival)
library(dplyr)
library(tidyr)
library(survminer)
library(ggplot2)
library(broom)
library(tidyverse)
library(lme4)
library(readxl)
library(stringr)
library(parallel)

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

# ------------- DATOS -------------

ANALITIC <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_1.csv"), sep = ",", header = TRUE)
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
# ------------------- MODELO DE COX GENERAL -------------------
print('------------------- MODELO DE COX GENERAL -------------------')

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE_ALL_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE_ALL_harz_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE_ALL_curv.png"), plot = g$plot, width = 18, height = 9, dpi = 300)

# -------------------------------------------------------------------
# ---------------------- GRAFICA DE COX -----------------------------
# --------------------- (sobre Fallecido) ---------------------------
# -------------------------------------------------------------------

# ------------------------ HAZARD RATIO -----------------------------

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FLL_ALL_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

g <- g + ylim(c(-3, 3))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FLL_ALL_harz_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FLL_ALL_curv.png"), plot = g$plot, width = 18, height = 9, dpi = 300)

# -------------------------------------------------------------------
# ---------------------- GRAFICAS DE COX ----------------------------
# ---------------------- (sobre FGE EKFC) ---------------------------
# -------------------------------------------------------------------

# ------------------------ HAZARD RATIO -----------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_FGE2, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE (EKFC) con TODOS LOS PACIENTES") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE2_ALL_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE2_ALL_harz_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE2_ALL_curv.png"), plot = g$plot, width = 18, height = 9, dpi = 300)

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE_HM_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))
print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE_HM_harz_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

# -------------------- (sobre Fallecido) ---------------------------

# ------------------------ HAZARD RATIO -----------------------------

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FLL_HM_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

# --------------------- (sobre FGE EKFC) ---------------------------

# ------------------------ HAZARD RATIO -----------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_hm_FGE2, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE (EKFC) CON HEMODIALISIS") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE2_HM_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))
print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE2_HM_harz_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE_NN_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

# -------------------- (sobre Fallecido) ---------------------------

# ------------------------ HAZARD RATIO -----------------------------

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
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FLL_NN_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

# Controlamos los límites del radio para que se aprencien los de menor radio pero que tienen menos incertidumbre
g <- g + ylim(c(-3, 3))
print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FLL_NN_harz_Sca.png"), plot = g, width = 14, height = 10, dpi = 300)

# --------------------- (sobre FGE EKFC) ---------------------------

# ------------------------ HAZARD RATIO -----------------------------

# Convertir el resumen del modelo de Cox en un dataframe incluyendo los intervalos de confianza
coeficientes_cox <- broom::tidy(modelo_cox_NN_FGE2, conf.int = TRUE, conf.level = 0.95)

# Añadir una nueva columna al dataframe para la significancia basada en el p-valor
coeficientes_cox$significancia <- ifelse(coeficientes_cox$p.value < 0.05, "Significativo", "No significativo")

# Crear el gráfico de hazard ratios con ggplot2, diferenciando por significancia
g <- ggplot(coeficientes_cox, aes(x = term, y = estimate)) +
  geom_point(aes(color = significancia), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significancia), width = 0.2) +
  scale_color_manual(values = c("Significativo" = "blue", "No significativo" = "red")) +
  coord_flip() +
  labs(x = "Covariables", y = "Hazard Ratio", title = "Efecto de las Covariables en el Riesgo Relativo sobre FGE (EKFC) SIN HEMODIALISIS Y TRANSPLANTE") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/CoxTest/COX_FGE2_NN_harz.png"), plot = g, width = 14, height = 10, dpi = 300)

# ------------------- EXPORTAR -------------------
print('------------------- EXPORTAR -------------------')

print('================================= FIN MODELO DE COX NEW =================================')

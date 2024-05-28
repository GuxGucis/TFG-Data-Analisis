print('================================= RISK =================================')

library(cmprsk)

library(survival)
library(dplyr)
library(tidyr)
library(survminer)
library(ggplot2)
library(viridis)
library(reshape2)
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

# ------------- DATOS -------------

ANALITIC <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_1.csv"), sep = ",", header = TRUE)

# ------------------- PREPARACIÓN -------------
print('------------------- PREPARACIÓN -------------------')

ANALITIC <- ANALITIC %>%
  mutate(Estado = case_when(
    Hemodialisis == 1 & Transplante == 0 ~ "hemodialisis",
    Hemodialisis == 0 & Transplante == 1 ~ "transplante", #No deberia haber de estas
    Hemodialisis == 1 & Transplante == 1 ~ "ambas", #No deberia haber de estas
    Hemodialisis == 0 & Transplante == 0 ~ "nada"
  ))
ANALITIC <- ANALITIC %>%
  select(-c(Hemodialisis, Transplante))
ANALITIC <- ANALITIC %>%
  relocate("Estado", .after = "FGE")
ANALITIC$fechatoma <- as.Date(ANALITIC$fechatoma)

ANALITIC <- ANALITIC %>%
  arrange(ID, fechatoma) %>%
  group_by(ID) %>%
  mutate(dias_transcurridos = round(as.numeric(difftime(fechatoma, lag(fechatoma, default = first(fechatoma)), units = "days")))) %>%
  ungroup()

ANALITIC <- ANALITIC %>%
  arrange(ID, fechatoma) %>%
  group_by(ID) %>%
  mutate(dias_acumulados = fechatoma - first(fechatoma)) %>%
  ungroup()

ANALITIC <- ANALITIC %>%
  arrange(ID, fechatoma) %>%
  group_by(ID) %>%
  mutate(tiempo_total = as.integer(last(fechatoma) - first(fechatoma))) %>%
  ungroup()

ANALITIC <- ANALITIC %>%
  relocate("dias_transcurridos", .after = "fechatoma")
ANALITIC <- ANALITIC %>%
  relocate("dias_acumulados", .after = "dias_transcurridos")
ANALITIC <- ANALITIC %>%
  relocate("tiempo_total", .after = "dias_acumulados")
ANALITIC <- ANALITIC %>%
  relocate("Edad", .after = "tiempo_total")

ANALITIC <- ANALITIC %>%
  arrange(ID, fechatoma) %>%
  group_by(ID) %>%
  mutate(FGE_microten = as.integer(lag(FGE, default = first(FGE)) > FGE)) # 1 si el FGE desciende y 0 si asciende o se mantiene igual
ANALITIC <- ANALITIC %>%
  relocate("FGE_microten", .after = "FGE")

# ------------------- JOINT MODEL ALL -------------------
print('------------------- JOINT MODEL ALL-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

covariables <- names(ANALITIC)[10:ncol(ANALITIC)]  # Select the appropriate columns as covariates
covariate_formula <- as.formula(paste("~", paste(covariables, collapse = " + ")))  # Create a formula

# Now generate the model matrix
covariate_matrix <- model.matrix(~ covariables - 1, data = ANALITIC)  # -1 to omit intercept

crrFit <- crr(ftime = ANALITIC$dias_acumulados, fstatus = ANALITIC$FGE_microten, cov1 = covariate_matrix)
summary(cumincFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/RK/RK_SurvCurv_FGE_ALL.png"), width = 2000, height = 2000)
plot(cuminc_results, xlab = "Time", ylab = "Cumulative Incidence FGE en todos los casos")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

cumincFit <- cuminc(time = ANALITIC$tiempo_total, status = ANALITIC$Fallecido, x = model.matrix(covariates, data = ANALITIC))
summary(cumincFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/RK/RK_SurvCurv_FLL_ALL.png"), width = 2000, height = 2000)
plot(cuminc_results, xlab = "Time", ylab = "Cumulative Incidence Fallecido en todos los casos")
dev.off()

# ------------------- JOINT MODEL HEMODIALISIS -------------------
print('------------------- JOINT MODEL HEMODIALISIS-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

ANALITIC_hm <- ANALITIC %>%
  filter(Estado == 'hemodialisis')

cumincFit <- cuminc(time = ANALITIC_hm$dias_acumulados, status = ANALITIC_hm$FGE_microten, x = model.matrix(covariates, data = ANALITIC_hm))
summary(cumincFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/RK/RK_SurvCurv_FGE_HM.png"), width = 2000, height = 2000)
plot(cuminc_results, xlab = "Time", ylab = "Cumulative Incidence FGE en HEMODIALISIS")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

cumincFit <- cuminc(time = ANALITIC_hm$tiempo_total, status = ANALITIC_hm$Fallecido, x = model.matrix(covariates, data = ANALITIC_hm))
summary(cumincFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/RK/RK_SurvCurv_FLL_HM.png"), width = 2000, height = 2000)
plot(cuminc_results, xlab = "Time", ylab = "Cumulative Incidence Fallecido en HEMODIALISIS")
dev.off()

# ------------------- JOINT MODEL NADA -------------------
print('------------------- JOINT MODEL NADA-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

ANALITIC_nn <- ANALITIC %>%
  filter(Estado == 'nada')

cumincFit <- cuminc(time = ANALITIC_nn$dias_acumulados, status = ANALITIC_nn$FGE_microten, x = model.matrix(covariates, data = ANALITIC_nn))
summary(cumincFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/RK/RK_SurvCurv_FGE_NN.png"), width = 2000, height = 2000)
plot(cuminc_results, xlab = "Time", ylab = "Cumulative Incidence FGE en NADA")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

cumincFit <- cuminc(time = ANALITIC_nn$tiempo_total, status = ANALITIC_nn$Fallecido, x = model.matrix(covariates, data = ANALITIC_nn))
summary(cumincFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/RK/RK_SurvCurv_FLL_NN.png"), width = 2000, height = 2000)
plot(cuminc_results, xlab = "Time", ylab = "Cumulative Incidence Fallecido en NADA")
dev.off()

print('================================= FIN RISK =================================')
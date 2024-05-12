print('================================= MODELOS CONJUNTOS =================================')

install.packages("JM")
library(JM)

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

ANALITIC <- read.csv(paste0(baseurl, "data/ANALITIC_clean.csv"), sep = ",", header = TRUE)

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

# Fit a linear mixed-effects model (for the longitudinal process)
lmeFit <- lme(FGE ~ dias_transcurridos, random = ~ dias_transcurridos | ID, data = ANALITIC)

# Fit a Cox model (for the survival process) incorporating the linear mixed-effects model
covariables <- names(ANALITIC)[10:ncol(ANALITIC)] #(Dos mas por lo dias_acumulado y tiempo total)
formula_FGE <- as.formula(paste("Surv(dias_acumulados, FGE_microten) ~ ", paste(covariables, collapse = " + ")))
coxFit <- coxph(formula_FGE, data = ANALITIC, x = TRUE)

# Fit the joint model
jointFit <- jointModel(lmeFit, coxFit, timeVar = "dias_transcurridos")
summary(jointFit)

# Predict survival using survfitJM
survPred <- survfitJM(jointFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/JM/JM_SurvCurv_FGE_ALL.png"), width = 2000, height = 2000)
plot(survPred, xlab = "Time", ylab = "Survival Probability", main = "Predicted Survival Curve FGE para todos los casos")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Fit a linear mixed-effects model (for the longitudinal process)
lmeFit <- lme(FGE ~ dias_transcurridos, random = ~ dias_transcurridos | ID, data = ANALITIC)

# Fit a Cox model (for the survival process) incorporating the linear mixed-effects model
formula_FLL <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ ", paste(covariables, collapse = " + ")))
coxFit <- coxph(formula_FLL, data = ANALITIC, x = TRUE)

# Fit the joint model
jointFit <- jointModel(lmeFit, coxFit, timeVar = "dias_transcurridos")
summary(jointFit)

# Predict survival using survfitJM
survPred <- survfitJM(jointFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/JM/JM_SurvCurv_FLL_ALL.png"), width = 2000, height = 2000)
plot(survPred, xlab = "Time", ylab = "Survival Probability", main = "Predicted Survival Curve Fallecido para todos los casos")
dev.off()

# ------------------- JOINT MODEL HEMODIALISIS -------------------
print('------------------- JOINT MODEL HEMODIALISIS-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

ANALITIC_hm <- ANALITIC %>%
  filter(Estado == 'hemodialisis')

# Fit a linear mixed-effects model (for the longitudinal process)
lmeFit <- lme(FGE ~ dias_transcurridos, random = ~ dias_transcurridos | ID, data = ANALITIC_hm)

# Fit a Cox model (for the survival process) incorporating the linear mixed-effects model
formula_FGE <- as.formula(paste("Surv(dias_acumulados, FGE_microten) ~ ", paste(covariables, collapse = " + ")))
coxFit <- coxph(formula_FGE, data = ANALITIC_hm, x = TRUE)

# Fit the joint model
jointFit <- jointModel(lmeFit, coxFit, timeVar = "dias_transcurridos")
summary(jointFit)

# Predict survival using survfitJM
survPred <- survfitJM(jointFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/JM/JM_SurvCurv_FGE_HM.png"), width = 2000, height = 2000)
plot(survPred, xlab = "Time", ylab = "Survival Probability", main = "Predicted Survival Curve FGE para Hemosdiálisis")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Fit a linear mixed-effects model (for the longitudinal process)
lmeFit <- lme(Fallecido ~ tiempo_total, random = ~ tiempo_total | ID, data = ANALITIC_hm)

# Fit a Cox model (for the survival process) incorporating the linear mixed-effects model
coxFit <- coxph(formula_FLL, data = ANALITIC_hm, x = TRUE)

# Fit the joint model
jointFit <- jointModel(lmeFit, coxFit, timeVar = "dias_transcurridos")
summary(jointFit)

# Predict survival using survfitJM
survPred <- survfitJM(jointFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/JM/JM_SurvCurv_FLL_HM.png"), width = 2000, height = 2000)
plot(survPred, xlab = "Time", ylab = "Survival Probability", main = "Predicted Survival Curve Fallecido para Hemodiálisis")
dev.off()

# ------------------- JOINT MODEL NADA -------------------
print('------------------- JOINT MODEL NADA-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

ANALITIC_nn <- ANALITIC %>%
  filter(Estado == 'nada')

# Fit a linear mixed-effects model (for the longitudinal process)
lmeFit <- lme(FGE ~ dias_transcurridos, random = ~ dias_transcurridos | ID, data = ANALITIC_nn)

# Fit a Cox model (for the survival process) incorporating the linear mixed-effects model
formula_FGE <- as.formula(paste("Surv(dias_acumulados, FGE_microten) ~ ", paste(covariables, collapse = " + ")))
coxFit <- coxph(formula_FGE, data = ANALITIC_nn, x = TRUE)

# Fit the joint model
jointFit <- jointModel(lmeFit, coxFit, timeVar = "dias_transcurridos")
summary(jointFit)

# Predict survival using survfitJM
survPred <- survfitJM(jointFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/JM/JM_SurvCurv_FGE_NN.png"), width = 2000, height = 2000)
plot(survPred, xlab = "Time", ylab = "Survival Probability", main = "Predicted Survival Curve FGE sin Hemodiálisis ni transplante")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Fit a linear mixed-effects model (for the longitudinal process)
lmeFit <- lme(Fallecido ~ tiempo_total, random = ~ tiempo_total | ID, data = ANALITIC_nn)

# Fit a Cox model (for the survival process) incorporating the linear mixed-effects model
coxFit <- coxph(formula_FLL, data = ANALITIC_nn, x = TRUE)

# Fit the joint model
jointFit <- jointModel(lmeFit, coxFit, timeVar = "dias_transcurridos")
summary(jointFit)

# Predict survival using survfitJM
survPred <- survfitJM(jointFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/JM/JM_SurvCurv_FLL_NN.png"), width = 2000, height = 2000)
plot(survPred, xlab = "Time", ylab = "Survival Probability", main = "Predicted Survival Curve Fallecido sin Hemodiálisis ni transplante")
dev.off()

print('================================= FIN MODELOS CONJUNTOS =================================')
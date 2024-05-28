print('================================= MULTI-ESTADO =================================')

library(msm)

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

covariables <- names(ANALITIC)[10:ncol(ANALITIC)] #(Uno mas por lo dias_acumulado)
covariates <- as.formula(paste("~ ", paste(covariables, collapse = " + ")))

msmFit <- msm(FGE_microten ~ dias_acumulados, subject = ID, data = ANALITIC,
              qmatrix = qmatrix, covariates = covariates)
summary(msmFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/MS/MS_SurvCurv_FGE_ALL.png"), width = 2000, height = 2000)
plot(msmFit)
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

msmFit <- msm(Fallecido ~ tiempo_total, subject = ID, data = ANALITIC,
              qmatrix = qmatrix, covariates = covariates)
summary(msmFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/MS/MS_SurvCurv_FLL_ALL.png"), width = 2000, height = 2000)
plot(msmFit)
dev.off()

# ------------------- JOINT MODEL HEMODIALISIS -------------------
print('------------------- JOINT MODEL HEMODIALISIS-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

ANALITIC_hm <- ANALITIC %>%
  filter(Estado == 'hemodialisis')

msmFit <- msm(FGE_microten ~ dias_acumulados, subject = ID, data = ANALITIC_hm,
              qmatrix = qmatrix, covariates = covariates)
summary(msmFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/MS/MS_SurvCurv_FGE_HM.png"), width = 2000, height = 2000)
plot(msmFit)
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

msmFit <- msm(Fallecido ~ tiempo_total, subject = ID, data = ANALITIC_hm,
              qmatrix = qmatrix, covariates = covariates)
summary(msmFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/MS/MS_SurvCurv_FLL_HM.png"), width = 2000, height = 2000)
plot(msmFit)
dev.off()

# ------------------- JOINT MODEL NADA -------------------
print('------------------- JOINT MODEL NADA-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

ANALITIC_nn <- ANALITIC %>%
  filter(Estado == 'nada')

msmFit <- msm(FGE_microten ~ dias_acumulados, subject = ID, data = ANALITIC_nn,
              qmatrix = qmatrix, covariates = covariates)
summary(msmFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/MS/MS_SurvCurv_FGE_NN.png"), width = 2000, height = 2000)
plot(msmFit)
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

msmFit <- msm(Fallecido ~ tiempo_total, subject = ID, data = ANALITIC_nn,
              qmatrix = qmatrix, covariates = covariates)
summary(msmFit)

# Plot the predicted survival curve
png(paste0(baseurl, "Graficas/MS/MS_SurvCurv_Fll_NN.png"), width = 2000, height = 2000)
plot(msmFit)
dev.off()

print('================================= FIN MULTI-ESTADO =================================')
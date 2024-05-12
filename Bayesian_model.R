print('================================= BAYESIANO =================================')

install.packages("rstanarm")
library(rstanarm)

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

covariables <- names(ANALITIC)[10:ncol(ANALITIC)] #(Dos mas por lo dias_acumulado y tiempo total)
formula_FGE <- as.formula(paste("Surv(dias_acumulados, FGE_microten) ~ ", paste(covariables, collapse = " + ")))

# Fit a Bayesian survival model using rstanarm
bayesFit <- stan_surv(formula = formula_FGE, data = ANALITIC,
                      basehaz = "weibull", chains = 4, iter = 2000)
summary(bayesFit)

# Plot the predicted survival curve
surv_fit <- survfit(weibullFit)
png(paste0(baseurl, "Graficas/BY/BY_SurvCurv_FGE_ALL.png"), width = 2000, height = 2000)
plot(bayesFit)
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

formula_FLL <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ ", paste(covariables, collapse = " + ")))

# Fit a Bayesian survival model using rstanarm
bayesFit <- stan_surv(formula = formula_FLL, data = ANALITIC,
                      basehaz = "weibull", chains = 4, iter = 2000)
summary(bayesFit)

# Plot the predicted survival curve
surv_fit <- survfit(weibullFit)
png(paste0(baseurl, "Graficas/BY/BY_SurvCurv_FLL_ALL.png"), width = 2000, height = 2000)
plot(bayesFit)
dev.off()

# ------------------- JOINT MODEL HEMODIALISIS -------------------
print('------------------- JOINT MODEL HEMODIALISIS-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

ANALITIC_hm <- ANALITIC %>%
  filter(Estado == 'hemodialisis')

# Fit a Bayesian survival model using rstanarm
bayesFit <- stan_surv(formula = formula_FGE, data = ANALITIC_hm,
                      basehaz = "weibull", chains = 4, iter = 2000)
summary(bayesFit)

# Plot the predicted survival curve
surv_fit <- survfit(weibullFit)
png(paste0(baseurl, "Graficas/BY/BY_SurvCurv_FGE_HM.png"), width = 2000, height = 2000)
plot(bayesFit)
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Fit a Bayesian survival model using rstanarm
bayesFit <- stan_surv(formula = formula_FLL, data = ANALITIC_hm,
                      basehaz = "weibull", chains = 4, iter = 2000)
summary(bayesFit)

# Plot the predicted survival curve
surv_fit <- survfit(weibullFit)
png(paste0(baseurl, "Graficas/BY/BY_SurvCurv_FLL_HM.png"), width = 2000, height = 2000)
plot(bayesFit)
dev.off()

# ------------------- JOINT MODEL NADA -------------------
print('------------------- JOINT MODEL NADA-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

ANALITIC_nn <- ANALITIC %>%
  filter(Estado == 'nada')

# Fit a Bayesian survival model using rstanarm
bayesFit <- stan_surv(formula = formula_FGE, data = ANALITIC_nn,
                      basehaz = "weibull", chains = 4, iter = 2000)
summary(bayesFit)

# Plot the predicted survival curve
surv_fit <- survfit(weibullFit)
png(paste0(baseurl, "Graficas/BY/BY_SurvCurv_FGE_NN.png"), width = 2000, height = 2000)
plot(bayesFit)
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Fit a Bayesian survival model using rstanarm
bayesFit <- stan_surv(formula = formula_FLL, data = ANALITIC_nn,
                      basehaz = "weibull", chains = 4, iter = 2000)
summary(bayesFit)

# Plot the predicted survival curve
surv_fit <- survfit(weibullFit)
png(paste0(baseurl, "Graficas/BY/BY_SurvCurv_FLL_NN.png"), width = 2000, height = 2000)
plot(bayesFit)
dev.off()

print('================================= FIN BAYESIANO =================================')
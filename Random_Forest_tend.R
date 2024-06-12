print('================================= RANDOM FOREST =================================')

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
library(patchwork)
library(randomForestSRC)
library(ggRandomForests)

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
# baseurl <- "D:/Documentos/Universidad/TFG/"

# ------------- DATOS -------------

df_rf <- read.csv(paste0(baseurl, "data/df_Cox1.csv"), sep = ",", header = TRUE)

# ------------------- PREPARACIÓN -------------
print('------------------- PREPARACIÓN -------------------')

df_rf <- df_rf %>%
  relocate("edad_inicio", .after = "Fallecido")

Importancia <- data.frame(Variable = names(df_rf)[6:ncol(df_rf)])
Importancia$imp_all_FGE <- 0
Importancia$imp_all_FLL <- 0
Importancia$imp_hm_FGE <- 0
Importancia$imp_hm_FLL <- 0
Importancia$imp_tr_FGE <- 0
Importancia$imp_tr_FLL <- 0
Importancia$imp_tr_hm_FGE <- 0
Importancia$imp_tr_hm_FLL <- 0
Importancia$imp_NN_FGE <- 0
Importancia$imp_NN_FLL <- 0

# df_rf$Fallecido <- factor(df_rf$Fallecido, levels = c(0, 1), labels = c("Vivo", "Fallecido"))
# df_rf$FGE <- factor(df_rf$FGE, levels = c(0, 1), labels = c("Asciende", "Desciende"))
# df_rf$Estado <- as.factor(df_rf$Estado)

# ------------------- MODELO DE RANDOM FOREST ALL -------------------
print('------------------- MODELO DE RANDOM FOREST ALL-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

covariables <- names(df_rf)[6:ncol(df_rf)]

formula_FGE <- as.formula(paste("Surv(tiempo_total, FGE) ~ ", paste(covariables, collapse = " + ")))

# Ajusta el modelo de Random Forest
rf_fit_FGE <- rfsrc(formula_FGE, data = df_rf,
                    forest = TRUE,
                    importance = TRUE,
                    tree.err = TRUE,
                    ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_ALL.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FGE)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FGE_ALL.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_all_FGE <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FGE_ALL.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FGE_ALL.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FGE_ALL.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE, collapse=TRUE, cens.model="rfsrc")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

formula_Fallecido <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ ", paste(covariables, collapse = " + ")))

# Ajusta el modelo de Random Forest
rf_fit_FLL <- rfsrc(formula_Fallecido, data = df_rf,
                    forest = TRUE,
                    importance = TRUE,
                    tree.err = TRUE,
                    ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_ALL.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FLL)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FLL_ALL.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_all_FLL<- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FLL_ALL.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FLL_ALL.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FLL_ALL.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL, collapse=TRUE, cens.model="rfsrc")
dev.off()

# ------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------
print('------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

df_rf_hm <- df_rf %>%
  filter(Estado == 'hemodialisis')

covariables <- names(df_rf_hm)[8:ncol(df_rf_hm)]

# Ajusta el modelo de Random Forest
rf_fit_FGE_hm <- rfsrc(formula_FGE, data = df_rf_hm,
                       forest = TRUE,
                       importance = TRUE,
                       tree.err = TRUE,
                       ntree = 35, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE_hm)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_HM.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FGE_hm)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FGE_HM.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_hm_FGE <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FGE_HM.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FGE_HM.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE_hm, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FGE_HM.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE_hm, collapse=TRUE, cens.model="rfsrc")
dev.off()


# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Ajusta el modelo de Random Forest
rf_fit_FLL_hm <- rfsrc(formula_Fallecido, data = df_rf_hm,
                       forest = TRUE,
                       importance = TRUE,
                       tree.err = TRUE,
                       ntree = 15, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL_hm)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_HM.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FLL_hm)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FLL_HM.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_hm_FLL <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FLL_HM.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FLL_HM.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_hm, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FLL_HM.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_hm, collapse=TRUE, cens.model="rfsrc")
dev.off()

# ------------------- MODELO DE RANDOM FOREST TRANSPLANTE -------------------
print('------------------- MODELO DE RANDOM FOREST TRANSPLANTE -------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

df_rf_tr <- df_rf %>%
  filter(Estado == 'transplante')

covariables <- names(df_rf_tr)[8:ncol(df_rf_tr)]

# Ajusta el modelo de Random Forest
rf_fit_FGE_tr <- rfsrc(formula_FGE, data = df_rf_tr,
                       forest = TRUE,
                       importance = TRUE,
                       tree.err = TRUE,
                       ntree = 100, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE_tr)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_TR.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FGE_tr)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FGE_TR.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_tr_FGE <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FGE_TR.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FGE_TR.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE_tr, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FGE_TR.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE_tr, collapse=TRUE, cens.model="rfsrc")
dev.off()


# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Ajusta el modelo de Random Forest
rf_fit_FLL_tr <- rfsrc(formula_Fallecido, data = df_rf_tr,
                       forest = TRUE,
                       importance = TRUE,
                       tree.err = TRUE,
                       ntree = 100, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL_tr)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_TR.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FLL_tr)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FLL_TR.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_tr_FLL <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FLL_TR.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FLL_TR.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_tr, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FLL_TR.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_tr, collapse=TRUE, cens.model="rfsrc")
dev.off()

# ------------------- MODELO DE RANDOM FOREST AMBAS -------------------
print('------------------- MODELO DE RANDOM FOREST AMBAS -------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

df_rf_hmtr <- df_rf %>%
  filter(Estado == 'ambas')

covariables <- names(df_rf_hmtr)[8:ncol(df_rf_hmtr)]

# Ajusta el modelo de Random Forest
rf_fit_FGE_hmtr <- rfsrc(formula_FGE, data = df_rf_hmtr,
                         forest = TRUE,
                         importance = TRUE,
                         tree.err = TRUE,
                         ntree = 100, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE_hmtr)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_HMTR.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FGE_hmtr)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FGE_HMTR.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_tr_hm_FGE <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FGE_HMTR.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FGE_HMTR.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE_hmtr, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FGE_HMTR.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE_hmtr, collapse=TRUE, cens.model="rfsrc")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Ajusta el modelo de Random Forest
rf_fit_FLL_hmtr <- rfsrc(formula_Fallecido, data = df_rf_hmtr,
                         forest = TRUE,
                         importance = TRUE,
                         tree.err = TRUE,
                         ntree = 100, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL_hmtr)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_HMTR.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FLL_hmtr)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FLL_HMTR.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_tr_hm_FLL <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FLL_HMTR.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FLL_HMTR.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_hmtr, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FLL_HMTR.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_hmtr, collapse=TRUE, cens.model="rfsrc")
dev.off()

# ------------------- MODELO DE RANDOM FOREST NADA -------------------
print('------------------- MODELO DE RANDOM FOREST NADA -------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

df_rf_nn <- df_rf %>%
  filter(Estado == 'nada')

covariables <- names(df_rf_nn)[8:ncol(df_rf_nn)]

# Ajusta el modelo de Random Forest
rf_fit_FGE_nn <- rfsrc(formula_FGE, data = df_rf_nn,
                       forest = TRUE,
                       importance = TRUE,
                       tree.err = TRUE,
                       ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE_nn)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_NN.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FGE_nn)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FGE_NN.png"), width = 1500, height = 1400)
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_NN_FGE <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FGE_NN.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FGE_NN.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FGE_nn, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FGE_NN.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_hmtr, collapse=TRUE, cens.model="rfsrc")
dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Ajusta el modelo de Random Forest
rf_fit_FLL_nn <- rfsrc(formula_Fallecido, data = df_rf_nn,
                       forest = TRUE,
                       importance = TRUE,
                       tree.err = TRUE,
                       ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL_nn)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# =====> GENERAL <=====
# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_NN.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# =====> ERROR <=====
errorRate <- gg_error(rf_fit_FLL_nn)
png(paste0(baseurl, "Graficas/Random_Forest/RM_ErrorRate_FLL_NN.png"))
plot(errorRate)
dev.off()

# =====> VARIABLES DE IMPORTANCIA <=====
# Improved Variable Importance Plot
importance <- as.data.frame(vimp_results$importance)
importance$Variable <- rownames(importance)
names(importance) <- c("Importance", "Variable" )
Importancia$imp_NN_FLL <- importance$Importance[match(Importancia$Variable, importance$Variable)]

# Plot using ggplot2
png(paste0(baseurl, "Graficas/Random_Forest/RM_VarImp_FLL_NN.png"), width = 1500, height = 1400)
ggplot(importance, aes(x = reorder(Variable, vimp_results$importance), y = vimp_results$importance)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Variable Importance Plot", x = "Variables", y = "Importance") +
  coord_flip()  # This flips the axis to make labels readable
dev.off()

# =====> CURVAS DE SUPERVIVIENCIA <=====
png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_km_FLL_NN.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_nn, collapse=TRUE, cens.model="km")
dev.off()

png(paste0(baseurl, "Graficas/Random_Forest/RM_SurvCurv_rfscr_FLL_NN.png"), width = 2000, height = 2000)
plot.survival(rf_fit_FLL_nn, collapse=TRUE, cens.model="rfsrc")
dev.off()

# ------------------- EXPORTAR -------------------
print('------------------- EXPORTAR -------------------')
write.csv(Importancia, paste0(baseurl, "data/importancia.csv"), row.names = FALSE)

print('================================= FIN RANDOM FOREST =================================')
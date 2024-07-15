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


df_rf_1 <- read.csv(paste0(baseurl, "Mice/tend_mice_1.csv"), sep = ",", header = TRUE)
df_rf_2 <- read.csv(paste0(baseurl, "Mice/tend_mice_2.csv"), sep = ",", header = TRUE)
df_rf_3 <- read.csv(paste0(baseurl, "Mice/tend_mice_3.csv"), sep = ",", header = TRUE)
df_rf_4 <- read.csv(paste0(baseurl, "Mice/tend_mice_4.csv"), sep = ",", header = TRUE)
df_rf_5 <- read.csv(paste0(baseurl, "Mice/tend_mice_5.csv"), sep = ",", header = TRUE)
df_rf_6 <- read.csv(paste0(baseurl, "Mice/tend_mice_6.csv"), sep = ",", header = TRUE)
df_rf_7 <- read.csv(paste0(baseurl, "Mice/tend_mice_7.csv"), sep = ",", header = TRUE)

Importancia <- data.frame(Variable = names(df_rf)[7:ncol(df_rf)])
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

Importancia2 <- data.frame(Variable = names(df_rf)[7:ncol(df_rf)])
Importancia2$imp_all_FGE <- 0
Importancia2$imp_hm_FGE <- 0
Importancia2$imp_tr_FGE <- 0
Importancia2$imp_tr_hm_FGE <- 0
Importancia2$imp_NN_FGE <- 0

mice <- list(1, 2, 3, 4, 5, 6, 7)

for (i in mice){

  # Construir nombres dinámicamente
  df_nombre <- paste0("df_rf_", i)

  df_rf <- get(df_nombre)


  df_rf <- df_rf %>%
    relocate("edad_inicio", .after = "Fallecido")

  # df_rf$Fallecido <- factor(df_rf$Fallecido, levels = c(0, 1), labels = c("Vivo", "Fallecido"))
  # df_rf$FGE <- factor(df_rf$FGE, levels = c(0, 1), labels = c("Asciende", "Desciende"))
  # df_rf$Estado <- as.factor(df_rf$Estado)

  # ------------------- MODELO DE RANDOM FOREST ALL -------------------
  print('------------------- MODELO DE RANDOM FOREST ALL-------------------')
  # ----------------------- (sobre FGE) ------------------------------
  print('------------------- (sobre FGE) -------------------')

  covariables <- names(df_rf)[7:ncol(df_rf)]

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE_ALL_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_all_FGE, imp_all_FGE + importance$Importance[match(Variable, importance$Variable)]))

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FLL_ALL_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FLL = ifelse(is.na(match(Variable, importance$Variable)), imp_all_FLL, imp_all_FLL + importance$Importance[match(Variable, importance$Variable)]))

  # ----------------------- (sobre FGE EKFC) ------------------------------
  print('------------------- (sobre FGE EKFC) -------------------')

  formula_FGE2 <- as.formula(paste("Surv(tiempo_total, FGE2) ~ ", paste(covariables, collapse = " + ")))

  # Ajusta el modelo de Random Forest
  rf_fit_FGE2 <- rfsrc(formula_FGE2, data = df_rf,
                      forest = TRUE,
                      importance = TRUE,
                      tree.err = TRUE,
                      ntree = 50, na.action = "na.impute")

  # Calcular la importancia de las variables
  vimp_results <- vimp(rf_fit_FGE2)

  # Imprimir los resultados de importancia de las variables
  print(vimp_results)

  # =====> GENERAL <=====
  # Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE2_ALL_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia2 <- Importancia2 %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_all_FGE, imp_all_FGE + importance$Importance[match(Variable, importance$Variable)]))

  # ------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------
  print('------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------')
  # ----------------------- (sobre FGE) ------------------------------
  print('------------------- (sobre FGE) -------------------')

  df_rf_hm <- df_rf %>%
    filter(Estado == 'hemodialisis')

  covariables <- names(df_rf_hm)[7:ncol(df_rf_hm)]

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE_HM_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_hm_FGE, imp_hm_FGE + importance$Importance[match(Variable, importance$Variable)]))

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FLL_HM_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FLL = ifelse(is.na(match(Variable, importance$Variable)), imp_hm_FLL, imp_hm_FLL + importance$Importance[match(Variable, importance$Variable)]))

  # ----------------------- (sobre FGE EKFC) ------------------------------
  print('------------------- (sobre FGE EKFC) -------------------')

  rf_fit_FGE2_hm <- rfsrc(formula_FGE2, data = df_rf_hm,
                         forest = TRUE,
                         importance = TRUE,
                         tree.err = TRUE,
                         ntree = 35, na.action = "na.impute")

  # Calcular la importancia de las variables
  vimp_results <- vimp(rf_fit_FGE2_hm)

  # Imprimir los resultados de importancia de las variables
  print(vimp_results)

  # =====> GENERAL <=====
  # Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE2_HM_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia2 <- Importancia2 %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_hm_FGE, imp_hm_FGE + importance$Importance[match(Variable, importance$Variable)]))

  # ------------------- MODELO DE RANDOM FOREST TRANSPLANTE -------------------
  print('------------------- MODELO DE RANDOM FOREST TRANSPLANTE -------------------')
  # ----------------------- (sobre FGE) ------------------------------
  print('------------------- (sobre FGE) -------------------')

  df_rf_tr <- df_rf %>%
    filter(Estado == 'transplante')

  covariables <- names(df_rf_tr)[7:ncol(df_rf_tr)]

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE_TR_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_tr_FGE, imp_tr_FGE + importance$Importance[match(Variable, importance$Variable)]))

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FLL_TR_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FLL = ifelse(is.na(match(Variable, importance$Variable)), imp_tr_FLL, imp_tr_FLL + importance$Importance[match(Variable, importance$Variable)]))

  # ----------------------- (sobre FGE EKFC) ------------------------------
  print('------------------- (sobre FGE EKFC) -------------------')

  rf_fit_FGE2_tr <- rfsrc(formula_FGE2, data = df_rf_tr,
                         forest = TRUE,
                         importance = TRUE,
                         tree.err = TRUE,
                         ntree = 100, na.action = "na.impute")

  # Calcular la importancia de las variables
  vimp_results <- vimp(rf_fit_FGE2_tr)

  # Imprimir los resultados de importancia de las variables
  print(vimp_results)

  # =====> GENERAL <=====
  # Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE2_TR_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia2 <- Importancia2 %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_tr_FGE, imp_tr_FGE + importance$Importance[match(Variable, importance$Variable)]))

  # ------------------- MODELO DE RANDOM FOREST AMBAS -------------------
  print('------------------- MODELO DE RANDOM FOREST AMBAS -------------------')
  # ----------------------- (sobre FGE) ------------------------------
  print('------------------- (sobre FGE) -------------------')

  df_rf_hmtr <- df_rf %>%
    filter(Estado == 'ambas')

  covariables <- names(df_rf_hmtr)[7:ncol(df_rf_hmtr)]

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE_HMTR_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_tr_hm_FGE, imp_tr_hm_FGE + importance$Importance[match(Variable, importance$Variable)]))

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FLL_HMTR_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FLL = ifelse(is.na(match(Variable, importance$Variable)), imp_tr_hm_FLL, imp_tr_hm_FLL + importance$Importance[match(Variable, importance$Variable)]))

  # ----------------------- (sobre FGE EKFC) ------------------------------
  print('------------------- (sobre FGE EKFC) -------------------')

  rf_fit_FGE2_hmtr <- rfsrc(formula_FGE2, data = df_rf_hmtr,
                           forest = TRUE,
                           importance = TRUE,
                           tree.err = TRUE,
                           ntree = 100, na.action = "na.impute")

  # Calcular la importancia de las variables
  vimp_results <- vimp(rf_fit_FGE2_hmtr)

  # Imprimir los resultados de importancia de las variables
  print(vimp_results)

  # =====> GENERAL <=====
  # Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE2_HMTR_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia2 <- Importancia2 %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_tr_hm_FGE, imp_tr_hm_FGE + importance$Importance[match(Variable, importance$Variable)]))

  # ------------------- MODELO DE RANDOM FOREST NADA -------------------
  print('------------------- MODELO DE RANDOM FOREST NADA -------------------')
  # ----------------------- (sobre FGE) ------------------------------
  print('------------------- (sobre FGE) -------------------')

  df_rf_nn <- df_rf %>%
    filter(Estado == 'nada')

  covariables <- names(df_rf_nn)[7:ncol(df_rf_nn)]

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE_NN_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_NN_FGE, imp_NN_FGE + importance$Importance[match(Variable, importance$Variable)]))

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
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FLL_NN_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia <- Importancia %>%
    mutate(imp_all_FLL = ifelse(is.na(match(Variable, importance$Variable)), imp_NN_FLL, imp_NN_FLL + importance$Importance[match(Variable, importance$Variable)]))

  # ----------------------- (sobre FGE EKFC) ------------------------------
  print('------------------- (sobre FGE EKFC) -------------------')

  rf_fit_FGE2_nn <- rfsrc(formula_FGE2, data = df_rf_nn,
                         forest = TRUE,
                         importance = TRUE,
                         tree.err = TRUE,
                         ntree = 50, na.action = "na.impute")

  # Calcular la importancia de las variables
  vimp_results <- vimp(rf_fit_FGE2_nn)

  # Imprimir los resultados de importancia de las variables
  print(vimp_results)

  # =====> GENERAL <=====
  # Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
  filename <- paste0(baseurl, "Graficas/Random_Forest2/RM_FGE2_NN_", i,".png")
  # Abre el dispositivo PNG
  png(filename, width = 1500, height = 1400)
  # Para visualizar los resultados puedes usar plot
  print(plot(vimp_results))
  dev.off()

  # =====> VARIABLES DE IMPORTANCIA <=====
  # Improved Variable Importance Plot
  importance <- as.data.frame(vimp_results$importance)
  importance$Variable <- rownames(importance)
  names(importance) <- c("Importance", "Variable" )
  Importancia2 <- Importancia2 %>%
    mutate(imp_all_FGE = ifelse(is.na(match(Variable, importance$Variable)), imp_NN_FGE, imp_NN_FGE + importance$Importance[match(Variable, importance$Variable)]))

}

#Media de las importancias
Importancia <- Importancia %>%
  mutate(across(where(is.numeric), ~ . / 7))

#Media de las importancias
Importancia2 <- Importancia2 %>%
  mutate(across(where(is.numeric), ~ . / 7))

# ------------------- EXPORTAR -------------------
print('------------------- EXPORTAR -------------------')
write.csv(Importancia, paste0(baseurl, "data/importanciaRM2.csv"), row.names = FALSE)
write.csv(Importancia2, paste0(baseurl, "data/importancia2RM2.csv"), row.names = FALSE)

print('================================= FIN RANDOM FOREST =================================')
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

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
# baseurl <- "D:/Documentos/Universidad/TFG/"

# ------------- DATOS -------------

df_rf <- read.csv(paste0(baseurl, "data/ANALITIC_2.csv"), sep = ",", header = TRUE)

# ------------------- PREPARACIÓN -------------
print('------------------- PREPARACIÓN -------------------')

df_rf <- df_rf %>%
  mutate(Estado = case_when(
    Hemodialisis == 1 & Transplante == 0 ~ "hemodialisis",
    Hemodialisis == 0 & Transplante == 1 ~ "transplante",
    Hemodialisis == 1 & Transplante == 1 ~ "ambas",
    Hemodialisis == 0 & Transplante == 0 ~ "nada"
  ))
df_rf <- df_rf %>%
  select(-c(Hemodialisis, Transplante))
df_rf <- df_rf %>%
  relocate("Estado", .after = "FGE")

df_rf <- df_rf %>%
  arrange(ID, fechatoma) %>%
  group_by(ID) %>%
  mutate(dias_transcurridos = round(as.numeric(difftime(fechatoma, lag(fechatoma, default = first(fechatoma)), units = "days")))) %>%
  ungroup()
df_rf <- df_rf %>%
  relocate("dias_transcurridos", .after = "fechatoma")
df_rf <- df_rf %>%
  relocate("Edad", .after = "dias_transcurridos")

df_rf <- df_rf %>%
  arrange(ID, fechatoma) %>%
  group_by(ID) %>%
  mutate(FGE_microten = as.integer(lag(FGE, default = first(FGE)) > FGE)) # 1 si el FGE desciende y 0 si asciende o se mantiene igual
df_rf <- df_rf %>%
  relocate("FGE_microten", .after = "FGE")

# df_rf$Fallecido <- factor(df_rf$Fallecido, levels = c(0, 1), labels = c("Vivo", "Fallecido"))
# df_rf$FGE <- factor(df_rf$FGE, levels = c(0, 1), labels = c("Asciende", "Desciende"))
# df_rf$Estado <- as.factor(df_rf$Estado)

# ------------------- MODELO DE RANDOM FOREST ALL -------------------
print('------------------- MODELO DE RANDOM FOREST ALL-------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

covariables <- names(df_rf)[8:ncol(df_rf)]

formula_FGE <- as.formula(paste("Surv(dias_transcurridos, FGE_microten) ~ ", paste(covariables, collapse = " + ")))

# Ajusta el modelo de Random Forest
rf_fit_FGE <- rfsrc(formula_FGE, data = df_rf, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# # Convert the variable importance to a data frame for ggplot
# vimp_data <- data.frame(Variable = names(vimp_results$importance),
#                         Importance = vimp_results$importance)
#
# # Create the plot
# g <- ggplot(vimp_data, aes(x = reorder(Variable, VIMP), y = VIMP)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   theme_minimal() +
#   labs(title = "Variable Importance in Random Forest Model",
#        x = "Variables",
#        y = "Importance") +
#   coord_flip() # Flipping the coordinates for better readability of variable names
#
# print(g)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_ALL.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# filename <- paste0(baseurl, "Graficas/Random_Forest/RM_sur_FGE_ALL.png")
# png(filename, width = 1500, height = 1400)
# plot(rf_fit_FGE, type = "survival", time.inc = 6)
# dev.off()

# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

formula_Fallecido <- as.formula(paste("Surv(tiempo_total, Fallecido) ~ ", paste(covariables, collapse = " + ")))

# Ajusta el modelo de Random Forest
rf_fit_FLL <- rfsrc(formula_Fallecido, data = df_rf, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_ALL.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# ------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------
print('------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

df_rf_hm <- df_rf %>%
  filter(Estado == 'hemodialisis')

covariables <- names(df_rf_hm)[8:ncol(df_rf_hm)]

# Ajusta el modelo de Random Forest
rf_fit_FGE_hm <- rfsrc(formula_FGE, data = df_rf_hm, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE_hm)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_HM.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()


# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Ajusta el modelo de Random Forest
rf_fit_FLL_hm <- rfsrc(formula_Fallecido, data = df_rf_hm, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL_hm)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_HM.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# ------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------
print('------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

df_rf_tr <- df_rf %>%
  filter(Estado == 'transplante')

covariables <- names(df_rf_tr)[8:ncol(df_rf_tr)]

# Ajusta el modelo de Random Forest
rf_fit_FGE_tr <- rfsrc(formula_FGE, data = df_rf_tr, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE_tr)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_TR.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()


# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Ajusta el modelo de Random Forest
rf_fit_FLL_tr <- rfsrc(formula_Fallecido, data = df_rf_tr, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL_tr)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_TR.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# ------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------
print('------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

df_rf_hmtr <- df_rf %>%
  filter(Estado == 'ambas')

covariables <- names(df_rf_hmtr)[8:ncol(df_rf_hmtr)]

# Ajusta el modelo de Random Forest
rf_fit_FGE_hmtr <- rfsrc(formula_FGE, data = df_rf_hmtr, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE_hmtr)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_HMTR.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()


# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Ajusta el modelo de Random Forest
rf_fit_FLL_hmtr <- rfsrc(formula_Fallecido, data = df_rf_hmtr, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL_hmtr)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_HMTR.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

# ------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------
print('------------------- MODELO DE RANDOM FOREST HEMODIALISIS -------------------')
# ----------------------- (sobre FGE) ------------------------------
print('------------------- (sobre FGE) -------------------')

df_rf_nn <- df_rf %>%
  filter(Estado == 'nada')

covariables <- names(df_rf_nn)[8:ncol(df_rf_nn)]

# Ajusta el modelo de Random Forest
rf_fit_FGE_nn <- rfsrc(formula_FGE, data = df_rf_nn, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FGE_nn)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FGE_NN.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()


# ----------------------- (sobre Fallecido) ------------------------------
print('------------------- (sobre Fallecido) -------------------')

# Ajusta el modelo de Random Forest
rf_fit_FLL_nn <- rfsrc(formula_Fallecido, data = df_rf_nn, ntree = 50, na.action = "na.impute")

# Calcular la importancia de las variables
vimp_results <- vimp(rf_fit_FLL_nn)

# Imprimir los resultados de importancia de las variables
print(vimp_results)

# Especifica el nombre del archivo y la ruta donde quieres guardar el gráfico
filename <- paste0(baseurl, "Graficas/Random_Forest/RM_FLL_NN.png")
# Abre el dispositivo PNG
png(filename, width = 1500, height = 1400)
# Para visualizar los resultados puedes usar plot
plot(vimp_results)
dev.off()

print('================================= FIN RANDOM FOREST =================================')
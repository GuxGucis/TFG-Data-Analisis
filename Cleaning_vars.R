print('================================= Limpieza de Variables =================================')

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

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
# baseurl <- "D:/Documentos/Universidad/TFG/"

# ------------- DATOS -------------

ANALITIC <- read.csv(paste0(baseurl, "data/ANALITIC_mi.csv"), sep = ",", header = TRUE)

# ------------------- Análisis previo -------------------
print('------------------- Análisis previo -------------------')

# POR COLUMNA
significancia <- read.csv(paste0(baseurl, "data/significancia.csv"), sep = ",", header = TRUE)
# Sumar total de no significancia por modelo
modelo_frecuencias <- colSums(significancia[, -c(1, ncol(significancia))])

# Gráfico de barras
bar_data <- data.frame(Modelo = names(modelo_frecuencias), Total = modelo_frecuencias)
g <- ggplot(bar_data, aes(x = reorder(Modelo, Total), y = Total, fill = Total)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Modelo", y = "Total de No Significativa", title = "Frecuencia de No Significancia por Modelo") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/Freq_Colum.png"), plot = g, width = 10, height = 12, dpi = 300)

# POR FILA
significancia <- read.csv(paste0(baseurl, "data/significancia.csv"), sep = ",", header = TRUE)
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
  labs(x = "Variable", y = "Total de No Significativa", title = "Frecuencia Total de No Significancia por Variable") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/Freq_Fila.png"), plot = g, width = 10, height = 12, dpi = 300)

# EN HEATMAP
significancia <- read.csv(paste0(baseurl, "data/significancia.csv"), sep = ",", header = TRUE)
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
    title = 'Heatmap de No Significancia por Modelo',
    x = 'Modelo',
    y = 'Variable',
    fill = 'Conteo de No Significancia'
  )

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/HeatMap.png"), plot = g, width = 10, height = 12, dpi = 300)

# DESVIACIÓN ESTANDAR
significancia <- read.csv(paste0(baseurl, "data/significancia.csv"), sep = ",", header = TRUE)
# Calcular desviación estándar
varianzas <- apply(significancia[, -c(1, ncol(significancia))], 2, sd)

# Gráfico de barras
var_data <- data.frame(Modelo = names(varianzas), SD = varianzas)
g <- ggplot(var_data, aes(x = reorder(Modelo, SD), y = SD, fill = SD)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Modelo", y = "Desviacion Estandar", title = "Desviacion Estandar de No Significativa por Modelo") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/Desviacion_Estandar.png"), plot = g, width = 10, height = 12, dpi = 300)


print('================================= FIN Limpieza de Variables =================================')
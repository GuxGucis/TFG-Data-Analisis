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
library(mice)
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

ANALITIC <- read.csv(paste0(baseurl, "data/ANALITIC_2.csv"), sep = ",", header = TRUE)
significancia <- read.csv(paste0(baseurl, "data/significancia.csv"), sep = ",", header = TRUE)
importance <- read.csv(paste0(baseurl, "data/importancia.csv"), sep = ",", header = TRUE)

# ------------------- Análisis previo -------------------
print('------------------- Análisis previo -------------------')

datos_ausentes <- sapply(ANALITIC, function(x) sum(is.na(x)))
datos_ausentes_df <- data.frame(columna = names(datos_ausentes), ausentes = datos_ausentes)
g <- ggplot(datos_ausentes_df, aes(x = reorder(columna, -ausentes), y = ausentes)) +
  geom_bar(stat = "identity", fill="#69b3a2", color="#e9ecef") +
  coord_flip() +
  labs(title = "Datos Ausentes por Columna después de limpieza", x = "Variable", y = "Datos Ausentes")

ggsave(paste0(baseurl, "Graficas/Cleaning/Ausentes_Preclean.png"), plot = g, width = 18, height = 12, dpi = 300)

# ------------------- Significancia COX -------------------
print('------------------- Significancia COX -------------------')

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
ggsave(paste0(baseurl, "Graficas/Cleaning/Freq_Colum_Significancia.png"), plot = g, width = 10, height = 12, dpi = 300)

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
  labs(x = "Variable", y = "Total de No Significativa", title = "Frecuencia Total de la No Significancia por Variable") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/Freq_Fila_Significancia.png"), plot = g, width = 10, height = 12, dpi = 300)

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
    title = 'Heatmap de la No Significancia por Modelo',
    x = 'Modelo',
    y = 'Variable',
    fill = 'Conteo de No Significancia'
  )

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/HeatMap_Significancia.png"), plot = g, width = 10, height = 12, dpi = 300)

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
  labs(x = "Modelo", y = "Desviacion Estandar", title = "Desviacion Estandar de la No Significativa por Modelo") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white", colour = "black"), plot.background = element_rect(fill = "white", colour = "black"))

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/Desviacion_Estandar_Significancia.png"), plot = g, width = 10, height = 12, dpi = 300)

# ------------------- Importancia RF -------------------
print('------------------- Importancia RF -------------------')

# EN HEATMAP
importance <- read.csv(paste0(baseurl, "data/importancia.csv"), sep = ",", header = TRUE)
# Convertir los datos a formato largo
long_data <- pivot_longer(importance,
                          cols = -Variable,
                          names_to = 'modelo',
                          values_to = 'importancia')

# Crear el heatmap
g <- ggplot(long_data, aes(x = modelo, y = Variable, fill = importancia)) +
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
    title = 'Heatmap de la Importancia por Modelo',
    x = 'Modelo',
    y = 'Variable',
    fill = 'Importancia'
  )

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/HeatMap_Importancia.png"), plot = g, width = 10, height = 12, dpi = 300)

# ------------------- Importancia RF FULL -------------------
print('------------------- Importancia RF FULL-------------------')
# ------------------PROBAR CUANDO ACABE MICE--------------------------------------
# EN HEATMAP
importance <- read.csv(paste0(baseurl, "data/importancia_ANALITICS.csv"), sep = ",", header = TRUE)
# Convertir los datos a formato largo
long_data <- pivot_longer(importance,
                          cols = -Variable,
                          names_to = 'modelo',
                          values_to = 'importancia')

# Crear el heatmap
g <- ggplot(long_data, aes(x = modelo, y = Variable, fill = importancia)) +
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
    title = 'Heatmap de la Importancia por Modelo DE LOS ANALITICS COMPLETO',
    x = 'Modelo',
    y = 'Variable',
    fill = 'Importancia'
  )

print(g)
ggsave(paste0(baseurl, "Graficas/Cleaning/HeatMap_Importancia_full.png"), plot = g, width = 10, height = 12, dpi = 300)

# ------------------- LIMPIAMOS -------------------
print('------------------- LIMPIAMOS -------------------')

# En base a los multiples modelos de cox y random forest, se puede ver (probablemente por una suma de los escasos datos que hay de dichos casos)
# que los modelos donde solo se ha hecho transplante o transplante y hemodialisis no son lo suficiente para extraer ningun resultado, en su mayoria dan modelos
# sin importancia y significancia (probablement porque no hay datos sufucientes)
# Para los restantes modelos si parece hacer datos suficientes, por lo que seguimos con esos
# En cuanto a las variables (se ve mas claramente en la significancia), vemos que hay datos que por muchas variante de imputacion ques haga no parece tener significancia
# en el modelo (aunque podría ser por la ausencia de datos), pero que aun asi, solo hace ruido al modelo, Por lo que discriminamos si la media de significancias de los modelos restantes es igual o mayor a 5

# Quitamos los modelos
ANALITIC_filt <- ANALITIC %>%
  filter(!(Transplante == 1 & Hemodialisis == 0)) %>%
  filter(!(Transplante == 1 & Hemodialisis == 1))

significancia <- significancia %>%
  select(-c(count_tr_FGE, count_tr_FLL, count_tr_hm_FGE, count_tr_hm_FLL))

# Calcular la media de no significancias
significancia$Media <- rowMeans(significancia[ , -which(names(significancia) == "variable")], na.rm = TRUE)

# Encontrar nombres de columnas cuya media de no significancias sea 5 o mayor
columns_to_remove <- significancia$variable[significancia$Media >= 5]

# Eliminar las columnas del DataFrame principal
ANALITIC_clean <- ANALITIC_filt[ , !(names(ANALITIC_filt) %in% columns_to_remove)]

# ------------------- NEW DATA STATUS -------------------
print('------------------- NEW DATA STATUS -------------------')

datos_ausentes <- sapply(ANALITIC_clean, function(x) sum(is.na(x)))
datos_ausentes_df <- data.frame(columna = names(datos_ausentes), ausentes = datos_ausentes)
g <- ggplot(datos_ausentes_df, aes(x = reorder(columna, -ausentes), y = ausentes)) +
  geom_bar(stat = "identity", fill="#69b3a2", color="#e9ecef") +
  coord_flip() +
  labs(title = "Datos Ausentes por Columna después de limpieza", x = "Variable", y = "Datos Ausentes")

ggsave(paste0(baseurl, "Graficas/Cleaning/Ausentes_Postclean.png"), plot = g, width = 18, height = 12, dpi = 300)

# DATOS POR ESTADO
condiciones <- list(
  "Hemodialisis" = ANALITIC_clean[ANALITIC_clean$Hemodialisis == 1 & ANALITIC_clean$Transplante == 0, ],
  "Nada" = ANALITIC_clean[ANALITIC_clean$Hemodialisis == 0 & ANALITIC_clean$Transplante == 0, ]
)

for (key in names(condiciones)) {
  datos_condicion <- condiciones[[key]]
  presentes <- colSums(!is.na(datos_condicion))
  ausentes <- colSums(is.na(datos_condicion))

  datos_grafica <- data.frame(
    Columna = rep(names(presentes), 2),
    Cantidad = c(presentes, ausentes),
    Tipo = rep(c("Presentes", "Ausentes"), each = length(presentes))
  )

  g <- ggplot(datos_grafica, aes(x = reorder(Columna, -Cantidad), y = Cantidad, fill = Tipo)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    labs(title = paste("Datos Presentes y Ausentes después de limpieza:", key),
         x = "Columnas",
         y = "Cantidad de Datos") +
    scale_fill_manual(values = c("steelblue", "tomato"))

  ggsave(paste0(baseurl, "Graficas/Cleaning/", key, "_Postclean.png"), plot = g, width = 18, height = 12, dpi = 300)
}

# ------------------- Rellenado de Datos con MICE -------------------
print('------------------- Rellenado de Datos con MICE -------------------')

# # CONFIGURACIÓN PARA PODER USAR VARIOS HILOS
# # Detectar el número de núcleos lógicos
# num_cores <- detectCores(logical = TRUE)
#
# # Crear un clúster con un núcleo menos que el total para dejar recursos para el sistema
# cl <- makeCluster(num_cores - 2)
#
# # Usar el clúster para paralelizar mice
# ANALITIC_mice <- mice(ANALITIC_clean, m=7, method='cart', seed=123, parallel = "snow", maxit=7, cluster=cl)
#
# # Detener el clúster una vez completada la imputación
# stopCluster(cl)
#
# # Selecciona un conjunto imputado
# ANALITIC_1 <- complete(ANALITIC_mice, 1)
# ANALITIC_2 <- complete(ANALITIC_mice, 2)
# ANALITIC_3 <- complete(ANALITIC_mice, 3)
# ANALITIC_4 <- complete(ANALITIC_mice, 4)
# ANALITIC_5 <- complete(ANALITIC_mice, 5)
# ANALITIC_6 <- complete(ANALITIC_mice, 6)
# ANALITIC_7 <- complete(ANALITIC_mice, 7)
#
# # ------------------- EXPORTAR -------------------
# print('------------------- EXPORTAR -------------------')
#
# write.csv(ANALITIC_1, paste0(baseurl, "Mice/ANALITIC_mice_1.csv"), row.names = FALSE)
# write.csv(ANALITIC_2, paste0(baseurl, "Mice/ANALITIC_mice_2.csv"), row.names = FALSE)
# write.csv(ANALITIC_3, paste0(baseurl, "Mice/ANALITIC_mice_3.csv"), row.names = FALSE)
# write.csv(ANALITIC_4, paste0(baseurl, "Mice/ANALITIC_mice_4.csv"), row.names = FALSE)
# write.csv(ANALITIC_5, paste0(baseurl, "Mice/ANALITIC_mice_5.csv"), row.names = FALSE)
# write.csv(ANALITIC_6, paste0(baseurl, "Mice/ANALITIC_mice_6.csv"), row.names = FALSE)
# write.csv(ANALITIC_7, paste0(baseurl, "Mice/ANALITIC_mice_7.csv"), row.names = FALSE)

write.csv(ANALITIC_clean, paste0(baseurl, "data/ANALITIC_clean.csv"), row.names = FALSE)

print('================================= FIN Limpieza de Variables =================================')
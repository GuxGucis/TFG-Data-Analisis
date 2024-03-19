
# ================================== INSTALACIONES ===========================
# R versión 4.3.2 y Python version 3.9

 # install.packages("tidyverse")
 # install.packages("ggplot2")
 # install.packages("dplyr")
 # install.packages("tidyr")
 # install.packages("readr")
 # install.packages("reshape2")
 # install.packages("conflicted")
 # install.packages("formattable")

# ================================== LIBRERIAS ===========================

 # library(tidyverse)  # Este paquete proporciona un conjunto de herramientas para manipular datos
 # library(ggplot2)    # Para crear gráficos
 # library(dplyr)      # Que proporciona un conjunto de funciones para la manipulación de datos, incluyendo filtrado, selección, mutación, entre otros. Es muy útil para realizar transformaciones de datos
 # library(tidyr)      # Formato a los datos para que se ajusten a un formato "tidy" (ordenado)
 # library(readr)      # Una biblioteca para leer datos rectangulares
 # library(reshape2)   # Una biblioteca útil para reorganizar y transformar conjuntos de datos
 # library(conflicted) # Para los paquetes que den conflicto entre ellos para que suelte un error y tengas que especificar cual usas
 # library(formattable)# Para dar formato a los resultados

# ================================== ENLACES =============================

# https://r-graph-gallery.com/
# https://r-charts.com/
# https://shiny.posit.co/

# ========================================================================

ANALITIC <- read.csv("D:/Documentos/Universidad/TFG/Analiticas.csv", sep = ";", header = TRUE)
INFORM <- read.csv("D:/Documentos/Universidad/TFG/Datos_informe.csv", sep = ";", header = TRUE)

# ========================================================================

#summary(ANALITIC)
#summary(INFORM)

# -------------------------- CON INFORM -------------------------------
# Cálculos de cuantos datos hay por paciente (cuantas entradas hay de cada paciente)

NumXPacienteI <- table(INFORM$GIDENPAC)
dfNumXPacienteI <- as.data.frame(NumXPacienteI)
dfNumXPacienteI <- setNames(dfNumXPacienteI, c("ID_Pacientes", "Freq"))
print(dfNumXPacienteI)
#print(paste("Paciente de las 1640 entradas en INFORMS: ", subset(dfNumXPacienteI, Freq == 1640)ID_Pacientes))
#print(NumXPacienteI)

# Cuanta cantidad de entradas hay por frecuencia de entradas

NumXNumI <- table(NumXPacienteI)
#print(NumXNum)

#     (En gráfica)
dfNumXNumI <- as.data.frame(NumXNumI)

print(dfNumXNumI)

ggplot(data = dfNumXNumI, aes(x = NumXPacienteI, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Recuento de cantidad de entradas INFORM", x = "N de entradas", y = "Frecuencia") +
  theme_minimal()

# Sumatorio de las frecuencias

total_frecuenciasI <- sum(dfNumXNumI$Freq)

# Imprimir el sumatorio de las frecuencias
print(paste('Sumatorio de las frecuencias INFORM (Cantidad de pacientes):', total_frecuenciasI))


# -------------------------- CON ANALITIC -------------------------------
# Cálculos de cuantos datos hay por paciente (cuantas entradas hay de cada paciente)

NumXPacienteA <- table(ANALITIC$gidenpac)
#print(NumXPaciente)
dfNumXPacienteA <- as.data.frame(NumXPacienteA)
dfNumXPacienteA <- setNames(dfNumXPacienteA, c("ID_Pacientes", "Freq"))
print(dfNumXPacienteA)

# Cuanta cantidad de entradas hay por frecuencia de entradas

NumXNumA <- table(NumXPacienteA)
#print(NumXNum)

#     (En gráfica)
dfNumXNumA <- as.data.frame(NumXNumA)
print(dfNumXNumA)

ggplot(data = dfNumXNumA, aes(x = NumXPacienteA, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Recuento de cantidad de entradas ANALITIC", x = "N de entradas", y = "Frecuencia") +
  theme_minimal()

# Sumatorio de las frecuencias

total_frecuenciasA <- sum(dfNumXNumA$Freq)

# Imprimir el sumatorio de las frecuencias
print(paste('Sumatorio de las frecuencias ANALITIC (Cantidad de pacientes):', total_frecuenciasA))

#Paciente vacio en el dataframe de INFORM
print(paste("Paciente de las 1640 entradas en INFORMS: ", subset(dfNumXPacienteI, Freq == 1640)$ID_Pacientes))

valores_no_encontrados <- dfNumXPacienteI$ID_Pacientes[!dfNumXPacienteI$ID_Pacientes %in% dfNumXPacienteA$ID_Pacientes]
print(valores_no_encontrados)


# ---------- MERGE DE LAS TABLAS PARA LA FECHA DE CITA (INFORM) Y LA DE TOMA) ------------
# ---------- unificando los id's para que solo queden los que esten en ambos -------------
# --> no hace bien el merge
## merged_df <- merge(INFORM[, c('GIDENPAC')], ANALITIC[, c('gidenpac')], by.x = c('GIDENPAC'), by.y = c('gidenpac'))

##write.csv(merged_df, file = "resultado_merge.csv", row.names = FALSE)

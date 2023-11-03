
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

ANALITIC <- read.csv("Analiticas.csv", sep = ";", header = TRUE)
INFORM <- read.csv("Datos_informe.csv", sep = ";", header = TRUE)

# ========================================================================

#summary(ANALITIC)
#summary(INFORM)

NumXPaciente <- table(INFORM$GIDENPAC)
print(NumXPaciente)

NumXNum <- table(NumXPaciente)
print(NumXNum)
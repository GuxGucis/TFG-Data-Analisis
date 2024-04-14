print('================================= INICIO INFORMES =================================')

# ------------------- LIBRERIAS -------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lme4)
library(readxl)

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
# baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
baseurl <- "D:/Documentos/Universidad/TFG/"


EXCEL <- read_excel(paste0(baseurl, "Grupo/analiticas_filtradas.xlsx"))

INFORM <- read.csv(paste0(baseurl, "Datos_informe.csv"), sep = ";", header = TRUE)

# ------------------- CORRECCIÓN DE NOMBRES -------------------
print('------------------- CORRECCIÓN DE NOMBRES -------------------')

namecol <- names(INFORM)

namecol1 <- gsub("X..", "Porc", namecol)
# Reemplaza múltiples puntos con uno solo
# IMPORTANTE!! SI NO SE TIENE LA LIBRERIA STRINGR NO FUNCIONARÁ
namecol1 <- str_replace_all(namecol1, "\\.{2,}", ".")
# Elimina el punto final si lo hay
namecol1 <- str_replace_all(namecol1, "\\.$", "")
# Quitar las tildes y las ñ (por n) para eliminar problemas al llamar a las columnas
namecol1 <- iconv(namecol1, "UTF-8", "ASCII//TRANSLIT")

print(namecol1)
colnames(INFORM) <- namecol1
INFORM$ID <- as.numeric(as.character(INFORM$ID))
EXCEL$ID <- as.numeric(as.character(EXCEL$ID))

# Hay una columna 'X' Completamente vacia que habria que quitar si eso

# ------------------- BUSQUEDA HEMODIALISIS -------------------
print('------------------- BUSQUEDA HEMODIALISIS -------------------')
# Bueno algo asi, pero como Lorena a encontrado mas nos quedaremos con esas

# Excluir la columna 'AntecFamiliaPedia' de la lista de columnas a considerar
columnas_de_interes <- setdiff(names(INFORM), "AntecFamiliaPedia")

# Asegurar que las columnas restantes estén en formato de texto (character)
INFORM[columnas_de_interes] <- lapply(INFORM[columnas_de_interes], as.character)

# Palabra clave a buscar
patron_busqueda <- "hemodi[aá]lisis|di[aá]lisis peritoneal| di[aá]lisis"

# Función para buscar el patrón en una fila, excluyendo 'AntecFamiliaPedia'
buscar_hemodialisis <- function(fila) {
  # Unir el texto de las columnas de interés (excluyendo AntecFamiliaPedia) en una sola cadena
  texto_unido <- tolower(paste(fila[columnas_de_interes], collapse = " "))

  # Buscar el patrón de transplante en el texto unido usando expresiones regulares
  if (str_detect(texto_unido, regex(patron_busqueda, ignore_case = TRUE))) {
    return(1)
  } else {
    return(0)
  }
}

# Aplicar la función a cada fila del dataframe INFORM para crear la nueva columna 'hemodialisis'
# ESTO SI NOS QUEDAMOS CON LO MIO
# INFORM$hemodialisis <- apply(INFORM, 1, buscar_hemodialisis)

# ------- EXCEL GRUPO -------

INFORM$ID <- as.numeric(as.character(INFORM$ID))
EXCEL$ID <- as.numeric(as.character(EXCEL$ID))

mapeo_hemodialisis <- EXCEL %>%
  select(ID, Hemodialisis) %>%
  distinct() %>%
  mutate(Hemodialisis = ifelse(Hemodialisis == "Si", 1, 0))

ANALITIC <- ANALITIC %>%
  left_join(mapeo_hemodialisis, by = "ID")

ANALITIC$Hemodialisis[is.na(ANALITIC$Hemodialisis)] <- 0

# ------- Cantidades -------

cantidad_ids  <- length(unique(ANALITIC$ID[ANALITIC$Hemodialisis == 1]))
print(paste('Cantidad de ids, pacientes, estan o han recibido un tratamiento de hemosdialisis: ', cantidad_ids))

cantidad_1s <- sum(ANALITIC$Hemodialisis == 1)
print(paste('Cantidad hemodialisis: ', cantidad_1s))

# ------------------- BUSQUEDA TRANSPLANTES (RENALES O RIÑON) -------------------
print('------------------- BUSQUEDA TRANSPLANTES (RENALES O RINON) -------------------')
# Bueno algo asi, pero como Lorena a encontrado mas nos quedaremos con esas

# Excluir la columna 'AntecFamiliaPedia' de la lista de columnas a considerar
columnas_de_interes <- setdiff(names(INFORM), "AntecFamiliaPedia")

# Asegurar que las columnas restantes estén en formato de texto (character)
INFORM[columnas_de_interes] <- lapply(INFORM[columnas_de_interes], as.character)

# Patrón de búsqueda para capturar variaciones de "transplante renal":
#  'transplante renal' o 'transplante de riñon' o ' transplante bilateral de riñon' o 'txr' o 'transplantectonomia' o 'transplante riñon' o 'tx renal' o ' tx riñon' o 'tx de riñon',
# y otros términos relacionados teniendo en cuenta mayúsculas, minúsculas y acentos
patron_busqueda <- "transplante\\s+renal|transplante\\s+de\\s+riñ[oó]n|transplante\\s+bilateral\\s+de\\s+riñ[oó]n|txr|transplantectomia|transplante\\s+riñ[oó]n|tx\\s+renal|tx\\s+riñ[oó]n|tx\\s+de\\s+riñ[oó]n"

# Función para buscar el patrón en una fila, excluyendo 'AntecFamiliaPedia'
buscar_transplante <- function(fila) {
  # Unir el texto de las columnas de interés (excluyendo AntecFamiliaPedia) en una sola cadena
  texto_unido <- tolower(paste(fila[columnas_de_interes], collapse = " "))

  # Buscar el patrón de transplante en el texto unido usando expresiones regulares
  if (str_detect(texto_unido, regex(patron_busqueda, ignore_case = TRUE))) {
    return(1)
  } else {
    return(0)
  }
}

# Aplicar la función a cada fila del dataframe INFORM para crear la nueva columna 'transplante'
# INFORM$transplante <- apply(INFORM, 1, buscar_transplante)

# ------- EXCEL GRUPO -------

INFORM$ID <- as.numeric(as.character(INFORM$ID))
EXCEL$ID <- as.numeric(as.character(EXCEL$ID))

mapeo_transplante <- EXCEL %>%
  select(ID, Transplante) %>%
  distinct() %>%
  mutate(Transplante = ifelse(Transplante == "Si", 1, 0))

ANALITIC <- ANALITIC %>%
  left_join(mapeo_transplante, by = "ID")

ANALITIC$Transplante[is.na(ANALITIC$Transplante)] <- 0

# ------- Cantidades -------

# Contar IDs únicos con al menos una mención de transplante
cantidad_ids <- length(unique(ANALITIC$ID[ANALITIC$Transplante == 1]))
print(paste('Cantidad de ids, pacientes, han tenido un transplante: ', cantidad_ids))

cantidad_1s <- sum(ANALITIC$Transplante)
print(paste('Cantidad transplante: ', cantidad_1s))

# ------------------- FALLECIDO -------------------
print('------------------- FALLECIDO -------------------')

ANALITIC <- ANALITIC %>%
  mutate(Fallecido = ifelse(is.na(FFallecido) | FFallecido == "", 0, 1))
print('================================= INICIO INFORMES =================================')

# ------------------- LIBRERIAS -------------------

library(tidyverse)
library(stringr)
library(parallel)
library(dplyr)
library(ggplot2)
library(lme4)
library(readxl)

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
# baseurl <- "D:/Documentos/Universidad/TFG/"

# ------------- DATOS -------------

ANALITIC <- read.csv(paste0(baseurl, "data/ANALITIC_1.csv"), sep = ",", header = TRUE)

EXCEL <- read_excel(paste0(baseurl, "Grupo/analiticas_filtradas.xlsx"))

INFORM <- read.csv(paste0(baseurl, "Datos_informe.csv"), sep = ";", header = TRUE)

RENTA <- read.csv(paste0(baseurl, "renta_cp.csv"), sep = ";", header = TRUE)
colnames(RENTA)[1] <-"renta2021"

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
INFORM$CodigoPostalResidencia <- as.numeric(as.character(INFORM$CodigoPostalResidencia))
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


ANALITIC <- ANALITIC %>%
  arrange(ID, fechatoma)

# ------------------- LIMPIEZA FINAL -------------------
print('------------------- LIMPIEZA FINAL -------------------')

ANALITIC <- ANALITIC %>%
  select(-c(gidenpac, CENTRO, FFECCITA, GPRESTAC, descprestacion, FFallecido))

ANALITIC$ITIPSEXO <- ifelse(ANALITIC$ITIPSEXO == "H", 0, 1) # 0 si es Hombre y 1 si es Mujer

ANALITIC <- ANALITIC %>%
  relocate("Cociente.Album.Creat", .after = "fechatoma")
ANALITIC <- ANALITIC %>%
  relocate("ITIPSEXO", .after = "fechatoma")
ANALITIC <- ANALITIC %>%
  relocate("FGE", .after = "ID")
ANALITIC <- ANALITIC %>%
  relocate("FGE2", .after = "FGE")
ANALITIC <- ANALITIC %>%
  relocate("Hemodialisis", .after = "FGE2")
ANALITIC <- ANALITIC %>%
  relocate("Transplante", .after = "Hemodialisis")
ANALITIC <- ANALITIC %>%
  relocate("Fallecido", .after = "Transplante")

# ------------------- GRAFICAS -------------------
print('------------------- GRAFICAS -------------------')

datos_ausentes <- sapply(ANALITIC, function(x) sum(is.na(x)))
datos_ausentes_df <- data.frame(columna = names(datos_ausentes), ausentes = datos_ausentes)
g <- ggplot(datos_ausentes_df, aes(x = reorder(columna, -ausentes), y = ausentes)) +
  geom_bar(stat = "identity", fill="#69b3a2", color="#e9ecef") +
  coord_flip() +
  labs(title = "Datos Ausentes por Columna", x = "Variable", y = "Datos Ausentes")

ggsave(paste0(baseurl, "Graficas/Cleaning/Ausentes.png"), plot = g, width = 18, height = 12, dpi = 300)

# DATOS POR ESTADO
condiciones <- list(
  "Hemodialisis" = ANALITIC[ANALITIC$Hemodialisis == 1 & ANALITIC$Transplante == 0, ],
  "Transplante" = ANALITIC[ANALITIC$Hemodialisis == 0 & ANALITIC$Transplante == 1, ],
  "Ambas" = ANALITIC[ANALITIC$Hemodialisis == 1 & ANALITIC$Transplante == 1, ],
  "Nada" = ANALITIC[ANALITIC$Hemodialisis == 0 & ANALITIC$Transplante == 0, ]
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
    labs(title = paste("Datos Presentes y Ausentes:", key),
         x = "Columnas",
         y = "Cantidad de Datos") +
    scale_fill_manual(values = c("steelblue", "tomato"))

  ggsave(paste0(baseurl, "Graficas/Cleaning/", key, ".png"), plot = g, width = 18, height = 12, dpi = 300)
}

# ------------------- RENTA -------------------
print('------------------- RENTA -------------------')

cp_unica <- INFORM %>%
  group_by(ID) %>%
  summarize(CodigoPostalResidencia = first(CodigoPostalResidencia))
# genero <- INFORM %>%
#   group_by(ID) %>%
#   summarize(ITIPSEXO = first(ITIPSEXO))
# genero$ITIPSEXO <- ifelse(genero$ITIPSEXO == "H", 0, 1) # 0 si es HOMBRE y 1 si es MUJER

ANALITIC <- ANALITIC %>%
  left_join(cp_unica, by = "ID")

ANALITIC <- ANALITIC %>%
  left_join(RENTA, by = "CodigoPostalResidencia")

ANALITIC <- ANALITIC %>%
  select(-c(CodigoPostalResidencia))

# ------------------- EXPORTAR -------------------
print('------------------- EXPORTAR -------------------')

write.csv(ANALITIC, paste0(baseurl, "data/ANALITIC_2.csv"), row.names = FALSE)

print('================================= FIN INFORMES =================================')
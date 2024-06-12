print('================================= INICIO ANALITICAS =================================')
# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
# baseurl <- "D:/Documentos/Universidad/TFG/"

ANALITIC <- read.csv(paste0(baseurl, "Analitics.csv"), sep = ";", header = TRUE)

# ------------------- LIBRERIAS -------------------

library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(lme4)
library(mice)
library(parallel)

# ------------------- CORRECCIÓN DE NOMBRES -------------------
print('------------------- CORRECCIÓN DE NOMBRES -------------------')

namecol <- names(ANALITIC)

namecol1 <- gsub("X..", "Porc", namecol)
# Reemplaza múltiples puntos con uno solo
# IMPORTANTE!! SI NO SE TIENE LA LIBRERIA STRINGR NO FUNCIONARÁ
namecol1 <- str_replace_all(namecol1, "\\.{2,}", ".")
# Elimina el punto final si lo hay
namecol1 <- str_replace_all(namecol1, "\\.$", "")
# Quitar las tildes y las ñ (por n) para eliminar problemas al llamar a las columnas
namecol1 <- iconv(namecol1, "UTF-8", "ASCII//TRANSLIT")

print(namecol1)
colnames(ANALITIC) <- namecol1

### ===> CONVERSIÓN A NUMERIC DE LA COLUMNAS (ES DECIR) CORRECCION DE LA TIPOLOGIA A LA CORRECTA

# Conversión a de string a números
columnas_numericas <- c(1, 5, 11:ncol(ANALITIC))
suppressWarnings({
  ANALITIC[, columnas_numericas] <- apply(ANALITIC[, columnas_numericas], 2, function(x) as.numeric(as.character(x)))
})

# Conversión a tipo fecha (funciona regular)
# FFECCITA, FFallecido y fechatoma
# columnas_fechas <- c(6, 9, 10)
# suppressWarnings({
#   ANALITIC[, columnas_fechas] <- apply(ANALITIC[, columnas_fechas], 2, function(x) as.Date(as.character(x)))
# })
ANALITIC$FFECCITA <- as.Date(ANALITIC$FFECCITA, format = "%d/%m/%Y")
ANALITIC$fechatoma <- as.Date(ANALITIC$fechatoma, format = "%d/%m/%Y")

#El resto son los textos

# ------------------- CAMBIAR VALORES RAROS A NA (-99999) -------------------
print('------------------- CAMBIAR VALORES RAROS A NA (-99999) -------------------')

# Lista de valores numéricos que quieres reemplazar por NA
valores_a_reemplazar <- c(-9999999) # Añade aquí cualquier otro valor que necesites

# Reemplaza los valores en las columnas desde la 11 en adelante
ANALITIC <- ANALITIC %>%
  mutate(across(11:ncol(.), ~replace(., . %in% valores_a_reemplazar, NA)))

# ------------------- NULOS -------------------
print('------------------- NULOS -------------------')

ncolumnas <- ncol(ANALITIC)
print(paste('Número de columnas: ', ncolumnas))

nulos_por_columna <- colSums(is.na(ANALITIC))

# Calcula el porcentaje de valores nulos en cada columna
porcentaje_nulos_por_columna <- nulos_por_columna / nrow(ANALITIC) * 100

# Crear un dataframe con la información
DF_nulos <- data.frame(
  Columna = names(ANALITIC),
  Porcentaje = porcentaje_nulos_por_columna
)

# Filtra para incluir solo columnas con nulos
DF_nulos <- DF_nulos[DF_nulos$Porcentaje > 0, ]

# Gráfico de barras para el porcentaje de valores nulos
grafico_barras <- ggplot(DF_nulos, aes(x = reorder(Columna, Porcentaje), y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Porcentaje de Valores Nulos por Columna", x = "", y = "Porcentaje") +
  theme_minimal() +
  coord_flip() # Voltea el gráfico para mejor visualización de los nombres de las columnas

print(grafico_barras)

print('------- quitar >= 90% nulos -------')
# Antes de la eliminación, imprimir el número de columnas
ncolumnas_antes <- ncol(ANALITIC)
print(paste('Número de columnas antes: ', ncolumnas_antes))

# Identifica las columnas que tienen 90% o más de datos faltantes
columnas_con_altos_nulos <- names(ANALITIC)[porcentaje_nulos_por_columna >= 90]

# Elimina esas columnas de ANALITIC
ANALITIC <- ANALITIC[, !(names(ANALITIC) %in% columnas_con_altos_nulos)]

# Después de la eliminación, imprimir el número de columnas
ncolumnas_despues <- ncol(ANALITIC)
print(paste('Número de columnas después: ', ncolumnas_despues))

# ------------------- ATIPICOS -------------------
print('------------------- ATIPICOS -------------------')

# for(i in 11:ncol(ANALITIC)) {
#   nombre_columna <- names(ANALITIC)[i]
#
#   print(paste("Resumen para la columna:", nombre_columna))
#   print(summary(ANALITIC[[nombre_columna]]))
#
#   # Filtra para excluir valores NA o no finitos en la columna actual
#   datos_filtrados <- ANALITIC %>% filter(!is.na(.[[nombre_columna]]) & is.finite(.[[nombre_columna]]))
#
#   # Verifica que después del filtrado quedan suficientes datos para graficar
#   if(nrow(datos_filtrados) > 0) {
#     p <- ggplot(datos_filtrados, aes_string(x = "factor(1)", y = nombre_columna)) +
#       geom_boxplot() +
#       labs(title = paste("Boxplot de", nombre_columna), y = nombre_columna, x = "") +
#       theme_minimal() +
#       theme(axis.title.x=element_blank(),
#             axis.text.x=element_blank(),
#             axis.ticks.x=element_blank())
#
#     print(p)
#   }
# }

# ------------------- Rellenado de Datos con MICE -------------------
print('------------------- Rellenado de Datos con MICE -------------------')
# computacionalmente imposible por ahora pero no estaria de mas la verdad

# # Aplica mice con el método 'cart'
# # mice_analitics <- mice(ANALITIC, m=3, method='cart', seed=123)
#
# # Detectar el número de núcleos lógicos
# num_cores <- detectCores(logical = TRUE)
#
# # Crear un clúster con un núcleo menos que el total para dejar recursos para el sistema
# cl <- makeCluster(num_cores - 1)
#
# # Usar el clúster para paralelizar mice
# mice_data <- mice(ANALITIC, m=3, method='cart', seed=123, maxit=3, cluster=cl)
#
# # Detener el clúster una vez completada la imputación
# stopCluster(cl)
#
# # Selecciona un conjunto imputado
# df_analitic_1 <- complete(mice_analitics, 1)
# df_analitic_2 <- complete(mice_analitics, 2)
# df_analitic_3 <- complete(mice_analitics, 3)
# df_analitic_4 <- complete(mice_analitics, 4)
# df_analitic_5 <- complete(mice_analitics, 5)

# ------------------- Filtrado glomerular estimado -------------------
print('------------------- Filtrado glomerular estimado -------------------')

# Crear una nueva columna "FGE" con valores predeterminados
ANALITIC$FGE <- 0
ANALITIC$Creatinina <- as.numeric(ANALITIC$Creatinina) ##SUELTA UN WARNING por convertir a numero un valor nulo

# !is.na(ANALITIC$Creatinina) ## Imprime que valores de Creatinina son nulos (sin el no se puede hacer la formula) SI HAY VALORES NULOS

# Aplicar la condición usando ifelse para las MUJERES
ANALITIC$FGE <- ifelse(ANALITIC$ITIPSEXO == "M",
                       (141 * pmin(ANALITIC$Creatinina / 0.7, 1)^(-0.329) * pmax(ANALITIC$Creatinina / 0.7, 1)^(-1.209) * 0.993^(ANALITIC$Edad) * 1.018),
                       ANALITIC$FGE)

# Aplicar la condición usando ifelse para las HOMBRES
ANALITIC$FGE <- ifelse(ANALITIC$ITIPSEXO == "H",
                       (141 * pmin(ANALITIC$Creatinina / 0.9, 1)^(-0.411) * pmax(ANALITIC$Creatinina / 0.9, 1)^(-1.209) * 0.993^(ANALITIC$Edad) * 1.018),
                       ANALITIC$FGE)

#Nulos y a 0 del FGE para futura limpieza de columnas
NullFGE <- sum(is.na(ANALITIC$FGE))
ZeroFGE <- sum(ANALITIC$FGE == 0)
print(paste("Valores nulos de FGE: ", NullFGE, " Cantidad de valores a 0 en FGE: ", ZeroFGE))

# ------------------- Cociente albuminuria/creatinina en orina -------------------
print('------------------- Cociente albuminuria/creatinina en orina -------------------')

#Calculamos el Cociente albuminuria/creatinina en orina
ANALITIC$Cociente.Album.Creat <- 0

ANALITIC$Cociente.Album.Creat <- ifelse(!(is.na(ANALITIC$Albumina)) & !(is.na(ANALITIC$Creatinina.orina)),
                                        (ANALITIC$Albumina / ANALITIC$Creatinina.orina),
                                        ANALITIC$Cociente.Album.Creat)

#Cambiamos los que estan a 0 a nulos, dado que son lo mismo y para futura limpieza y control
ANALITIC$Cociente.Album.Creat <- ifelse(ANALITIC$Cociente.Album.Creat == 0, NA, ANALITIC$Cociente.Album.Creat)

#Nulos y a 0 del FGE para futura limpieza de columnas
NullCC <- sum(is.na(ANALITIC$Cociente.Album.Creat))
ZeroCC <- sum(ANALITIC$Cociente.Album.Creat == 0)
print(paste("Valores nulos del Cociente de albuminuria: ", NullCC, " Cantidad de valores a 0 en el Cociente de albuminaria: ", ZeroCC))

# ANALITIC <- ANALITIC %>%
#   select(
#     1:3, # Selecciona las primeras tres columnas para mantenerlas en su lugar
#     ncol(.)-1, ncol(.), # Selecciona las últimas dos columnas para moverlas
#     4:(ncol(.)-2) # Selecciona el resto de las columnas para moverlas después de las últimas dos
#   )

# ------------------- LIMPIEZA FILAS -------------------
print('------------------- LIMPIEZA FILAS -------------------')
#Dado que sin FGE o el cociente albuniaria es casi imposible media la evolución, eliminas las filas que carecen de ambas
# puesto que a priori para el estudio de la evolucion de los pacientes y su correlación

print(paste("Número de filas que tiene tanto el FGE como el Cociente nulos: ", sum(is.na(ANALITIC$Cociente.Album.Creat) & is.na(ANALITIC$FGE))))
print(paste("Tamaño de filas original de Analitics: ", nrow(ANALITIC)))

ANALITIC <- ANALITIC %>%
  filter(!(is.na(ANALITIC$Cociente.Album.Creat) & is.na(ANALITIC$FGE)))

print(paste("Tamaño tras el filtrado: ", nrow(ANALITIC)))

# ------------------- ID's NO COINCIDENTES -------------------
print('------------------- IDs NO COINCIDENTES -------------------')
#Para los ID's que no coinciden con el otro excel, y por lo tanto, faltan datos de los mismos

# Leer los ID's del archivo de texto
ids_texto <- readLines(paste0(baseurl, "IDs.txt"))

# Convertir a numérico si los ID's son numéricos
ids_texto <- as.numeric(ids_texto)
print(paste("Numero de Ids Coincidentes: ", length(ids_texto)))
print(paste("Numero de Ids distintos en el dataframe: ", length(unique(ANALITIC$ID))))
# Claramente hay Id's que no estan en la otra hoja, por lo que eliminamos estos id's para que coincidan con la otra tabla de datos

ANALITIC <- ANALITIC[ANALITIC$ID %in% ids_texto, ]
print(paste("Numero de Ids distintos en el dataframe(tras el filtrado): ", length(unique(ANALITIC$ID))))

# ------------------- FGE ANALISIS -------------------
print('------------------- FGE ANALISIS -------------------')

#               grafica por paciente

# #agrupamos por ID y fechatoma para ver la evolución por paciente de este valor
# ANALITIC <- ANALITIC %>%
#   arrange(ID, fechatoma)
#
# #Creamos un bucle para para crear un plot por paciente para ver las evoluciones
# unique_ids <- unique(ANALITIC$ID)
#
# for(patient_id in unique_ids) {
#   patient_data <- ANALITIC[ANALITIC$ID == patient_id & !is.na(ANALITIC$FGE), ]
#
#   #Contamos cuantas tomas hay
#   num_dates <- length(unique(patient_data$fechatoma))
#
#   # Y solo usamos los que tienen 5 o mas (menos parace un poco absurdo para ver la evolución teniendo tantos datos)
#   if(num_dates >= 5) {
#     p <- ggplot(patient_data, aes(x = fechatoma, y = FGE, group = ID)) +
#       geom_line(colour="#000099") +
#       geom_point(colour="#CC0000") +
#       labs(title = paste("Evolution of FGE for Patient", patient_id),
#            x = "Date of Sample (fechatoma)",
#            y = "Filtrado Glomerular Estimado (FGE)") +
#       theme_minimal()
#     # Save the plot
#     ggsave(paste("D:/gugui/Documentos/Universidad/TFG/Evoluciones_FGE/FGE_evolution_patient_", patient_id, ".png", sep = ""), plot = p, width = 10, height = 6)
#   }
# }

#                 grafica conjunta

# # Preparamos la información y cogemos (como antes) solo aquellos que tengas al menos 5 tomas
# filtered_data <- ANALITIC %>%
#   group_by(ID) %>%
#   filter(!is.na(FGE) & n() >= 5) %>%
#   ungroup()
#
# p <- ggplot(filtered_data, aes(x = fechatoma, y = FGE, group = ID, color = as.factor(ID))) +
#   geom_line(alpha = 0.5) + # Set transparency to make overlapping lines more readable
#   geom_point(alpha = 0.5, size = 1) + # Optional: add points with some transparency
#   labs(title = "Evolution of FGE Across Patients",
#        x = "Date of Sample (fechatoma)",
#        y = "Filtrado Glomerular Estimado (FGE)",
#        color = "Patient ID") +
#   theme_minimal() +
#   theme(
#     legend.position = "none" # Hide the legend to avoid clutter
#   ) +
#   scale_color_viridis_d() # Use a discrete color scale for better visibility
# # ggsave(paste("D:/gugui/Documentos/Universidad/TFG/Evoluciones_FGE/Evolution of FGE Across Patients", patient_id, ".png", sep = ""), plot = p, width = 10, height = 6)
# ggsave("D:/Documentos/Universidad/TFG/Graficas/Evolution of FGE Across Patients.png", plot = p, width = 10, height = 6)


# ------------------- CORRELACIONES -------------------
print('------------------- CORRELACIONES -------------------')

# Modelo lineal mixto
# for(i in 11:ncol(ANALITIC)) {
#   nombre_columna <- names(ANALITIC)[i]
#   modelo <- lmer(FGE ~ nombre_columna + fechatoma + (1 | ID), data = ANALITIC)
# }
#
# # Ver resumen del modelo
# summary(modelo)

# ------------------- EXPORTAR -------------------
print('------------------- EXPORTAR -------------------')
# write.csv(ANALITIC, "D:/gugui/Documentos/Universidad/TFG/NewANALITIC.csv", row.names = FALSE)
write.csv(ANALITIC, paste0(baseurl, "data/ANALITIC_1.csv"), row.names = FALSE)

print('================================= FIN ANALITICAS =================================')

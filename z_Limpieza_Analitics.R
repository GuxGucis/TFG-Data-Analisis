
# Para la torre
ANALITIC <- read.csv("D:/gugui/Documentos/Universidad/TFG/Analitics.csv", sep = ";", header = TRUE)

# Para el portatil
# ANALITIC <- read.csv("D:/Documentos/Universidad/TFG/Analitics.csv", sep = ";", header = TRUE)

library(tidyverse)
library(dplyr)
library(ggplot2)

# summary(ANALITIC)

# - Creatinina sérica (a más alto, peor evolución)  --> Creatinina

# - Cociente albuminuria/creatinina en orina (a más alto, peor evolución) --> Albúmina/Creatinina.orina

# - Filtrado glomerular estimado (fórmula CKD-EPI) (a más bajo, peor evolución)
#     - Filtrado glomerular = 141 × min(Scr/κ , 1)^α × max(Scr/κ , 1)^−1,209 × 0,993^edad × 1,018 (si es mujer)
#       × 1,159 (si es de raza negra)

#           o Scr: creatinina sérica
#           o κ: 0,7 si es mujer y 0,9 si es hombre
#           o α: −0,329 si es mujer y −0,411 si es hombre
#           o min(Scr/κ , 1): el valor que sea menor entre Scr/κ y 1
#           o max(Scr/κ , 1): el valor que sea mayor entre Scr/κ y 1

#===========================LIMPIEZA DE LOS NOMBRES=================================

# print("---------------------PRE LIMPIEZA DE COLUMNAS--------------------------------")
# print(names(ANALITIC))

namecol <- names(ANALITIC)

namecol1 <- gsub("X..", "Porc", namecol)
# Reemplaza múltiples puntos con uno solo
# IMPORTANTE!! SI NO SE TIENE LA LIBRERIA STRINGR NO FUNCIONARÁ
namecol1 <- str_replace_all(namecol1, "\\.{2,}", ".")
# Elimina el punto final si lo hay
namecol1 <- str_replace_all(namecol1, "\\.$", "")
# Quitar las tildes y las ñ (por n) para eliminar problemas al llamar a las columnas
namecol1 <- iconv(namecol1, "UTF-8", "ASCII//TRANSLIT")

print("--------------------- LIMPIEZA DE COLUMNAS --------------------------------")
#
# print(namecol1)

colnames(ANALITIC) <- namecol1

### ===> CONVERSIÓN A NUMERIC DE LA COLUMNAS (ES DECIR) CORRECCION DE LA TIPOLOGIA A LA CORRECTA

columnas_numericas <- c(1, 5, 11:ncol(ANALITIC))
suppressWarnings({
  ANALITIC[, columnas_numericas] <- apply(ANALITIC[, columnas_numericas], 2, function(x) as.numeric(as.character(x)))
})## Para quitar los warnings por valores nulos

columnas_fechas <- c(6, 9, 10)
suppressWarnings({
  ANALITIC[, columnas_fechas] <- apply(ANALITIC[, columnas_fechas], 2, function(x) as.Date(as.character(x)))
})## Para quitar los warnings por valores nulos

#===========================Estado Columnas=================================
print("--------------------- Estado Columnas --------------------------------")

nulos_por_columna <- colSums(is.na(ANALITIC))

# Encuentra las columnas que tienen al menos un valor nulo
columnas_con_nulos <- which(nulos_por_columna > 0)

# Calcula el porcentaje de valores nulos en cada columna
porcentaje_nulos_por_columna <- nulos_por_columna[columnas_con_nulos] / nrow(ANALITIC) * 100

# Número de filas en el dataframe
ncolumnas <- nrow(ANALITIC)
# print(paste("El numero de filas es de:", ncolumnas))
#nulos_por_columna[columnas_con_nulos]

null50 <- which((nulos_por_columna[columnas_con_nulos] / nrow(ANALITIC) * 100) >= 50)
null90 <- which((nulos_por_columna[columnas_con_nulos] / nrow(ANALITIC) * 100) >= 90)

# --> # Imprime las columnas con valores nulos, la cantidad de valores nulos y el porcentaje
print(paste("El numero de columnas con totales es de: ", length(ANALITIC), "La cantidad de columnas con valores nulos es: ", length(columnas_con_nulos)))
print(paste("Cantidad de columnas con el 50% o mas de valores nulos: ", length(null50), "Columnas el 90% o mas de los valores nulos: ", length(null90)))
# print(paste("Columna", names(columnas_con_nulos), "; Nulos =", nulos_por_columna[columnas_con_nulos], "; Porcentaje = ", (nulos_por_columna[columnas_con_nulos]/ncolumnas)*100))

## ggplot2 !!!!!!!!!!

# Crear un dataframe con la información
datos_grafico <- data.frame(
  Columna = names(nulos_por_columna),
  Nulos = nulos_por_columna,
  Porcentaje = as.numeric((nulos_por_columna/ncolumnas)*100)
)

# Gráfico de barras para la cantidad de valores nulos
grafico_barras <- ggplot(datos_grafico, aes(x = Columna, y = Nulos)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Cantidad de Valores Nulos por Columna", x = "Columna", y = "Numero de Nulos") +
  theme_minimal()

# Gráfico de líneas para el porcentaje de valores nulos
grafico_lineas <- ggplot(datos_grafico, aes(x = Columna, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Porcentaje de Valores Nulos por Columna", x = "Columna", y = "Porcentaje") +
  theme_minimal()

# # Mostrar ambos gráficos
print(grafico_barras)
print(grafico_lineas)

# ===> #  Se pueden observar que hay columnas con más del 90% de los datos vacios, posiblemente estos se puedan eliminar
        # dado que es probable que estos no aporten nada a al analisis. Por si acaso por el momento no se eliminan pero son canditatos a ser borrados.
        # Se precisaría de un análisis de correlación para ver como influyen estos parámetros realmente.
        # Ademas se observa que todas menos diez de las columnas tienen valores nulos, la mayoria superando el 50%

#===========================Filtrado glomerular estimado=================================
print("--------------------- Filtrado glomerular estimado --------------------------------")

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

#Imprime los valores de que son nulos en FGE y no en Creatinina (que es el unico facto que puede producir nulos (dado que en edad no hay nulos))
# print(ANALITIC[is.na(ANALITIC$FGE) & !(is.na(ANALITIC$Creatinina)), c("ID", "Creatinina", "FGE")])
NullFGE <- sum(is.na(ANALITIC$FGE))
ZeroFGE <- sum(ANALITIC$FGE == 0)
print(paste("Valores nulos de FGE: ", NullFGE, " Cantidad de valores a 0 en FGE: ", ZeroFGE))

# ===> # Da la casualidad de que cuando se crea la columna para el FGE se observa que hay 13 nulos de mas con respecto a la Creatinina, posiblemente porque hay
        # valores de la misma a "-9999999" que producen un valor nulo seguramente en la operación. ¿Que significa la Creatinina a "-9999999"

# ANALITIC$FGE  ## Imprime la nueva columna con los resultados del filtrado glomerular estimado

# ==== >#Da la impresión que se rellenan aquellos con Creatinina --> buscar si para el resto hay otro valor o algo o si no usar el de la columna FG que hay en informes??

#===========================Cociente albuminuria/creatinina en orina=================================
print("--------------------- Cociente albuminuria/creatinina en orina --------------------------------")

#Visualizamos como estan los datos necesarios a usar
# print(paste("El numero de valores de Albuminaria: ", length(ANALITIC$Albumina), " de los cuales nulos son: ", sum(is.na(ANALITIC$Albumina))))
# print(paste("El numero de valores de Creatinina en orina: ", length(ANALITIC$Creatinina.orina), " de los cuales nulos son: ", sum(is.na(ANALITIC$Creatinina.orina))))

# ==> # No hay mucho donde tirar con este cociente dado que hay gran cantidad de nulos entre ambos, pero lo crearemos igualemente por si aporta información a futuro
      # Aunque parece que ya existe un cociente usando la microalbuminaria, por si las moscas creamos este
      # Es ese valor de ANALITIC$Cociente.Microalbuminaria.Creatinina el que nos mencionan??

ANALITIC$Cociente.Album.Creat <- 0

ANALITIC$Cociente.Album.Creat <- ifelse(!(is.na(ANALITIC$Albumina)) & !(is.na(ANALITIC$Creatinina.orina)),
                       (ANALITIC$Albumina / ANALITIC$Creatinina.orina),
                       ANALITIC$Cociente.Album.Creat)

# NullCC <- sum(is.na(ANALITIC$Cociente.Album.Creat))
# ZeroCC <- sum(ANALITIC$Cociente.Album.Creat == 0)
# print(paste("Valores nulos del Cociente de albuminuria: ", NullCC, " Cantidad de valores a 0 en el Cociente de albuminaria: ", ZeroCC))

ANALITIC$Cociente.Album.Creat <- ifelse(ANALITIC$Cociente.Album.Creat == 0, NA, ANALITIC$Cociente.Album.Creat)

NullCC <- sum(is.na(ANALITIC$Cociente.Album.Creat))
ZeroCC <- sum(ANALITIC$Cociente.Album.Creat == 0)
print(paste("Valores nulos del Cociente de albuminuria: ", NullCC, " Cantidad de valores a 0 en el Cociente de albuminaria: ", ZeroCC))

NullFGECC <- sum(is.na(ANALITIC$FGE) & is.na(ANALITIC$Cociente.Album.Creat))
print(paste("Cantidad en la que ambos son nulos, tanto FGE con el Cociente de albuminuria: ", NullFGECC))

#=========================== QUITAR COSAS =================================
print("--------------------- Quitamos FGE y Cociente nulos --------------------------------")

cat("Número de filas que tiene tanto el FGE como el Cociente nulos: ", sum(is.na(ANALITIC$Cociente.Album.Creat) & is.na(ANALITIC$FGE)), "\n")
cat("Tamaño original de Analitics: ", nrow(ANALITIC), "\n")
# Vemos que hay filas que tienen ambos valores nulos, por que procedemos a quitarlos (sin ninguno de los dos no hacemos mucho

ANALITIC <- ANALITIC %>%
  filter(!(is.na(ANALITIC$Cociente.Album.Creat) & is.na(ANALITIC$FGE)))

cat("Tamaño tras el filtrado: ", nrow(ANALITIC), "\n")


print("--------------------- Quitamos ID's que no coincidan con la otra pagina --------------------------------")

# Leer los ID's del archivo de texto
ids_texto <- readLines("D:/gugui/Documentos/Universidad/TFG/IDs.txt")

# Convertir a numérico si los ID's son numéricos
ids_texto <- as.numeric(ids_texto)
cat("Numero de Ids Coincidentes: ", length(ids_texto), "\n")
cat("Numero de Ids distintos en el dataframe: ", length(unique(ANALITIC$ID)), "\n")
# Claramente hay Id's que no estan en la otra hoja, por lo que eliminamos estos id's para que coincidan con la otra tabla de datos

ANALITIC <- ANALITIC[ANALITIC$ID %in% ids_texto, ]
cat("Numero de Ids distintos en el dataframe(tras el filtrado): ", length(unique(ANALITIC$ID)), "\n")

print("--------------------- Max y mins --------------------------------")

# Calcular el mínimo de cada columna
minimos_por_columna <- sapply(ANALITIC, min, na.rm = TRUE)

# Calcular el máximo de cada columna
maximos_por_columna <- sapply(ANALITIC, max, na.rm = TRUE)

# Imprimir los mínimos y máximos
print(minimos_por_columna)
print(maximos_por_columna)

print("--------------------- Columnas (casi) Vacias --------------------------------")
# Se checkeo antes el estado
# En principio nos vamos a centrar en aquellos que tengan el 90% vacio (veremos si nos lo quedamos)

# Calcular el porcentaje de datos nulos por columna
porcentaje_nulos <- sapply(ANALITIC, function(columna) sum(is.na(columna)) / nrow(ANALITIC))

# Identificar las columnas con al menos el 90% de datos nulos
columnas_mayor90_nulos <- names(porcentaje_nulos[porcentaje_nulos >= 0.95])

# Imprimir los nombres de estas columnas
print(columnas_mayor90_nulos)

# Obtener los índices (números) de las columnas con más del 90% de datos nulos
# indices_columnas_mayor90_nulos <- which(porcentaje_nulos >= 0.9)

# Imprimir los índices de estas columnas
# print(indices_columnas_mayor90_nulos)

cantidad_columnas_mayor90_nulos <- sum(porcentaje_nulos >= 0.95)

# Imprimir la cantidad de estas columnas
cat("Cantidad de columnas con el 95% o mas de datos vacíos",cantidad_columnas_mayor90_nulos)

#=========================== EDA =================================
print("--------------------- EDA --------------------------------")

# Examinar estadísticas básicas de algunas columnas clínicas clave
# Empezando con la edad
# ------------> summary(ANALITIC$Edad)

# Revisar también estadísticas para otras variables clínicas importantes
# Suponiendo que estas columnas existen en el conjunto de datos, ajusta según tus columnas
variables_clinicas <- c('Creatinina', 'FGE', 'Cociente.Album.Creat')  # Ejemplos de columnas
# ------------> summary(ANALITIC[variables_clinicas])

# Filtrar la columna 'Creatinina' para excluir valores extremos
creatinina_filtrada <- ANALITIC %>% filter(Creatinina >= 0.5, Creatinina <= 20) %>% pull(Creatinina)

# Crear un gráfico combinado con histograma y boxplot
par(mfrow = c(1, 2))

# Histograma
hist(creatinina_filtrada, main="Histograma de Creatinina (Filtrado)", xlab="Creatinina", ylab="Frecuencia", col="lightblue", border="black")

# Boxplot
boxplot(creatinina_filtrada, horizontal=TRUE, main="Boxplot de Creatinina (Filtrado)", xlab="Creatinina")

# Restaurar configuración gráfica
par(mfrow = c(1, 1))

# Filtrar los datos para excluir valores extremadamente atípicos en 'FGE'
datos_filtrados <- ANALITIC %>% filter(FGE >= 0 & FGE <= 150)

# Histograma y Boxplot para 'FGE'
par(mfrow = c(1, 2))

# Histograma
hist(datos_filtrados$FGE, main = "Histograma de FGE (Filtrado)", xlab = "FGE")

# Boxplot
boxplot(datos_filtrados$FGE, main = "Boxplot de FGE (Filtrado)", ylab = "FGE")

#=========================== Exportar =================================
print("--------------------- Exportar --------------------------------")

# Selecting only the required columns
data_to_export <- ANALITIC[, c("ID", "FFECCITA", "fechatoma", "Creatinina", "Creatinina.orina", "Cociente.Album.Creat", "FGE")]

# Exporting to CSV
write.csv(data_to_export, "D:/gugui/Documentos/Universidad/TFG/ExportedData.csv", row.names = FALSE)

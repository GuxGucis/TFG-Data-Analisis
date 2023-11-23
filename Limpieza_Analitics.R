
# Para la torre
#ANALITIC <- read.csv("D:/gugui/Documentos/Universidad/TFG/Analitics.csv", sep = ";", header = TRUE)

# Para el portatil
ANALITIC <- read.csv("D:/Documentos/Universidad/TFG/Analitics.csv", sep = ";", header = TRUE)

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
namecol1 <- str_replace_all(namecol1, "\\.{2,}", ".")
# Elimina el punto final si lo hay
namecol1 <- str_replace_all(namecol1, "\\.$", "")

# print("---------------------LIMPIEZA DE COLUMNAS--------------------------------")
# #
# print(namecol1)

colnames(ANALITIC) <- namecol1

#===========================Estado Columnas=================================
print("---------------------Estado Columnas--------------------------------")

nulos_por_columna <- colSums(is.na(ANALITIC))

# Encuentra las columnas que tienen al menos un valor nulo
columnas_con_nulos <- which(nulos_por_columna > 0)

# Calcula el porcentaje de valores nulos en cada columna
porcentaje_nulos_por_columna <- nulos_por_columna[columnas_con_nulos] / nrow(ANALITIC) * 100

# Número de filas en el dataframe
ncolumnas <- nrow(ANALITIC)
# print(paste("El numero de filas es de:", ncolumnas))
#nulos_por_columna[columnas_con_nulos]

# Imprime las columnas con valores nulos, la cantidad de valores nulos y el porcentaje

# print(paste("Columna", names(columnas_con_nulos), "; Nulos =", nulos_por_columna[columnas_con_nulos], "; Porcentaje = ", (nulos_por_columna[columnas_con_nulos]/ncolumnas)*100))

#Se pueden observar que hay columnas con más del 90% de los datos vacios, posiblemente estos se puedan eliminar
# dado que es probable que estos no aporten nada a al analisis. Por si acaso por el momento no se eliminan pero son canditatos a ser borrados.
# Se precisaría de un análisis de correlación para ver como influyen estos parámetros realmente.

#===========================Filtrado glomerular estimado=================================
print("---------------------Filtrado glomerular estimado--------------------------------")

# Crear una nueva columna "FGE" con valores predeterminados
ANALITIC$FGE <- 0

ANALITIC$Creatinina <- as.numeric(ANALITIC$Creatinina)

!is.na(ANALITIC$Creatinina)

# Aplicar la condición usando ifelse para las MUJERES
ANALITIC$FGE <- ifelse(ANALITIC$ITIPSEXO == "M" & !is.na(ANALITIC$Creatinina),
                       (141 * pmin(ANALITIC$Creatinina / 0.7, 1)^(-0.329) * pmax(ANALITIC$Creatinina / 0.7, 1)^(-1.209) * 0.993^(ANALITIC$Edad) * 1.018),
                       ANALITIC$FGE)

# Aplicar la condición usando ifelse para las HOMBRES
ANALITIC$FGE <- ifelse(ANALITIC$ITIPSEXO == "H" & !is.na(ANALITIC$Creatinina),
                       (141 * pmin(ANALITIC$Creatinina / 0.9, 1)^(-0.411) * pmax(ANALITIC$Creatinina / 0.9, 1)^(-1.209) * 0.993^(ANALITIC$Edad) * 1.018),
                       ANALITIC$FGE)

ANALITIC$FGE

##Da la impresión que se rellenan aquellos con Creatinina --> buscar si para el resto hay otro valor o algo o si no usar el de la columna FG que hay en informes??
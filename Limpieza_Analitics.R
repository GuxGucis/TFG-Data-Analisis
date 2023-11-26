
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
# IMPORTANTE!! SI NO SE TIENE LA LIBRERIA STRINGR NO FUNCIONARÁ
namecol1 <- str_replace_all(namecol1, "\\.{2,}", ".")
# Elimina el punto final si lo hay
namecol1 <- str_replace_all(namecol1, "\\.$", "")
# Quitar las tildes y las ñ (por n) para eliminar problemas al llamar a las columnas
namecol1 <- iconv(namecol1, "UTF-8", "ASCII//TRANSLIT")

print("---------------------LIMPIEZA DE COLUMNAS--------------------------------")
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

null50 <- which((nulos_por_columna[columnas_con_nulos] / nrow(ANALITIC) * 100) >= 50)
null90 <- which((nulos_por_columna[columnas_con_nulos] / nrow(ANALITIC) * 100) >= 90)

# --> # Imprime las columnas con valores nulos, la cantidad de valores nulos y el porcentaje
# print(paste("El numero de columnas con totales es de: ", length(ANALITIC), "La cantidad de columnas con valores nulos es: ", length(columnas_con_nulos)))
# print(paste("Cantidad de columnas con el 50% o mas de valores nulos: ", length(null50), "Columnas el 90% o mas de los valores nulos: ", length(null90)))
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
# print(grafico_barras)
# print(grafico_lineas)

# ===> #  Se pueden observar que hay columnas con más del 90% de los datos vacios, posiblemente estos se puedan eliminar
        # dado que es probable que estos no aporten nada a al analisis. Por si acaso por el momento no se eliminan pero son canditatos a ser borrados.
        # Se precisaría de un análisis de correlación para ver como influyen estos parámetros realmente.
        # Ademas se observa que todas menos diez de las columnas tienen valores nulos, la mayoria superando el 50%

#===========================Filtrado glomerular estimado=================================
print("---------------------Filtrado glomerular estimado--------------------------------")

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

# ===> # Da la casualidad de que cuando se crea la columna para el FGE se observa que hay 13 nulos de mas con respecto a la Creatinina, posiblemente porque hay
        # valores de la misma a "-9999999" que producen un valor nulo seguramente en la operación. ¿Que significa la Creatinina a "-9999999"

# ANALITIC$FGE  ## Imprime la nueva columna con los resultados del filtrado glomerular estimado

# ==== >#Da la impresión que se rellenan aquellos con Creatinina --> buscar si para el resto hay otro valor o algo o si no usar el de la columna FG que hay en informes??

#===========================Cociente albuminuria/creatinina en orina=================================
print("---------------------Cociente albuminuria/creatinina en orina--------------------------------")

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

#NO SE COMO SACAR LAS GRAFICAS POR PACIENTES PARA VER UN POCO LAS EVOLUCIONES CON ESE COCIENTE
#PERO NO HAY MANERA

# # Filtrar datos para IDs con más de 3 valores de cociente disponibles
# datos_filtrados <- ANALITIC %>%
#   group_by(ID) %>%
#   filter(sum(!is.na(Cociente.Album.Creat) & Cociente.Album.Creat != 0) > 3) %>%
#   ungroup()
#
# datos_filtrados$ID <- factor(datos_filtrados$ID)
#
# # Y los ordenamos segun el ID y la fecha de toma
# datos <- datos_filtrados[order(ANALITIC$ID, ANALITIC$fechatoma), ]
#
# # Crear gráfico de líneas por paciente
# CocienteXPaciente <- ggplot(datos, aes(x = fechatoma, y = Cociente.Album.Creat, group = ID, color = ID)) +
#   geom_line() +
#   labs(x = 'Fecha Toma', y = 'Cociente') +
#   theme_minimal() +
#   facet_wrap(~ID, scales = 'free_y')
#
# print(CocienteXPaciente)

ANALITIC <- read.csv("D:/gugui/Documentos/Universidad/TFG/Analitics.csv", sep = ";", header = TRUE)

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

namecol1 <- gsub("X.", "Porc", namecol)
# Reemplaza múltiples puntos con uno solo
namecol1 <- str_replace_all(namecol1, "\\.{2,}", ".")
# Elimina el punto final si lo hay
namecol1 <- str_replace_all(namecol1, "\\.$", "")

# print("---------------------LIMPIEZA DE COLUMNAS--------------------------------")
#
# print(namecol1)

colnames(ANALITIC) <- namecol1

#===========================Estado Columnas=================================
print("---------------------Estado Columnas--------------------------------")

nulos_por_columna <- colSums(is.na(ANALITIC))

# Encuentra las columnas que tienen al menos un valor nulo
columnas_con_nulos <- which(nulos_por_columna > 0)

# Número de columnas en el dataframe
print(paste("El numero de datos es de: ", nrow(ANALITIC)))

# Imprime solo las columnas con valores nulos
print(nulos_por_columna[columnas_con_nulos])
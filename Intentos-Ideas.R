#===========================POR PACIENTE=================================
print("---------------------Por paciente--------------------------------")

ANALITIC <- ANALITIC %>% arrange(ID, fechatoma)

pacientes_filtrados <- ANALITIC %>%
  group_by(ID) %>%
  filter(sum(!is.na(FGE)) > 7) %>%
  ungroup()

paciente_seleccionado <- pacientes_filtrados$ID[1]

# Filtrar el dataframe solo para el paciente seleccionado
datos_paciente <- pacientes_filtrados %>%
  filter(ID == paciente_seleccionado)

#===========================No se, algo de quitar=================================

datos_filtrados <- ANALITIC %>%
  group_by(ID) %>%
  filter(any(!is.na(FGE)) | any(!is.na(Cociente.Album.Creat)))

# Ver el tamaño del conjunto de datos original y del filtrado
cat("Tamaño original:", nrow(ANALITIC), "\n")
cat("Tamaño filtrado:", nrow(datos_filtrados), "\n")
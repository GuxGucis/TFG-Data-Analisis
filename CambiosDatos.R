
library(dplyr)

ANALITIC_1$FGE2 <- NULL
ANALITIC$ID <- as.integer(ANALITIC$ID)
ANALITIC_1$ID <- as.integer(ANALITIC_1$ID)
ANALITIC$fechatoma <- as.Date(ANALITIC$fechatoma)
ANALITIC_1$fechatoma <- as.Date(ANALITIC_1$fechatoma)

# Asegurémonos de que no hay duplicados en data1 para las combinaciones de ID y fechatoma
ANALITIC <- ANALITIC %>%
  distinct(ID, fechatoma, .keep_all = TRUE)

# Selecciona solo las columnas necesarias de data1
data_select <- ANALITIC %>% select(ID, fechatoma, FGE2)

# Utiliza left_join para añadir solo la columna FGE2 a data2 sin añadir filas ni columnas adicionales
ANALITIC_1 <- ANALITIC_1 %>%
  left_join(data_select, by = c("ID", "fechatoma"))

ANALITIC_1 <- ANALITIC_1 %>%
  relocate("FGE2", .after = "FGE")
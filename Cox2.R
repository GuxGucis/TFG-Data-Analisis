print('================================= MODELO DE COX =================================')

library(survival)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)

# ------------------- CARGADO DE DATOS -------------------

# ------------- TORRE -------------
# baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# ------------- PORTATIL -------------
baseurl <- "D:/Documentos/Universidad/TFG/"

# ------------------- Rellenado de Datos con MICE -------------------
print('------------------- Rellenado de Datos con MICE -------------------')
# computacionalmente imposible por ahora pero no estaria de mas la verdad

# Aplica mice con el método 'cart'
# mice_analitics <- mice(ANALITIC, m=3, method='cart', seed=123)

# # CONFIGURACIÓN PARA PODER USAR VARIOS HILOS
# # Detectar el número de núcleos lógicos
# num_cores <- detectCores(logical = TRUE)
#
# # Crear un clúster con un núcleo menos que el total para dejar recursos para el sistema
# cl <- makeCluster(num_cores - 2)
#
# # Usar el clúster para paralelizar mice
# df_cox_mice <- mice(df_cox, m=5, method='cart', seed=123, maxit=5, parallel = "snow", cluster=cl)
#
# # Detener el clúster una vez completada la imputación
# stopCluster(cl)
#
# # Selecciona un conjunto imputado
# df_cox_1 <- complete(df_cox_mice, 1)
# df_cox_2 <- complete(df_cox_mice, 2)
# df_cox_3 <- complete(df_cox_mice, 3)
# df_cox_4 <- complete(df_cox_mice, 4)
# df_cox_5 <- complete(df_cox_mice, 5)

# write.csv(df_cox_1, paste0(baseurl, "Mice/tend_mice_1.csv"), row.names = FALSE)
# write.csv(df_cox_2, paste0(baseurl, "Mice/tend_mice_2.csv"), row.names = FALSE)
# write.csv(df_cox_3, paste0(baseurl, "Mice/tend_mice_3.csv"), row.names = FALSE)
# write.csv(df_cox_4, paste0(baseurl, "Mice/tend_mice_4.csv"), row.names = FALSE)
# write.csv(df_cox_5, paste0(baseurl, "Mice/tend_mice_5.csv"), row.names = FALSE)
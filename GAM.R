# ------------------- MODELOS ADITIVOS GENERALIZADOS -------------------
print('------------------- MODELOS ADITIVOS GENERALIZADOS -------------------')

# Cargar los paquetes
library(mgcv)
library(survival)

ANALITIC <- ANALITIC %>%
  left_join(select(df_cox, ID, tiempo_total), by = "ID")

ANALITIC <- ANALITIC %>%
  left_join(select(df_km, ID, Estado), by = "ID")

# Revisar datos faltantes y decidir un método de manejo
# Por ejemplo, si decides eliminar filas con NA:
ANALITIC_GAM <- na.omit(ANALITIC)

# Suponiendo que tus datos tienen las columnas 'Tiempo', 'Evento' y 'Variable1'
# 'Tiempo' es el tiempo hasta el evento o censura,
# 'Evento' es un indicador de si ocurrió el evento (1) o si es censurado (0),
# y 'Variable1' es una variable explicativa.

# Ajustar un modelo de supervivencia GAM
modelo_gam <- gam(Surv(tiempo_total, FGE) ~ s(Estado), data = ANALITIC_GAM, family = cox.ph())

# Ver el resumen del modelo
summary(modelo_gam)
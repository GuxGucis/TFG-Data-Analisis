print('================================= MODELOS ADITIVOS GENERALIZADOS =================================')

library(mgcv)
library(survival)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)

# -------------------------------------------------------------------
# ------------------------ PREPARACIÓN ------------------------------
# -------------------------------------------------------------------
print('------------------- PREPARACIÓN -------------------')

# Introducimos columnas de informa para el modelo

df_gam <- ANALITIC

df_gam <- df_gam %>%
  left_join(select(df_cox, ID, tiempo_total), by = "ID")

df_gam <- df_gam %>%
  left_join(select(df_km, ID, Estado), by = "ID")

df_gam <- df_gam %>%
  select(-c("gidenpac", "GPRESTAC", "FFECCITA", "FFallecido", "Cociente.Album.Creat"))

df_gam <- df_gam %>%
  relocate("tiempo_total", .after = "fechatoma")
df_gam <- df_gam %>%
  relocate("FGE", .after = "tiempo_total")
df_gam <- df_gam %>%
  relocate("Hemodialisis", .after = "FGE")
df_gam <- df_gam %>%
  relocate("Transplante", .after = "Hemodialisis")
df_gam <- df_gam %>%
  relocate("Fallecido", .after = "Transplante")
df_gam <- df_gam %>%
  relocate("Estado", .after = "Fallecido")

# ----------- NULOS -----------
# Mediante el uso de la librería mice

# m=5 especifica que se generarán 5 conjuntos de datos completos (puedes ajustar este número según tus necesidades).
# method='pmm' usa el método de imputación por predicción media, que es adecuado para muchos tipos de datos. mice automáticamente selecciona el mejor método de imputación para cada columna basándose en el tipo de datos, pero puedes personalizar esto si es necesario.
# seed=123 asegura la reproducibilidad de tus resultados.

# Lo aplicamos solo en las columnas de datos analiticos, que es donde estan los nulos, cojiendo id y fecha para que no se pierda el orden de datos
# df_gam_data <- df_gam[, c(1, 5, 12:ncol(df_gam))]
# nombres_columnas <- names(df_gam_data)
# metodos <- rep("pmm", length(nombres_columnas))
# nombres_excluidos <- c("ID", "fechatoma")
# metodos[nombres_columnas %in% nombres_excluidos] <- ""
#
# mice_data_gam <- mice(df_gam_data, m=5, method=metodos, seed=123)
#
# selected_group <- complete(mice_data, 1)
# df_gam_1 <- merge(df_gam[, 1:11], selected_group, by=c("ID", "fechatoma"))

mice_gam <- mice(df_gam, m=5, method="cart", seed=500)
df_gam_1 <- complete(mice_gam, 1)

# Posible metodo para hacer una prelimpieza, que es teniendo un datos nulo entre datos rellenos, se rellena con la media de los otros dos.
zdf_gam <- df_gam %>%
  group_by(ID) %>%
  mutate(across(.cols = where(is.numeric), # Aplica la imputación solo a columnas numéricas
                .fns = ~ ifelse(is.na(.),
                                (lag(., default = NA) + lead(., default = NA)) / 2,
                                .))) %>%
  ungroup()

# Grafica de nulos
na_counts <- sapply(df_gam_1, function(x) sum(is.na(x)))
na_counts_df <- data.frame(columna = names(na_counts), na_count = na_counts)

ggplot(na_counts_df, aes(x = reorder(columna, -na_count), y = na_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Pone las columnas en el eje vertical
  labs(x = "Columna", y = "Cantidad de Datos Faltantes",
       title = "Cantidad de Datos Faltantes por Columna") +
  theme_minimal()

# Revisar datos faltantes y decidir un método de manejo
# Por ejemplo, si decides eliminar filas con NA:
ANALITIC_GAM <- na.omit(ANALITIC)

print('------------------- MODELO GAM -------------------')
# ------------------------------------------------------------------
# ------------------------ MODELO GAM ------------------------------
# ----------------------- (sobre FGE) ------------------------------
# ------------------------------------------------------------------

# Suponiendo que tus datos tienen las columnas 'Tiempo', 'Evento' y 'Variable1'
# 'Tiempo' es el tiempo hasta el evento o censura,
# 'Evento' es un indicador de si ocurrió el evento (1) o si es censurado (0),
# y 'Variable1' es una variable explicativa.

# Ajustar un modelo de supervivencia GAM
modelo_gam <- gam(Surv(tiempo_total, FGE) ~ s(Estado), data = ANALITIC_GAM, family = cox.ph())

# Ver el resumen del modelo
summary(modelo_gam)
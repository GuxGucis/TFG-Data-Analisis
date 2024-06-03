# Ensure necessary libraries are loaded
library(survival)
library(JM)
library(MASS)
library(nlme)
library(dplyr)

baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# Load the data
data <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_1.csv"), sep = ",", header = TRUE)

# Convertir fechatoma a formato Date
data$fechatoma <- as.Date(data$fechatoma)

data <- data %>%
  arrange(ID, fechatoma) %>%
  group_by(ID) %>%
  mutate(tiempo_total = as.integer(last(fechatoma) - first(fechatoma))) %>% # Días totales
  mutate(dias_transcurridos = round(as.numeric(difftime(fechatoma, lag(fechatoma, default = first(fechatoma)), units = "days")))) %>%
  mutate(FGE_microten = as.integer(lag(FGE, default = first(FGE)) > FGE)) %>% # 1 si el FGE desciende y 0 si asciende o se mantiene igual
  ungroup()

data <- data %>%
  relocate("FGE_microten", .after = "FGE")
data <- data %>%
  relocate("Edad", .after = "fechatoma")
data <- data %>%
  relocate("FGE_microten", .after = "fechatoma")
data <- data %>%
  relocate("tiempo_total", .after = "dias_transcurridos")

# Filtrar las filas con datos faltantes en las variables utilizadas en los modelos
variables_modelo <- c("FGE_microten", "dias_transcurridos", "Edad", "Hemodialisis", "Cociente.Album.Creat",
                      "Porcbasofilos", "Porclinfocitos", "Porcmonocitos", "Acido.Folico", "ALAT.GPT",
                      "Albumina", "Bilirrubina.directa", "Bilirrubina.total", "Calcio", "CHCM",
                      "Cifra.de.Plaquetas", "CO2.suero", "Colesterol.de.LDL.Formula.de.Friedewald",
                      "Creatinina", "Creatinina.orina", "Densidad", "Fosfatasa.alcalina", "Gamma.GT",
                      "HDL.Colesterol", "Hemoglobina.A1c", "LDH", "Linfocitos.V.Absoluto",
                      "Monocitos.V.Absoluto", "Parathormona.Intacta", "Peso", "Potasio",
                      "Potasio.en.orina", "Proteina.C.reactiva", "Proteinas.totales", "Sodio.orina",
                      "T4.libre", "Talla", "Temperatura.Axilar", "TSH", "Vitamina.B12",
                      "Volumen.plaquetar.medio")

data <- data %>%
  filter(complete.cases(data[variables_modelo]))

# Asegurarse de que las IDs coincidan en ambos modelos
ids_longitudinal <- unique(data$ID)
ids_survival <- unique(data$ID)

# Filtrar el conjunto de datos para usar solo IDs que estén en ambos conjuntos
data <- data %>% filter(ID %in% ids_longitudinal & ID %in% ids_survival)

# Comprobar las primeras filas del dataframe para asegurarse de que los datos se han cargado correctamente
cat("Primeras filas del dataframe:\n")
print(head(data))

# Ajustar el modelo longitudinal de efectos mixtos usando lme
cat("Ajustando el modelo longitudinal de efectos mixtos...\n")

longitudinal_model <- lme(FGE_microten ~ dias_transcurridos + Edad + Hemodialisis + Cociente.Album.Creat + Porcbasofilos +
  Porclinfocitos + Porcmonocitos + Acido.Folico + ALAT.GPT + Albumina +
  Bilirrubina.directa + Bilirrubina.total + Calcio + CHCM + Cifra.de.Plaquetas +
  CO2.suero + Colesterol.de.LDL.Formula.de.Friedewald + Creatinina +
  Creatinina.orina + Densidad + Fosfatasa.alcalina + Gamma.GT + HDL.Colesterol +
  Hemoglobina.A1c + LDH + Linfocitos.V.Absoluto + Monocitos.V.Absoluto +
  Parathormona.Intacta + Peso + Potasio + Potasio.en.orina + Proteina.C.reactiva +
  Proteinas.totales + Sodio.orina + T4.libre + Talla + Temperatura.Axilar + TSH +
  Vitamina.B12 + Volumen.plaquetar.medio,
                          random = ~ 1 | ID, data = data)

# Mostrar resumen del modelo longitudinal para asegurarse de que se ha ajustado correctamente
cat("Resumen del modelo longitudinal:\n")
print(summary(longitudinal_model))

# Ajustar el modelo de Cox para la supervivencia
cat("Ajustando el modelo de Cox para la supervivencia...\n")

survival_model <- coxph(Surv(dias_transcurridos, FGE_microten) ~ Edad + Hemodialisis + Cociente.Album.Creat + Porcbasofilos +
  Porclinfocitos + Porcmonocitos + Acido.Folico + ALAT.GPT + Albumina +
  Bilirrubina.directa + Bilirrubina.total + Calcio + CHCM + Cifra.de.Plaquetas +
  CO2.suero + Colesterol.de.LDL.Formula.de.Friedewald + Creatinina +
  Creatinina.orina + Densidad + Fosfatasa.alcalina + Gamma.GT + HDL.Colesterol +
  Hemoglobina.A1c + LDH + Linfocitos.V.Absoluto + Monocitos.V.Absoluto +
  Parathormona.Intacta + Peso + Potasio + Potasio.en.orina + Proteina.C.reactiva +
  Proteinas.totales + Sodio.orina + T4.libre + Talla + Temperatura.Axilar + TSH +
  Vitamina.B12 + Volumen.plaquetar.medio + cluster(ID), data = data, x = TRUE)

# Mostrar resumen del modelo de supervivencia para asegurarse de que se ha ajustado correctamente
cat("Resumen del modelo de supervivencia:\n")
print(summary(survival_model))

# Verificar si el número de observaciones coincide
cat("Número de observaciones en el modelo longitudinal (lme):\n")
print(length(longitudinal_model$data$ID))
cat("Número de observaciones en el modelo de supervivencia (coxph):\n")
print(length(survival_model$y[, 1]))

# Crear el modelo conjunto
cat("Creando el modelo conjunto...\n")
joint_model <- jointModel(longitudinal_model, survival_model, timeVar = "dias_transcurridos")

# Mostrar resumen del modelo conjunto
cat("Resumen del modelo conjunto:\n")
print(summary(joint_model))

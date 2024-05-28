# Ensure necessary libraries are loaded
library(survival)
library(JM)
library(MASS)
library(nlme)
library(dplyr)

baseurl <- "D:/gugui/Documentos/Universidad/TFG/"

# Load the data
data <- read.csv(paste0(baseurl, "Mice/ANALITIC_mice_1.csv"), sep = ",", header = TRUE)

# Convert 'fechatoma' to Date
data$fechatoma <- as.Date(data$fechatoma, format="%Y-%m-%d")

# Data preprocessing
data <- data %>%
  group_by(ID) %>%
  arrange(fechatoma) %>%
  mutate(OrdenFecha = row_number() - 1,
         tiempo_total = as.integer(last(fechatoma) - first(fechatoma))) %>%
  ungroup() %>%
  mutate(Estado = case_when(
    Hemodialisis == 1 & Transplante == 0 ~ "hemodialisis",
    Hemodialisis == 0 & Transplante == 1 ~ "transplante",
    Hemodialisis == 1 & Transplante == 1 ~ "ambas",
    Hemodialisis == 0 & Transplante == 0 ~ "nada"
  ))

# Identify subjects present in both datasets
ids_longitudinal <- unique(data$ID)
ids_survival <- unique(data$ID[data$tiempo_total > 0 & !is.na(data$Fallecido)])

common_ids <- intersect(ids_longitudinal, ids_survival)

# Filter the data to include only common IDs
data <- data %>% filter(ID %in% common_ids)

# Fit the longitudinal submodel
lme_fit <- lme(FGE ~ OrdenFecha, random = ~ OrdenFecha | ID, data = data)
summary(lme_fit)

# Fit the survival submodel
surv_fit <- coxph(Surv(tiempo_total, Fallecido) ~ Estado + cluster(ID), data = data, x = TRUE)
summary(surv_fit)

# Fit the joint model
joint_fit <- jointModel(lme_fit, surv_fit, timeVar = "OrdenFecha", method = "Cox-PH-GH")
summary(joint_fit)


# Select relevant columns for the survival analysis
survival_data <- data[, c("ID", "fechatoma", "Event1", "Fallecido", "Hemodialisis", "Transplante", "Edad",
                          "Cociente.Album.Creat", "Porcbasofilos", "Porclinfocitos", "Porcmonocitos",
                          "Acido.Folico", "ALAT.GPT", "Albumina", "Bilirrubina.directa", "Bilirrubina.total",
                          "Calcio", "CHCM", "Cifra.de.Plaquetas", "CO2.suero", "Colesterol.de.LDL.Formula.de.Friedewald",
                          "Creatinina", "Creatinina.orina", "Densidad", "Fosfatasa.alcalina", "Gamma.GT",
                          "HDL.Colesterol", "Hemoglobina.A1c", "LDH", "Linfocitos.V.Absoluto", "Monocitos.V.Absoluto",
                          "Parathormona.Intacta", "Peso", "Potasio", "Potasio.en.orina", "Proteina.C.reactiva",
                          "Proteinas.totales", "Sodio.orina", "T4.libre", "Talla", "Temperatura.Axilar", "TSH",
                          "Vitamina.B12", "Volumen.plaquetar.medio")]
print('================================= CURVAS KAPLAN-MEIER =================================')

library(survival)
library(survminer)
library(dplyr)

# -------------------------------------------------------------------
# ------------------------ PREPARACIÓN ------------------------------
# -------------------------------------------------------------------
print('------------------- PREPARACIÓN -------------------')

df_km <- df_cox

# Columna Estado para crear los grupo de kaplan-meier
df_km <- df_km %>%
  mutate(Estado = case_when(
    Hemodialisis == 1 & Transplante == 0 ~ "hemodialisis",
    Hemodialisis == 0 & Transplante == 1 ~ "transplante",
    Hemodialisis == 1 & Transplante == 1 ~ "ambas",
    Hemodialisis == 0 & Transplante == 0 ~ "nada"
  ))

# Crear el dataframe EXTRA seleccionando solo las columnas que quiero guardar
EXTRA <- df_km %>%
  select(ID, Hemodialisis, Transplante)

# Columnas no necesarias
df_km <- df_km %>%
  select(-c(Hemodialisis, Transplante))

print('------------------- MODELO KAPLAN-MEIER -------------------')
# ------------------------------------------------------------------
# ------------------- MODELO KAPLAN-MEIER --------------------------
# ----------------------- (sobre FGE) ------------------------------
# ------------------------------------------------------------------

# Recordamos --> 1 Si desciende el valor 0 si asciende, en el caso del FGE significa 1 si la enfermedad empeora
# Crear el objeto Surv y ajustar el modelo de Kaplan-Meier
modelo_km_FGE <- survfit(Surv(tiempo_total, FGE) ~ Estado, data = df_km)

# ------------------------------------------------------------------
# ------------------- MODELO KAPLAN-MEIER --------------------------
# -------------------- (sobre Fallecido) ---------------------------
# ------------------------------------------------------------------

modelo_km_Fallecido <- survfit(Surv(tiempo_total, Fallecido) ~ Estado, data = df_km)

print('------------------- GRAFICA KAPLAN-MEIER -------------------')
# -------------------------------------------------------------------
# ------------------- GRAFICA KAPLAN-MEIER --------------------------
# ------------------------ (sobre FGE) ------------------------------
# -------------------------------------------------------------------

# Graficar el modelo de Kaplan-Meier con ggsurvplot
g <- ggsurvplot(modelo_km_FGE, data = df_km,
           xlab = "Tiempo",
           ylab = "Probabilidad de Supervivencia",
           title = "Curva de Kaplan-Meier por Estado sobre FGE",
           palette = c("#E7B800","#2E9FDF", "#3ADF2E", "#DE2EDF"), # Puedes cambiar los colores según prefieras
           pval = TRUE, # Muestra el p-valor del test de log-rank
           risk.table = TRUE # Añade una tabla de personas en riesgo por tiempo
)

print(g)

# -------------------------------------------------------------------
# ------------------- GRAFICA KAPLAN-MEIER --------------------------
# --------------------- (sobre Fallecido) ---------------------------
# -------------------------------------------------------------------

# Graficar el modelo de Kaplan-Meier con ggsurvplot
g <- ggsurvplot(modelo_km_Fallecido, data = df_km,
                xlab = "Tiempo",
                ylab = "Probabilidad de Supervivencia",
                title = "Curva de Kaplan-Meier por Estado sobre Fallecimiento",
                palette = c("#E7B800","#2E9FDF", "#3ADF2E", "#DE2EDF"), # Puedes cambiar los colores según prefieras
                pval = TRUE, # Muestra el p-valor del test de log-rank
                risk.table = TRUE # Añade una tabla de personas en riesgo por tiempo
)

print(g)


### Proyecto TMS - Análisis de datos HDRS
### Fecha: 02/10/18
### Por: Alan Davalos

# Carga de librerías necesarias
# Si es necesario, instalarlas usando install.packages("nombre_del_paquete")
library(dplyr)

# Configuración inicial
setwd("/path/to/directory") # Establece tu directorio de trabajo aquí

# Carga de datos
TMS_HDRS <- read.csv("HDRS.csv", header = TRUE)

# Preprocesamiento
TMS_HDRS <- TMS_HDRS %>%
  mutate(
    group = factor(group, levels = c(1, 2), labels = c("Sham", "Treatment")),
    rid = factor(rid),
    stage = factor(stage),
    score_categories = factor(score_categories, levels = c(1, 2, 3, 4, 5), labels = c("normal", "minor depression", "less than major depressive", "major depressive", "more than major depressive"))
  )

# Análisis descriptivo
table(TMS_HDRS$group, TMS_HDRS$stage)
summary(TMS_HDRS)

# Función personalizada para estadísticas descriptivas
mystats <- function(x, na.omit=TRUE) {
  if(na.omit) {
    x <- na.omit(x)
  }
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  min_val <- min(x)
  max_val <- max(x)
  q <- quantile(x, probs=c(0.25, 0.50, 0.75))
  return(list(mean=m, n=n, sd=s, min=min_val, max=max_val, quantiles=q))
}

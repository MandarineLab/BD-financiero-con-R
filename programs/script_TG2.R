# ********************************************* TG2 ************************************************

# Vemos el directorio
getwd()

# Instalamos las librerias necesarias
install.packages("tidyverse")
install.packages("writexl")
install.packages("readr")
install.packages("ggplot2")
install.packages("DBI")
install.packages("dplyr")
install.packages("lubridate")
install.packages("readxl")

#cargar librerias necesarias
library(tidyverse)
library(writexl)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(DBI)
library(readxl)

#******************************************** PASO 1 **************************************************** 

#Lectura de base de datos
reservas <- readr::read_csv("data/raw/PD04650MD.csv", col_names = c("fecha", "reservas_internacionales"))
tasa_interes <- readr::read_csv("data/raw/PD04692MD.csv", col_names = c("fecha", "tasa_interes"))
bono_soles <- readr::read_csv("data/raw/PD31893DD.csv", col_names = c("fecha", "bono_soles"))
bono_dolares <- readr::read_csv("data/raw/PD31894DD.csv", col_names = c("fecha", "bono_dolares"))
tc_compra <- readr::read_csv("data/raw/PD04637PD.csv", col_names = c("fecha", "tc_compra"))
tc_venta <- readr::read_csv("data/raw/PD04638PD.csv", col_names = c("fecha", "tc_venta"))
indice_bvl <- readr::read_csv("data/raw/PD38026MD.csv", col_names = c("fecha", "indice_bvl"))

#******************************************** PASO 2 ****************************************************

datos <- list(reservas, tasa_interes, bono_soles, bono_dolares, tc_compra, tc_venta, indice_bvl)
nombres_columnas <- c("reservas_internacionales", "tasa_interes", "bono_soles", 
                      "bono_dolares", "tc_compra", "tc_venta", "indice_bvl")

for (i in seq_along(datos)) {
  # Reemplazar "n.d." con NA
  datos[[i]][[nombres_columnas[i]]][datos[[i]][[nombres_columnas[i]]] == "n.d."] <- NA  
  # Convertir a numérico y redondear a 2 decimales
  datos[[i]][[nombres_columnas[i]]] <- round(as.numeric(datos[[i]][[nombres_columnas[i]]]), 2)
}

reservas <- datos[[1]]
tasa_interes <- datos[[2]]
bono_soles <- datos[[3]]
bono_dolares <- datos[[4]]
tc_compra <- datos[[5]]
tc_venta <- datos[[6]]
indice_bvl <- datos[[7]]

# Eliminamos valores missings
reservas <- na.omit(reservas)
tasa_interes <- na.omit(tasa_interes)
bono_soles <- na.omit(bono_soles)
bono_dolares <- na.omit(bono_dolares)
tc_compra <- na.omit(tc_compra)
tc_venta <- na.omit(tc_venta)
indice_bvl <- na.omit(indice_bvl)

# Función para convertir meses en español a su formato numérico
convertir_meses <- function(fecha) {
  # Reemplazo manual de los meses
  fecha <- gsub("Ene", "01", fecha)  # Enero
  fecha <- gsub("Feb", "02", fecha)  # Febrero
  fecha <- gsub("Mar", "03", fecha)  # Marzo
  fecha <- gsub("Abr", "04", fecha)  # Abril
  fecha <- gsub("May", "05", fecha)  # Mayo
  fecha <- gsub("Jun", "06", fecha)  # Junio
  fecha <- gsub("Jul", "07", fecha)  # Julio
  fecha <- gsub("Ago", "08", fecha)  # Agosto
  fecha <- gsub("Set", "09", fecha)  # Septiembre
  fecha <- gsub("Oct", "10", fecha)  # Octubre
  fecha <- gsub("Nov", "11", fecha)  # Noviembre
  fecha <- gsub("Dic", "12", fecha)  # Diciembre
  
  return(fecha)
}

# Función para reemplazar meses en la columna de fecha
reemplazar_meses <- function(data) {
  data <- data %>%
    mutate(
      # Solo reemplazar los meses en la columna fecha
      fecha = convertir_meses(fecha)
    )
  return(data)
}

reservas_fecha <- reemplazar_meses(reservas)
tasa_interes_fecha <- reemplazar_meses(tasa_interes)
bono_soles_fecha <- reemplazar_meses(bono_soles)
bono_dolares_fecha <- reemplazar_meses(bono_dolares)
tc_compra_fecha <- reemplazar_meses(tc_compra)
tc_venta_fecha <- reemplazar_meses(tc_venta)
indices_bvl_fecha <- reemplazar_meses(indice_bvl)

# Convertir a formato fecha 
reservas_fecha$fecha <- as.Date(reservas_fecha$fecha, format = "%d%m%y")
tasa_interes_fecha$fecha <- as.Date(tasa_interes_fecha$fecha, format = "%d%m%y")
bono_soles_fecha$fecha <- as.Date(bono_soles_fecha$fecha, format = "%d%m%y")
bono_dolares_fecha$fecha <- as.Date(bono_dolares_fecha$fecha, format = "%d%m%y")
tc_compra_fecha$fecha <- as.Date(tc_compra_fecha$fecha, format = "%d%m%y")
tc_venta_fecha$fecha <- as.Date(tc_venta_fecha$fecha, format = "%d%m%y");
indices_bvl_fecha$fecha <- as.Date(indices_bvl_fecha$fecha, format = "%d%m%y")

# Función para crear las columnas año, mes y día
agregar_columnas_tiempo <- function(df) {
  df$anio <- year(df$fecha)  # Extraer el año
  df$mes <- month(df$fecha)  # Extraer el mes
  df$dia <- day(df$fecha)    # Extraer el día
  
  return(df)
}

# Aplicar la función a cada dataframe
reservas_fecha <- agregar_columnas_tiempo(reservas_fecha)
tasa_interes_fecha <- agregar_columnas_tiempo(tasa_interes_fecha)
bono_soles_fecha <- agregar_columnas_tiempo(bono_soles_fecha)
bono_dolares_fecha <- agregar_columnas_tiempo(bono_dolares_fecha)
tc_compra_fecha <- agregar_columnas_tiempo(tc_compra_fecha)
tc_venta_fecha <- agregar_columnas_tiempo(tc_venta_fecha)
indices_bvl_fecha <- agregar_columnas_tiempo(indices_bvl_fecha)

#******************************************** PASO 3 ****************************************************

# Merge con inner join
series_2024 <- Reduce(function(x, y) inner_join(x, y, by = "fecha"), 
                      list(reservas_fecha, tasa_interes_fecha, bono_soles_fecha, 
                           bono_dolares_fecha, tc_compra_fecha, tc_venta_fecha, 
                           indices_bvl_fecha))

# Calcular el promedio del TC de compra y de venta y asignarlo directamente al dataframe
series_2024$tc_promedio <- (series_2024$tc_compra + series_2024$tc_venta) / 2

# Solo las columnas de interés
series_2024 <- series_2024[, c("fecha", "anio", "mes", "dia", 
                               "reservas_internacionales", "tasa_interes", 
                               "bono_soles", "bono_dolares", 
                               "tc_compra", "tc_venta", "indice_bvl", "tc_promedio")]

write_xlsx(series_2024, "data/inter/series_2024.xlsx")

series_2024$tc_promedio <- round(series_2024$tc_promedio, 2) # redondeamos a 2 decimales

write_xlsx(series_2024, "data/final/BBDD DB financiero.xlsx")


#******************************************** PASO 4 ****************************************************

BBDD <- read_excel("./data/final/BBDD DB financiero.xlsx")

# a. Estadisticos media, mediana, valor mínimo y valor máximo de cada serie por mes
estadisticas_mensuales <- BBDD %>%
  group_by(mes) %>%
  summarise(
    reservas_internacionales_media = mean(reservas_internacionales, na.rm = TRUE),
    reservas_internacionales_mediana = median(reservas_internacionales, na.rm = TRUE),
    reservas_internacionales_min = min(reservas_internacionales, na.rm = TRUE),
    reservas_internacionales_max = max(reservas_internacionales, na.rm = TRUE),
    
    tasa_interes_media = mean(tasa_interes, na.rm = TRUE),
    tasa_interes_mediana = median(tasa_interes, na.rm = TRUE),
    tasa_interes_min = min(tasa_interes, na.rm = TRUE),
    tasa_interes_max = max(tasa_interes, na.rm = TRUE),
    
    bono_soles_media = mean(bono_soles, na.rm = TRUE),
    bono_soles_mediana = median(bono_soles, na.rm = TRUE),
    bono_soles_min = min(bono_soles, na.rm = TRUE),
    bono_soles_max = max(bono_soles, na.rm = TRUE),
    
    bono_dolares_media = mean(bono_dolares, na.rm = TRUE),
    bono_dolares_mediana = median(bono_dolares, na.rm = TRUE),
    bono_dolares_min = min(bono_dolares, na.rm = TRUE),
    bono_dolares_max = max(bono_dolares, na.rm = TRUE),
    
    tc_compra_media = mean(tc_compra, na.rm = TRUE),
    tc_compra_mediana = median(tc_compra, na.rm = TRUE),
    tc_compra_min = min(tc_compra, na.rm = TRUE),
    tc_compra_max = max(tc_compra, na.rm = TRUE),
    
    tc_venta_media = mean(tc_venta, na.rm = TRUE),
    tc_venta_mediana = median(tc_venta, na.rm = TRUE),
    tc_venta_min = min(tc_venta, na.rm = TRUE),
    tc_venta_max = max(tc_venta, na.rm = TRUE),
    
    indice_bvl_media = mean(indice_bvl, na.rm = TRUE),
    indice_bvl_mediana = median(indice_bvl, na.rm = TRUE),
    indice_bvl_min = min(indice_bvl, na.rm = TRUE),
    indice_bvl_max = max(indice_bvl, na.rm = TRUE),
    
    tc_promedio_media = mean(tc_promedio, na.rm = TRUE),
    tc_promedio_mediana = median(tc_promedio, na.rm = TRUE),
    tc_promedio_min = min(tc_promedio, na.rm = TRUE),
    tc_promedio_max = max(tc_promedio, na.rm = TRUE)
  )

# Exportar el resultado a un archivo Excel
write_xlsx(estadisticas_mensuales, "data/final/Estadisticas_mensuales_financieras.xlsx")

# *************************************************
#                b. Gráficos
# *************************************************

# las variaciones porcentuales (con ajuste de longitud -> NA)
BBDD$var_reservas_internacionales <- c(NA, diff(BBDD$reservas_internacionales) / BBDD$reservas_internacionales[-length(BBDD$reservas_internacionales)] * 100)
BBDD$var_tasa_interes <- c(NA, diff(BBDD$tasa_interes) / BBDD$tasa_interes[-length(BBDD$tasa_interes)] * 100)
BBDD$var_bono_soles <- c(NA, diff(BBDD$bono_soles) / BBDD$bono_soles[-length(BBDD$bono_soles)] * 100)
BBDD$var_bono_dolares <- c(NA, diff(BBDD$bono_dolares) / BBDD$bono_dolares[-length(BBDD$bono_dolares)] * 100)
BBDD$var_indice_bvl <- c(NA, diff(BBDD$indice_bvl) / BBDD$indice_bvl[-length(BBDD$indice_bvl)] * 100)
BBDD$tc_compra_var <- c(NA, diff(BBDD$tc_compra_var) / BBDD$tc_compra_var[-length(BBDD$tc_compra_var)] * 100)
BBDD$tc_venta_var <- c(NA, diff(BBDD$tc_venta_var) / BBDD$tc_venta_var[-length(BBDD$tc_venta_var)] * 100)
BBDD$tc_promedio_var <- c(NA, diff(BBDD$tc_promedio) / BBDD$tc_promedio[-length(BBDD$tc_promedio)] * 100)

# Gráfico de variaciones porcentuales de Reservas Internacionales
ggplot(BBDD, aes(x = fecha, y = var_reservas_internacionales)) +
  geom_line(color = "blue", na.rm = TRUE) +
  labs(title = "Evolución las Reservas Internacionales",
       x = "Fecha", 
       y = "Variación %") +
  theme_minimal()
ggsave("graphs/reservas_internacionales.jpg", plot = last_plot(), width = 10, height = 6)

# Gráfico de variaciones porcentuales de Tasa de Interés
ggplot(BBDD, aes(x = fecha, y = var_tasa_interes)) +
  geom_line(color = "red", na.rm = TRUE) +
  labs(title = "Tasa de Interés Intercambiaria",
       x = "Fecha", 
       y = "Variación %") +
  theme_minimal()
ggsave("graphs/var_tasa_interes.jpg", plot = last_plot(), width = 10, height = 6)

# Gráfico de variaciones porcentuales de Bono en Soles y Dólares
ggplot(BBDD) +
  geom_line(aes(x = fecha, y = var_bono_soles, color = "Bono Soles"), na.rm = TRUE) +
  geom_line(aes(x = fecha, y = var_bono_dolares, color = "Bono Dólares"), na.rm = TRUE) +
  labs(title = "Rendimiento de Bonos Gobierno peruano",
       x = "Fecha", 
       y = "Variación %",
       color = "Serie") +
  theme_minimal()
ggsave("graphs/var_bonos_gobierno.jpg", plot = last_plot(), width = 10, height = 6)

# Gráfico de variaciones porcentuales del Índice BVL
ggplot(BBDD, aes(x = fecha, y = var_indice_bvl)) +
  geom_line(color = "green", na.rm = TRUE) +
  labs(title = "Índice General Bursátil - BVL",
       x = "Fecha", 
       y = "Variación %") +
  theme_minimal()
ggsave("graphs/var_indice_bvl.jpg", plot = last_plot(), width = 10, height = 6)

# Gráfico TC Intercambiario

ggplot(BBDD, aes(x = fecha)) +
  geom_line(aes(y = tc_promedio_var, color = "Variación % TC Promedio"), na.rm = TRUE) +
  geom_line(aes(y = tc_compra_var, color = "Variación % TC Compra"), na.rm = TRUE) +
  geom_line(aes(y = tc_venta_var, color = "Variación % TC Venta"), na.rm = TRUE) +
  labs(
    title = "TC Interbancario",
    x = "Fecha",
    y = "Variación Porcentual (%)",
    color = "Series"
  ) +
  theme_minimal()

ggsave("graphs/variaciones_tc.jpg", plot = last_plot(), width = 10, height = 6)

#¿considera que alguna variable tiene un comportamiento similar a la del tipo de cambio promedio? 
# Las Reservas Internacionales muestran cambios que siguen directamente el TC
# El Indice BVL muestra fluctuaciones leves relacionadas a cambios en TC
# La tasa de interés muestra picos y valles que coinciden temporalmente con los del tipo de cambio

# *************************************************
#                c. Regresión
# *************************************************

modelo_regresion <- lm(tc_promedio ~ reservas_internacionales + tasa_interes + bono_soles + indice_bvl, data = BBDD)
summary(modelo_regresion)

BBDD$predicciones <- predict(modelo_regresion)

# *************************************************
#             d. Test Estadístico
# *************************************************

media_real <- mean(BBDD$tc_promedio)
media_prediccion <- mean(BBDD$predicciones)

# Prueba de medias (t-test)
t_test <- t.test(BBDD$predicciones, BBDD$tc_promedio)

# Prueba de Kolmogorov-Smirnov
ks_test <- ks.test(BBDD$predicciones, BBDD$tc_promedio)

print(t_test)

# Un p-value cercano a 1 
# no hay evidencia suficiente para rechazar la H0
# H0: las medias de las predicciones y los valores reales son iguales

print(ks_test)

# un p-value cercano a 0
# se rechaza la HO
# H0: las distribuciones son significativamente diferentes (valor predicho y real)

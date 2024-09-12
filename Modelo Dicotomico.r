##### Paquetes que vamos necesitando los importamos aca ####
install.packages("carData")
install.packages("car")
install.packages("dplyr")
install.packages("emmeans")
install.packages("ggplot2")
install.packages("vcd")
install.packages("nlme")
#install.packages("installr", dependencies = TRUE) #actuaiza a la aultima version de R
#library(installr)
#updateR()
version
library("nlme")
library("vcd")
library ("car")
library ("dplyr")
library ("emmeans")
library ("ggplot2")
##Me tiro error por no tener la ultima version, con esto lo actualizo los paquetes.
update.packages(ask = FALSE, checkBuilt = TRUE)

##### Importo datos y veo las variables #####
rm(list=ls()) #limpio memoria
#setwd("F:/Juli/Documents/Bioestadistica/Concurso GAB") #seteo directorio
setwd("C:/Users/lauta/Downloads")
datos <- read.csv("DatosGrado.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
head(datos) #chequeo que haya quedado bien
# El Id y los dias desde el armado no son variables a estudiar! Tener cuidado que el ID no cohincide con el n de plantas. tenemso en total 66 plantas!

summary(datos) # Especie y tratamiente son Caracter! los paso a factores

#Las paso a factores
datos$Especie <- as.factor(datos$Especie)
datos$Tratamiento <- as.factor(datos$Tratamiento)

summary(datos) # Ahora si summary me dice los niveles de cada factor :)
#VARIABLES EXPLICATORIAS:
### Especie: Junco (22), Pehuaja (24), Totora (20)
### Tratamiento: Con remojo (34), Sin remojo (32)
#VARIABLE RESPUESTA:
#Raiz mas larga: Variable respuesta cuantitativa continua, se espera distribucion normal, dominio (0,29. 
### OJO! la variable nunca puede tomar valores negativos! probar distribucion GAMMA!!
# las raices q valen cero en realidad  son plantas que no properaron! Las saco! me voy a fijar que porcentaje de plantitas de cada tipo porspero.

table(subset(datos, Raiz_mas_larga == 0)$Especie) # Cuento cuántas plantas tienen Raiz_mas_larga= 0 para cada especie

##### codigo falopa de a ver si funca para hacer tabla con % de raiz cero para cada tratamiento#### 
tabla_completa <- datos %>%
  mutate(Especie = as.factor(Especie),
         Tratamiento = as.factor(Tratamiento)) %>%
  group_by(Especie, Tratamiento) %>%
  summarise(
    Total = n(),
    Raiz_cero = sum(Raiz_mas_larga == 0),
    Raiz_mas_larga = sum(Raiz_mas_larga > 0),
    Porcentaje_Raiz_cero = (Raiz_cero / Total) * 100
  ) %>%
  ungroup() %>%
  # Tabla con totales por especie
  bind_rows(
    datos %>%
      mutate(Especie = as.factor(Especie)) %>%
      group_by(Especie) %>%
      summarise(
        Total = n(),
        Raiz_cero = sum(Raiz_mas_larga == 0),
        Raiz_mas_larga = sum(Raiz_mas_larga > 0),
        Porcentaje_Raiz_cero = (Raiz_cero / Total) * 100
      ) %>%
      mutate(Tratamiento = "Todos") # Añadimos una columna para diferenciar
  ) %>%
  # Tabla con totales por tratamiento
  bind_rows(
    datos %>%
      mutate(Tratamiento = as.factor(Tratamiento)) %>%
      group_by(Tratamiento) %>%
      summarise(
        Total = n(),
        Raiz_cero = sum(Raiz_mas_larga == 0),
        Raiz_mas_larga = sum(Raiz_mas_larga > 0),
        Porcentaje_Raiz_cero = (Raiz_cero / Total) * 100
      ) %>%
      mutate(Especie = "Todos") # Añadimos una columna para diferenciar
  ) %>%
  # Tabla con totales generales
  bind_rows(
    datos %>%
      summarise(
        Total = n(),
        Raiz_cero = sum(Raiz_mas_larga == 0),
        Raiz_mas_larga = sum(Raiz_mas_larga > 0),
        Porcentaje_Raiz_cero = (Raiz_cero / Total) * 100
      ) %>%
      mutate(Especie = "Todos", Tratamiento = "Todos") # Añadimos columnas para diferenciar
  )

print(tabla_completa) # Funciona joya :)

# grafico
# Instalar y cargar los paquetes necesarios
install.packages("vcd")
library(vcd)
library(dplyr)

# Crear la tabla de contingencia para combinaciones de especie y tratamiento
tabla_mosaico <- datos %>%
  mutate(Especie = as.factor(Especie),
         Tratamiento = as.factor(Tratamiento),
         Raiz = ifelse(Raiz_mas_larga == 0, "Sin raíz", "Con raíz")) %>%
  count(Especie, Tratamiento, Raiz)

# Convertir a tabla de contingencia
tabla_mosaico <- xtabs(n ~ Especie + Tratamiento + Raiz, data = tabla_mosaico)

# Crear un vector de colores para las combinaciones de Especie, Tratamiento y Raíz
colores <- c("Junco.Sin raíz.Con remojo" = "#8B0000",  # Rojo oscuro
             "Junco.Sin raíz.Sin remojo" = "#FF6347",  # Rojo claro
             "Pehuajó.Sin raíz.Con remojo" = "#006400",  # Verde oscuro
             "Pehuajó.Sin raíz.Sin remojo" = "#90EE90",  # Verde claro
             "Totora.Sin raíz.Con remojo" = "#00008B",  # Azul oscuro
             "Totora.Sin raíz.Sin remojo" = "#87CEFA",  # Azul claro,
             "Junco.Con raíz" = "white",
             "Pehuajó.Con raíz" = "white",
             "Totora.Con raíz" = "white")

# Función personalizada para asignar colores a las celdas
mi_color <- function(especie, tratamiento, raiz) {
  key <- paste(especie, raiz, tratamiento, sep = ".")
  if (key %in% names(colores)) {
    return(colores[[key]])
  } else {
    return("white")  # Color predeterminado para combinaciones no especificadas
  }
}

# Crear la lista de colores para las celdas del gráfico de mosaico
celda_colores <- apply(expand.grid(dimnames(tabla_mosaico)), 1, 
                       function(row) mi_color(row[[1]], row[[2]], row[[3]]))

# Crear el gráfico de mosaico con los colores personalizados
mosaic(~ Especie + Tratamiento + Raiz, data = tabla_mosaico,
       gp = gpar(fill = celda_colores),
       main = "Distribución de Plantas sin Raíz por Especie y Tratamiento",
       legend = FALSE)
#################### 

#Habria que hacer un modelado para ver si hay diferencias significativas entre las especies y el desarrollo de raiz
#variable respuesta dicotomica = Con o sin raiz
#variables explicatorias: Dos factores Especies (tres niveles) y tratamiento (dos niveles)

#1. Hago tabla con las respuesta dicotomica
datos$Raiz <- ifelse(datos$Raiz_mas_larga > 0, 1, 0)

#2. Planteo modelo binomial
# Ajustar el modelo logístico con interacción entre Tratamiento y Especie
modelo <- glm(Raiz ~ Tratamiento * Especie, data = datos, family = binomial)
summary(modelo)

# Evaluación de los efectos de cada factor con ANOVA
anova(modelo, test = "Chisq")

# Obtener las medias marginales estimadas para las combinaciones de Tratamiento y Especie
library(emmeans)
emm <- emmeans(modelo, ~ Tratamiento * Especie)

# Comparaciones post-hoc para las combinaciones de Tratamiento y Especie
comparaciones <- pairs(emm, adjust = "tukey")
summary(comparaciones)

# Mostrar los resultados de las comparaciones
print(comparaciones)

# Crear una nueva columna con las predicciones del modelo
datos$Probabilidad <- predict(modelo, type = "response")

# Gráfico de las probabilidades predichas
ggplot(datos, aes(x = Tratamiento, y = Probabilidad, color = Especie)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidades predichas de tener raíz",
       x = "Tratamiento", y = "Probabilidad de tener raíz") +
  theme_minimal()


library(emmeans)
library(ggplot2)


#############################################################
#Cuantifico efecto del tratamiento para cada especie y luego comparo entre especies
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(purrr)

#############################################################
# Cuantifico efecto del tratamiento para cada especie y luego comparo entre especies

# Ajustar un modelo logístico para cada especie
modelos <- datos %>%
  group_by(Especie) %>%
  nest() %>%
  mutate(modelo = map(data, ~ glm(Raiz ~ Tratamiento, data = ., family = binomial)))

# Estimar el efecto del tratamiento para cada especie
efectos <- modelos %>%
  mutate(emm = map(modelo, ~ emmeans(.x, ~ Tratamiento)),
         contrastes = map(emm, ~ as.data.frame(contrast(.x, method = "pairwise"))))

# Extraer los resultados de los efectos
resultados_efectos <- efectos %>%
  unnest(contrastes) %>%
  select(Especie, contrast, estimate, SE, df, z.ratio, p.value)

# Mostrar los resultados de los efectos
print(resultados_efectos)

# Comparar los efectos del tratamiento entre especies
comparacion_efectos <- efectos %>%
  unnest(contrastes) %>%
  group_by(contrast) %>%
  summarise(
    estimate_diff = diff(estimate),
    SE_diff = sqrt(sum(SE^2)),
    z.ratio_diff = estimate_diff / SE_diff,
    p.value_diff = 2 * (1 - pnorm(abs(z.ratio_diff)))
  )

# Mostrar los resultados de la comparación de efectos
print(comparacion_efectos)

# Crear una nueva columna con las predicciones del modelo para cada especie
datos <- datos %>%
  left_join(modelos %>% select(Especie, modelo), by = "Especie") %>%
  mutate(Probabilidad = map2_dbl(modelo, row_number(), ~ predict(.x, newdata = datos[.y, ], type = "response"))) %>%
  select(-modelo)

# Gráfico de las probabilidades predichas
Gdico<- ggplot(datos, aes(x = Tratamiento, y = Probabilidad, color = Especie)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidades predichas de tener raíz",
       x = "Tratamiento", y = "Probabilidad de tener raíz") +
  theme_minimal()
Gdico

##############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(purrr)

#############################################################
# Cuantifico efecto del tratamiento para cada especie y luego comparo entre especies

# Ajustar un modelo logístico para cada especie
modelos <- datos %>%
  group_by(Especie) %>%
  nest() %>%
  mutate(modelo = map(data, ~ glm(Raiz ~ Tratamiento, data = ., family = binomial)))

# Estimar el efecto del tratamiento para cada especie
efectos <- modelos %>%
  mutate(emm = map(modelo, ~ emmeans(.x, ~ Tratamiento)),
         contrastes = map(emm, ~ as.data.frame(contrast(.x, method = "pairwise"))))

# Extraer los resultados de los efectos
resultados_efectos <- efectos %>%
  unnest(contrastes) %>%
  select(Especie, contrast, estimate, SE, df, z.ratio, p.value)

# Mostrar los resultados de los efectos
print(resultados_efectos)

# Comparar los efectos del tratamiento entre especies
comparacion_efectos <- efectos %>%
  unnest(contrastes) %>%
  group_by(contrast) %>%
  summarise(
    estimate_diff = diff(estimate),
    SE_diff = sqrt(sum(SE^2)),
    z.ratio_diff = estimate_diff / SE_diff,
    p.value_diff = 2 * (1 - pnorm(abs(z.ratio_diff)))
  )

# Mostrar los resultados de la comparación de efectos
print(comparacion_efectos)

# Crear una tabla con las probabilidades de no tener raíz para cada combinación de "Especie" y "Tratamiento"
probabilidades <- efectos %>%
  mutate(emm_df = map(emm, as.data.frame)) %>%
  unnest(emm_df) %>%
  mutate(prob_no_raiz = 1 - emmean) %>%
  select(Especie, Tratamiento, prob_no_raiz, asymp.LCL, asymp.UCL)

# Mostrar la tabla de probabilidades
print(probabilidades)

# Crear una nueva columna con las predicciones del modelo para cada especie
datos <- datos %>%
  left_join(modelos %>% select(Especie, modelo), by = "Especie") %>%
  mutate(Probabilidad = map2_dbl(modelo, row_number(), ~ predict(.x, newdata = datos[.y, ], type = "response"))) %>%
  select(-modelo)

# Gráfico de las probabilidades predichas
Gdico <- ggplot(datos, aes(x = Tratamiento, y = Probabilidad, color = Especie)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidades predichas de tener raíz",
       x = "Tratamiento", y = "Probabilidad de tener raíz") +
  theme_minimal()

# Mostrar el gráfico
print(Gdico)

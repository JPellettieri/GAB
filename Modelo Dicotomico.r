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
#setwd("C:/Users/lauta/Downloads")
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

# Reordenar los niveles de la variable "PresenciaRaiz"
tabla_mosaico$Raiz <- factor(datos$Raiz, levels = c("Sin raíz", "Con raíz"))

# Crear un vector de colores para las combinaciones de Especie, Tratamiento y Raíz
colores <- c("Junco.Con raíz.Con remojo" = "#8A312C",  # Rojo oscuro
             "Junco.Con raíz.Sin remojo" = "#EA7974",  # Rojo claro
             "Pehuajó.Con raíz.Con remojo" = "#738F2A",  # Verde oscuro
             "Pehuajó.Con raíz.Sin remojo" = "#CCEB7F",  # Verde claro
             "Totora.Con raíz.Con remojo" = "#31628C",  # Azul oscuro
             "Totora.Con raíz.Sin remojo" = "#7FB6EB",  # Azul claro,
             "Junco.Sin raíz" = "grey",
             "Pehuajó.Sin raíz" = "grey",
             "Totora.Sin raíz" = "grey")

# Función personalizada para asignar colores a las celdas
mi_color <- function(especie, tratamiento, raiz) {
  key <- paste(especie, raiz, tratamiento, sep = ".")
  if (key %in% names(colores)) {
    return(colores[[key]])
  } else {
    return("grey")  # Color predeterminado para combinaciones no especificadas
  }
}

# Crear la lista de colores para las celdas del gráfico de mosaico
celda_colores <- apply(expand.grid(dimnames(tabla_mosaico)), 1, 
                       function(row) mi_color(row[[1]], row[[2]], row[[3]]))
# Crear el gráfico de mosaico con los colores personalizados
Mosaico<-mosaic(~ Especie + Tratamiento + Raiz, data = tabla_mosaico,
       gp = gpar(fill = celda_colores),
       main = "Distribución de Plantas sin Raíz por Especie y Tratamiento",
       legend = FALSE)
Mosaico

##### Grafico de Barras####
library(ggplot2)
library(dplyr)

# Calcular los porcentajes de respuesta para cada combinación de especie y tratamiento
porcentajes <- datos %>%
  group_by(Especie, Tratamiento, Raiz = ifelse(Raiz_mas_larga == 0, "Sin raíz", "Con raíz")) %>%
  summarise(Conteo = n()) %>%
  group_by(Especie, Tratamiento) %>%
  mutate(Porcentaje = Conteo / sum(Conteo) * 100) %>%
  ungroup()  # Desagrupar para evitar problemas con ggplot

# Filtrar para mantener solo las filas correspondientes a "Con raíz"
porcentajes <- porcentajes %>%
  filter(Raiz == "Con raíz") %>%
  mutate(comb = paste(Especie, Raiz, Tratamiento, sep = "."))

# Crear un factor ordenado para Especie y Tratamiento según el orden deseado
porcentajes<- porcentajes %>%
  mutate(Orden = factor(paste(Especie, Tratamiento, sep = " "),
                        levels = c("Junco Con remojo", "Junco Sin remojo",
                                   "Pehuajó Con remojo", "Pehuajó Sin remojo",
                                   "Totora Con remojo", "Totora Sin remojo")))

# Crear una columna con las combinaciones para usar en la asignación de colores
porcentajes <- porcentajes %>%
  mutate(comb = paste(Especie, Raiz, Tratamiento, sep = "."))

# Definir los colores para las combinaciones de Especie, Tratamiento y Raíz
colores <- c("Junco.Con raíz.Con remojo" = "#8A312C",
             "Junco.Con raíz.Sin remojo" = "#EA7974",  # Rojo claro
             "Pehuajó.Con raíz.Con remojo" = "#738F2A",  # Verde oscuro
             "Pehuajó.Con raíz.Sin remojo" = "#CCEB7F",  # Verde claro
             "Totora.Con raíz.Con remojo" = "#31628C",  # Azul oscuro
             "Totora.Con raíz.Sin remojo" = "#7FB6EB")

# Crear el gráfico de barras apiladas

ggplot(porcentajes, aes(x = Orden, y = Porcentaje, fill = comb)) +
  geom_bar(stat = "identity", position = "stack", color = "grey") +
  scale_fill_manual(values = colores) +
  labs(title = "Porcentaje de Plantas con Raíz por Especie y Tratamiento",
       x = "Combinación Especie-Tratamiento", y = "Porcentaje") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si es necesario

#################### 

#Habria que hacer un modelado para ver si hay diferencias significativas entre las especies y el desarrollo de raiz
#variable respuesta dicotomica = Con o sin raiz
#variables explicatorias: Dos factores Especies (tres niveles) y tratamiento (dos niveles)

#1. Hago tabla con las respuesta dicotomica
datos$Raiz <- ifelse(datos$Raiz_mas_larga > 0, 1, 0)

#2. Planteo modelo binomial
# Modelo logístico para evaluar el efecto del Tratamiento y Especie en la presencia de raíz
modelo <- glm(Raiz ~ Tratamiento+Especie, data = datos, family = binomial) # poisson es un tipo de binomial
summary(modelo)

# Evaluación de los efectos de cada factor con ANOVA
anova(modelo, test = "Chisq")

#3 grafico de medias con diferencias significativas.

# Crear una nueva columna con las predicciones del modelo
datos$Probabilidad <- predict(modelo, type = "response")

# Gráfico de las probabilidades predichas
ggplot(datos, aes(x = Tratamiento, y = Probabilidad, color = Especie)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidades predichas de tener raíz",
       x = "Tratamiento", y = "Probabilidad de tener raíz") +
  theme_minimal()

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



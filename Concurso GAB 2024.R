
##### Consigna ####
Los datos proporcionados en el archivo DatosGrado.csv (*) corresponden a registros de la
longitud de la raiz más larga en cada biorollo, para cada especie y tratamiento, al final de la
experiencia (día 93)
- Explorar los datos y describir la muestra. ¿Qué puede decir del valor cero en la
variable longitud de la raíz? ¿Tiene sentido incluir estos datos en un análisis de
longitud de la raíz?
  - ¿Qué porcentaje de plantas no sacó raíz? ¿Hay diferencias entre especies en la
proporción de plantas que desarrollan raíces?
  - El crecimiento de la raíz, ¿se ve afectado por el tratamiento de lixiviado de manera
diferencial para las tres especies?
  - Recomendar la/s especie/s de macrófitas que más posibilidad tiene/n de
establecerse en el ambiente y el tipo de tratamiento de lixiviado que considere
adecuado para la restauración de la comunidad vegetal en los arroyos bajo estudio.

(*) 'data.frame': 66 obs. of 5 variables:
  Diccionario de variables:
  ID_biorollo: identificador único del biorollo, integer.
Dias_desde_armado: días transcurridos desde el armado del biorollo, integer.
Especie: especie de macrófita, factor, 3 niveles ("Junco", "Pehuajó" y "Totora").
Tratamiento: tipo de lixiviado, factor, 2 niveles ("Con remojo" y "Sin remojo")
Raiz_mas_larga: longitud de la raiz más larga (cm), numeric. Consigna

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
#######


#### ANALISIS GRAFICO EXPLORATORIO DE LOS DATOS####
# Para evitar problemas borro todo y meto los datos para filtrarlo y hacer el modelo
rm(list=ls()) #limpio memoria
#setwd("F:/Juli/Documents/Bioestadistica/Concurso GAB") #seteo directorio
Sin_Filtrar <- read.csv("DatosGrado.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
Sin_Filtrar$Especie <- as.factor(Sin_Filtrar$Especie)
Sin_Filtrar$Tratamiento <- as.factor(Sin_Filtrar$Tratamiento)
# Saco los datos que tienen Raiz=0
Datos<- Sin_Filtrar %>%
  filter(Raiz_mas_larga != 0)
summary(Datos) #chequeo, funciono bien :)

####Como queda ahora la distribucion?
# Crear el gráfico de barras para la distribución del número muestral por tratamiento
Datos$Tratamiento_Especie <- interaction(Datos$Tratamiento, Datos$Especie)
# Crear un gráfico de barras que muestre la cantidad de observaciones por combinación de tratamiento y especie
ggplot(Datos, aes(x = Tratamiento_Especie)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribución del Número Muestral por Tratamiento y Especie",
       x = "Tratamiento y Especie",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Grafico de perfiles,  para evaluar relacion entre factores
(mediasDatos<-aggregate(Datos$Raiz_mas_larga~Datos$Especie+Datos$Tratamiento, Datos,mean)) # tabla de medias
colnames(mediasDatos) <- c("Especie", "Tratamiento", "Raiz_mas_larga")
str(mediasDatos) #veo la estructura

g1 <- ggplot(mediasDatos, aes(x=Tratamiento, y=Raiz_mas_larga, colour=Especie, group=Especie))
g1 <- g1 + geom_line(aes(linetype=Especie), linewidth=.6) + geom_point(aes(shape=Especie), size=3) 
g1 <- g1 + geom_jitter(data = Datos, width = 0.05, height = 0)
g1 # Se ve con claridad una interaccion entre las variables especie y tratamiento. Sin remojo, totora y pehuajo muestran una tendencia a mayor desarrollo de raiz.

#ENTONCES: ANOVA de dos factores, Evaluar interaccion!!


                                   #### Modelo y veo supuestos####
#Posibles modelos:
* Modelo normal con interaccion
* Modelo normal sin interaccion
* Modelo normal con/sin interacion con la variancia modelada (varIden) |factor , |Especie y |Factor*Especie
* Modelo con distribucion Gamma Con interaccion o sin interaccion
* Modelo con distribucion Gamma con modelado de varianza??? esto existe? 

g3<-ggplot(Datos, aes(x = Raiz_mas_larga, fill = Especie)) +
  geom_density(alpha = 0.8) +
  labs(x = "Largo raíz (cm)", y = "Densidad") +
  scale_fill_manual(values = c("red", "green", "lightblue")) +
  theme_minimal()
g3
  
####Entonces planteo modelo Normal de comparacion de medias con interaccion####
modelo1<-lm(Raiz_mas_larga ~ Tratamiento*Especie, Datos)

#suepuestos
e<-resid(modelo1) # residuos
re<-rstandard(modelo1) #residuos estandarizados
pre<-predict(modelo1) #predichos
par(mfrow = c(1, 2))
#Homocedasticidad
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 1" )
abline(0,0) ## Rechazo H0!! hay que modelar varianza!
leveneTest(Raiz_mas_larga ~ Tratamiento*Especie, Datos)

#Normalidad
qqPlot(e, main = "QQplot -Modelo 1") #da feo, cagamos
shapiro.test(e) #0,02!! rechazo normalidad

##### Modelado de Varianza####
library("nlme")
Modelo_varIdent<-gls(Raiz_mas_larga ~ Tratamiento*Especie, weights=varIdent(form=~1|Tratamiento*Especie), data=Datos)
anova(Modelo_varIdent)
print(Modelo_varIdent) #nos informa los desvios estandar de cada X en funcion de un devio de referencia

#Chequeo supuestos
e<-resid(Modelo_varIdent) # residuos
re<-rstandard(Modelo_varIdent) #residuos estandarizados
pre<-predict(Modelo_varIdent) #predichos
par(mfrow = c(1, 2))
#Homocedasticidad
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados ",main="RE vs PRED - Modelo_varIdent" )
abline(0,0) ## Rechazo H0!! hay que modelar varianza!
leveneTest(Raiz_mas_larga ~ Tratamiento*Especie,Datos)

#Normalidad
qqPlot(e, main = "QQplot - Modelo_varIdent") #da feo, cagamos
shapiro.test(e)

##### Planteo modelo GAMMA, comparacion de medias con interaccion ####

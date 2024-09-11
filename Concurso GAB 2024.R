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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #No quedo balanceado pero no es nada grave

###Grafico de DENSIDAD
g3<-ggplot(Datos, aes(x = Raiz_mas_larga, fill = Especie)) +
  geom_density(alpha = 0.8) +
  labs(x = "Largo raíz (cm)", y = "Densidad") +
  scale_fill_manual(values = c("red", "green", "lightblue")) +
  theme_minimal()
g3 ]#se ve con claridad los dos picos dentro de cada especie
#Hago grafico de densidad pero incluyo a la otra variable
gDensidad <- ggplot(Datos, aes(x = Raiz_mas_larga, fill = interaction(Tratamiento, Especie))) +
  geom_density(alpha = 0.8) +
  labs(x = "Largo raíz (cm)", y = "Densidad", fill = "Tratamiento y Especie") +
  scale_fill_manual(values = c("red", "green", "lightblue", "purple", "orange", "yellow")) + 
  theme_minimal() +
  theme(legend.position = "top")
gDensidad
# Es mucho bardo no se lee bien hago dos graficos separados por tratamiento
gDensidadTratamiento <- ggplot(Datos, aes(x = Raiz_mas_larga, fill = Especie)) +
  geom_density(alpha = 0.8) +
  labs(x = "Largo raíz (cm)", y = "Densidad", fill = "Especie") +
  scale_fill_manual(values = c("red", "green", "lightblue")) + 
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~ Tratamiento)
gDensidadTratamiento  # Aun asi se siguen viendo dos picos en "con remojo" raro y junco no tiene ningun pico en absoluto

#hago un grafico por especie para ver visualmente el efecto del tratamiento en cada especie
gDensidadEspecie <- ggplot(Datos, aes(x = Raiz_mas_larga, fill = Tratamiento)) +
  geom_density(alpha = 0.8) +
  labs(x = "Largo raíz (cm)", y = "Densidad", fill = "Tratamiento") +
  scale_fill_manual(values = c( "#006400", "#90EE90")) + 
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~ Especie)
gDensidadEspecie
### BOXPLOT para evaluar relacion varianza y media


### Grafico de PERFILES,  para evaluar relacion entre factores
(mediasDatos<-aggregate(Datos$Raiz_mas_larga~Datos$Especie+Datos$Tratamiento, Datos,mean)) # tabla de medias
colnames(mediasDatos) <- c("Especie", "Tratamiento", "Raiz_mas_larga")
str(mediasDatos) #veo la estructura

g1 <- ggplot(mediasDatos, aes(x=Tratamiento, y=Raiz_mas_larga, colour=Especie, group=Especie))
g1 <- g1 + geom_line(aes(linetype=Especie), linewidth=.6) + geom_point(aes(shape=Especie), size=3) 
g1 <- g1 + geom_jitter(data = Datos, width = 0.05, height = 0)
g1 # Se ve con claridad una interaccion entre las variables especie y tratamiento. Sin remojo, totora y pehuajo muestran una tendencia a mayor desarrollo de raiz.

#ENTONCES: ANOVA de dos factores, Evaluar interaccion!!
BoxPlot <- ggplot(Datos, aes(x = interaction(Tratamiento, Especie), y = Raiz_mas_larga, fill = Especie)) +
  geom_boxplot() +
  labs(x = "Tratamiento y Especie", y = "Largo de la raíz (cm)", fill = "Especie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar las etiquetas del eje X
        legend.position = "top")
BoxPlot # no es muy claro que haya un aumento de la varianza con un aumento de la media... Not sure si usar GAMMA

#### Modelo y veo supuestos####
#Posibles modelos:
#* Modelo normal con interaccion
#* Modelo normal sin interaccion
#* Modelo normal con/sin interacion con la variancia modelada (varIden) |factor , |Especie y |Factor*Especie
#* Modelo con distribucion Gamma Con interaccion o sin interaccion
#* Modelo con distribucion Gamma con modelado de varianza??? esto existe? 


####Entonces planteo modelo Normal de comparacion de medias con interaccion####

MNormal<-glm(Raiz_mas_larga ~ Especie * Tratamiento, 
             data = Datos)

#suepuestos
e<-resid(MNormal) # residuos
re<-rstandard(MNormal) #residuos estandarizados
pre<-predict(MNormal) #predichos
par(mfrow = c(1, 2))
#Homocedasticidad
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo Normal" )
abline(0,0) ## Rechazo H0!! hay que modelar varianza!
leveneTest(Raiz_mas_larga ~ Tratamiento*Especie, Datos) #Rechazo homocedasticidad

#Normalidad
qqPlot(e, main = "QQplot -MNormal") # da feo
?qqPlot
sqqplotshapiro.test(e) #0,02!! rechazo normalidad

##### Modelado de Varianza####
library("nlme")
Modelo_varIdent<-gls(Raiz_mas_larga ~ Tratamiento*Especie, weights=varIdent(form=~1|Tratamiento*Especie), data=Datos)
anova(Modelo_varIdent)
print(Modelo_varIdent) #nos informa los desvios estandar de cada X en funcion de un devio de referencia

#Chequeo supuestos
par(mfrow = c(1, 2))
#Homocedasticidad #residuos vs Predichos y levene
ResVsPred_Norm_Iden<- plot(Modelo_varIdent, main="RE vs PRED - Modelo varIdent", xlab = "Valores Predichos", 
                           ylab = "Residuos" ) # Hermosoooo
ResVsPred_Norm_Iden

leveneTest( residuals(Modelo_varIdent)~ Tratamiento * Especie, data = Datos) #Da feo pero quiza puedo quedarme con el grafico y fue...

#Normalidad
qqPlot(e, main = "QQplot - Modelo_varIdent")  #corrio y dio feo
shapiro.test(residuals(Modelo_varIdent)) # rechazo normalidad :(
e=resid(Modelo_varIdent)
ggplot(e, main = "QQplot - Modelo_varIdent")
print(e)

####### Tratando de arreglar el ggplot####
residuos_df <- data.frame(residuos = resid(Modelo_varIdent))
# Crear el gráfico QQ
ggplot(residuos_df, aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line() +
  # Añadir líneas que representan el intervalo de confianza
  geom_abline(intercept = mean(residuos_df$residuos) - sd(residuos_df$residuos), slope = 1, linetype = "dashed", color = "blue") +
  geom_abline(intercept = mean(residuos_df$residuos) + sd(residuos_df$residuos), slope = 1, linetype = "dashed", color = "blue") +
  ggtitle("QQplot - Modelo_varIdent") +
  theme_minimal() # horrible


##### Planteo modelo GAMMA, comparacion de medias con interaccion ####
# Ajustar el modelo Gamma con enlace logarítmico usando glm
modelo_gamma <- glm(Raiz_mas_larga ~ Especie * Tratamiento, 
                    data = Datos, 
                    family = Gamma(link = "log"))
#Chequeo supuestos
par(mfrow = c(2, 2))
# Gráficos para el modelo Gamma
plot(modelo_gamma, main = "Modelo Gamma") # no cumple supuesto de homocedasticidad, se puede modelar varianza de gamma?




# Busco transformar los datos para que ajuste a una distribucion normal#
##################LOG Normal###########
install.packages("MASS")
library(MASS)
# Ajustar el modelo con la variable dependiente original
modelo <- glm(Raiz_mas_larga ~ Especie * Tratamiento, data = Datos)

# Realizar la búsqueda de la mejor transformación Box-Cox
boxcox_resultado <- boxcox(modelo)

# Encontrar el mejor valor de lambda
mejor_lambda <- boxcox_resultado$x[which.max(boxcox_resultado$y)]
print(paste("El mejor valor de lambda para la transformación Box-Cox es:", mejor_lambda))

# Aplicar la transformación al modelo usando el mejor lambda
Datos$transformada <- ifelse(mejor_lambda == 0,
                             log(Datos$Raiz_mas_larga),
                             (Datos$Raiz_mas_larga^mejor_lambda - 1) / mejor_lambda)

# Ajustar el modelo con la variable transformada
modelo_transformado <- glm(transformada ~ Especie * Tratamiento, data = Datos)

# Resumen del modelo transformado
summary(modelo_transformado)



### Busca la mejor transformacion sea como sea ####
install.packages("bestNormalize")
library(bestNormalize)
bn <- bestNormalize(Datos$Raiz_mas_larga)

# Ver el resultado
print(bn)

# Aplicar la mejor transformación a los datos
Datos$transformada <- bn$x.t

# Ajustar el modelo con la variable transformada
modelo_transformado <- glm(transformada ~ Especie * Tratamiento, data = Datos)

# Resumen del modelo transformado
summary(modelo_transformado )
anova(modelo_transformado )

#chequeo supuestos
residuals <- residuals(modelo_transformado)
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

#Homocedasticidad
plot(fitted(modelo_transformado), residuals)
abline(h = 0, col = "red")
install.packages("lmtest")
library(lmtest)
bptest(modelo_transformado)


##################### No es normal!! hay que usar metodos no parametricos!!! #####################
MNoPrametrico<-kruskal.test(Raiz_mas_larga ~ interaction(Especie, Tratamiento), data = Datos)
# Prueba post-hoc con Wilcoxon para comparar grupos
pairwise.wilcox.test(Datos$Raiz_mas_larga, Datos$Tratamiento, p.adjust.method = "bonferroni")

#otra opcion: la prueba de Dunn
#install.packages("dunn.test")
library(dunn.test)
# Lista de combinaciones de Especie y Tratamiento
combinaciones <- unique(interaction(Datos$Especie, Datos$Tratamiento))
dunn.test(Datos$Raiz_mas_larga, Datos$Combinacion, method = "bonferroni")

####Intento medio falopa para calcular IC: Bootstrap Percentil####
#install.packages("boot")
library(boot)

# Función para calcular la mediana
calc_median <- function(data, indices) {
  d <- data[indices, ]  # muestra los datos en base a los índices
  return(median(d$Raiz_mas_larga))
}


# Bucle para realizar el bootstrap por cada combinación
for (combinacion in combinaciones) {
  # Filtrar los datos por combinación de Especie y Tratamiento
  datos_comb <- subset(Datos, interaction(Especie, Tratamiento) == combinacion)
  
  # Aplicar bootstrap con 1000 muestras
  resultado_boot <- boot(data = datos_comb, statistic = calc_median, R = 1000)
  
  # Calcular intervalos de confianza (IC) al 95%
  IC <- boot.ci(resultado_boot, type = "perc")
  
  # Mostrar los resultados
  cat("Combinación:", combinacion, "\n")
  cat("Mediana estimada:", median(datos_comb$Raiz_mas_larga), "\n")
  cat("IC del 95%: [", IC$perc[4], ",", IC$perc[5], "]\n\n")
}

########### Plot para graficar medis e IC##################
#install.packages("ggsignif")
library(ggplot2)
library(ggsignif)

# Crear un dataframe con las combinaciones, medianas e intervalos de confianza
datos_grafico <- data.frame(
  Combinacion = c("Junco.Con remojo", "Pehuajó.Con remojo", "Totora.Con remojo", 
                  "Totora.Sin remojo", "Junco.Sin remojo", "Pehuajó.Sin remojo"),
  Mediana = c(10.75, 10.5, 6, 8, 14, 23),
  IC_min = c(7.5, 8, 4, 7, 3, 22),
  IC_max = c(16, 15, 7, 9, 26, 25)
)

# GRafico Medianas con IC
p <- ggplot(datos_grafico, aes(x = Combinacion, y = Mediana)) +
  geom_point(size = 4, color = "gray") +  # Medianas
  geom_errorbar(aes(ymin = IC_min, ymax = IC_max), width = 0.2, color = "black") +  # Intervalos de confianza
  geom_violin(aes(x = Combinacion, y = Mediana), draw_quantiles = c(0.5), fill = "lightblue", alpha = 0.3) +  # Violines
  theme_minimal() +
  ggtitle("Medianas estimadas y distribuciones por combinación de Especie y Tratamiento") +
  xlab("Combinación Especie - Tratamiento") +
  ylab("Mediana de Raiz más larga") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x
p

# Agregar las comparaciones significativas usando ggsignif
#p + geom_signif(
  #comparisons = list(c("Pehuajó.Sin remojo", "Pehuajó.Con remojo"),  # Comparaciones significativas
                     #c("Totora.Sin remojo", "Totora.Con remojo")),
  #map_signif_level = TRUE,  # Muestra asteriscos según el nivel de significancia
  #y_position = c(27, 12),  # Ajustar la posición de las líneas de significancia
  #tip_length = 0.03  # Largo de las líneas
#)


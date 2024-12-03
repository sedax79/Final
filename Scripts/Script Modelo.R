

#Librerias
library(ggplot2)
library(psych)
library(skimr)
library(broom)



#Cargar base de datos

BD_final<-read.csv('C:/Mineria de datos/Final/BD/BD_Final_S_Acc_DOT.csv')

names(BD_final)

str(BD_final)

###________Preparar Base para los modelos________###


# Logaritmizando la variable Precio
BD_final$Precio_log <- log(BD_final$precio)




#________Analisis descriptivo________#

Descrip_basc <- summary(BD_final)

Descrip_basc

#________Analisis descriptivo con otras librerias________#



# Análisis descriptivo con 'describe()'
Descrip_basc2 <- describe(BD_final)

Descrip_basc2




# Análisis descriptivo con 'skim()'
Descrip_basc3 <- skim(BD_final)

Descrip_basc3

###________PLOTS________###



#________Graficos de dispersion________#

#Relación entre Precio y distancia al CBD por tipo de vivienda ($ Y LOG)
ggplot(data = BD_final) +
  geom_point(mapping = aes(x = Distance, y = Precio_log)) + 
  facet_wrap(~ tipo, nrow = 3) +
  theme_minimal()+
  labs(title = "Relación entre Precio y distancia al CBD por tipo de vivienda", x = "Distancia", y = "Precio (Log)")

#Relación entre Precio e Indice de accesibilidas por tipo de vivienda ($ Y LOG)
ggplot(data =BD_final)+ 
  geom_point(mapping = aes(x = I_A, y = Precio_log)) + 
  facet_wrap(~ tipo, nrow = 3) +
  theme_minimal() +
  labs(title = "Relación entre Precio e Indice de Accesibilidad por tipo de vivienda", x = "Indx_Acc", y = "Precio (Log)")



#________Graficos de distribución (histogramas)________#


###______INDIVIDUALES________###


#Precio ($ y Log)
ggplot(BD_final, aes(x = precio)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Precios de Vivienda", x = "Precio ($)", y = "Frecuencia")

#Area construida m2
ggplot(BD_final, aes(x = AREA_CONST)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de M2 de Construcción", x = "AREA_CONST", y = "Frecuencia")

#Baños
ggplot(BD_final, aes(x = BANOS)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Baños", x = "BANOS", y = "Frecuencia")

#Recamaras
ggplot(BD_final, aes(x = RECAMARAS)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de RECAMARAS", x = "RECAMARAS", y = "Frecuencia")

#Edad_inmueble
ggplot(BD_final, aes(x = Edad_inmue)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de la Edad del inmueble", x = "Edad_inmue", y = "Frecuencia")

#Distancia al CBD
ggplot(BD_final, aes(x = Distance)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de la distancia al CBD", x = "Distancia m", y = "Frecuencia")

#Indice de Accesibilida
ggplot(BD_final, aes(x = I_A)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución del Indice de Accesibilidad", x = "Indice", y = "Frecuencia")


###____________UNIDO__________###

library(tidyr)
library(dplyr)


# Convertir las columnas relevantes en formato long

#Grafico unico de variables continuas
BD_long <- BD_final %>%
  select(precio, Precio_log, AREA_CONST, BANOS, RECAMARAS, Edad_inmue, Distance, I_A) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Valor"
  )


# Grafico combinado

library(ggplot2)

ggplot(BD_long, aes(x = Valor)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) + # Usa facet_wrap para separar por variable
  labs(title = "Distribución de Variables Continuas", x = "Valor", y = "Frecuencia") +
  theme_minimal()




#________Graficos de caja (Boxplot)________#

#________(Continuas)________#
#Precio ($ y Log)

ggplot(BD_final, aes(y = Precio_log)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución del Precio", y = "Precio (Log)")


#Area Construida

ggplot(BD_final, aes(y = AREA_CONST)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución del Area construida M2", y = "M2")

#RECAMARAS

ggplot(BD_final, aes(y = RECAMARAS)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución de RECAMARAS", y = "RECAMARAS")

#BANOS

ggplot(BD_final, aes(y = BANOS)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución de BANOS", y = "BANOS")

#Edad_inmue

ggplot(BD_final, aes(y = Edad_inmue)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución de la edad del inmueble", y = "Edad_inmue")

#Distancia al CBD

ggplot(BD_final, aes(y = Distance)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución de la distancia al CBD", y = "Distancia")

#Indice de Accesibilidad

ggplot(BD_final, aes(y = I_A)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución del Indice de accesibilidad", y = "Indice")

#________(Continuas UNICO)________#

# Crear un único gráfico con facetas
ggplot(BD_long, aes(y = Valor)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +  # Panel para cada variable, con escalas independientes
  labs(title = "Distribuciones de Variables Continuas", y = "Valor") +
  theme_minimal()



#________(Continuas vs categoricas)________#


#Con tipo de vivienda

#Precio ($ y Log)
ggplot(BD_final, aes(x = tipo, y = Precio_log)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Precio por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Precio (Log)")


#Area Construida

ggplot(BD_final, aes(x = tipo, y = AREA_CONST)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Area construida M2 por tipo de vivienda", x = "Tipo de vivienda", y = "M2")

#RECAMARAS

ggplot(BD_final, aes(x = tipo, y = RECAMARAS)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "No. Recamaras por tipo de vivienda", x = "Tipo de vivienda", y = "Recamaras")

#BANOS

ggplot(BD_final, aes(x = tipo, y = BANOS)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "No. Baños por tipo de vivienda", x = "Tipo de vivienda", y = "Baños")

#Edad_inmue

ggplot(BD_final, aes(x = tipo, y = Edad_inmue)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Edad del inmueble por tipo de vivienda", x = "Tipo de vivienda", y = "Edad")

#Distancia al CBD

ggplot(BD_final, aes(x = tipo, y = Distance)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución de la distancia al CBD por tipo de vivienda", x = "Tipo de vivienda" , y = "Distancia")

#Indice de Accesibilidad

ggplot(BD_final, aes(x = tipo, y = I_A)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  labs(title = "Distribución del Indice de accesibilidad por tipo de vivienda", x = "Tipo de vivienda", y = "Indice")

#________(Categoricas UNICO)________#

#Grafico unico de variables continuas vs categoricas
BD_long1 <- BD_final %>%
  select(precio, Precio_log, AREA_CONST, BANOS, RECAMARAS, Edad_inmue, Distance, I_A, tipo) %>%  # Selecciona las columnas relevantes
  pivot_longer(
    cols = c(precio, Precio_log, AREA_CONST, BANOS, RECAMARAS, Edad_inmue, Distance, I_A),  # Las columnas que se van a convertir a formato largo
    names_to = "Variable",  # El nombre de la columna que contendrá los nombres de las variables
    values_to = "Valor"  # El nombre de la columna que contendrá los valores de las variables
  )


ggplot(BD_long1, aes(x = tipo, y = Valor)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +  # Panel por cada variable
  labs(title = "Distribuciones de Variables por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Valor") +
  theme_minimal()

#________Graficos de violin________#



# Gráfico de violín de los precios por tipo de vivienda
ggplot(BD_final, aes(x = tipo, y = Precio_log)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribución del Precio por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Precio (Log)")


# Gráfico de violín de la distancia al CBD por tipo de vivienda

ggplot(BD_final, aes(x = tipo, y = Distance)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribución de la Distancia al CBD por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Distancia")


# Gráfico de violín de la cantidad de recamaras por tipo de vivienda

ggplot(BD_final, aes(x = tipo, y = RECAMARAS)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribución de la cantidad de recamaras por Tipo de Vivienda", x = "Tipo de Vivienda", y = "RECAMARAS")

# Gráfico de violín de la cantidad de Baños por tipo de vivienda

ggplot(BD_final, aes(x = tipo, y = BANOS)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribución de la cantidad de baños por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Baños")

# Gráfico de violín de la cantidad de m2 por tipo de vivienda

ggplot(BD_final, aes(x = tipo, y = AREA_CONST)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribución de la cantidad de m2 por Tipo de Vivienda", x = "Tipo de Vivienda", y = "M2")

# Gráfico de violín de la edad por tipo de vivienda

ggplot(BD_final, aes(x = tipo, y = Edad_inmue)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribución de la edad por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Edad")

# Gráfico de violín de la distribucion del IA por tipo de vivienda

ggplot(BD_final, aes(x = tipo, y = I_A)) +
  geom_violin(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Distribución del ndice de accesibilidad por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Indice")


#________(Violin UNICO)________#


ggplot(BD_long1, aes(x = tipo, y = Valor)) +
  geom_violin(fill = "lightgreen") +
  facet_wrap(~ Variable, scales = "free") +  # Panel por cada variable
  labs(title = "Distribuciones de Variables por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Valor") +
  theme_minimal()


#________Matriz de correlacion________#
#COLINIALIDAD

library(ggcorrplot)

# Calcular la matriz de correlación (var continuas)
cor_matrix <- cor(BD_final[, c("Precio_log", "AREA_CONST", "RECAMARAS", "BANOS", "Edad_inmue", "Distance", "I_A", "CPV_Prop_s")], use = "complete.obs")

ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, colors = c("red", "white", "blue"))



# Correlacion polycor (categoricas)

# Asegúrate de que tipo_vivienda sea una variable ordinal
BD_final$tipo_vivienda <- factor(BD_final$tipo_vivienda, ordered = TRUE)

# Instalar y cargar la librería polycor
install.packages("polycor")
library(polycor)

# Calcular correlación policórica entre dos variables ordinales
correlacion_pol <- polychor(BD_final$tipo_vivienda, BD_final$precio)
correlacion_pol


#____________________________________________________________________

# Calcular la desviación estándar de cada columna
sd_values <- apply(BD_final[, c("AREA_CONST","RECAMARAS","BANOS","No_de_piso","Edad_inmue","Jardin","Cisterna","Seguridad",
                                "Zona.priva","Alberca","Lavanderí","Terraza","Gimnasio","Cuarto.de","Chimenea","Sótano","Aire.acond","Elevador",
                                "Urbanizado","CPV_Prop_e","CPV_Prop_a","CPV_Prop_d","CPV_Prop_s","Distance","I_A")], 2, sd)

# Ver las columnas con desviación estándar igual a cero
zero_sd_cols <- names(sd_values[sd_values == 0])
print(zero_sd_cols)

#Eliminacion de vairables inutiles

#sotano, urbanizacion, no_pisos

#Quitar el que no tiene baño, o ponerle 1

BD_final <- BD_final %>%
  mutate(BANOS = case_when(
    BANOS == 0 ~ 1,
    TRUE ~ BANOS  # mantiene valores existente
  ))

#____________________________________________________________________







#ANALISIS INFERENCIAL#



str(BD_final)

#Seleccion final de variables
BD_modelo <- BD_final [c("ID","precio","Precio_log","AREA_CONST","RECAMARAS","BANOS","Edad_inmue","Jardin","Cisterna","Seguridad","Alberca","Lavanderí","Terraza","Gimnasio","Cuarto.de","Chimenea","Aire.acond","Elevador","CPV_Prop_s","Distance","I_A","tipo_vivienda")]

str(BD_modelo)

#Preparar base (cambiar a factores)

BD_modelo <- BD_modelo %>%
  mutate(
    across(c(precio, Precio_log,AREA_CONST, RECAMARAS, BANOS, Edad_inmue), as.numeric),
    across(c(Jardin, Cisterna, Seguridad, Alberca, Lavanderí, Terraza, Gimnasio, Cuarto.de, Chimenea, Aire.acond, Elevador, tipo_vivienda, I_A), as.factor)
  )
str(BD_modelo)


#________Regresión lineal multiple________#

# Realizar la regresión lineal precio log
modelo <- lm(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = BD_modelo)


# Ver el resumen del modelo
summary(modelo)


#Realizar la regresión lineal precio nominal

modelo1 <- lm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = BD_modelo)

summary(modelo1)

# Realizar la regresión lineal precio log sin recamara
modelo2 <- lm(Precio_log ~ AREA_CONST + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = BD_modelo)


# Ver el resumen del modelo
summary(modelo2)


#Realizar la regresión lineal precio nominal sin recamara

modelo3 <- lm(precio ~ AREA_CONST + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = BD_modelo)

summary(modelo3)


#VIF

# Instalar y cargar el paquete car si no lo tienes
install.packages("car")
library(car)

# Calcular el VIF
vif(modelo)
vif(modelo1)
vif(modelo2)
vif(modelo3)



# Diagnóstico de residuos
par(mfrow = c(2, 2))  # Disposición para 4 gráficos
plot(modelo_cuadratico_multiple)  # Gráficos de diagnóstico


#________Regresión cuadratica multiple________#

# Ajuste de un modelo con términos cuadráticos para varias variables


modelo_cuadratico_multiple <- lm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + I(Distance^2) + I_A + tipo_vivienda, data = BD_modelo)


# Ver los resultados
summary(modelo_cuadratico_multiple)



modelo_cuadratico_multiple1 <- lm(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s +  I(Distance^2)+ I_A + tipo_vivienda, data = BD_modelo)


# Ver los resultados
summary(modelo_cuadratico_multiple1)





# Instalar y cargar el paquete lmtest si no lo tienes
install.packages("lmtest")
library(lmtest)

# Realizar la prueba de Breusch-Pagan
bptest(modelo)

# Instalar y cargar el paquete sandwich
install.packages("sandwich")
library(sandwich)

# Prueba de White
white_test <- bptest(modelo, ~ fitted(modelo) + I(fitted(modelo)^2), data = BD_final)
summary(white_test)


#____________mostrar y comparar modelos_________#



install.packages("broom")
install.packages("dplyr")
library(broom)
library(dplyr)

#lista de modelos
lista_modelos <- list(
  "Modelo lineal Log" = modelo,
  "Modelo lineal $" = modelo1
)

resultados_modelos <- purrr::map_dfr(lista_modelos, tidy, .id = "Modelo")

resumen_modelos <- purrr::map_dfr(lista_modelos, glance, .id = "Modelo")


write.csv(resultados_modelos, "C:/Mineria de datos/Final/BD/resultados_coeficientes.csv", row.names = FALSE)
write.csv(resumen_modelos, "C:/Mineria de datos/Final/BD/resultados_resumen.csv", row.names = FALSE)


#____________mostrar y comparar modelos_________#


#install.packages("gtsummary")
#install.packages("C:/Mineria de datos/glue_1.8.0.zip", repos = NULL, type = "source")
#.rs.restartR()

#library(glue)
#library(gtsummary)

#packageVersion("glue")


#________________Entrenamiento y predicciones__________________#


# Cargar librería necesaria
set.seed(123)  # Para reproducibilidad

# Dividir los datos
library(caret)

#dividir datos

#precio

index <- createDataPartition(BD_modelo$precio, p = 0.7, list = FALSE)
data_train <- BD_modelo[index, ]  # 70% de los datos
data_test <- BD_modelo[-index, ]  # 30% restantes

summary(data_train$tipo_vivienda)  # Sustituye por cualquier otra variable categórica

str(data_train)

cols_categoricas <- sapply(data_train, is.factor)  # Identifica factores
lapply(data_train[, cols_categoricas], levels)    # Lista los niveles

lapply(data_train[, sapply(data_train, is.factor)], function(x) table(x))




#Modelo simple
modelo_pred <- lm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_train)

summary(modelo_pred)

#Modelo mejorado
modelo_pred1 <- lm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I(Distance^2) + I_A + tipo_vivienda, data = data_train)

summary(modelo_pred1)

#Modelo mejorado (solo cuadrado)
modelo_pred2 <- lm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + I(Distance^2) + I_A + tipo_vivienda, data = data_train)

summary(modelo_pred2)



#precio log

#Crear particion

index1 <- createDataPartition(BD_modelo$Precio_log, p = 0.7, list = FALSE)
data_train1 <- BD_modelo[index1, ]  # 70% de los datos
data_test1 <- BD_modelo[-index1, ]  # 30% restantes


#Modelo simple
modelo_pred3 <- lm(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_train1)

summary(modelo_pred3)

#Modelo mejorado
modelo_pred4 <- lm(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I(Distance^2) + I_A + tipo_vivienda, data = data_train1)

summary(modelo_pred4)

#Modelo mejorado (solo cuadrado)
modelo_pred5 <- lm(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + I(Distance^2) + I_A + tipo_vivienda, data = data_train1)

summary(modelo_pred5)

#________________predicciones__________________#



#PRECIO $

# Predicciones en el conjunto de prueba
predicciones <- predict(modelo_pred2, newdata = data_test)

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_test$precio - predicciones)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_test$precio, predicciones)^2
cat("R²:", r2, "\n")




#PRECIO LOG

# Predicciones en el conjunto de prueba
predicciones1 <- predict(modelo_pred5, newdata = data_test1)

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_test1$Precio_log - predicciones1)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_test1$Precio_log, predicciones1)^2
cat("R²:", r2, "\n")




#PLOT

library(ggplot2)
ggplot(data_test1, aes(x = Precio_log, y = predicciones1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()

residuos <- residuals(modelo_pred5)


hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(fitted(modelo_pred5), residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Ajustados", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")

shapiro.test(residuos)



###________________________________MACHINE LEARNING___________________________________###


library(readr)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(plyr)
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(MASS)
library(stats)
library(RWeka)
library(neuralnet)
library(skimr)
library(DataExplorer)
library(ggpubr)
library(mosaicData)
library(h2o)
library(dplyr)
library(recipes)
library(NeuralNetTools)
library(nnet)
library(kernlab)
library(ranger)
library(caretEnsemble)
library(tictoc)
library(readxl)

str(BD_modelo)


#2. MODELOS DE MACHINE LEARNING

precio_num_medio <- mean(BD_modelo$precio)
precio_num_medio

precio_log_medio <- mean(BD_modelo$Precio_log)
precio_log_medio

RMSE_media_num<-RMSE(precio_num_medio,BD_modelo$precio)
RMSE_media_num

RMSE_media_log<-RMSE(precio_log_medio,BD_modelo$Precio_log)
RMSE_media_log


#2.3. ARBOLES DE REGRESION y ARBOLES MODELO



#Precio $


#Particion

index_AR <- createDataPartition(BD_modelo$precio, p = 0.7, list = FALSE)
data_trainAR <- BD_modelo[index_AR, ]  # 70% de los datos
data_testAR <- BD_modelo[-index_AR, ]  # 30% restantes

#Entrenamiento del modelo

AR <- rpart(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_trainAR)

AR1 <- rpart(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I(Distance^2) + I_A + tipo_vivienda, data = data_trainAR)

AR2 <- rpart(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + I(Distance^2) + I_A + tipo_vivienda, data = data_trainAR)

AR

summary(AR)


#Visualizacion

rpart.plot(AR2, digits = 3)


#Prediccion

prediccionAR <- predict(AR2, newdata = data_testAR)


#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testAR$precio - prediccionAR)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testAR$precio, prediccionAR)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testAR, aes(x = precio, y = prediccionAR)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()



residuos <- data_testAR$precio - prediccionAR



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(prediccionAR, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)

#Precio LOG


#Particion

index_ARL <- createDataPartition(BD_modelo$Precio_log, p = 0.7, list = FALSE)
data_trainARL <- BD_modelo[index_ARL, ]  # 70% de los datos
data_testARL <- BD_modelo[-index_ARL, ]  # 30% restantes

#Entrenamiento del modelo

AR3 <- rpart(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_trainARL)

AR4 <- rpart(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I(Distance^2) + I_A + tipo_vivienda, data = data_trainARL)

AR5 <- rpart(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + I(Distance^2) + I_A + tipo_vivienda, data = data_trainARL)




#Visualizacion

rpart.plot(AR3, digits = 3)


#Prediccion

prediccionAR1 <- predict(AR3, newdata = data_testARL)



#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testARL$Precio_log - prediccionAR1)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testARL$Precio_log, prediccionAR1)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testARL, aes(x = Precio_log, y = prediccionAR1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()



residuos1 <- data_testARL$Precio_log - prediccionAR1



hist(residuos1, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos1)
qqline(residuos1, col = "red")

plot(prediccionAR1, residuos1, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos1)



#MODELOS M5P#


#Precio $

#Particion

index_M5P <- createDataPartition(BD_modelo$precio, p = 0.7, list = FALSE)
data_trainM5P <- BD_modelo[index_M5P, ]  # 70% de los datos
data_testM5P <- BD_modelo[-index_M5P, ]  # 30% restantes

#Mejora del modelo (M5P)
modelo_M5P <- M5P(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_trainM5P)


summary(modelo_M5P)


#PREDICTORES
pred_m5p <- predict(modelo_M5P, data_testM5P)

summary(pred_m5p)

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testM5P$precio - pred_m5p)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testM5P$precio, pred_m5p)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testM5P, aes(x = precio, y = pred_m5p)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()



residuos <- data_testM5P$precio - pred_m5p



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(pred_m5p, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)



#Precio LOG

#Particion

index_M5P1 <- createDataPartition(BD_modelo$Precio_log, p = 0.7, list = FALSE)
data_trainM5P1 <- BD_modelo[index_M5P1, ]  # 70% de los datos
data_testM5P1 <- BD_modelo[-index_M5P1, ]  # 30% restantes

#Mejora del modelo (M5P)
model_m5plog <- M5P(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_trainM5P1)


#PREDICTORES
pred_m5pl <- predict(model_m5plog, data_testM5P1)

summary(pred_m5pl)


#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testM5P1$Precio_log - pred_m5pl)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testM5P1$Precio_log, pred_m5pl)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testM5P1, aes(x = Precio_log, y = pred_m5pl)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()



residuos <- data_testM5P1$Precio_log - pred_m5pl



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(pred_m5pl, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)


#__________Redes Neuronales___________#


BDRNN <- BD_modelo

str(BDRNN)



#$Precio$


#ParticiÓn

index_RNN <- createDataPartition(BDRNN$precio, p = 0.7, list = FALSE)
data_trainRNN <- BDRNN[index_RNN, ]  # 70% de los datos
data_testRNN <- BDRNN[-index_RNN, ]  # 30% restantes


#Normalizar

preProcessRangeModel <- preProcess(data_trainRNN, method = c("range"))


trainproc <- predict(preProcessRangeModel, data_trainRNN)

summary(trainproc)


#Configuración de validación cruzada
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#Hiperparametro del modelo
grid <- expand.grid(size = c(2:12), decay = c(0, 0.001, 0.01))



#ENTRENAMIENTO DE LA RED NEURONAL

net <- train(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = trainproc, method = "nnet", trControl = control, tuneGrid = grid, metric = "RMSE")
net
plot(net)
net$bestTune

#Entrenamiento de la Red Neuronal ajustada

grid1 <- expand.grid(size = c(7), decay = c(0.001))

net1 <- train(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = trainproc, method = "nnet", trControl = control, tuneGrid = grid1, metric = "RMSE")
net1

#Visualización y analisis

plotnet(net1, pos_col = "green", neg_col = "blue")
garson(net1)
varImp(net1)


#Evaluacion en el conjunto de entrenamiento

predtrainproc <- predict(net1, newdata = data_trainRNN)
RMSEtr <- RMSE(predtrainproc, trainproc$precio)
RMSEtr


#Evaluacion en el conjunto de prueba


testproc <- predict(preProcessRangeModel, data_testRNN)

predtestproc <- predict(net1, newdata = testproc)

RMSEtest <- RMSE(predtestproc, testproc$precio)
RMSEtest

#Escala original de las predicciones
# Cálculo de los valores mínimo y máximo de la variable objetivo en el conjunto de prueba original
minprec <- min(data_testRNN$precio)
maxprec <- max(data_testRNN$precio)


predtest <- predtestproc * (maxprec - minprec) + minprec
RMSEtest_original <- RMSE(predtest, data_testRNN$precio)
RMSEtest_original



#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testRNN$precio - predtest)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testRNN$precio, predtest)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testRNN, aes(x = precio, y = predtest)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()




residuos <- data_testRNN$precio - predtest



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(predtest, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)



#Precio log


#ParticiÓn

index_RNN1 <- createDataPartition(BDRNN$Precio_log, p = 0.7, list = FALSE)
data_trainRNN1 <- BDRNN[index_RNN1, ]  # 70% de los datos
data_testRNN1 <- BDRNN[-index_RNN1, ]  # 30% restantes



#Normalizar

preProcessRangeModel1 <- preProcess(data_trainRNN1, method = c("range"))


trainproc1 <- predict(preProcessRangeModel1, data_trainRNN1)

summary(trainproc1)


#Configuración de validación cruzada
control1 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#Hiperparametro del modelo
grid2 <- expand.grid(size = c(2:12), decay = c(0, 0.001, 0.01))



#ENTRENAMIENTO DE LA RED NEURONAL

net2 <- train(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = trainproc1, method = "nnet", trControl = control1, tuneGrid = grid2, metric = "RMSE")
net2
plot(net2)
net2$bestTune

#Entrenamiento de la Red Neuronal ajustada

grid3 <- expand.grid(size = c(9), decay = c(0.01))

net3 <- train(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = trainproc1, method = "nnet", trControl = control1, tuneGrid = grid3, metric = "RMSE")
net3

#Visualización y analisis

plotnet(net3, pos_col = "green", neg_col = "blue")
garson(net3)
varImp(net3)


#Evaluacion en el conjunto de entrenamiento

predtrainproc1 <- predict(net3, newdata = data_trainRNN1)
RMSEtr1 <- RMSE(predtrainproc1, trainproc1$Precio_log)
RMSEtr1


#Evaluacion en el conjunto de prueba


testproc1 <- predict(preProcessRangeModel1, data_testRNN1)

predtestproc1 <- predict(net3, newdata = testproc1)

RMSEtest1 <- RMSE(predtestproc1, testproc1$Precio_log)
RMSEtest1

#Escala original de las predicciones
# Cálculo de los valores mínimo y máximo de la variable objetivo en el conjunto de prueba original
minprec1 <- min(data_testRNN1$Precio_log)
maxprec1 <- max(data_testRNN1$Precio_log)


predtest1 <- predtestproc1 * (maxprec1 - minprec1) + minprec1
RMSEtest_original1 <- RMSE(predtest1, data_testRNN1$precio)
RMSEtest_original1



#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testRNN1$Precio_log - predtest1)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testRNN1$Precio_log, predtest1)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testRNN1, aes(x = Precio_log, y = predtest1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


residuos <- data_testRNN1$Precio_log - predtest1



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(predtest1, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)



#____________________________________.5. SUPPORT VECTOR MACHINE





BD_SVM <- BD_modelo 

str(BD_SVM)



# PRECIO $


index_SVM <- createDataPartition(BD_SVM$precio, p = 0.7, list = FALSE)
data_trainSVM <- BD_SVM[index_SVM, ]  # 70% de los datos
data_testSVM <- BD_SVM[-index_SVM, ]  # 30% restantes






#1. Kernel Lineal
svm <- ksvm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_trainSVM,
            kernel = "vanilladot")

svm

predSVM <- predict(svm, newdata = data_testSVM)
RMSEtest_SVM<-RMSE(predSVM, data_testSVM$precio)
RMSEtest_SVM 


#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testSVM$precio - predSVM)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testSVM$precio, predSVM)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testSVM, aes(x = precio, y = predSVM)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


residuos <- data_testSVM$precio - predSVM



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(predSVM, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)






#2. Kernel Gaussiano

svm2 <- ksvm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_trainSVM,
             kernel = "rbfdot")

svm2

predSVM1 <- predict(svm2, newdata = data_testSVM)
RMSEtest_SVM2<-RMSE(predSVM1, data_testSVM$precio)
RMSEtest_SVM2 #mejor resultado


#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testSVM$precio - predSVM1)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testSVM$precio, predSVM1)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testSVM, aes(x = precio, y = predSVM1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


residuos <- data_testSVM$precio - predSVM1



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(predSVM1, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)

#Precio LOG

index_SVM1 <- createDataPartition(BD_SVM$Precio_log, p = 0.7, list = FALSE)
data_trainSVM1 <- BD_SVM[index_SVM1, ]  # 70% de los datos
data_testSVM1 <- BD_SVM[-index_SVM1, ]  # 30% restantes





#3. Kernel Gaussiano*******************

svm3 <- ksvm(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_trainSVM1,
             kernel = "rbfdot")


svm3

pred3 <- predict(svm3, newdata = data_testSVM1)
str(pred3)
RMSEtest_SVM3<-RMSE(pred3, data_testSVM1$Precio_log)
RMSEtest_SVM3

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_testSVM1$Precio_log - pred3)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_testSVM1$Precio_log, pred3)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_testSVM1, aes(x = Precio_log, y = pred3)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


residuos <- data_testSVM1$Precio_log - pred3



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(pred3, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)



#2.6. ENSEMBLES


#precio $


BD_EN <- BD_modelo

index_EN <- createDataPartition(BD_EN$precio, p = 0.7, list = FALSE)
data_train_EN <- BD_EN[index_EN, ]  # 70% de los datos
data_test_EN <- BD_EN[-index_EN, ]  # 30% restantes


# Establecer parámetro de control.
control_EN <- trainControl(method="repeatedcv", number=10, repeats=3,      
                        savePredictions=TRUE, search = "random")

# Establecer la lista de los algoritmos.
algorithmList <- c('rf','lm', 'svmRadial', "nnet")

# Entrenamos el conjunto de modelos.
set.seed(123)
modelsEN <- caretList(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_train_EN, 
                    metric="RMSE",
                    trControl=control_EN, 
                    methodList=algorithmList)

#Resultados
results <- resamples(modelsEN)
summary(results)
dotplot(results)
modelCor(resamples(modelsEN))







#mejores resultados --> RF, svmRadial

#STACK CON  RF COMO SECOND LEVEL ALGORITHM

stackControl <- trainControl(method="repeatedcv", 
                             number=10,
                             repeats=3, 
                             savePredictions=TRUE,
                             search="random")

stack.rf <- caretStack(modelsEN, method="rf",
                       metric="RMSE", 
                       trControl = stackControl)

print(stack.rf)



#PREDICCIONES DE STACKS

pred_RF<-predict(stack.rf , newdata=data_test_EN)
pred_RF <- pred_RF$pred
str(pred_RF)
RMSEtest_EN_RF<-RMSE(pred_RF, data_test_EN$precio) 
RMSEtest_EN_RF

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_test_EN$precio - pred_RF)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_test_EN$precio, pred_RF)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_test_EN, aes(x = precio, y = pred_RF)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


#STACK CON LM COMO SECOND LEVEL ALGORITHM

RNGkind("Super", "Inversion")
set.seed(123)
stack.lm <- caretStack(modelsEN, method="lm", metric="RMSE", trControl=stackControl)

print(stack.lm)

#PREDICTORES

pred.LM<-predict(stack.lm, newdata=data_test_EN)

pred.LM <- pred.LM$pred
str(pred.LM)

RMSEtest_EN_LM<-RMSE(pred.LM, data_test_EN$precio)
RMSEtest_EN_LM

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_test_EN$precio - pred.LM)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_test_EN$precio, pred.LM)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_test_EN, aes(x = precio, y = pred.LM)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()



#STACK CON SVMR COMO SECOND LEVEL ALGORITHM

# Control de entrenamiento para el stacking con svmRadial
stackControl_svm <- trainControl(method = "repeatedcv", 
                                 number = 10,
                                 repeats = 3, 
                                 savePredictions = TRUE,
                                 search = "random")

# Crear el modelo de stacking con svmRadial como meta-modelo
stack.svm <- caretStack(modelsEN, 
                        method = "svmRadial",   # Cambiar a svmRadial
                        metric = "RMSE", 
                        trControl = stackControl_svm)

# Resultados del modelo stacking con svmRadial
print(stack.svm)

#PREDICTORES

pred_E_SVM<-predict(stack.svm, newdata=data_test_EN)

pred_E_SVM <- pred_E_SVM$pred
str(pred_E_SVM)

RMSEtest_EN_SVM<-RMSE(pred_E_SVM, data_test_EN$precio)
RMSEtest_EN_SVM

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_test_EN$precio - pred_E_SVM)^2))

cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_test_EN$precio, pred_E_SVM)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_test_EN, aes(x = precio, y = pred_E_SVM)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()



##CONCLUSIONES ENSEMBLE --> mejor RMSE --> stacking con RF como second level algorithm.


#_________________________RANDOM FOREST______________________#

#2.7. RANDOM FOREST

BD_RF <- BD_modelo

#PRECIO $

#Particion
index_RF <- createDataPartition(BD_RF$precio, p = 0.7, list = FALSE)
data_train_RF <- BD_RF[index_RF, ]  # 70% de los datos
data_test_RF <- BD_RF[-index_RF, ]  # 30% restantes



#ver que hiperparametros pueden optimizarse en ranger
modelLookup("rf")

ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats =3)

grid_rf <- expand.grid(.mtry = c(2:20))

m_rf <- train(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_train_RF, method = "rf",
              metric = "RMSE", trControl = ctrl,
              tuneGrid = grid_rf)

m_rf
plot(m_rf)

varImp(m_rf)

#prediccion

predRF<-predict(m_rf, newdata=data_test_RF)
RMSEtest_RF<-RMSE(predRF, data_test_RF$precio)
RMSEtest_RF

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_test_RF$precio - predRF)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_test_RF$precio, predRF)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_test_RF, aes(x = precio, y = predRF)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


residuos <- data_test_RF$precio - predRF



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(predRF, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)





#PRECIO LOG

#Particion
index_RF1 <- createDataPartition(BD_RF$Precio_log, p = 0.7, list = FALSE)
data_train_RF1 <- BD_RF[index_RF1, ]  # 70% de los datos
data_test_R1F <- BD_RF[-index_RF1, ]  # 30% restantes



#ver que hiperparametros pueden optimizarse en ranger
modelLookup("rf")

ctrl1 <- trainControl(method = "repeatedcv",
                     number = 10, repeats =3)

grid_rf1 <- expand.grid(.mtry = c(2:20))

m_rf1 <- train(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_train_RF1, method = "rf",
              metric = "RMSE", trControl = ctrl1,
              tuneGrid = grid_rf1)

m_rf1
plot(m_rf1)

varImp(m_rf)

#prediccion

predRF1<-predict(m_rf1, newdata=data_test_R1F)
RMSEtest_RF1<-RMSE(predRF1, data_test_R1F$Precio_log)
RMSEtest_RF1

#Evaluar predicciones

# Calcular el RMSE
rmse <- sqrt(mean((data_test_R1F$Precio_log - predRF1)^2))
cat("RMSE:", rmse, "\n")

# Calcular el R²
r2 <- cor(data_test_R1F$Precio_log, predRF1)^2
cat("R²:", r2, "\n")

#PLOT

library(ggplot2)
ggplot(data_test_R1F, aes(x = Precio_log, y = predRF1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


residuos <- data_test_R1F$Precio_log - predRF1



hist(residuos, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

qqnorm(residuos)
qqline(residuos, col = "red")

plot(predRF1, residuos, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")



shapiro.test(residuos)

ggplot(data.frame(residuos), aes(x = residuos)) + 
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Curva de densidad de los errores", x = "Errores")









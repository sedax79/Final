
#Librerias

library(ggplot2)
library(psych)
library(skimr)
library(tidyr)
library(dplyr)
library(ggcorrplot)
library(car)
library(caret)

#Librerias MACHINE LEARNING

library(readr)
library(Hmisc)
library(corrplot)
library(plyr)
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
library(recipes)
library(NeuralNetTools)
library(nnet)
library(kernlab)
library(ranger)
library(caretEnsemble)
library(tictoc)
library(readxl)


###___________Cargar base de datos__________###

BD_final<-read.csv('C:/Mineria de datos/Final/BD/BD_Final_S_Acc_DOT.csv')

names(BD_final)

str(BD_final)



###________Preparar Base para los modelos________###

# Logaritmizando la variable Precio

BD_final$Precio_log <- log(BD_final$precio)
str(BD_final)

# Convertir el 0 a 1 en baños (No puede haber vivienda sin baño)

BD_final <- BD_final %>%
  mutate(BANOS = case_when(
    BANOS == 0 ~ 1,
    TRUE ~ BANOS  # mantiene valores existente
  ))




###________ANALISIS DESCRIPTIVO________###

Descrip_basc <- summary(BD_final)

Descrip_basc



###________Plots________###

#________Graficos de dispersion________#

#Relación entre Precio y distancia al CBD por tipo de vivienda ($ Y LOG)

ggplot(data = BD_final) +
  geom_point(mapping = aes(x = Distance, y = Precio_log)) + 
  facet_wrap(~ tipo, nrow = 3) +
  theme_minimal()+
  labs(title = "Relación entre Precio y distancia al CBD por tipo de vivienda", x = "Distancia", y = "Precio (Log)")

ggplot(data = BD_final) +
  geom_point(mapping = aes(x = Distance, y = precio)) + 
  facet_wrap(~ tipo, nrow = 3) +
  theme_minimal()+
  labs(title = "Relación entre Precio y distancia al CBD por tipo de vivienda", x = "Distancia", y = "Precio ($)")



#Relación entre Precio e Indice de accesibilidas por tipo de vivienda ($ Y LOG)

ggplot(data =BD_final)+ 
  geom_point(mapping = aes(x = I_A, y = Precio_log)) + 
  facet_wrap(~ tipo, nrow = 3) +
  theme_minimal() +
  labs(title = "Relación entre Precio e Indice de Accesibilidad por tipo de vivienda", x = "Indx_Acc", y = "Precio (Log)")

ggplot(data =BD_final)+ 
  geom_point(mapping = aes(x = I_A, y = precio)) + 
  facet_wrap(~ tipo, nrow = 3) +
  theme_minimal() +
  labs(title = "Relación entre Precio e Indice de Accesibilidad por tipo de vivienda", x = "Indx_Acc", y = "Precio ($)")



#________Graficos de distribución (histogramas)________#

#___________Grafico unico de variables continuas

#Convertir las columnas relevantes en formato long

BD_long <- BD_final %>%
  select(precio, Precio_log, AREA_CONST, BANOS, RECAMARAS, Edad_inmue, Distance, I_A) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Valor"
  )

# Grafico combinado

ggplot(BD_long, aes(x = Valor)) +
  geom_histogram(bins = 60, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) + # Usa facet_wrap para separar por variable
  labs(title = "Distribución de Variables Continuas", x = "Valor", y = "Frecuencia") +
  theme_minimal()



#________Graficos de caja (Boxplot)________#


#________Grafico unico de variables continuas

ggplot(BD_long, aes(y = Valor)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +  # Panel para cada variable, con escalas independientes
  labs(title = "Distribuciones de Variables Continuas", y = "Valor") +
  theme_minimal()


#________(Continuas vs Tipo de vivienda)

#Convertir las columnas relevantes en formato long

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

ggplot(BD_long1, aes(x = tipo, y = Valor)) +
  geom_violin(fill = "lightgreen") +
  facet_wrap(~ Variable, scales = "free") +  # Panel por cada variable
  labs(title = "Distribuciones de Variables por Tipo de Vivienda", x = "Tipo de Vivienda", y = "Valor") +
  theme_minimal()


#________Matriz de correlacion________#

#COLINIALIDAD

# Calcular la matriz de correlación (var continuas)
cor_matrix <- cor(BD_final[, c("Precio_log", "AREA_CONST", "RECAMARAS", "BANOS", "Edad_inmue", "Distance", "I_A", "CPV_Prop_s")], use = "complete.obs")

ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, colors = c("red", "white", "blue"))



#_____________ANALISIS INFERENCIAL_______________#


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
summary(modelo)


#Realizar la regresión lineal precio nominal
modelo1 <- lm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = BD_modelo)
summary(modelo1)


#VIF

# Calcular el VIF
vif(modelo)
vif(modelo1)


# Diagnóstico de residuos

par(mfrow = c(2, 2))  # Disposición para 4 gráficos
plot(modelo1)  # Gráficos de diagnóstico

#__________Mostrar y comparar modelos_________#

lista_modelos <- list(
  "Modelo lineal Log" = modelo,
  "Modelo lineal $" = modelo1
)

resultados_modelos <- purrr::map_dfr(lista_modelos, tidy, .id = "Modelo")

resumen_modelos <- purrr::map_dfr(lista_modelos, glance, .id = "Modelo")


write.csv(resultados_modelos, "C:/Mineria de datos/Final/BD/resultados_coeficientes.csv", row.names = FALSE)
write.csv(resumen_modelos, "C:/Mineria de datos/Final/BD/resultados_resumen.csv", row.names = FALSE)



#________________ANALISIS PREDICTIVO__________________#


# Plantar semilla para reproductivilidad
set.seed(123)  

#____________Regresion lineal multiple______________#

# PRECIO ($)

#Particion

index <- createDataPartition(BD_modelo$precio, p = 0.7, list = FALSE)
data_train <- BD_modelo[index, ]
data_test <- BD_modelo[-index, ]

#Modelo simple
modelo_pred1 <- lm(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_train)
summary(modelo_pred)



# PRECIO (LOG)

#Particion

index1 <- createDataPartition(BD_modelo$Precio_log, p = 0.7, list = FALSE)
data_train1 <- BD_modelo[index1, ]
data_test1 <- BD_modelo[-index1, ]


#Modelo simple
modelo_pred2 <- lm(Precio_log ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_train1)
summary(modelo_pred3)



#________________PREDICCION__________________#


# Predicciones en el conjunto de prueba

#PRECIO $
predicciones <- predict(modelo_pred1, newdata = data_test)

#PRECIO LOG
predicciones1 <- predict(modelo_pred2, newdata = data_test1)



#__________Evaluar predicciones__________#


#RMSE y R2

#PRECIO $

rmse <- sqrt(mean((data_test$precio - predicciones)^2))
cat("RMSE:", rmse, "\n")

r2 <- cor(data_test$precio, predicciones)^2
cat("R²:", r2, "\n")


#PRECIO LOG

rmse <- sqrt(mean((data_test1$Precio_log - predicciones1)^2))
cat("RMSE:", rmse, "\n")

r2 <- cor(data_test1$Precio_log, predicciones1)^2
cat("R²:", r2, "\n")


#VIF

# Calcular el VIF
vif(modelo_pred1)
vif(modelo_pred2)


#_________RESIDUOS____________#

par(mfrow = c(2, 2))  # Disposición para 4 gráficos

#Precio $
plot(modelo_pred1)  # Gráficos de diagnóstico

#Precio Log
plot(modelo_pred2)  # Gráficos de diagnóstico

#Guardar Residuos
residuoslm <- residuals(modelo_pred1)
residuoslm1 <- residuals(modelo_pred2)

hist(residuoslm, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")
hist(residuoslm1, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

shapiro.test(residuoslm)
shapiro.test(residuoslm1)

#_________PLOT AJUSTES____________#

#PRECIO $

library(ggplot2)
ggplot(data_test, aes(x = precio, y = predicciones)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


#PRECIO Log

library(ggplot2)
ggplot(data_test1, aes(x = Precio_log, y = predicciones1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()



###________________________________MACHINE LEARNING___________________________________###


#____________Random Forest_____________#

BD_RF <- BD_modelo



#PRECIO $

#Particion
index_RF <- createDataPartition(BD_RF$precio, p = 0.8, list = FALSE)
data_train_RF <- BD_RF[index_RF, ]  
data_test_RF <- BD_RF[-index_RF, ]  

#Hiperparametros
modelLookup("rf")

#Validacion cruzada
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats =3)

grid_rf <- expand.grid(.mtry = c(2:20))

#Entrenamiento del modelo
m_rf <- train(precio ~ AREA_CONST + RECAMARAS + BANOS + Edad_inmue + Jardin + Cisterna + Seguridad + Alberca + Lavanderí + Terraza + Gimnasio + Cuarto.de + Chimenea + Aire.acond + Elevador + CPV_Prop_s + Distance + I_A + tipo_vivienda, data = data_train_RF, method = "rf",
              metric = "RMSE", trControl = ctrl,
              tuneGrid = grid_rf)

m_rf
plot(m_rf)

#Importancia de variables
varImp(m_rf)

#Prediccion

predRF<-predict(m_rf, newdata=data_test_RF)
RMSEtest_RF<-RMSE(predRF, data_test_RF$precio)
RMSEtest_RF

#Evaluar predicciones

# Calcular el RMSE
rmseRF <- sqrt(mean((data_test_RF$precio - predRF)^2))
cat("RMSE:", rmseRF, "\n")

# Calcular el R²
r2RF <- cor(data_test_RF$precio, predRF)^2
cat("R²:", r2RF, "\n")

#RESIDUOS Y AJUSTE

residuosRF <- data_test_RF$precio - predRF

qqnorm(residuosRF)
qqline(residuosRF, col = "red")

plot(predRF, residuosRF, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")

hist(residuosRF, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

shapiro.test(residuosRF)

ggplot(data.frame(residuosRF1), aes(x = residuosRF1)) + 
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Curva de densidad de los errores", x = "Errores")

#Ajuste

ggplot(data_test_RF, aes(x = precio, y = predRF)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()


#PRECIO LOG

#Particion
index_RF1 <- createDataPartition(BD_RF$Precio_log, p = 0.8, list = FALSE)
data_train_RF1 <- BD_RF[index_RF1, ]
data_test_R1F <- BD_RF[-index_RF1, ]

#Hiperparametro
modelLookup("rf")

ctrl1 <- trainControl(method = "repeatedcv",
                      number = 10, repeats =3)

grid_rf1 <- expand.grid(.mtry = c(2:20))

#Entrenamiento del modelo
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
rmseRF1 <- sqrt(mean((data_test_R1F$Precio_log - predRF1)^2))
cat("RMSE:", rmseRF1, "\n")

# Calcular el R²
r2RF1 <- cor(data_test_R1F$Precio_log, predRF1)^2
cat("R²:", r2RF1, "\n")

#RESIDUOS Y AJUSTE


residuosRF1 <- data_test_R1F$Precio_log - predRF1


qqnorm(residuosRF1)
qqline(residuosRF1, col = "red")

plot(predRF1, residuosRF1, 
     main = "Residuos vs Predicciones", 
     xlab = "Valores Predichos", 
     ylab = "Residuos", 
     pch = 20)
abline(h = 0, col = "red")

hist(residuosRF1, breaks = 20, main = "Histograma de los residuos", xlab = "Residuos", col = "blue")

shapiro.test(residuosRF1)

ggplot(data.frame(residuosRF1), aes(x = residuosRF1)) + 
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Curva de densidad de los errores", x = "Errores")

#Ajuste
ggplot(data_test_R1F, aes(x = Precio_log, y = predRF1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Valores Reales",
       x = "Valores Reales",
       y = "Predicciones") +
  theme_minimal()



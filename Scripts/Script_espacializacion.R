#Espacializar
library(ggplot2)
library(sf)

BD_final1<-read.csv('C:/Mineria de datos/Final/BD/Base_final_filtrada.csv')


str(data_test_R1F$ID)  # Reemplaza 'data_testAR' por el nombre de tu base
str(BD_final1$ID)    # Reemplaza 'otra_base' por el nombre de la otra base

data_test_R1F$ID <- as.character(data_test_R1F$ID)
BD_final1$ID <- as.character(BD_final1$ID)


# Realizar la unión
data_unida <- merge(data_test_R1F, BD_final1[, c("ID", "lat", "lon")], by = "ID", all.x = TRUE)


data_testRF_sf <- st_as_sf(data_unida, coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = data_testRF_sf, aes(color = residuos), size = 2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Mapa de Sesgo de Predicción", color = "Sesgo (Residuos)") +
  theme_minimal()


data_testRF_sf$precio_m2 <- data_testRF_sf$Precio_log / data_testRF_sf$AREA_CONST




write.csv(data_unida, "C:/Mineria de datos/Final/BD/Base_Res.csv", row.names = FALSE)

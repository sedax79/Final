
#Libreria

library(dplyr)
library(stringr)


#_________________Unir BD con censo__________________#


#Cargar datos del Censo

BD <- read.csv("C:/Mineria de datos/Final/SHP/cpv20.csv", 
                 colClasses = c("MUN" = "character", 
                                "LOC" = "character",
                                "MZA" = "character"))
str(BD1)
names(BD)


#Construir clave de manzana

BD$CVE_MZA1 <- paste0(BD$MUN,BD$LOC,BD$AGEB,BD$MZA)

head(BD$CVE_MZA1)
tail(BD1)
head(BD1)


#Construir variable de servicios (proporcion)

#Eliminar totales 
BD <- BD[!grepl("(?i)total", BD$NOM_LOC, perl = TRUE), ]


#Extraer variables
BD1 <- BD[c("CVE_MZA1","VIVTOT","VPH_C_ELEC","VPH_AEASP","VPH_DRENAJ","VPH_C_SERV")]

#Convertir en numericas
BD1 <- BD1 %>%
  mutate(
    across(c(VPH_C_ELEC, VPH_AEASP, VPH_DRENAJ, VPH_C_SERV), as.numeric))

#Obtener proporciones
BD1 <- BD1 %>%
  mutate(
    Prop_el = (VPH_C_ELEC/VIVTOT)*100,
    Prop_ag = (VPH_AEASP/VIVTOT)*100,
    Prop_dj = (VPH_DRENAJ/VIVTOT)*100,
    Prop_sv = (VPH_C_SERV/VIVTOT)*100)



write.csv(BD1, "C:/Mineria de datos/Final/SHP/CPV_clean.csv", row.names = FALSE)


#PROCESO EN QGIS
#PROCESO EN QGIS
#PROCESO EN QGIS
#PROCESO EN QGIS
#PROCESO EN QGIS
#PROCESO EN QGIS


#Cargar BD con indice de accesibilidad (proceso que se hizo en Qgis)

BD_final<-read.csv('C:/Mineria de datos/Final/BD/Base_C_S__IA_final.csv')

names(BD_final)
head(BD_final)


#Seleccion final de variables

BD_final<- BD_final[c("ID","precio","tipo","AREA_CONST","RECAMARAS","BANOS","No_de_piso","Edad_inmue","Jardin","Cisterna","Seguridad",
                      "Zona.priva","Alberca","Lavanderí","Terraza","Gimnasio","Cuarto.de","Chimenea","Sótano","Aire.acond","Elevador",
                      "Urbanizado","CPV_Prop_e","CPV_Prop_a","CPV_Prop_d","CPV_Prop_s","Distance","I_A")]
str(BD_final)



#__________________Limpieza final______________#

#Cambiar a 1 y 0 variables de amenidades

BD_final <- BD_final %>%
  mutate(across(c("Jardin","Cisterna","Seguridad",
                  "Zona.priva","Alberca","Lavanderí","Terraza","Gimnasio","Cuarto.de","Chimenea","Sótano","Aire.acond","Elevador",
                  "Urbanizado"), ~ ifelse(. == "VERDADERO", 1, 0)))

str(BD_final)


#cambiar a categorias variable tipo

BD_final <- BD_final %>%
  mutate(tipo_vivienda = case_when(
    tipo == "Departamento" ~ 1,
    tipo == "Casa" ~ 2,
    tipo == "Casa en condominio" ~ 3,
    TRUE ~ NA_real_  # Maneja casos no definidos (si los hay)
  ))


str(BD_final)

write.csv(BD_final, "C:/Mineria de datos/Final/BD/BD_Final_S_Acc_DOT.csv", row.names = FALSE)





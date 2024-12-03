
#____________________#####Manipulación de datos#####____________________#

#__________Librerias__________#

library(dplyr)
library(stringr)

#__________Funciones__________#

#paste()        (Base)    -> "Hace lo similar de arriba, solo que si hay un separador, que si no se especifica es " ""
#mutate()       (DPLYR)   -> "se usa para modificar o agregar nuevas columnas a un data frame"
#ifelse()       (Base)    -> "Realiza una evaluación condicional dentro de un data frame. ifelse(condición, valor_si_verdadero, valor_si_falso)"
#grepl()        (Base)    -> "Busca una expresión regular dentro de una cadena de texto y devuelve un vector lógico (TRUE o FALSE)"
#gsub()         (Base)    -> "Se utiliza para realizar reemplazos de texto en una cadena de caracteres mediante expresiones regulares. Realiza un remplazo en todas las recurrencias, no solo la primera. gsub(pattern, replacement, x)"
#substr()       (Base)    -> "Extrae una subcadena de un string"
#nchar()        (Base)    -> "Calcula la longitud de caracteres en un string"
#str_extract()  (STRINGR) -> "Se usa para extraer la primera coincidencia de un patrón (expresión regular) de un vector de cadenas"
#strsplit()     (Base)    -> "Divide las cadenas de texto de un vector según un patrón específico"
#unlist()       (Base)    -> "Convierte la lista resultante de strsplit() en un vector planificado (sin listas anidadas)"
#unique()       (Base)    -> "Devuelve los valores únicos de un vector."



#__________###Manipulación del Data Frame###__________#

#Elementos necesarios#


BD <-read.csv('C:/Mineria de datos/Final/BD/BD_raw.csv')


df <- BD


#__________Limpieza__________#

#Identificar su en la colm. "amenidades" hay una cadena de texto, y si es verdadero, pegar ese texto en colm. "inf_colonia". Ademas, remplaza en "amenidades" con NA.
df <- df %>%
  mutate(
        inf_colonia = ifelse(grepl("Precio por/m2", amenidades), 
                         paste(amenidades), inf_colonia),
    
    # Reemplazar las filas que contienen "Precio por/m2..." en amenidades por NA
    amenidades = ifelse(grepl("Precio por/m2", amenidades), paste(rep("NA", 5), collapse = ", "), amenidades)
  )


#Eliminar el texto "en Venta en DF / CDMX"
df$tipo <- gsub(" en Venta en DF / CDMX", "", df$tipo)


#Extraer los últimos 3 caracteres y crear la columna 'moneda'
df <- df %>%
  mutate(
    moneda = substr(precio, nchar(precio) - 2, nchar(precio)),  # Extraer los últimos 3 caracteres
    precio = substr(precio, 1, nchar(precio) - 3)  # Eliminar los últimos 3 caracteres
  )


# Eliminar el primer carácter '$' de la columna 'precio'
df <- df %>%
  mutate(precio = substr(precio, 2, nchar(precio)))


#__________Separar las características en diferentes columnas__________#

df <- df %>%
  mutate(
    
    #Extraer valores para cada característica usando expresiones regulares
    RECAMARAS = str_extract(caracteristicas, "(?<=RECÁMARAS)\\d+"),
    BANOS = str_extract(caracteristicas, "(?<=BAÑOS)\\d+"),
    ESTACIONAMIENTOS = str_extract(caracteristicas, "(?<=ESTACIONAMIENTOS)\\d+"),
    No_de_pisos = str_extract(caracteristicas, "(?<=No. de pisos)\\d+"),
    Edad_inmueble = str_extract(caracteristicas, "(?<=Edad del inmueble)\\d+|N/D"),
    AREA_TERRENO = str_extract(caracteristicas, "(?<=ÁREA TERRENO)\\d+"),
    AREA_CONSTRUIDA = str_extract(caracteristicas, "(?<=ÁREA CONSTRUIDA)\\d+"),
    Tamano_jardin = str_extract(caracteristicas, "(?<=Tamaño del jardin)\\d+"),
    Jardin = str_extract(caracteristicas, "(?<=Jardín)\\d+")
  ) %>%
  
  #Convertir caracteres vacíos a NA
  mutate(across(everything(), ~ ifelse(. == "", NA, .)))


#_____Explicación de expresion regular______#

#(?<=Tamaño del jardin): Es una búsqueda positiva hacia atrás (positive lookbehind). Esto significa que la expresión buscará solo los números que aparezcan inmediatamente después de la frase "Tamaño del jardin". 
#La parte Tamaño del jardin no se incluirá en el resultado, solo se utiliza como referencia para localizar los números que siguen.
#El ?<= es un operador que especifica que el patrón anterior debe aparecer antes del valor que queremos extraer.
#\\d+: Este es el patrón que indica que se deben extraer uno o más dígitos (\\d significa "un dígito" y el + significa "uno o más").
#\\d es una forma de escribir el carácter que representa un dígito en las expresiones regulares. En R, el doble backslash (\\) es necesario para escapar el carácter especial en la cadena de texto.
#+ significa que debe haber uno o más dígitos.



#__________Extraer amenidades__________#


#Obtener los valores únicos de amenidades#
amenidades_unicas <- unique(unlist(strsplit(df$amenidades, ", ")))


# Paso 2: Crear una columna para cada amenidad
for (amenidad in amenidades_unicas) {
  df[[amenidad]] <- ifelse(grepl(amenidad, df$amenidades), 1, 0)
}




#__________Ordenar y extraer Inf_colonias__________#


# Crear las columnas "Precio m2" y "zona"
df <- df %>%
  mutate(
    # Para la columna "zona", extraemos lo que está entre "Precio por/m2 promedio en " y la coma
    zona = sub("Precio por/m2 promedio en (.*?),.*", "\\1", inf_colonia),
    
    # Para la columna "Precio m2", extraemos el valor entre "$" y "mil MN"
    `Precio m2` = sub(".*\\$ ([0-9.]+) mil MN", "\\1", inf_colonia)
  )

head(df)

#__________Seleccion de variables__________#

#Conocer nombres de columnas

names(df)

#Seleccion final de variables
df_filtrado <- df[, c("ID","calle_colonia","delegacion","cp","tipo","precio","moneda","lat","lon","AREA_CONSTRUIDA","RECAMARAS","BANOS","No_de_pisos","Edad_inmueble","Jardin","Cisterna","Seguridad privada","Zona privada","Alberca","Lavandería","Terraza","Gimnasio","Cuarto de servicio","Chimenea","Sótano","Aire acondicionado","Elevador","Urbanizado")]


#Filtrado de observaciones
df_filtrado <- df_filtrado %>%
  filter(tipo %in% c("Casa en condominio","Casa", "Departamento"))

#Cambiar valores NA Y "N/D"

df_filtrado <- df_filtrado %>%
  mutate(No_de_pisos = ifelse(is.na(No_de_pisos), 1, No_de_pisos))

df_filtrado <- df_filtrado %>%
  mutate(RECAMARAS = ifelse(is.na(RECAMARAS), 1, RECAMARAS))

df_filtrado <- df_filtrado %>%
  mutate(Edad_inmueble = ifelse(is.na(Edad_inmueble) | Edad_inmueble == "N/D", 1, Edad_inmueble))

df_filtrado <- df_filtrado %>%
  mutate(Jardin = ifelse(is.na(Jardin), 0, Jardin))


#Cambiar de caracter a numero o factor

df_filtrado <- df_filtrado %>%
  mutate(
    across(c(No_de_pisos, AREA_CONSTRUIDA, RECAMARAS, BANOS, Edad_inmueble), as.numeric),
    across(c(Jardin, Cisterna, `Seguridad privada`, `Zona privada`, Alberca, 
             Lavandería, Terraza, Gimnasio, `Cuarto de servicio`, Chimenea, 
             Sótano, `Aire acondicionado`, Elevador, Urbanizado), as.factor)
  )


#Cambiar valores mayores a 5 en No_pisos (Se contabilizan el numero de pisos del edificio, no del departamento)
df_filtrado <- df_filtrado %>%
  mutate(No_de_pisos = ifelse(No_de_pisos > 10, 1, No_de_pisos))

#Eliminar observaciones con NA

df_filtrado <- df_filtrado %>%
  filter(!is.na(AREA_CONSTRUIDA))

str(df_filtrado)


#__________Guardar base de datos__________#

write.csv(df_filtrado, "C:/Mineria de datos/Final/BD/Base_final_filtrada.csv", row.names = FALSE)


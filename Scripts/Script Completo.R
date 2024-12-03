
#____________________#####Scrapping#####____________________#

#__________Librerias__________#

library(httr)
library(rvest)
library(xml2)
library(purrr)
library(dplyr)

#__________Funciones__________#

#paste0()           (Base)    -> "Hace un solo elemento uniendo texto o el producto de otra función en una cadena de texto sin espacios"
#paste()            (Base)    -> "Hace lo similar de arriba, solo que si hay un separador, que si no se especifica es " ""
#GET()              (HTTR)    -> "Hace una llamada, una solucitud a la pagina y se guarda en un elemento"
#status_code()      (HTTR)    -> "Verifica cual es el codigo de respuesta de la solicitud a la pagina. Si es = 200, fue una solicitud exitosa"
#content()          (HTTR)    -> "Se utiliza para extraer el contenido de una respuesta HTTP (obtenida típicamente con una función como GET() o POST()) en un formato específico, en este caso, como texto"
#file_path()        (Base)    -> "Se utiliza para construir rutas de archivos de manera compatible con el sistema operativo, combinando varios elementos de ruta en un solo string"
#write()            (Base)    -> "se utiliza para escribir contenido en un archivo de texto"
#list.files()       (Base)    -> "se utiliza para listar los archivos contenidos en un directorio específico, Arg son: directorio, patron, fullnames"
#html_nodes()       (RVEST)   -> "selecciona todos los nodos que coinciden con una expresión XPath especificada"
#html_attr()        (RVEST)   -> "Extrae el valor del atributo "href" de los nodos seleccionados, que es donde se encuentran los enlaces de las etiquetas <a> en HTML"
#xml_find_first()   (XML2)    -> "se utiliza para encontrar el primer nodo de un documento XML o HTML que coincide con una expresión XPath o selector CSS. Tres arg. El html, el path, y ns (espacios de noombres)"
#xml_text()         (XML2)    -> "se utiliza para extraer el contenido de texto de un nodo XML o HTML"
#is.na()            (Base)    -> "Verifica si en un elemento hay NA"
#xml_attr           (XML2)    -> "se utiliza para extraer el valor de un atributo (content) de un nodo XML o HTML"
#xml_lenght         (XML2)    _> "se utiliza para obtener la longitud de un objeto XML, es decir, el número de nodos que se encuentran en un nodo específico o en un conjunto de nodos dentro de un documento XML o HTML"
#map_chr()          (PURR)    -> "Se utiliza para aplicar una función a cada elemento de una lista o vector, o rango, y devolver un vector de caracteres"
#do.call()          (Base)    -> "se usa para aplicar una función a una lista de argumentos. Permite pasar los elementos de la lista como si fueran argumentos individuales de la función"
#rbind              (Base)    -> "Se usa para unir filas"
#Print()            (Base)    -> ""


#__________Limpiar el entorno__________# 

rm(list = ls())

#__________###Extracción de páginas de resultados###__________#


#Elementos necesarios#

scraper_api_key <- "5a6a85a1636a4c787ab78d3ad906dfe9"

base_url <- "https://propiedades.com/df/venta?pagina="

num_paginas <- 100

directorio_destino <- "C:/Mineria de datos/Final/Paginas/"


#Loop para extraer páginas de resultados#

for (pagina in 1:num_paginas) {
  
  #Construir una URL dinamica
  pagina_url <- paste0(base_url, pagina, "#remates=2")
  url_api_pagina <- paste0("http://api.scraperapi.com/?api_key=", scraper_api_key, "&url=", URLencode(pagina_url), "&render=true")
  
  max_intentos <- 2
  
  # Realiza solicitud a la página de resultados
  for (intento in 1:max_intentos) {
    response <- GET(url_api_pagina)
    
    
    # Verifica si la solicitud fue exitosa
    if (status_code(response) == 200) {
      
      # Extrae el contenido HTML como texto
      page_content <- content(response, "text")
      
      # Construir la ruta completa para guardar el archivo HTML
      archivo_destino <- file.path(directorio_destino, paste0("pagina_", pagina, ".html"))
      
      # Guardar el HTML de cada página en el archivo especificado
      write(page_content, archivo_destino)
      
      print(paste("Página", pagina, "extraída exitosamente. Guardada en:", archivo_destino))
      break
      
    } else {
      #Mensaje de error si la solicitud no es exitosa
      print(paste("Error al acceder a la página", pagina, "- Código de estado:", status_code(response)))
      print(paste("Detalles del error:", error_details))
    }
  }
}

#__________###Extracción de links de páginas de propiedades###__________#


#Elementos necesarios#

enlaces_propiedades <- c()

directorio_html <- "C:/Mineria de datos/Final/Paginas/"

archivos_html <- list.files(directorio_html, pattern = "\\.html$", full.names = TRUE)

num_divs <- 24


#Loop para extraer links, iterando cada página de propiedades#

for (archivo_html in archivos_html) {
  
  #Leer el contenido HTML
  page <- read_html(archivo_html)
  
  #Ciclo para recorrer cada div[i] y extraer los enlaces
  for (i in 1:num_divs) {
    
    #Construir el XPath dinámicamente cambiando el índice [i]
    xpath <- paste0("/html/body/div[1]/div/main/section/div[1]/div/div[2]/div/div[", i, "]/section/section/section[2]/section/div[2]/a")
    
    #Extraer los enlaces del XPath generado
    enlaces <- page %>%
      html_nodes(xpath = xpath) %>%
      html_attr("href")
    
    #Concatenar los enlaces extraídos al vector de enlaces
    enlaces_propiedades <- c(enlaces_propiedades, enlaces)
  }
  
  # Mostrar un mensaje indicando que se han procesado los enlaces de la página
  print(paste("Enlaces extraídos de:", archivo_html))
}

#__________###Extracción de páginas de propiedades###__________#


#Elementos necesarios#

directorio_destino <- "C:/Mineria de datos/Final/propiedades/"

enlaces_propiedades #Se llenó en el loop anterior


#Loop para recorrer cada enlace de propiedad#

for (link in enlaces_propiedades) {
  
  #Construir URL dinamica
  url_propiedad <- paste0("http://api.scraperapi.com/?api_key=", scraper_api_key, "&url=", URLencode(link), "&render=true")
  
  max_intentos <- 2
  
  #Realiza solicitud a la página de resultados
  for (intento in 1:max_intentos) {
    propiedad_response <- GET(url_api_pagina)
    
    
    #Verifica si la solicitud fue exitosa
    if (status_code(propiedad_response) == 200) {
      
      #Extrae el contenido HTML como texto
      page_content <- content(propiedad_response, "text")
      
      #Construir la ruta completa para guardar el archivo HTML
      archivo_destino <- file.path(directorio_destino, paste0("propiedad_", basename(link), ".html"))
      
      #Guardar el HTML de cada página en el archivo especificado
      write(page_content, archivo_destino)
      
      print(paste("Página extraída exitosamente. Guardada en:", archivo_destino))
      break
      
    } else {
      #Mensaje de error si la solicitud no es exitosa
      print(paste("Error al acceder a la propiedad", link, "- Código de estado:", status_code(propiedad_response)))
      print("Detalles del error: Verifica la URL y la configuración de la API.")
    }
  }
}

#__________###Extracción de información de páginas de propiedades###__________#


#Funciones#


#Función (1) para extraer datos con XPath
extract_xpath <- function(doc, path) {
  node <- xml_find_first(doc, path)
  if (!is.na(node)) {
    xml_text(node)
  } else {
    NA
  }
}


#Función (2) para extraer los datos fijos
extract_fixed_data <- function(doc) {
  
  #xml_find_first() busca este primer nodo meta que tiene el atributo itemprop='latitude'
  #xml_attr() extrae el "contenido" del nodo extraido
  latitud <- xml_attr(xml_find_first(doc, "/html/body/div[1]/div/main/div[2]/span/meta[@itemprop='latitude']"), "content")
  longitud <- xml_attr(xml_find_first(doc, "/html/body/div[1]/div/main/div[2]/span/meta[@itemprop='longitude']"), "content")
  
  list(
    direccion_completo = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div[1]/div[2]/h1"),
    calle_colonia = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div[1]/div[2]/h1/em"),
    cp = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div[1]/div[2]/h1/span"),
    tipo = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div[1]/div[2]/h2"),
    delegacion = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div[1]/div[2]/h3[1]/span[2]"),
    ciudad = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div[1]/div[2]/h3[1]/span[3]"),
    precio = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[1]/div/div[2]/h2"),
    ID = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[1]/div[2]/div/div/div[1]/div[2]/div[2]"),
    lon = longitud,
    lat = latitud
  )
}


#Función (3) para extraer los datos variables en la sección 1
extract_variable_data_s1 <- function(doc) {
  list(
    descripcion = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[1]/div[1]/div/div/p"),
    caracteristicas = map_chr(2:10, ~ extract_xpath(doc, paste0("/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[1]/div[2]/div/div/div[", ., "]/div[2]"))),
    amenidades = map_chr(1:5, ~ extract_xpath(doc, paste0("/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[1]/div[3]/div/div/div[", ., "]"))),
    inf_colonia = list(
      col = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[1]/div[4]/div/div/div[1]/h3"),
      prx = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[1]/div[4]/div/div/div[1]/div/div")
    )
  )
}


#Función (4) para extraer los datos del anunciante en la sección 2
extract_variable_data_s2 <- function(doc) {
  list(
    fila1 = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[2]/div/div/div/div/div/div/div/span[1]"),
    fila2 = extract_xpath(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[2]/div/div/div/div/div/div/div/span[2]")
  )
}



#Loop para recorrer cada HTML de propiedad#

#Elementos necesarios#

all_data <- list()

folder_path <- "C:/Mineria de datos/Final/propiedades/"

files <- list.files(folder_path, pattern = "\\.html$", full.names = TRUE)

#Loop 

for (file in files) {
  
  # Leer el archivo HTML
  doc <- read_html(file)
  
  # Extraer datos fijos y variables
  fixed_data <- extract_fixed_data(doc)
  variable_data_s1 <- extract_variable_data_s1(doc)
  
  # Comprobar si existe la sección 2 antes de extraer datos
  if (xml_length(xml_find_first(doc, "/html/body/div[1]/div/main/div[2]/div[2]/div[3]/div[1]/section[2]")) > 0) {
    variable_data_s2 <- extract_variable_data_s2(doc)
  } else {
    variable_data_s2 <- list(fila1 = NA, fila2 = NA)
  }
  
  # Combinar todos los datos en una sola lista
  combined_data <- c(fixed_data, variable_data_s1, variable_data_s2)
  
  # Convertir la lista combinada en un data frame y agregarla a la lista `data4`
  all_data[[length(all_data) + 1]] <- as.data.frame(t(combined_data), stringsAsFactors = FALSE)
}

#Data Frame final#

#Combina en un solo data frame utilizando rbind(), apilando las filas de todos los data frames dentro de la lista
final_data <- do.call(rbind, all_data)

#Crea una BD. Las columnas que son listas, las colapsa en un solo campo
BD <- final_data %>%
  mutate(across(where(is.list), ~ map_chr(.x, ~ paste(unlist(.), collapse = ", "))))

write.csv(BD, "C:/Mineria de datos/Final/BD/BD_raw.csv", row.names = FALSE)


#____________________#####Manipulación de datos#####____________________#

#__________Librerias__________#

library(tidyverse)
library(dplyr)
library(tidyr)
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


#Cambiar valores mayores a 5 en No_pisos
df_filtrado <- df_filtrado %>%
  mutate(No_de_pisos = ifelse(No_de_pisos > 5, 1, No_de_pisos))

#Eliminar observaciones con NA

df_filtrado <- df_filtrado %>%
  filter(!is.na(AREA_CONSTRUIDA))

str(df_filtrado)


#__________Guardar base de datos__________#

write.csv(df_filtrado, "C:/Mineria de datos/Final/BD/Base_final_filtrada.csv", row.names = FALSE)

















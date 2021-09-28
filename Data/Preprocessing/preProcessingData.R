# Este script tiene como objetivo preparar los datos de entrada.
# La zona de testeo que se eligió es Chillán
# El indicador a modelar es accesibilidad escolar utilizando el modelo gravitacional de 3 pasos de Wang et al (2012)
# El test consiste en lo siguiente:
#  Modelar accesibilidad escolar a educación básica municipal en la comuna de Chillán


# Librerías a utilizar
require(tidyverse); require(sf)

#### Bloque para leer manzanas ####
# Las variables de interés son:
#  objectid: la id de la manzana
#  n6a14: la población en edad escolar básica

mzns <- read_sf("input files/mzn_ponderadas.shp") %>%
  rename_all(tolower) %>%
  select(objectid, n6a14) %>%
  st_transform(4326)


#### Preparar datos escolares ####

# Cargar matrículas máximas a nivel de Chile.
# Este archivo contiene la cantidad de estudiantes máximos en el periodo 2004-2020
# El dato se encuentra a tres escalas:
#  Educación Básica
#  Educación Media Científico-Humanista
#  Educación Media Técnico-Profesional

load("matriculaMaxima.RDS")
matriculaMaxima <- matriculaMaxima %>%
  select(rbd, basica_cap)

# Bloque para extraer colegios de Chillán, y sus coordenadas. Se aplican los siguientes filtros.
# Para el test:
# Deben ser colegios municipales (cod_depe2 == 1)
# Deben ser colegios abiertos (estado_estab == 1)
# Deben estar en la comuna de Chillán (cod_com_rbd %in% c(16101, 16103))

directorioEscuelas <- read.csv2("input files/directorio_ee_2020.csv", dec = ".", encoding = "UTF-8") %>%
  rename_all(tolower) %>%
  filter(cod_com_rbd %in% c(16101, 16103), estado_estab == 1, cod_depe2 == 1) %>% # Filtrar colegios de interés 
  select(rbd, nom_rbd, cod_com_rbd, latitud, longitud, cod_depe2) %>% #Seleccionar variables de interés
  mutate(rbd = rbd %>% as.character, # el campo rbd está en numerico. Pasar a character
         nom_rbd = str_to_title(nom_rbd)) %>% # Cambiar el formato de los nombres de colegios
  left_join(matriculaMaxima)

# Crear un sf con los datos de matrícula
# El CRS corresponde al espg 4326 que es WGS84 geodésico
# Adicionalmente, se hace un clip con los datos en función del limite urbano

# Para ello, primero se lee el limite urbano.
# El clip requiere que los datos estén en crs planar
# Una vez realizada la intersección, se vuelven a poner en latlon

limurbano <- read_sf("input files/limurbano.shp") %>%
  st_transform(32719) %>%
  select()

escuelasSf <- st_as_sf(x = directorioEscuelas, coords = c("longitud", "latitud"), crs = 4326) %>%
  st_transform(32719) %>%
  st_intersection(limurbano) %>%
  st_transform(4326) %>%
  rename(codcomdrbd = cod_com_rbd) %>%
  filter(basica_cap > 0)

### Fin Preprocesamiento ###

# Exportar los archivos

write_sf(mzns, "processed files/mzns_procesadas.shp", driver = "ESRI shapefile", delete_layer = T)
write_sf(escuelasSf, "processed files/rbd_procesadas.shp", driver = "ESRI shapefile", delete_layer = T)

save(mzns, file= "processed files/mzns_procesadas.RDS")
save(escuelasSf, file="processed files/rbd_procesadas.RDS")

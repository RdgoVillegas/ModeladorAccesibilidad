# Script para generar las isocronas de la oferta y la demanda, necesarios para el modelo gravitacional

# Librerías a utilizar

library(opentripplanner); library(otpr); library(tidyverse); library(glue);library(sf)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Configurar OTP
path_data <- "c:/OTP"
path_otp <- "c:/OTP/otp.jar"
log2 <- otp_setup(otp = path_otp, router = "chile", dir = path_data, memory = (2048 * 5))
otpcon <- otp_connect()
routingOptions <- otp_routing_options()
routingOptions$walkSpeed <- 1.33

# Leer los datos
# Manzanas
load("processed files/mzns_procesadas.RDS")
# Extraer centroides
mznsCentroide <- mzns %>% 
  st_transform(32719) %>% 
  st_centroid() %>% 
  st_transform(4326)
# Guardar los centroides para uso posterior en el programa
save(mznsCentroide, file = "mznsCentroide.RDS")


# escuelas
load("processed files/rbd_procesadas.RDS")
# Calcular isocronas de escuelas
iso_rbd <-otp_isochrone(otpcon = otpcon, 
                              fromPlace = st_coordinates(escuelasSf), 
                              fromID = escuelasSf$rbd, 
                              mode = "WALK",
                              maxWalkDistance = 1500,
                              routingOptions = routingOptions,
                              date_time = as.POSIXct(paste0("2020-10-12 ", "09:00:00"),tz=Sys.timezone()), 
                              cutoffSec =  c(1200, 2400, 3600)) %>%
  mutate(type = st_geometry_type(.),
         area = st_area(.) %>% as.numeric()) %>%
  st_transform(iso_rbd, 32719) # Transformar a WGS84 
# Guardar resultados
save(iso_rbd, file = "iso_rbd.RDS")
#load(file = "iso_rbd.RDS")
#iso_rbd <- st_transform(iso_rbd, 32719)
# Lo primero que hay que hacer es un cookie cutter de las isocronas.
# Ej: El polígono de la isocrona de un colegio X a 3600 segundos no puede solaparse con la de 2400 ni 1200.
# Para eso, se utiliza st_difference.
# Me imagino que hay formas más eficientes de hacerlo, pero por el tiempo lo haré con un mero forloop

# Crear objeto vacio que contendran las isocronas cortadas
iso_rbdcut <- data.frame()

# Por cada colegio:
for (i in 1:length(iso_rbd$fromPlace %>% unique)){
  # filtrar todas las isocronas de un colegio i
  shp <- filter(iso_rbd, fromPlace == unique(fromPlace)[i])
  # extraer la isocrona de 3600 segs
  zona3600 <- shp[shp$time == 3600,] %>% 
    st_difference(shp[shp$time == 2400,]) %>% 
    select(id, time, fromPlace)
  # extraer la isocrona de 2400 segs
  zona2400 <- shp[shp$time == 2400,] %>%
    st_difference(shp[shp$time == 1200,]) %>% 
    select(id, time, fromPlace)
  # extraer la isocrona de 1200 segs
  zona1200 <- shp[shp$time == 1200,] %>% 
    select(id, time, fromPlace) 
  # combinas las zonas en un único df
  iso_zonas <- rbind(zona1200, zona2400) %>%
    rbind(zona3600)
  # Unir con lo ya procesado
  iso_rbdcut <- rbind(iso_rbdcut, iso_zonas) 
}

# Ahora, intersectar 
mzn_rbd <- mznsCentroide %>%
  st_transform(32719) %>%
  st_intersection(iso_rbdcut) %>%
  `st_geometry<-`(NULL)

# El resultado es un dataframe que contiene los origenes y destinos 
# de cada manzana/colegio y a qué isocrona pertenecen.
# Hay algunos colegios o manzanas que no estarán 
# pues sus centroides no están a distancias recorribles en 3600 segundos caminando

# Guardar
save(mzn_rbd, file = "mzn_rbd.RDS")




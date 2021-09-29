# Script con funciones de ayuda

##### Indicador grativacional ####

# Este indicador es una simplificación del indicador de tres pasos de Wang et al (2012).
# En este, se asume que la distancia caminando de A-B es la misma que B-A.
# En otras palabras, si desde el servicio A el tiempo de viaje para llegar a la manzana B es 12 min,
# Entonces, el tiempo de viaje desde la manzana B hacia el servicio A es de 12 min.
# Esto es sólo válido para viajes a pie, pues viajes en transporte público, privado, o incluso bicileta
# pueden requerir diferentes rutas en función de la orientación de las calles y servicios.

# A partir de este principio se pueden utilizar sólo las isocronas de los servicios (escuelas).
# Producto de esto, la cantidad de cálculos necesarios se reduce y el indicador es más rápido.

# Etapa I:  Cargar librerías
require(tidyverse); require(sf)


accessIndicatorFun <- function(mzn_rbd, escuelasSf, mzns){
  
  # Etapa II: Asignar pesos en función de isocrona.
  # Estos pesos son derivados de una función logística aplicada a los tiempos de viaje al colegio a pie
  # Los datos para generar dicha función logística se encuentran en las Encuestas de Origen y Destino.
  # Para este test se utilizaron los de Santiago, pero podría expandirlo usando la EOD de Chillán.
  
  # Los pesos de la Distance Decay Function:
  # Meter los promedios entre isocronas: 7.5,  22.5, 37.5, 52.5
  # Esto va a dar como resultado el peso de los servicios dentro de una isocrona dada.
  
  # Peso entre 0 y 20 = 10 min -> 1
  # Peso entre 20 y 40 = 30 min -> 0.34415585
  # Peso entre 40 y 60 = 50 min -> 0.09043242
  if (class(escuelasSf)[1] == "sf") {
    escuelasSf <- escuelasSf %>%  
      `st_geometry<-`(NULL)
  }
  
  
  mzn_rbd <- mzn_rbd %>% 
    mutate(peso = case_when(time == 3600 ~ 0.09043242,
                            time == 2400 ~ 0.34415585,
                            time == 1200 ~ 1.0)) %>%
    # Luego, se combina con la base de escuelas que contiene la capacidad de carga. i.e: el número de niñes en la escuela.
    left_join(escuelasSf, by = c("fromPlace" = "rbd")) 
  
  # Etapa III: Calcular el indicador gravitacional
  # Este indicador consta de 3 pasos:
  # 1 - Calcular la demanda
  # 2 - Calcular la oferta
  # 3 - Combinar ambos
  
  # Paso 1- Calcular la demanda
  # Cada manzana tiene un peso para cada colegio en su rango. Ese peso depende de la distancia a la que se encuentre.
  # El peso Gij de la manzana X al colegio A está dado por: 
  # GXA = El peso de A / sum(peso de todos los colegios en las isocronas de la manzana)
  demand <- mzn_rbd %>%
    group_by(objectid) %>% # agrupar por manzanas
    mutate(sigma_Gij = sum(peso), # La suma de los pesos
           Gij = peso / sigma_Gij)# El peso de un colegio j en la manzana i
  
  # Paso 2- calcular la oferta:
  # Cada colegio tiene un peso para cada manzana dentro de su isocrona.
  # Ese peso depende la distancia a la que se encuentre (la isocrona)
  # GPW corresponde a la demanda ponderada del colegio (Gij),
  # Multiplicada por el peso de la distancia entre el colegio y la isocrona (peso)
  # Multiplicada por la población de la manzana (n6a14).
  # En otras palabras: El peso de la demanda del colegio * el peso de la demanda de la manzana * la población de la manzana
  # La capacidad Rj de un colegio j está dadao por:
  # Rj = SA / sum((G1A * Peso * Población) + (G2A * Peso * Población) + (G3A * Peso * Población)  etc)
  # En otras palabras, la capacidad del colegio / por toda la demanda.
  
  supply <- demand %>% # Tomar la tabla de demanda, que tiene Gij
    group_by(fromPlace) %>% # Agrupar por escuelas
    mutate(GPW = Gij*peso*n6a14) %>% # calcular GWP
    summarise(R = unique(basica_cap) / sum(GPW)) # Calcular R
  
  # Paso 3- Combinar ambos en el indicador final
  
  # Por cada isocrona de una manzana 1:
  #   Por cada colegio en la isocrona:
  #     Multiplicar el peso manzana de la isocrona, por la capacidad del colegio y por el peso colegio de la isocrona.
  
  access_escolar <- demand %>% # Tomar tabla de demanda
    select(objectid, fromPlace, peso, Gij) %>% # Seleccionar variables importantes
    left_join(supply) %>% # Combinar con tabla de oferta
    mutate(Access = Gij * R * peso) %>% # Calcular cuantos cupos le corresponden a cada manzana por cada colegio 
    group_by(objectid) %>% # Agrupar por manzanas
    summarise(Access = sum(Access)%>% round(2))  # Sumar los cupos por manzana
  
  # Paso final: Pasar a shapefile
  
  access_mnzs <- mzns %>%
    left_join(access_escolar) %>%
    mutate(Access = replace_na(Access, 0) )
  return(access_mnzs)
} 





####  Generar mapas  ####
require(htmltools); require(leaflet)
accessMapGenerator <- function(polygonLayer, pointLayer){
  
  cortes <- c(0, .25, .5, .75, 1, 1.25, 1.5, 1.75, 2)
  binpal <- colorBin("PuOr",cortes, bins= 9, pretty = T)
  
  map <- leaflet(options = leafletOptions(doubleClickZoom= FALSE)) %>%
    addTiles(urlTemplate = "https://tile.nextzen.org/tilezen/vector/v1/256/all/{z}/{x}/{y}.topojson",
             options = tileOptions(tms = TRUE)) %>%
    addPolygons(data = polygonLayer,
                popup = ~paste("N6a14:", n6a14, "<br/>", "Acceso:", Access),
                stroke = T,
                color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = .5,
                fillColor = ~binpal(Access),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addMarkers(data = pointLayer, 
               popup = ~paste("Nombre:",nom_rbd,"<br/>", "Capacidad:", basica_cap)) %>%
    addLegend("bottomright", pal = binpal, data = polygonLayer, values = ~Access,
              title = "Matrículas por niñe", opacity = .5)
  return(map)
}

popMapGenerator <- function(polygonLayer, pointLayer){
  
  binpal <- colorBin("Blues", polygonLayer$n6a14, bins = 6, pretty = TRUE)
  
  map <- leaflet(options = leafletOptions(doubleClickZoom= FALSE)) %>%
    addTiles() %>%
    addPolygons(data = polygonLayer,
                popup = ~paste("N6a14:", n6a14, "<br/>", "Acceso:", Access),
                stroke = T,
                color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = .5,
                fillColor = ~binpal(n6a14),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                options = pathOptions(clickable = FALSE)) %>%
    addMarkers(data = pointLayer, 
               popup = ~paste("Nombre:",nom_rbd,"<br/>", "Capacidad:", basica_cap)) %>%
    addLegend("bottomright", pal = binpal, data = polygonLayer, values = ~n6a14 ,
              title = "Población de niñes", opacity = .5)
  return(map)
}




#### Generar Histogramas ####

histograma <- function(datos){
  output <- ggplot(datos, aes(x = Access)) +
    geom_histogram(aes(y = ..density..),color = "black", fill = "#FDB863", alpha = .8) + 
    geom_density(alpha = 0.4, fill = "#FF6666") + theme_minimal() +
    scale_y_continuous(name = "Densidad (manzanas)") +
    scale_x_continuous(name = "Accesibilidad (Matrículas por niño)", breaks = seq(0, 2, .25))
  output <- plotly::ggplotly(output)
  return(output)
}

correlacion <- function(datos){
  output <- ggplot(datos, aes(x = n6a14, y = Access)) +
    geom_point() +
    scale_y_continuous(name = "Accesibilidad (Matrículas por niño)", breaks = seq(0, 2, .25)) +
    scale_x_continuous(name = "Niños viviendo en la manzana" ) +
    theme_minimal() 
  output <- plotly::ggplotly(output)
  return(output)
    
}


# Función para obtener isocronas de la nueva escuela.
# Hace la consulta al servidor de CEDEUS y arroja un sf con la isocrona
# Utiliza la función Isochrone de OpenTripPlanner.
isocrona <- function(coordinates, mode, date, time, maxWalkDistance = 1500, cutoffSec, viaje.velocidad = 1.138){
  # Generar el query
  consulta.url <- paste0('http://otpv2.cedeus.cl/otp/routers/chile/isochrone?fromPlace=', 
                         coordinates, 
                         '&mode=', mode, 
                         '&date=',date,
                         "&time=",time,
                         '&maxWalkDistance=',maxWalkDistance,
                         '&cutoffSec=', cutoffSec,
                         '&walkSpeed=', viaje.velocidad) 
  print(consulta.url)
  # Rescatar la consulta. No he implementado error handling pero quizás en el futuro.
  walkshed.texto <- getURL(consulta.url)
  # Pasar la isocrona a sf
  polygon <- read_sf(walkshed.texto) %>%
    st_transform(4326) %>%
    st_zm("ZM")
  return(polygon)
}

# Función que llama a las isocronas, y las procesa para generar las manzanas cubiertas.
# Toma como entrada la nueva escuela, los centroides de las manzanas, y los datos del gravitacional

addSchoolIsochrones <- function(newSchool, centroides, existingData) {
  # Formatear las coordenadas de la nueva escuela
  coords <- st_coordinates(newSchool)
  
  coordinates <- paste0(coords[2], "0%2C",coords[1])
  # Hacer las consultas pertinentes
  isos <- lapply(c(3600,2400,1200), function(x){ 
    isocrona(coordinates = coordinates, mode = "WALK", 
             date = "2020-10-12", time = "09:00:00", 
             cutoffSec = x, viaje.velocidad = 1.33) %>%
      st_transform(32719)})
  # Pasar la lista a df
  isos <- do.call(rbind, isos)
  # Hay que hacer es un cookie cutter de las isocronas.
  # Ej: El polígono de la isocrona de un colegio X a 3600 segundos no puede solaparse con la de 2400 ni 1200.
  # Para eso, se utiliza st_difference.
  # Me imagino que hay formas más eficientes de hacerlo
  
  iso1200 <- isos %>% filter(time == 1200) %>% 
    select(time) %>% 
    mutate(id = 1)
  iso2400 <- isos %>% filter(time == 2400) %>%
    st_difference(isos %>% filter(time == 1200)) %>%
    select(time) %>% 
    mutate(id = 2)
  iso3600 <- isos %>% filter(time == 3600) %>%
    st_difference(isos %>% filter(time == 2400)) %>% 
    select(time) %>% 
    mutate(id = 3)
  # Combinar todas las isocronas cortadas, y asignarles el rbd de fantasía
  iso_rbdcut <- rbind(iso1200, iso2400) %>%
    rbind(iso3600) %>%
    mutate(fromPlace = newSchool$rbd)

  # Hacer la intersección con las manzanas
  new_mzn_rbd <- centroides %>%
    st_transform(32719) %>%
    st_intersection(iso_rbdcut) %>%
    `st_geometry<-`(NULL) %>%
    rbind(existingData)

  
  
  

  return(new_mzn_rbd)
}
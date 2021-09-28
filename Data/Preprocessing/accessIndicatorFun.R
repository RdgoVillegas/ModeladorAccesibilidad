# Script para generar el indicador gravitacional

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


acessIndicatorFun <- function(mzn_rbd, escuelasSf, mzns){
  
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
  
  mzn_rbd <- mzn_rbd %>% 
    mutate(peso = case_when(time == 3600 ~ 0.09043242,
                            time == 2400 ~ 0.34415585,
                            time == 1200 ~ 1.0)) %>%
    # Luego, se combina con la base de escuelas que contiene la capacidad de carga. i.e: el número de niñes en la escuela.
    left_join(escuelasSf %>% 
                `st_geometry<-`(NULL), by = c("fromPlace" = "rbd")) 
  
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



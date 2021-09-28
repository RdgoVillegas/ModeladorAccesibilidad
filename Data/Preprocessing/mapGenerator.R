# Script para generar gráficos
require(htmltools); require(leaflet)
accessMapGenerator <- function(polygonLayer, pointLayer){
  
  cortes <- c(0, .25, .5, .75, 1, 1.25, 1.5, 1.75, 2)
  binpal <- colorBin("PuOr",cortes, bins= 9, pretty = T)
  
  map <- leaflet() %>%
    addTiles() %>%
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
              title = "Matrículas por niñe",
              opacity = .5)
  return(map)
}

popMapGenerator <- function(polygonLayer, pointLayer){
  
  binpal <- colorBin("Blues", polygonLayer$n6a14, bins = 6, pretty = FALSE)
  
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = polygonLayer,
                popup = ~paste("N6a14:", n6a14, "<br/>", "Acceso:", Access),
                stroke = T,
                color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = .5,
                fillColor = ~binpal(n6a14),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addMarkers(data = pointLayer, 
               popup = ~paste("Nombre:",nom_rbd,"<br/>", "Capacidad:", basica_cap)) %>%
    addLegend("bottomright", pal = binpal, data = polygonLayer, values = ~n6a14 ,
              title = "Población de niñes",
              opacity = .5)
  return(map)
}



#### Shiny App code ####
# Author: Rodrigo Villegas Salgado
# Email: rdvillegas@uc.cl
# Status: Development


# Neceisto implementar un botón que diga "añadir colegio" que haga lo siguiente:
#  Pedir una capacidad
#  Pedir una localización
#  Guardar


library(shiny); library(tidyverse); library(sf);library(shinydashboard)

library(plotly); library(RCurl)

source("funs/helperFun.R", encoding = "UTF-8")

# Icono customizado
greenLeafIcon <- makeIcon(
    iconUrl = "http://www.clker.com/cliparts/v/E/r/a/2/E/google-maps-marker-for-residencelamontagne.svg.hi.png",
    iconWidth = 25, iconHeight = 40,
    iconAnchorX = 0, iconAnchorY = 50
)
# este bloque crea un set de paneles desplegables para añadir escuelas

parameter_tabs <- tabsetPanel(
    id = "params",
    type = "hidden",
    tabPanel(title = "Panel 1", value = "panel0"),
    tabPanel(title = "Panel 2", value = "panel1",
             textInput("newSchoolName", "Nombre de la Escuela", value = paste0("Nueva Escuela #", sample(1:15, 1))),
             numericInput("Capacidad", "Capacidad del Colegio", value = 450),
             actionButton("applyChanges", "Calcular Cambios"))
)

cortes <- c(0, .25, .5, .75, 1, 1.25, 1.5, 1.75, 2)

binpal <- colorBin("PuOr",cortes, bins= 9, pretty = T)

load(file = "Data/mzn_rbd.RDS")
load(file = "Data/mzns_procesadas.RDS")
load(file = "Data/rbd_procesadas.RDS")
load(file = "Data/mznsCentroide.RDS")

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader( title = "Accesibilidad Escolar en Chillán", titleWidth  = 400),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(

        verticalLayout(
            
            h3("Indicador de Accesibilidad Escolar"),
            p("Esta herramienta permite evaluar la incidencia de la construcción de una nueva escuela municipal en un indicador de accesibilidad escolar municipal"),
            p("El indicador de accesibilidad evalua la cantidad de matrículas disponibles por niño en función de la cercanía de las escuelas, cuantas matrículas tienen disponibles, y cuantos niños conviven cerca. De esta manera, el indicador de accesibilidada escolar integra oferta, demanda, y transporte"),
            p("Para usarlo, se debe hacer click en 'Añadir Escuela', ingresar un nombre, una capacidad, y luego seleccionar un lugar del mapa para la escuela. Luego, se debe hacer click en 'Calcular Cambios'. El mapa se actualizará, presentando los efectos de la nueva escuela en el indicador."),
            actionButton("actionSchool", "Añadir Escuela"),
            parameter_tabs,
            p("Desarrollado por Rodrigo Villegas Salgado")
        )
        
            

        # Show a plot of the generated distribution
    ),
    
    dashboardBody(
        tags$style(type = "text/css", "#map {height: calc(100vh - 250px) !important;}
                                       #mapPlotPop {height: calc(100vh - 250px) !important;}"),
        
        # Cajitas con indicadores generales
        fluidRow(valueBoxOutput("total_kids"),
        valueBoxOutput("total_slots"),
        valueBoxOutput("mean_access")),
        # Pestañas de información
        tabsetPanel(
            id = "tabs",
            tabPanel(
                title = "Accesibilidad",
                value = "page1",
                fluidRow( leafletOutput("map") )
            ),
            tabPanel(
                title = "Población",
                value = "page2",
                fluidRow( leafletOutput("mapPlotPop") )
            ),
            tabPanel(
                title = "Análisis",
                value = "page3",
                fluidRow( plotlyOutput("histPlot"),
                          plotlyOutput("corPlot"))
            )
        )
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    # Si el usuario clickea "Añadir Escuela", Desplegar el menu para añadir colegios
    observeEvent(input$actionSchool, {
        
        updateTabsetPanel(inputId = "params", selected = paste0("panel", input$actionSchool %% 2))
    }) 
    # Si el usario estaba añadiendo una escuela, y luego lo desactiva, eliminar el punto añadido
    observeEvent(input$actionSchool, {
        
        if (input$actionSchool %% 2 == 0) {
            leafletProxy('map')    %>%
                clearMarkers() %>%
                addMarkers(data = indicator$existingSchools, 
                           popup = ~paste("Nombre:",nom_rbd,"<br/>", "Capacidad:", basica_cap))
        }
    }) 
    
    # Observador para calcular nuevamente el indicador después de añadir la escuela.
    
    observeEvent(input$applyChanges, {

        if(input$actionSchool %% 2 == 1) {

            
            indicator$existingSchools <- rbind(indicator$existingSchools, indicator$newSchool)
            
            updateTabsetPanel(inputId = "params", selected = paste0("panel0"))
            
            tmp.mzn_rbd <- addSchoolIsochrones(indicator$newSchool, mznsCentroide, mzn_rbd)
            print(head(tmp.mzn_rbd))

            indicator$accesibilidad <- accessIndicatorFun(tmp.mzn_rbd, indicator$existingSchools, mzns)
            
            leafletProxy('map')    %>%
                clearMarkers() %>%
                addMarkers(data = indicator$existingSchools, 
                           popup = ~paste("Nombre:",nom_rbd,"<br/>", "Capacidad:", basica_cap))
            
        }
        
        
        
    }) 
    

    # Reactivo para añadir colegios. Si actionSchool está activo, permite añadir un punto
    # map_click es un valor especial con las coords de donde se hace click
    observeEvent(input$map_click, {
        if (input$actionSchool %% 2 == 1){
            click = input$map_click    
            leafletProxy('map')    %>%
                clearMarkers() %>%
                addMarkers(data = indicator$existingSchools, 
                           popup = ~paste("Nombre:",nom_rbd,"<br/>", "Capacidad:", basica_cap)) %>%
                addMarkers(lng = click$lng, lat = click$lat, icon = greenLeafIcon)
            # Crear el dataframe
            print( click$lat)
            print(click$lng)
            nuevaEscuela <- data.frame(rbd = sample(50000:10000, 1) %>% as.character, 
                                       nom_rbd = input$newSchoolName, 
                                       codcomdrbd = 1234, cod_depe2 = 1,
                                       basica_cap = input$Capacidad,
                                       lat = click$lat, lon = click$lng) %>%
                st_as_sf(coords = c('lon', 'lat'), crs = 4326)
            
            print(names(nuevaEscuela))
            # Actualizar las escuelas
            indicator$newSchool <- nuevaEscuela

            
        } 
    })
    

    # Calcular el indicador gravitacional
    indicator <- reactiveValues(accesibilidad = accessIndicatorFun(mzn_rbd, escuelasSf, mzns),
                                newSchool = data.frame(),
                                existingSchools = escuelasSf) 
    # Mapa Accesibilidad
    output$map <- renderLeaflet({
        
        accessMapGenerator(indicator$accesibilidad, indicator$existingSchools)
    })
    # Mapa Población
    output$mapPlotPop <- renderLeaflet({
        
        popMapGenerator(indicator$accesibilidad, indicator$existingSchools)
    })
    
    # Grafico histograma
    output$histPlot <- renderPlotly({ histograma(indicator$accesibilidad) }) 
    # Grafico Correlación
    output$corPlot <- renderPlotly({ correlacion(indicator$accesibilidad) }) 
    # Grafico Boxplot
    

        
        
        
    
    # Cupos totales (server) ------------------------------------------
    output$total_slots <- renderValueBox({
        # The following code runs inside the database.
        # pull() bring the results into R, which then
        # it's piped directly to a valueBox()
        indicator$existingSchools %>%
            `st_geometry<-`(NULL) %>%
            summarise(mean = sum(basica_cap, na.rm = T)) %>%
            pull()  %>%
            as.integer() %>%
            prettyNum(big.mark = ".") %>%
            valueBox(subtitle = "Capacidad escolar comunal")
        
    })
    # Niños totales (server) ------------------------------------------
    output$total_kids <- renderValueBox({
        # The following code runs inside the database.
        # pull() bring the results into R, which then
        # it's piped directly to a valueBox()
        indicator$accesibilidad %>%
            `st_geometry<-`(NULL) %>%
            summarise(suma = sum(n6a14, na.rm = T)) %>%
            pull()  %>%
            as.integer() %>%
            prettyNum(big.mark = ".") %>%
            valueBox(subtitle = "Niños en la comuna")
        
    })
    # Accesibilidad promedio (server) ------------------------------------------
    output$mean_access <- renderValueBox({
        # The following code runs inside the database.
        # pull() bring the results into R, which then
        # it's piped directly to a valueBox()
        indicator$accesibilidad %>%
            `st_geometry<-`(NULL) %>%
            summarise(mean = mean(Access, na.rm = T)) %>%
            pull()  %>%
            round(2) %>%
            prettyNum(big.mark = ".", decimal.mark = ",") %>%
            valueBox(subtitle = "Accesibilidad promedio")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

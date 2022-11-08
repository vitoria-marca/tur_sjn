library(shiny)

library(dplyr)

library(leaflet)

library(DT)

library(sf)

library('shinyWidgets')
library('shinydashboard')
library('shinythemes')
library('leaflet.extras')

# Import data and preprocess------------------

bb_data <- read.csv("input_data/pts_turisticos_sjn.csv", stringsAsFactors = FALSE )
bb_data$y <-  as.numeric(bb_data$longitude)
bb_data$x <-  as.numeric(bb_data$latitude)

bb_data<-bb_data%>%select(-longitude, -latitude)
# new column for the popup label

bb_data <- mutate(bb_data, cntnt=paste0('<strong>Tipo: </strong>',tipo,
                                        '<br><strong>Local:</strong> ', local,
                                        '<br><strong>Endereço:</strong> ', endereco,
                                        '<br><strong>Bairro:</strong> ',bairro,
                                        '<br><strong>Contato:</strong> ',contato)) 

# ícones
classicon <- makeIcon(
  iconUrl = case_when(
    bb_data$tipo == "camping" ~ "icons/accommodation_camping.png",
    bb_data$tipo == "hotel" ~ "icons/accommodation_hotel.png",
    bb_data$tipo == "lancheria" ~ "icons/food_fastfood.png",
    bb_data$tipo == "padaria" ~ "icons/food_cafe.png",
    bb_data$tipo == "ponto turistico" ~ "icons/tourist_museum.png",
    bb_data$tipo == "pousada" ~ "icons/accommodation_house.png",
    bb_data$tipo == "restaurante" ~ "icons/restaurant.png"
  ),
  iconWidth = 25, iconHeight = 25,
  shadowWidth = 10, shadowHeight = 10
)
    


# Convert to SF

bb_sf<- st_as_sf(bb_data, coords = c("x", "y"), 
                 crs = 32722, agr = "constant") 

# Project to WGS lat lon
bb_sf<-st_transform(bb_sf,crs=4326)

bb_sf$latitude<-st_coordinates(bb_sf)[,2]
bb_sf$longitude<-st_coordinates(bb_sf)[,1]

# UI---------------
ui<-navbarPage("WebSIG Turismo São José do Norte", id="main",
           tabPanel("Mapa", 
                    sidebarLayout(
                      sidebarPanel(id = "mapcontrols",width = 5,
                                   pickerInput("tipo_select", "Tipo",
                                               choices = unique(bb_sf$tipo),
                                               selected = unique(bb_sf$tipo),
                                               options = list(`actions-box` = TRUE),
                                               multiple = TRUE)),
                      mainPanel(width=10,
                                leafletOutput("bbmap",height = 800))
                      
                    )),
           tabPanel("Sobre o WebSIG",includeMarkdown("about.md")))

# Server------------
server<-shinyServer(function(input, output) {
 
  reactive_db = reactive({
    bb_sf %>% filter (tipo %in% input$tipo_select)
  })
  
    # create the leaflet map  #nao mexi aqui tb
  output$bbmap <- renderLeaflet({
    leaflet() %>% 
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Circulos",
                          "Marcadores"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addCircles(data=reactive_db(), lng = ~longitude, lat = ~latitude, group='Circulos') %>%
      addTiles()%>%
      addMarkers(data = reactive_db(), lat =  ~latitude, lng =~longitude,
                       popup = ~as.character(cntnt), icon=classicon, group='Marcadores')
  })
  

  
})

# Run---------
shinyApp(ui, server)

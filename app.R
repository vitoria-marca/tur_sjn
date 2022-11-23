library('leaflet.minicharts')
library('shiny')
library('dplyr')
library('leaflet')
library('DT')
library('sf')
library('shinyWidgets')
library('shinydashboard')
library('shinythemes')
library('leaflet.extras')
library('leaflegend')
library('stringr')

# Import data and preprocess------------------

bb_data <- read.csv("input_data/pts_turisticos_sjn.csv", stringsAsFactors = FALSE )
bb_data$y <-  as.numeric(bb_data$longitude)
bb_data$x <-  as.numeric(bb_data$latitude)

bb_data<-bb_data%>%select(-longitude, -latitude)
# new column for the popup label

bb_data <- mutate(bb_data, cntnt=paste0('<strong>tipo: </strong>',tipo,
                                        '<br><strong>Local:</strong> ', local,
                                        '<br><strong>Endereço:</strong> ', endereco,
                                        '<br><strong>Bairro:</strong> ',bairro,
                                        '<br><strong>Contato:</strong> ',contato)) 

# ícones
classicon <- makeIcon(
  iconUrl = case_when(
    bb_data$tipo == "camping" ~ "icons/camping.png",
    bb_data$tipo == "hotel" ~ "icons/hotel.png",
    bb_data$tipo == "lancheria" ~ "icons/lancheria.png",
    bb_data$tipo == "padaria" ~ "icons/cafe.png",
    bb_data$tipo == "ponto turistico" ~ "icons/turismo.png",
    bb_data$tipo == "pousada" ~ "icons/pousada.png",
    bb_data$tipo == "restaurante" ~ "icons/restaurante.png",
    bb_data$tipo == "rota encantos" ~ "icons/turismo.png"
  ),
  iconWidth = 25, iconHeight = 25,
  shadowWidth = 10, shadowHeight = 10
)

# html_legend <- "<img src ='icons/camping.png'>Camping"


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
                          sidebarPanel(id = "mapcontrols",width = 2,
                                       pickerInput("tipo_select", "Tipo",
                                                   choices = unique(bb_sf$tipo),
                                                   selected = unique(bb_sf$tipo),
                                                   multiple = TRUE)),
                          mainPanel(width=10,
                                    leafletOutput("bbmap",height = 650))
                          
                        )
               ),
               tabPanel("Sobre o WebSIG",includeMarkdown("about.md"))
)

# Server------------
## Reactive BD -------------
server <- function(input, output) {
  
  reactive_db = reactive({
    bb_sf %>% filter (tipo %in% input$tipo_select)
  })
  
  # filteredData <- reactive({
  #   if (input$tipo_select == "Todos") {
  #     bb_data
  #   } else {
  #     filter(bb_data, tipo == input$tipo_select)
  #   }
  # })
  
  # filteredIcon <- reactive({
  #   if (input$tipo_select == "Todos") {
  #     classicon
  #   } else {
  #     tipo$iconUrl <- rep(paste0("icon/", str_replace_all(input$tipo_select, " ", "_"), ".png"), 23)
  #   }
  #   classicon
  # })
  
  #Map------------  
  output$bbmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data= reactive_db(),lat = ~latitude, lng = ~longitude, 
                 icon = classicon, 
                 label = ~tipo, 
                 labelOptions = (labelOptions(textsize = "12px")
                                 )
                 )%>%
      addScaleBar(position = "bottomright") %>%
      addLegendImage(images = list("icons/camping.png",
                                   "icons/hotel.png",
                                   "icons/lancheria.png",
                                   "icons/cafe.png",
                                   "icons/turismo.png",
                                   "icons/pousada.png",
                                   "icons/restaurante.png"),
                     labels = c('Camping', 'Hotel','Lancheria','Padaria','Pontos turísticos','Pousada','Restaurante'), 
                     width = c(25, 25, 25, 25, 25, 25, 25), 
                     height = c(25, 25, 25, 25, 25, 25, 25),
                     orientation = 'vertical',
                     title = NULL,
                     position = 'topright',
                     options(textsize = '11px')
                     )
      
      # addMinicharts(
      #   popup = popupArgs(
      #     html = paste0(
      #       "<div>",
      #       "<h3>",
      #       bb_data$tipo
      #       # "</h3>",
      #       # "ID: ",
      #       # new_d$surveyID1,
      #       # "<br>",
      #       # "Alter: ",
      #       # new_d$Alter,
      #       # "</div>"
      # addPolygons(label = bb_data, 
      #             labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", textsize = "15px",
      #                                                      direction = "auto")), 
      #             popup = ~ paste("tipo:", bb_data$tipo
      #                             # , "<br/>","<b/>","URL:", url
                                  # )
      
    
  })
  
}
## Run---------
shinyApp(ui, server)

pop


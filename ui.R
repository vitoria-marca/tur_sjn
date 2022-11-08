library(shiny)
library(leaflet)

navbarPage("WebSIG Turismo Sâo José do Norte", id="main",
           tabPanel("Mapa", leafletOutput("bbmap", height=1000))
           tabPanel("Sobre o WebSIG",includeMarkdown("about.md")))
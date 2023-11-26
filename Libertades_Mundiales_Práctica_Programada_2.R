library(shiny)
library(leaflet)
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(plotly)
library(gt)
library(waiter)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(gapminder)


ui <- dashboardPage( 
  
  skin = "yellow",
  dashboardHeader(
    
    title = "Tipos de Libertad Mundial",
    
    titleWidth = 340),
  
  dashboardSidebar(disable = FALSE,
                   
                   width = 340,
                   
                   sidebarMenu(
                     
                     selectInput("pais", h4("Seleccione un país:"), choices = NULL, selected = NULL),
                     
                     sliderInput("range", "Seleccione un rango de años:",
                                 min = min(datos_libertad$anio), max = max(datos_libertad$anio),
                                 value = c(min(datos_libertad$anio), max(datos_libertad$anio)), step = 1),
                     
                     
                     radioButtons("radio", label = h4("Seleccione la visualización"),
                                  choices = list("Ranking" = 1, "Puntaje" = 2), 
                                  selected = 1),
                     
                     downloadButton("descargar_datos", "Descargar Datos")
                     
                   )),
  dashboardBody(
    
    
    tabsetPanel(
      
      tabPanel("Libertad Humana",plotOutput("grafico_humana")),
      
      tabPanel("Libertad Personal", plotOutput("grafico_personal")),
      
      
      tabPanel("Libertad Económica",plotOutput("grafico_economica")),
      
    ),
    
  ),
  
)


server <- function(input, output, session) {
  
  datos_libertad <- read.csv("datos/datos_libertad.csv")
  
}

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

server <- function(input, output, session) {
  
  datos_libertad <- read.csv("datos/datos_libertad.csv")
  
  observe({ 
    
    updateSelectInput(session, "pais", choices = unique(datos_libertad$pais)) 
    
    
    
    
  })
  
  datos_filtrados <- reactive({
    filter(datos_libertad, pais == input$pais, anio >= input$range[1], anio <= input$range[2])
  })
  
  
  
  
  output$grafico_humana <- renderPlot({
    
    if (input$radio == 1) {
      ggplot(datos_filtrados()) +
        geom_smooth(mapping = aes(x = anio, y = libertad_humana_ranking, colour = "yellow")) +
        ggtitle(paste("Evolución de la Libertad Humana en", input$pais)) +
        theme(plot.title = element_text(size = 18, face = "bold",hjust = 0.5)) +
        
        labs(x = "Año", y = "Ranking") 
    } else {
      
      
      ggplot(datos_filtrados()) +
        geom_smooth(mapping = aes(x = anio, y = libertad_humana_puntaje, colour = "yellow")) +
        ggtitle(paste("Evolución de la Libertad Humana en",input$pais)) +
        theme(plot.title = element_text(size = 18, face = "bold",hjust = 0.5)) +
        
        labs(x = "Año", y = "Puntaje") 
      
      
      
      
      
      
    }
  }
  )
  
  output$grafico_personal <- renderPlot({
    
    if (input$radio == 1) {
      ggplot(datos_filtrados()) +
        geom_smooth( aes(x = anio, y = libertad_personal_ranking, colour = "yellow")) +
        
        ggtitle(paste("Evolución de la Libertad Personal en", input$pais)) +
        
        theme(plot.title = element_text(size = 18, face = "bold",hjust = 0.5)) +
        
        labs(x = "Año", y = "Ranking") 
      
    } else {
      
      
      ggplot(datos_filtrados()) +
        geom_smooth(mapping = aes(x = anio, y = libertad_personal_puntaje, colour = "yellow")) +
        
        ggtitle(paste("Evolución de la Libertad Personal en",input$pais)) +
        
        theme(plot.title = element_text(size = 18, face = "bold",hjust = 0.5)) +
        
        labs(x = "Año", y = "Puntaje") 
      
    }
  }
  )
  
  
  
  output$grafico_economica <- renderPlot({
    
    if (input$radio == 1) {
      ggplot(datos_filtrados()) +
        geom_smooth(mapping = aes(x = anio, y = libertad_economica_ranking, colour = "yellow")) +
        ggtitle(paste("Evolución de la Libertad Personal en", input$pais)) +
        
        theme(plot.title = element_text(size = 18, face = "bold",hjust = 0.5)) +
        
        labs(x = "Año", y = "Ranking") 
    } else {
      
      
      ggplot(datos_filtrados()) +
        geom_smooth(mapping = aes(x = anio, y = libertad_economica_puntaje, colour = "yellow")) +
        ggtitle(paste("Evolución de la Libertad Personal en",input$pais)) +
        theme(plot.title = element_text(size = 18, face = "bold",hjust = 0.5)) +
        
        labs(x = "Año", y = "Puntaje") 
      
    }
    
  })
  
  
  
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste("datos_", input$pais, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_filtrados(), file)
    }
  )
  
  
  
  
  
}






shinyApp(ui, server)

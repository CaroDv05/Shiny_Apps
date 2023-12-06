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
  
  skin = "green",
  dashboardHeader(title = "Datos Spotify",
                  
                  titleWidth = 200),
  
  dashboardSidebar(disable = FALSE,
                   
                   width = 200,
                   
                   sidebarMenu(
                     
                     menuItem("Tabla de datos", tabName = "datos"),
                     
                     menuItem("Gráficos", tabName = "gráficos")
                     
                   )),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "datos",
              
              h2("Tabla de datos"),
              
              tabsetPanel(
                
                tabPanel("Año y Género", 
                         
                         h3("Elija el año y género que desea ver"),
                         
                         selectInput("year", "Año:", choices = NULL, selected = NULL),
                         
                         selectInput("top.genre", "Género:", choices = NULL, selected = NULL),
                         
                         actionButton("filtrar", "Filtrar Datos"),
                         
                         downloadButton("descargar_datos", "Descargar Datos"),
                         
                         dataTableOutput("tabla_spotify"))
                
              ), #tabsetpanel
              
      ),
      
      tabItem(
        
        tabName = "gráficos",
        
        h2("Gráficos"),
        
        tabsetPanel(
          
          tabPanel("Gráfico 1", 
                   
                   h3("Visualización de gráfico: Bailabilidad vs Energía"),
                   
                   plotOutput("grafico_spotify")),
          
          tabPanel("Gráfico 2", 
                   
                   h3("Visualización de gráfico: Duración vs Popularidad"),
                   
                   plotOutput("grafico_spotify2")),
          
          tabPanel("Gráfico 3", 
                   
                   h3("Visualización de gráfico: Valencia vs BMP"),
                   
                   plotOutput("grafico_spotify3")),
          
          
         
          
          
          
        ) #tabsetpanel
        
      ) 
      
    ),
  ),
  
)

server <- function(input, output, session) {
  
  datos_spotify <- read.csv2("datos/spotify_2000_2023.csv")
  
  observe({ 
    
    updateSelectInput(session, "year", choices = unique(datos_spotify$year)) 
    
    updateSelectInput(session, "top.genre", choices = unique(datos_spotify$top.genre)) 
    
    updateSelectInput(session, "año", choices = unique(datos_empleo$anyo))
    
  })
  
  observeEvent(input$filtrar, {
    
    output$tabla_spotify <- renderDataTable({
      
      datos_filtrados <- datos_spotify[
        
        datos_spotify$year == input$year & datos_spotify$top.genre == input$top.genre, ]
      
    }, options = list(scrollX = TRUE))
    
  })
  

  
  
  
  output$grafico_spotify <- renderPlot({
    
    datos_filtrados <- datos_spotify[
      
      datos_spotify$year == input$year & datos_spotify$top.genre == input$top.genre, ]
    
    datos_dance_vs_duration <- datos_filtrados|> 
      
      filter(!is.na(danceability) & !is.na(duration)) 
    
    ggplot(datos_dance_vs_duration) +
      
      geom_point(mapping = aes(x = danceability, y = energy), colour = "green",stat = "identity") +
  
      theme_minimal() +
      
      labs(title = "Bailabilidad vs Energía de la canción",
           
           subtitle = "Comparación del nivel de bailabilidad con la energía de la canción",
           
           x = "Bailabilidad",
           
           y = "Energía")
    
  })
  
  output$grafico_spotify2 <- renderPlot({
    
    datos_filtrados <- datos_spotify[
      
      datos_spotify$year == input$year & datos_spotify$top.genre == input$top.genre, ]
    
    datos_duration_popularity <- datos_filtrados|> 
      
      filter(!is.na(duration) & !is.na(popularity)) 
    
    ggplot(datos_duration_popularity) +
      
      geom_point(mapping = aes(x = duration, y = popularity), colour = "green",stat = "identity") +
      
      theme_minimal() +
      
      labs(title = "Duración vs Popularidad de la canción",
           
           subtitle = "Comparación de la duración y popularidad de la canción",
           
           x = "Duración",
           
           y = "Popularidad")
    
  })
  
  output$grafico_spotify3 <- renderPlot({
    
    datos_filtrados <- datos_spotify[
      
      datos_spotify$year == input$year & datos_spotify$top.genre == input$top.genre, ]
    
    datos_valence_bpm<- datos_filtrados|> 
      
      filter(!is.na(valence) & !is.na(bpm)) 
    
    
    ggplot(datos_valence_bpm) +
      
      geom_point(mapping = aes(x = valence ,y = bpm), colour = "green",stat = "identity")+

      
      theme_minimal() +
      
      labs(title = "Valencia VS BMP",
           
           subtitle = "La valencia vs el tempo de la canción",
           
           x = "Valencia",
           
           y = "BMP")
    
    
    
    
  })
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste("spotify_", input$year, ".csv", sep = "")
    },
    content = function(file) {
      
      datos_filtrados <- datos_spotify[
        
        datos_spotify$year == input$year & datos_spotify$top.genre == input$top.genre, ]
      
      write.csv(datos_filtrados, file)
    }
  )
  
  
  
  
  
}












shinyApp(ui, server)
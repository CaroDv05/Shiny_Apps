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
  
  skin = "red",
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
                         
                         dataTableOutput("tabla_spotify")),
                
              ), #tabsetpanel
              
      ),
      
      tabItem(
        
        tabName = "gráficos",
        
        h2("Gráficos"),
        
        tabsetPanel(
          
          tabPanel("Gráfico 1", 
                   
                   h3("Visualización de gráfico: Países con mayor desempleo en América Latina"),
                   
                   plotOutput("grafico_empleos")),
          
          tabPanel("Gráfico 2", 
                   
                   h3("Visualización de gráfico: Países con mayor desempleo en América Latina"),
                   
                   plotOutput("grafico_empleos2")),
          
          tabPanel("Gráfico 3", 
                   
                   h3("Visualización de gráfico: Países con mayor empleo informal en América Latina"),
                   
                   plotOutput("grafico_empleos3"))
          
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
  
  observeEvent(input$filtro, {
    
    output$tabla_años <- renderDataTable({
      
      datos_filtrados <- datos_empleo[
        
        datos_empleo$anyo == input$año, ]
      
    }, options = list(scrollX = TRUE))
    
  })
  
  
  
  output$grafico_empleos <- renderPlot({
    
    datos_genero <- datos_empleo|> 
      
      
      
      filter(!is.na(desempleo_mujeres) & !is.na(desempleo_hombres)) |> 
      
      group_by(codigo_pais_region) |> 
      
      summarise(cantidad = sum(desempleo_mujeres) + sum(desempleo_hombres))
    
    ggplot(datos_genero) +
      
      geom_bar(mapping = aes(x = codigo_pais_region, y = cantidad), colour = "red",stat = "identity")+
      
      scale_y_continuous(breaks = seq(0, 400000, by = 50000))+
      
      theme_minimal() +
      
      labs(title = "Países con mayor desempleo en América Latina",
           
           subtitle = "Hombres y mujeres en condición de desempleo",
           
           x = "País",
           
           y = "Cantidad de personas desempleadas")
    
  })
  
  output$grafico_empleos2 <- renderPlot({
    
    datos_desempleo_educacion<- datos_empleo|> 
      
      filter(!is.na(desempleo_educacion_mujeres) & !is.na(desempleo_educacion_hombres)) |> 
      
      group_by(codigo_pais_region) |> 
      
      summarise(cantidad = sum(desempleo_educacion_mujeres) + sum(desempleo_educacion_hombres))
    
    
    ggplot(datos_desempleo_educacion) +
      
      geom_bar(mapping = aes(x = codigo_pais_region ,y = cantidad), colour = "red",stat = "identity")+
      
      scale_y_continuous(breaks = seq(0, 400000, by = 50000))+
      
      theme_minimal() +
      
      labs(title = "Países con mayor desempleo en América Latina",
           
           subtitle = "Hombres y mujeres con estudios terciarios en condición de desempleo",
           
           x = "País",
           
           y = "Cantidad de personas desempleadas")
    
  })
  
  output$grafico_empleos3 <- renderPlot({
    
    datos_empleo_informal<- datos_empleo|> 
      
      filter(!is.na(empleo_informal_mujeres) & !is.na(empleo_informal_hombres)) |> 
      
      group_by(codigo_pais_region) |> 
      
      summarise(cantidad = sum(empleo_informal_mujeres) + sum(empleo_informal_hombres))
    
    
    ggplot(datos_empleo_informal) +
      
      geom_bar(mapping = aes(x = codigo_pais_region ,y = cantidad), colour = "red",stat = "identity")+
      
      scale_y_continuous(breaks = seq(0, 400000, by = 50000))+
      
      theme_minimal() +
      
      labs(title = "Países con mayor empleo informal en América Latina",
           
           subtitle = "Hombres y mujeres en condición de empleo informal",
           
           x = "País",
           
           y = "Cantidad de personas con empleo informal")
    
    
    
    
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
library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(ggplot2)
library(gapminder)
library(readr)

ui <- dashboardPage( 
  
  skin = "red",
  dashboardHeader(title = "Empleabilidad según género (LATAM)",
                  
                  titleWidth = 370),
  
  dashboardSidebar(disable = FALSE,
                   
                   width = 370,
                   
                   sidebarMenu(
                    
                     menuItem("Tabla de datos", tabName = "datos"),
                     
                     menuItem("Gráficos", tabName = "gráficos")
                     
                   )),
  
  dashboardBody(
    
    tabItems(
      
          tabItem(tabName = "datos",
                  
              h2("Tabla de datos"),
              
              tabsetPanel(
                
                tabPanel("País", 
                         
                         h3("Elija el país que desea ver"),
                         
                         selectInput("pais", "País:", choices = NULL, selected = NULL),
                         
                         selectInput("años", "Año:", choices = NULL, selected = NULL),
                         
                         actionButton("filtrar", "Filtrar Datos"),
                         
                         dataTableOutput("tabla_empleos")),
                
                tabPanel("Años", 
                         
                         h3("Elija el año que desea ver"),
                         
                         selectInput("año", "Años:", choices = NULL, selected = NULL),
                         
                         actionButton("filtro", "Filtrar Datos"),
                         
                         dataTableOutput("tabla_años")
                         
                )
                
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
  
  datos_empleo <- read.csv("datos/datos_empleo_genero.csv")
  
  observe({ 
    
    updateSelectInput(session, "pais", choices = unique(datos_empleo$pais_region)) 
    
    updateSelectInput(session, "años", choices = unique(datos_empleo$anyo)) 
    
    updateSelectInput(session, "año", choices = unique(datos_empleo$anyo))
    
  })
  
  observeEvent(input$filtrar, {
    
    output$tabla_empleos <- renderDataTable({
      
      datos_filtrados <- datos_empleo[
        
        datos_empleo$pais_region == input$pais & datos_empleo$anyo == input$años, ]
      
    }, options = list(scrollX = TRUE))
    
  })
  
  observeEvent(input$filtro, {
    
    output$tabla_años <- renderDataTable({
      
      datos_filtrados <- datos_empleo[
        
        datos_empleo$anyo == input$año, ]
      
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
  
}












shinyApp(ui, server)
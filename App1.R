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

  
  
  
  
  
  
  
  
  
  shinyApp(ui, server)
library(lubridate)
library(dplyr)
library(caret)
library(e1071)
library(ranger)
library(ggmap)
library(DT)
#library(mapproj)

Subconjunto<-readRDS("SubconDatos.rds")
lm.tune<-readRDS("ModeloLinealPrecioSuperficie.rds")
lm.ranger<-readRDS("Modeloforest.rds")
opcionesComuna<-readRDS("Comunas.rds")
map<-readRDS("map.rds")
library(shiny)
library(shinymaterial)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- material_page(
  title = "Tasador",
  tags$br(),
  material_row(
    material_column(
      width = 2,
      material_card(
        title = "Precio",
        depth = 4,
        textOutput("precio")
      ),
      material_card(
        title = "",
        depth = 4,
        actionButton("Button", "Update"),
        sliderInput("ban", "Baños",
                    min = 0, max = 10, value = 1),
        sliderInput("hab", "Habitaciones",
                    min = 0, max = 10, value = 2),
        sliderInput("supt", "Superficie Total (m2)",
                    min = 0, max = 1000, value = 30),
        sliderInput("supc", "Superficie Construida (m2)",
                    min = 0, max = 1000, value = 30),
        sliderInput("reg", "Región",
                    min = 0, max = 15, value = 1),
        selectizeInput("com", "comuna", opcionesComuna, selected = "Santiago", multiple = FALSE,
                       options = NULL)
      )

      ),
    material_column(
      
      plotOutput("map")
    )
    
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dt <- reactiveValues(m=data.frame(      habitaciones=c(as.integer(1)),
                         superficie_construida=c(as.double(1)),
                         superficie_terreno=c(as.integer(1)),           
                         bathroom=c(as.integer(1)),
                         region=c(as.integer(1)),
                         comuna=c(as.character("Santiago"))
  ))
  
  
  dtMapa <- reactiveValues(m=Subconjunto%>%filter(region==13))
  
  
  observeEvent(input$Button,{
    dt$m <- data.frame(habitaciones=c(as.integer(input$hab)),
                       superficie_construida=c(as.double(input$supc)),
                       superficie_terreno=c(as.integer(input$supt)),           
                       bathroom=c(as.integer(input$ban)),
                       region=c(as.integer(input$reg)),
                       comuna=c(as.character(input$com))
    )
  })
  
  observeEvent(input$Button,{
    dtMapa$m <-Subconjunto%>%filter(region==input$reg,
                                    comuna==input$com)
  })
  
  

  output$precio<-renderText({
  dt$m%>%
      mutate(Region_Comuna= paste(region,comuna,sep="-")
      )%>%
      mutate(Region_Comuna=as.factor(Region_Comuna),
             comuna=as.factor(comuna),
             region=as.factor(region))%>%
      mutate(RatioSuperficie=superficie_construida/superficie_terreno,
             HabitacionSuperficie=habitaciones/superficie_construida,
             BathroomHabitacion=bathroom/habitaciones)->dt2
    
    dt2$precio_metcuadrado<-predict(lm.tune, dt2)
    
    dt2%>%
      mutate(precio_aprox_sup=superficie_construida*precio_metcuadrado,
             precio_aprox_const=superficie_terreno*precio_metcuadrado
      )->dt2
    dt2$precio_pred<-predict(lm.ranger, dt2)
    
    as.double(dt2$precio_pred)
  })
  
  output$map<-renderPlot(
    
    map+
    geom_density2d(data = dtMapa$m, 
                         aes(x = Longitud, y = Latitud), size = 0.3) + 
      stat_density2d(data = dtMapa$m,
                     aes(x = Longitud, y = Latitud, fill = ..level.., alpha = ..level..), size = 0.01, 
                     bins = 8, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE)
    
    
  )
  
}
# Run the application 
shinyApp(ui = ui, server = server)
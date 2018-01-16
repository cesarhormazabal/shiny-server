# pacman::p_load(shiny,highcharter,DT,readr,dplyr,shinymaterial,visNetwork,shinydashboard)

library(shiny)
library(highcharter)
library(DT)
library(readr)
library(dplyr)
library(shinymaterial)
library(visNetwork)
library(shinydashboard)

# dir <- dirname(parent.frame(2)$ofile)
# setwd(dir)

Data_Ejemplo_Camiones <- read_delim("Data_Ejemplo_Camiones.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)
camiones<-unique(Data_Ejemplo_Camiones$Camion)
datanormal<-as_tibble(x=rnorm(1000, mean = 130, sd = 10))
datanormal<-datanormal%>%
  mutate(carga_cat = cut(value, breaks = seq(20, 200, 10)))%>%
  group_by(carga_cat) %>%
  summarise(conteo = n())


ui <- material_page(title = "Camiones Mineros",
    material_side_nav(
    selectInput("var", 
                label = "Choose a variable to display",
                choices = c("Cargas óptimas", 
                            "Cargas Medias",
                            "Cargas Deficientes"),
                selected = "Percent White"),
    selectizeInput(
      'input_camion', 'Seleccione un camión',  
      choices = camiones,multiple = TRUE
      ,options = list(
        placeholder = 'Selecccione una opción',
        onInitialize = I('function() { this.setValue("RH11234"); }')
      )
    ),
    actionButton("Generar Informe", "Enviar Reporte"),
    actionButton("Simular Data", "Simular Data")
    
  ),
  material_parallax(
    image_source =
      "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/CamionFermont.png/1200px-CamionFermont.png"
  ),
material_row(
  material_column (width=9,
    material_card(title = 
                    "Carga",
                  highchartOutput("distHighPlot",height="240px")
                  ,depth=5
    ),material_card(title = 
                      "Relación entre conductor y camión",
                    visNetworkOutput("network")
                    ,depth=5
    )
    )
  ,
  material_column(width=3,
                  material_card(
                    HTML(
                      '
                    <div class="card-content">
                    <span class="card-title"><span style="font-weight:bold; color:#2196f3">TKPH</span></span>
                    Cambio en TKPH <span style="font-size:10px; font-style: italic">&nbsp;&nbsp;(Past 7 days)</span> <br><span style="font-size:33px; color: #212121 "> - 10% </span>
                    </div>
                    '
                    ),depth=5
                    
                  ),
                  material_card(
                    HTML(
                      '
                  <div class="card-content">
                    <span class="card-title"><span style="font-weight:bold; color:#2196f3">RENDIMIENTO CAMIONES</span></span>
                    Cambio en rendimiento <span style="font-size:10px; font-style: italic">&nbsp;&nbsp;(Past 7 days)</span> <br><span style="font-size:33px; color: #212121 "> + 30% </span>
                    </div>
                    '
                    ),depth=5
                    
                  ),
                  material_card(
                    HTML(
                      '
                      <div class="card-content">
                      <span class="card-title"><span style="font-weight:bold; color:#2196f3">RENDIMIENTO CONDUCTORES</span></span>
                      Cambio en rendimiento <span style="font-size:10px; font-style: italic">&nbsp;&nbsp;(Past 7 days)</span> <br><span style="font-size:33px; color: #212121 "> + 20% </span>
                      </div>
                      '
                    ),depth=5
                    
                    )
                  


)

)
,
    fluidRow(class = "myRow3",
              dataTableOutput("tablaui")
             )
  

)
rating <- c(2, 3, 5, 4, 1, 5, 3, 1, 4)
date_time <- c("2015-05-14", "2015-05-07", "2015-05-06", "2015-04-11", "2015-01-07", "2014-12-06", "2014-04-11", "2014-01-07", "2013-12-06")
repliesOnly <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
data <- data.frame(rating, date_time, repliesOnly)
data$rating <- as.character(data$rating)


server <- function(input, output) {
  
  
  data_truck<-reactive(
    {
      
      Data_Ejemplo_Camiones[which(Data_Ejemplo_Camiones$Camion %in% input$input_camion),]
    }
  )
  
  output$tablaui<-DT::renderDataTable(
    
    data_truck()
  )
  

  
  output$distHighPlot <- renderHighchart({
    
    data_hist <- data_truck() %>%
      mutate(carga_cat = cut(Carga, breaks = seq(20, 200, 10))) %>%
      group_by(carga_cat) %>%
      summarise(conteo_h = n())
  
    datanormal$conteo<-datanormal$conteo*(sum(data_hist$conteo_h,na.rm=TRUE)/sum(datanormal$conteo,na.rm=TRUE))
    
    datagraph<-datanormal%>%left_join(data_hist,by="carga_cat")
      h3<-hchart(datagraph, "column", hcaes(x = carga_cat, y = conteo_h))
      
      # x    <- data_truck()$Carga
      # h2 <- hist(x, breaks =11, plot = FALSE)
      #<-hchart(h3)
      #h3<-hc_yAxis_multiples(h3, create_yaxis(naxis = 2))
      h3%>%
        hc_add_series(datagraph,type="spline",hcaes(x = carga_cat, y = conteo))%>%
        hc_credits(
          enabled = TRUE,
          text = "Desarrollado por César",
          href = "https:\\www.chormazabal.tk")%>%
        hc_add_theme(hc_theme_gridlight(
          chart = list(
          backgroundColor = "white")))
  })
  
  
  output$network <- renderVisNetwork({
    
    # data used in next examples
    nb <- 10
    nodes <- data.frame(id = 1:nb, label = paste("Conductor", 1:nb),
                        group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                        title = paste0("<p>", 1:nb,"<br>Informacio Relevante !</p>"), stringsAsFactors = FALSE)
    
    edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                        to = c(3,7,2,7,9,1,5,3,2,9),
                        value = rnorm(nb, 10), label = paste("Camión", 1:nb),
                        title = paste0("<p>", 1:nb,"<br>Información Relevante !</p>"))
    
    visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
      visGroups(groupname = "A", shape = "icon", 
                icon = list(code = "f0d1", size = 75)) %>%
      visGroups(groupname = "B", shape = "icon", 
                icon = list(code = "f007", color = "red")) %>%
      visGroups(groupname = "C", shape = "icon", 
                icon = list(code = "f0d1", color = "black")) %>%
      visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T)) %>%
      addFontAwesome() 
    
  })
  
  
  
  
  
  
  
  datadata <- data
  
  makeReactiveBinding("datadata")
  
  #Data Reactiva al input, y a botón actualizar
  newData <- reactive({
    input$Button
    isolate({
      datadata <- data
      datadata <- subset(datadata, rating %in% input$checkGroups)
    })
  })
  
  #Texto reactivo a cambios en la data
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
  
  #Imagen reactiva a cambios en la data
  output$Imagen_Seleccionada<-renderImage(
    {
      filename <- normalizePath(file.path('./images',
                                          paste('image', input$var, '.jpg', sep='')))
      
      # Return a list containing the filename
      list(src = filename,
           #width = width,
           height = 120)
      
    },deleteFile=FALSE
  )
  
  output$plot <- renderHighchart({
    
    datadata <- newData()
    
    # plot <- nPlot(rating ~ date_time, data = datadata, 
    #               type = "multiBarHorizontalChart", dom = 'plot')
    # return(plot)
    # 
    
    hc <- highchart() %>% 
      hc_xAxis(data$date_time) %>% 
      hc_add_series(name = "Tokyo", data$rating)%>% 
      hc_add_theme(hc_theme_gridlight())%>%
      #hc_plotOptions(chart=list(height="50%"))
      hc_size(height = 100)
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)


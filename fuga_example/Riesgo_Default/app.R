library(shiny)
library(shinymaterial)
library(markdown)
library(caret)
library(highcharter)
library(visNetwork)
library(DT)
DT:::DT2BSClass(c('compact', 'cell-border'))
p<-readRDS("www/plotHchart.rds")

t<-readRDS("www/VisT.rds")

dt<-readRDS("www/dtcredit.rds")

# Wrap shinymaterial apps in material_page
ui <- material_page(
  title = "Análisis Default",
  material_row(
    material_column(width=6,
           material_card(
             depth = 5,
             visNetworkOutput("network")
             #includeHTML("www/arbol.html")
           )
    ),
    material_column(width=6,
                    material_card(
                      depth = 5,
                      highchartOutput("hcontainer")
                    )
    )
  ),
  material_row(
    material_card(
      depth = 5,
      DTOutput('tbl')
    )
  )



  
)

server <- function(input, output) {
  output$hcontainer <- renderHighchart({
    p
  })
  output$network<-renderVisNetwork({
    t
  })
  output$tbl = renderDT(
    dt,options = list(
      autoWidth = TRUE)
    )
}

shinyApp(ui = ui, server = server)


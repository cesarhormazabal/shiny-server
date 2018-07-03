library(visNetwork)
library(rpart)
library(readxl)
library(dplyr)
library(sparkline)
library(caret)
library(highcharter)
library(DT)

default_of_credit_card_clients <- read_excel("default of credit card clients.xls")
#names(default_of_credit_card_clients)
default_of_credit_card_clients%>%
  mutate(`Default Mes Siguiente`=as.factor(`Default Mes Siguiente`))->default_of_credit_card_clients

# Basic classification tree
res <- rpart(`Default Mes Siguiente`~., data=default_of_credit_card_clients,control=rpart.control(cp=0.001))

printcp(res)
visTree(res, main = "Árbol Clasificacion Default", width = "100%")->VisT
saveRDS(VisT,"VisT.rds")
importancia<-caret::varImp(res)
importancia%>%
  rownames()->importancia$Nombres

importancia%>%
  filter(Overall>0)%>%
  mutate(Overall=log(Overall))%>%
  arrange(desc(Overall))%>%
  droplevels()->importancia

highchart() %>% 
  hc_add_series(importancia, "bar", hcaes(x = Nombres, y = Overall), name = "Importancia") %>%
  hc_xAxis(categories = importancia$Nombres)->PlothChart

dtcredit<-DT::datatable(default_of_credit_card_clients)
saveRDS(default_of_credit_card_clients,"dtcredit.rds")
saveRDS(PlothChart,"plotHchart.rds")

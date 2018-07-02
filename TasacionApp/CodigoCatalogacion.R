modeloBosque<-readRDS("Modeloforest.rds")
library(lubridate)
library(dplyr)
library(caret)

Subconjunto<-readRDS("SubconDatos.rds")
lm.tune<-readRDS("ModeloLinealPrecioSuperficie.rds")
lm.ranger<-readRDS("Modeloforest.rds")

#Universo disponible de comuna y region
saveRDS(unique(Subconjunto$comuna),"Comunas.rds")
unique(Subconjunto$region)

typeof(Subconjunto$region)

dt<-data.frame(
             habitaciones=c(as.integer(1)),
             superficie_construida=c(as.double(1)),
             superficie_terreno=c(as.integer(1)),           
             bathroom=c(as.integer(1)),
             region=c(as.integer(1)),
             comuna=c(as.character("Santiago"))
    )

dt%>%
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

View(dt2)

as.double(dt2$precio_pred)


###############################################################################
Subconjunto%>%
  View

Subconjunto%>%
  mutate(Region_Comuna= paste(region,comuna,sep="-")
  )%>%
  mutate(Region_Comuna=as.factor(Region_Comuna),
         comuna=as.factor(comuna),
         region=as.factor(region))%>%
  filter(tipo_vivienda!="Comercial" & tipo_vivienda!="Oficina")%>%
  filter(precio< quantile(Subconjunto$precio, 0.95))%>%
  filter(precio> quantile(Subconjunto$precio>30, 0.95))%>%
  mutate(Fechafinal=ymd(Fechafinal))%>%
  mutate(RatioSuperficie=superficie_construida/superficie_terreno,
         HabitacionSuperficie=habitaciones/superficie_construida,
         BathroomHabitacion=bathroom/habitaciones,
         precio_por_superficie=precio/superficie_terreno)%>%
  filter(habitaciones<quantile(Subconjunto$habitaciones, 0.9995),
         bathroom<quantile(Subconjunto$bathroom, 0.99)
  )->Subconjunto_2




#comuna+RatioSuperficie

# lm.tune <- train(precio_por_superficie ~ comuna+RatioSuperficie
#                  , data = training_psup,
#                  method = "lm")

lm.tune<-readRDS("ModeloLinealPrecioSuperficie.rds")

Subconjunto_2$precio_metcuadrado<-predict(lm.tune, Subconjunto_2)

Subconjunto_2%>%
  mutate(precio_aprox_sup=superficie_construida*precio_metcuadrado,
         precio_aprox_const=superficie_terreno*precio_metcuadrado
  )->Subconjunto_2

# ranger.tune <- train(precio ~ precio_aprox_sup+
#                        comuna+
#                        precio_aprox_const+
#                        superficie_terreno +
#                        superficie_construida+
#                        RatioSuperficie+
#                        habitaciones+
#                        HabitacionSuperficie+
#                        BathroomHabitacion+
#                        bathroom
#                      , data = training_fin,
#                      method = "ranger")

lm.ranger<-readRDS("Modeloforest.rds")
Subconjunto_2$precio_pred<-predict(lm.ranger, Subconjunto_2)
View(Subconjunto_2)

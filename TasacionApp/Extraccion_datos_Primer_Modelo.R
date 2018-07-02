install.packages("dbplyr")
install.packages("RMySQL")


library(dplyr)

con <- DBI::dbConnect(RMySQL::MySQL(), 
                      host = "159.89.151.245",
                      user = "root",
                      dbname="tasaciones",
                      password = rstudioapi::askForPassword("Database password"))


tabla_precios <- tbl(con, 
                     sql("
                        SELECT A.ID, 
                         A.URL, 
                         A.region, 
                         A.comuna, 
                         A.direccion,
                         A.habitaciones,
                         A.superficie_construida,
                         A.bathroom,
                         A.superficie_terreno,
                         A.tipo_vivienda,
                         A.Fecha_publicacion,
                         A.Longitud,
                         A.Latitud,
                         B.precio,
                         B.Fechafinal, 
                         B.desviacion,
                         datediff(B.Fechafinal, A.Fecha_publicacion) DiasPublicada
                         FROM tasaciones.Basicos A inner join 
                         (select A.ID, 
                                B.Fecha as Fechafinal, 
                                 A.precio,B.desviacion
                                from Historial_Precios A
                         inner join (
                                  select ID, max(fecha) fecha, STDDEV(precio) desviacion
                                  from Historial_Precios
                                  group by ID) B
                         on A.fecha=B.fecha and A.ID=B.ID
                         ) B
                         on A.ID=B.ID
                         where A.region is not null and
                         A.comuna is not null and
                         A.direccion is not null and
                         A.habitaciones is not null and
                         A.superficie_construida is not null and
                         A.bathroom is not null and
                         A.superficie_terreno is not null and
                         A.Fecha_publicacion is not null and 
                         A.tipo_vivienda is not null and
                         A.Longitud is not null and
                         A.Latitud is not null"
                         ))

tabla_precios%>%
  collect()->Subconjunto


library(lubridate)
library(dplyr)

names(Subconjunto)
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

quantile(Subconjunto$bathroom, 0.99)

saveRDS(Subconjunto,"SubconDatos.rds")


str(Subconjunto_2)
summary(Subconjunto_2)
library(caret)

train.index <- createDataPartition(Subconjunto_2$comuna, 
                                   p = .9, list = FALSE)
training <- Subconjunto_2[ train.index,]
testing  <- Subconjunto_2[-train.index,]

train.index2 <- createDataPartition(training$comuna, 
                                   p = .3, list = FALSE)

training_psup <- training[ train.index2,]
training_fin <- training[-train.index2,]

lm.tune <- train(precio_por_superficie ~ comuna+RatioSuperficie
                 , data = training_psup,
                 method = "lm")

saveRDS(lm.tune,"ModeloLinealPrecioSuperficie.rds")

training_fin$precio_metcuadrado<-predict(lm.tune, training_fin)

training_fin%>%
  mutate(precio_aprox_sup=superficie_construida*precio_metcuadrado,
         precio_aprox_const=superficie_terreno*precio_metcuadrado
         )->training_fin




ranger.tune <- train(precio ~ precio_aprox_sup+
                   comuna+
                   precio_aprox_const+
                   superficie_terreno +
                   superficie_construida+
                   RatioSuperficie+
                   habitaciones+
                   HabitacionSuperficie+
                   BathroomHabitacion+
                   bathroom
                 , data = training_fin,
                 method = "ranger")

#saveRDS(lm.tune,"ModeloLinealBase.rds")


# lm.tune <- train(precio ~ comuna+
#                    superficie_terreno
#                  , data = training,
#                  method = "lm")

lm.tune<-readRDS("ModeloLinealPrecioSuperficie.rds")

testing$precio_metcuadrado<-predict(lm.tune, testing)

testing%>%
  mutate(precio_aprox_sup=superficie_construida*precio_metcuadrado,
         precio_aprox_const=superficie_terreno*precio_metcuadrado
  )->testing



testing$precio_pred<-predict(ranger.tune, testing)

testing%>%
  mutate(PorcentajeSobrePrecio=(precio_pred-precio)/precio)->testing


Metrics::rmse(testing$precio,testing$precio_pred)



p <- ggplot(testing, aes(x=precio_pred,precio))+geom_point()+
  scale_x_continuous(limits = c(0, 20000)) +
  scale_y_continuous(limits = c(0, 20000))
x11()
p

base::sort(lmPrice$coefficients)
base::sort(Subconjunto$precio, decreasing = TRUE)
p <- ggplot(Subconjunto_2, aes(x=region, y=precio)) + 
  geom_boxplot()
x11()

library(plotly)
library(highcharter)

?highcharter::hcboxplot()
unique(Subconjunto_2$region)
Index=13
hcboxplot(x = Subconjunto_2[Subconjunto_2$region==Index,]$precio, var = Subconjunto_2[Subconjunto_2$region==Index,]$comuna)





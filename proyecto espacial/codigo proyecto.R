pack<-c("rlang","lubridate","data.table","tidyverse","readxl","writexl","dplyr","beepr","XML",  "tidyverse","dbplyr","broom","purrr","kohonen","gridExtra","factoextra","audio","dendextend",
        "odbc","DBI","utf8","ROCR","readr","tidyr","e1071","randomForest","stats","base","ggplot2","plotly","reticulate",
        "assertthat","glmtoolbox","gtools","writexl","openxlsx","zoo","mondate","lubridate","arrow","kable","kableExtra","arrow","DT","flextable","dlookr")

#"fabletools","tsibble","fable"
#install.packages("SmartEDA", dependences=TRUE)
#detach("package:MASS", unload = TRUE)
lapply(pack,require, character.only = TRUE)

library(glmtoolbox)
library(leaflet)
library(leaflet.extras)
library(geosphere) # LIBRERIA QUE APOYA A MEDIR DISNTANCIAS

ruta='C:\\Users\\1192923513\\Documents\\Universidad\\estadistica espacial\\'
data <- read_parquet(str_c(ruta,"base de datos para serie y espacial.parket"))
names(data)


### escoger el repuesto que mas tiene ventas

a <- data %>%filter(grepl("MIT", data$NombreCentro)) %>%  count(IdReferencias) 

### filtro para bases de datos
pasa_uso<-data %>% filter(IdReferencias=='MD050317' & grepl("MIT", data$NombreCentro) & !is.na(Latitud))



##### MEDIR DISTANCIAS ENTRE CENTROS Y CLIENTES


lat_ref <- 4.676151499056953  # Latitud de la ubicación de referencia
lon_ref <- -74.08714444586805 # Longitud de la ubicación de referencia

### FILTOR POR CENTRO

pasa_uso_1<-pasa_uso %>% filter(NombreCentro=='MIT-Bta-AV 68' )

names(pasa_uso)

# Calcular distancias utilizando la función distVincentySphere
distancias <- distVincentySphere(
  cbind(lon_ref, lat_ref),
  cbind(pasa_uso_1$Longitud, pasa_uso_1$Latitud)
)

# Agregar las distancias al dataframe de ubicaciones
pasa_uso_1$Distancia <- distancias

# Organizar el dataframe por distancia de menor a mayor
ubicaciones_ordenadas <- pasa_uso_1[order(distancias), ]
pasa_uso_1$orden_espacial <- distancias
names(ubicaciones_ordenadas)

################### parte descriptiva



mi_mapa <- leaflet() %>%
  addTiles() # Esto agrega los azulejos de Google Maps

# Agregar los puntos desde el DataFrame
mi_mapa <- mi_mapa %>%
  addMarkers(data = pasa_uso_1 %>% select(Latitud,Longitud,NombreTercero) %>% distinct() ,  # Nombre del DataFrame
                   lat = ~Latitud,  # Columna de latitud en el DataFrame
                   lng = ~Longitud,  # Columna de longitud en el DataFrame
                   popup = ~NombreTercero
             ,clusterOptions = markerClusterOptions())

# Visualizar el mapa
mi_mapa


marcador_especial <- data.frame(
  Latitud = 4.676151499056953,
  Longitud = -74.08714444586805,
  NombreTercero = "MIT-Bta-AV 68"
)

mi_mapa <- mi_mapa %>%
  addCircleMarkers(
    data = marcador_especial,
    lat = ~Latitud,
    lng = ~Longitud,
    radius = 10,
    color = "Black",
    fillOpacity = 0.5,
    popup = ~NombreTercero
  )















## MODELO ESPACIO-TEMPORAL 
# MES
ubicaciones_ordenadas <- ubicaciones_ordenadas %>%
  mutate(FechaAlta = floor_date(FechaAlta, "month")) %>%
  mutate(FechaAlta = FechaAlta + days(1))

class(pasa_uso_1$Edad)
datos_agrupados <- ubicaciones_ordenadas %>%
  group_by(NifCif,FechaAlta,Edad,orden_espacial ) %>%
  summarise(
    Unidades = sum(Unidades),
    Precio = sum(Precio),
    # Agrega otras variables que desees sumar aquí
  )
d<-datos_agrupados[,c("Unidades","FechaAlta","Edad","orden_espacial","Precio")]
dim(d)
d1<-na.omit(d)
d2<-d1[(d1$Unidades>=0)&(!is.na(d1$Edad)),]
fit1<-glmgee(Unidades~Edad+FechaAlta, family= poisson(log),id=orden_espacial, corstr='AR-M-dependent(1)',data=d2)
summary(fit1)

fit2 <- update(fit1, corstr="Exchangeable")  
fit3 <- update(fit1, corstr="AR-M-dependent(1)")  
fit4 <- update(fit1, corstr="AR-M-dependent(2)")  

a <- AGPC(fit1, fit2, fit3, fit4, verbose=FALSE)
b <- SGPC(fit1, fit2, fit3, fit4, verbose=FALSE)
cbind(a, SGPC=b[,"SGPC"]) 

  
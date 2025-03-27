pack<-c("rlang","lubridate","data.table","tidyverse","readxl","writexl","dplyr","beepr","XML",  "tidyverse","dbplyr","broom","purrr","kohonen","gridExtra","factoextra","audio","dendextend",
        "odbc","DBI","utf8","ROCR","readr","tidyr","e1071","randomForest","stats","base","ggplot2","plotly","reticulate",
        "assertthat","gtools","writexl","openxlsx","zoo","mondate","lubridate","arrow","kable","kableExtra","arrow","DT","flextable","dlookr")

#"fabletools","tsibble","fable"
#install.packages("SmartEDA", dependences=TRUE)
#detach("package:MASS", unload = TRUE)
lapply(pack,require, character.only = TRUE)

library(glmtoolbox)


ruta='C:\\Users\\1192923513\\Documents\\Universidad\\estadistica espacial\\'
data <- read_parquet(str_c(ruta,"base de datos para serie y espacial.parket"))



### escoger el repuesto que mas tiene ventas

a <- data %>%filter(grepl("MIT", data$NombreCentro)) %>%  count(IdReferencias) 

### filtro para bases de datos
pasa_uso<-data %>% filter(IdReferencias=='MD050317' & grepl("MIT", data$NombreCentro) & !is.na(Latitud))



# 4.676151499056953, -74.08714444586805  MIT-Bta-AV 68
# 4.615144375113387, -74.08191855433094 MIT-Bta.San Facón
# 4.1190160307523005, -73.63865750168989 MIT-Villavo-Anillo Vial
# 4.432630389038779, -75.18727670353935 MIT-Ibague-Mirolindo
# 4.61472754465024, -74.08924940867918 MIT-Bta-Cll 13
# 3.3959444066209494, -76.54658273225174 MIT-Cali-Pasoancho
# 7.122057231055523, -73.11692610353447 MIT-B/manga-Cra 27


##### MEDIR DISTANCIAS ENTRE CENTROS Y CLIENTES

library(geosphere) # LIBRERIA QUE APOYA A MEDIR DISNTANCIAS


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


## MODELO ESPACIO-TEMPORAL 
if(ubicaciones_ordenadas$Unidades)
class(pasa_uso_1$Edad)
fit1<-glmgee(Unidades+100~ FechaAlta +Edad, family= Gamma(log),id=orden_espacial, corstr='Independence',data=ubicaciones_ordenadas)

fit2 <- update(fit1, corstr="Exchangeable")  
fit3 <- update(fit1, corstr="AR-M-dependent(1)")  
fit4 <- update(fit1, corstr="AR-M-dependent(2)")
fit5 <- update(fit1, corstr="AR-M-dependent(3)")  
fit6 <- update(fit1, corstr="AR-M-dependent(4)")  

a <- AGPC(fit1, fit2, fit3, fit4, fit5, fit6, verbose=FALSE)
b <- SGPC(fit1, fit2, fit3, fit4, fit5, fit6, verbose=FALSE)
(a, SGPC=b[,"SGPC"]
  
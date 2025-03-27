rm(list = ls())

pack<-c("rlang","lubridate","data.table","tidyverse","readxl","writexl","dplyr","beepr","XML",
        "tidyverse","dbplyr","broom","purrr","kohonen","gridExtra","factoextra","audio","dendextend",
        "odbc","DBI","utf8","ROCR","readr","tidyr","e1071","randomForest","stats","base","ggplot2","plotly","reticulate",
        "assertthat","gtools","writexl","openxlsx","zoo","mondate",'MASS','nortest',"lmtest","car","sandwich")

#"fabletools","tsibble","fable"

#install.packages("sandwich", drependences=TRUE)
lapply(pack,require, character.only = TRUE)



## proyecto de regresion lineal.

ruta_regresion='C:/Users/1192923513/Documents/PERSONAL/universidad/regresion/'



### base de datos.

datos<- read.csv(str_c(ruta_regresion,'Hotel Reservations.csv'))

#names(datos)[c(17,2,6)]<-c('y','1','2')

muestra<-sample(dim(datos)[1],5000)
datos1=datos[c(muestra),]

class(datos$booking_status)
datos$booking_status<-as.factor(datos$booking_status)
names(datos)
datos1<-datos1 %>% mutate(estado=case_when(booking_status=='Not_Canceled'~1,TRUE~0),fecha_arrive=str_c(arrival_year,arrival_month,arrival_date, sep='-'))%>% dplyr::select(-c(arrival_year,arrival_month,arrival_date))

## Modelo de regresion Multiple

datos1<-na.omit(datos1)
modelo<-lm(datos1$avg_price_per_room~datos1$no_of_adults)
for(i in 1:length(datos1$avg_price_per_room)){
if(datos1$avg_price_per_room[i]==0){
  datos1$avg_price_per_room[i]=0.05
}
}
boxcox(datos1$avg_price_per_room~datos1$no_of_adults, data = datos1,
       lambda = seq(-4, 5, len = 50))
mod2<-lm(datos1$avg_price_per_room~datos1$no_of_adults)
b<-lillie.test(mod2$residuals)
plot(mod2$residuals)
?boxcox
summary(modelo)

a<-shapiro.test(modelo$residuals)
shapiro.test(modelo$residuals)$p.value
ks.test(modelo$residuals,'pnorm')
qqnorm(residuals(modelo))



bptest(modelo)

plot(modelo$residuals)

lillie.test(modelo$residuals)


vif(modelo)




####################################################
###################################################
######### M O D E L O  D E  H U B E R ##############
###################################################
###################################################


mods<-rlm(datos1$avg_price_per_room~datos1$no_of_adults)
summary(mods)


training_rows <- sample(1:nrow(datos1), 0.8 * nrow(datos1))  
training_data <- datos1[training_rows, ] 
test_data <- datos1[-training_rows, ] 

lm_Predicted <- predict(mod2, test_data)
rob_Predicted <- predict(mods, test_data)

lm_actuals_pred <- cbind(lm_Predicted, test_data$income)
rob_actuals_pred <- cbind(rob_Predicted, test_data$income)

mean(apply(lm_actuals_pred, 1, min)/
       apply(lm_actuals_pred, 1, max)) 
summary(mods)
summary(mod2)

qqplot(mods$residuals)


###################################################################
############## REGRESION LINEAL MULTIPLE ##########################
###################################################################




muestra<-sample(dim(datos)[1],5000)
datos1=datos[c(muestra),]

names(datos)
modelo<-lm(avg_price_per_room~no_of_adults+no_of_weekend_nights+no_of_week_nights+type_of_meal_plan, data = datos1)


summary(modelo)

# de acuerdo a la prueba de hipotesis local se sacan las variables no de noches por semana y numero de noches fines de semana

modelo<-lm(avg_price_per_room~no_of_adults+type_of_meal_plan, data = datos1)


####################################################
### Modelo lineal generalizado gamma ###############
####################################################
####################################################


modelo <- glm(avg_price_per_room~no_of_adults+type_of_meal_plan, data = datos1, family = Gamma(link = "log"))
modelo <- glm(avg_price_per_room~no_of_adults+type_of_meal_plan, data = datos1, family = inverse.gaussian(link = "log"))

choose.files()
  summary(modelo)

  
  
library(glmtoolbox)
  envelope(modelo)
plot(modelo)

########################################
####### datos atipicos #####################

outlierTest(modelo, cutoff=Inf, n.max=10)
influenceIndexPlot(modelo, vars="Cook",main = "Distncia de cook del modelo logistico")


a<-dffits(modelo)

b<-data.frame(a)
plot((a))
a[a > 2*sqrt((3+1)/5000)]
indice <- which(a<(-0.1))

datos2<-datos1[-c(indice),]

modelo_AJUSTADO <- glm(avg_price_per_room~no_of_adults+type_of_meal_plan, data = datos2, family = Gamma(link = "log"))
envelope(modelo_AJUSTADO)


########################################
####### Supuestos #####################

## Homocedasticidad

lillie.test(modelo$residuals)

bptest(modelo)
chisq.test(modelo)

resettest(modelo, power=3)

# Estimar matriz de varianza-covarianza robusta
vcov <- vcovHC(modelo, type = "HC0")
coeftest(modelo, vcov = vcov)

# Obtener residuos estandarizados
residuos_est <- rstandard(modelo)

# Graficar residuos estandarizados
plot(residuos_est ~ fitted(modelo), 
     xlab = "Valores ajustados",
     ylab = "Residuos estandarizados")
abline(h = 0, lty = 2)


dwtest(modelo)

vif(mod2)

#######################################
######## MODELO DE HUBER ##############

#ya que no se cumplieron supuestos cambiamos a un modelo robusto.

datos1<-na.omit(datos1)
modelo<-rlm(avg_price_per_room~no_of_adults+no_of_weekend_nights+no_of_week_nights+type_of_meal_plan, data = datos1)

summary(modelo)
dwtest(modelo)


plot(modelo$fitted.values, resid(modelo))








@
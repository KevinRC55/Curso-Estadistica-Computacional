#-------------------------------------------Lectura de datos-------------------------------------------------
#Lectura de csv
datos_sucios <- read.csv(file = "./IDEFC_NM_jul2021.csv") #IDEFC = Incidencia Delictiva Fuero Común

#Limitamos nuestros datos a incidencias delictivas de 2015 a 2020
datos_sucios <- datos_sucios[datos_sucios$Año <= 2020, ]

#Algunas variables numéricas están como caracteres y usan "," para representar miles
datos_sucios[8:19] <- sapply(datos_sucios[8:19], gsub, pattern = ",", replacement = "")#Quitamos las ","
datos_sucios[8:19] <- sapply(datos_sucios[8:19], as.integer)#Convertimos a enteros

library(skimr)
skim(datos_sucios)

#-------------------------------------------Manejo de datos-------------------------------------------------

#Cambiamos el orden de las columnas y seleccionamos aquellas con las que trabajaremos
library(dplyr)

datos_sucios <- select(datos_sucios, Año, Entidad, Tipo.de.delito, Subtipo.de.delito, Modalidad, 
                       Bien.jurídico.afectado, 8:19)
#Cambiamos el nombre de las columnas

datos_sucios <- rename(datos_sucios, 
                       Estado = Entidad, 
                       TipoDelito = Tipo.de.delito, 
                       SubtipoDelito = Subtipo.de.delito, 
                       BienJuridicoAfectado = Bien.jurídico.afectado)

#Guardamos nuestros datos ordenados
rownames(datos_sucios) <- NULL

write.csv(datos_sucios,'Incidencia_Delictiva_Ordenado.csv', row.names = FALSE, fileEncoding = "UTF-8")

datos_ordenados <- read.csv(file = "./Incidencia_Delictiva_Ordenado.csv", encoding = "UTF-8")

#Separamos nuestras variables Cualitativas y Cuantitativas 
cualitativas <- select(datos_ordenados, Estado, TipoDelito, SubtipoDelito, Modalidad, BienJuridicoAfectado)
cuantitativas <- select(datos_ordenados, Año, 7:18)

#------------------------------------------Datos faltantes e imputación--------------------------------------

#Revisamos si tenemos datos faltantes

library(naniar)
library(VIM)

gg_miss_var(datos_ordenados, show_pct = TRUE)  # muestra % de faltantes

aggr(datos_ordenados,col=c("blue", "red"),
     combined= F, bars=F, numbers=T,sortVars= T, 
     prop = c(TRUE, FALSE))# conjunto de renglones con faltantes


#Generamos datos faltantes
library(missMethods)

#Elegimos las columnas donde generaremos faltantes
columns <- c("Enero", "Julio", "Diciembre")

datos_incompletos <- delete_MCAR(datos_ordenados, 0.15, cols_mis = columns)  # elimina el 15%

#Observamos los datos faltantes generados
library(UpSetR)

vis_miss(datos_incompletos) # las lineas negras indican la posicion del dato faltante en cada columna

datos_completos <- datos_ordenados

library(ggplot2)

datos_completos$missEnero <- is.na(datos_incompletos$Enero)
datos_completos$missJulio <- is.na(datos_incompletos$Julio)
datos_completos$missDiciembre <- is.na(datos_incompletos$Diciembre)

ggplot(datos_completos, aes(x = Enero, y = Año, col = missEnero)) + geom_point()
ggplot(datos_completos, aes(x = Julio, y = Año, col = missJulio)) + geom_point()
ggplot(datos_completos, aes(x = Diciembre, y = Año, col = missDiciembre)) + geom_point()

#Comprobamos el patron MCAR
mcar_test(datos_incompletos)
# DECISION: valor alto de ji-cuadrada y p-value bajo: los datos no son MCAR

#Imputamos con el metodo mean de mice para datos numericos.
library(mice)

imputed_data <- mice((datos_incompletos)[,names(datos_incompletos) %in% columns],m = 1, maxit = 1, 
                     method = "mean")

complete.data <- mice::complete(imputed_data)

#Redondeamos nuestros datos, ya que es la cantidad de registros de incidentes delictivos
complete.data <- mutate_if(complete.data, is.numeric, round)

datos_imputados <- datos_incompletos
datos_imputados$Enero <- complete.data$Enero
datos_imputados$Julio <- complete.data$Julio
datos_imputados$Diciembre <- complete.data$Diciembre

write.csv(datos_imputados,'Incidencia_Delictiva_Imputados.csv', row.names = FALSE, fileEncoding = "UTF-8")

#Comparamos diferentes metodos de imputacion de mice

#sample
imputed_data <- mice((datos_incompletos)[,names(datos_incompletos) %in% columns],m = 1, maxit = 1, 
                     method = "sample")

imputados_sample <- mice::complete(imputed_data)
imputados_sample <- mutate_if(imputados_sample, is.numeric, round)

#norm.nob
imputed_data <- mice((datos_incompletos)[,names(datos_incompletos) %in% columns],m = 1, maxit = 1, 
                     method = "norm.nob")

imputados_norm.nob <- mice::complete(imputed_data)
imputados_norm.nob <- mutate_if(imputados_norm.nob, is.numeric, round)

#cart
imputed_data <- mice((datos_incompletos)[,names(datos_incompletos) %in% columns],m = 1, maxit = 1, 
                     method = "cart")

imputados_cart <- mice::complete(imputed_data)
imputados_cart <- mutate_if(imputados_cart, is.numeric, round)

#rf
imputed_data <- mice((datos_incompletos)[,names(datos_incompletos) %in% columns],m = 1, maxit = 1, 
                     method = "rf")

imputados_rf <- mice::complete(imputed_data)
imputados_rf <- mutate_if(imputados_rf, is.numeric, round)

#Comparamos con summary
summary(complete.data) #Datos imputados con mean
summary(imputados_sample)
summary(imputados_norm.nob)
summary(imputados_cart)
summary(imputados_rf)
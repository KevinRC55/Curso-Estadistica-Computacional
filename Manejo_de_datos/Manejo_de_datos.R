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

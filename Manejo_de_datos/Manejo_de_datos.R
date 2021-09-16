#-------------------------------------------Lectura de datos-------------------------------------------------

#Lectura de csv
datos_sucios <- read.csv(file = "./MX_youtube_trending_data.csv"
                         , encoding = "UTF-8")

#Libreria para leer archivos .json
#install.packages("jsonlite")
library(jsonlite)
library(dplyr)

#Lectura de archivo .json que contiene el nombre de las categorías
categorias <- as.data.frame(fromJSON("./MX_category_id.json"))
#Nos quedamos con las variables que nos interesan
categorias <- data.frame(categorias$items.id, 
                         categorias$items.snippet$title, 
                         categorias$items.snippet$assignable)

categorias <- rename(categorias, categoryId = categorias.items.id, 
                     category = categorias.items.snippet.title, 
                     assignable = categorias.items.snippet.assignable)
#Left-join de categorias en datos_sucios
categorias$categoryId <- as.integer(categorias$categoryId)

datos_sucios <- left_join(datos_sucios, categorias, by = "categoryId")

#Separamos las primeras 5 etiquetas en la variable "tags" 
#install.packages("stringr")
library(stringr)

datos_sucios <- cbind(datos_sucios, str_split_fixed(datos_sucios$tags, fixed("|"), 6))


#Cambiamos el formato de fechas a Date yyyy-mm-dd
typeof(datos_sucios$publishedAt)
typeof(datos_sucios$trending_date)

datos_sucios$publishedAt <- as.Date(datos_sucios$publishedAt)
datos_sucios$trending_date <- as.Date(datos_sucios$trending_date) 

class(datos_sucios$publishedAt)
class(datos_sucios$trending_date)

#Limitamos nuestros datos a vídeos publicados de Enero 2021 a Agosto 2021
datos_sucios <- datos_sucios[datos_sucios$publishedAt >= "2021-01-01" 
                             & datos_sucios$publishedAt <= "2021-08-31", ]

#-------------------------------------------Manejo de datos-------------------------------------------------

#Cambiamos el orden de las columnas y seleccionamos aquellas con las que trabajaremos
datos_sucios <- select(datos_sucios, title, publishedAt, trending_date, channelTitle, category,  
                       `1`, `2`, `3`, `4`, `5`, view_count, likes, dislikes, comment_count, comments_disabled)

#Cambiamos el nombre de las columnas

datos_sucios <- rename(datos_sucios, 
                       Title = title, 
                       PublishedDate = publishedAt, 
                       TrendingDate = trending_date, 
                       ChannelName = channelTitle, 
                       Category = category,  
                       Tag1 = `1`, Tag2 = `2`, Tag3 = `3`, Tag4 = `4`, Tag5 = `5`,
                       Views = view_count,
                       Likes = likes, 
                       Dislikes = dislikes,
                       Comments = comment_count,
                       CommentsDisabled = comments_disabled)

rownames(datos_sucios) <- NULL

write.csv(datos_sucios,'MX_YouTube_Ordenados.csv', row.names = FALSE, fileEncoding = "UTF-8")

datos_ordenados <- read.csv(file = "./MX_YouTube_Ordenados.csv"
                         , encoding = "UTF-8")

cualitativas <- select(datos_ordenados, ChannelName, Category, Tag1, Tag2, Tag3, Tag4, Tag5, CommentsDisabled)
cuantitativas <- select(datos_ordenados, Views, Likes, Dislikes, Comments)

#------------------------------------------Datos faltantes e imputación--------------------------------------


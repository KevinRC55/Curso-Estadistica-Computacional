datos <- read.csv(file = "./Incidencia_Delictiva_Ordenado.csv", encoding = "UTF-8")

#Limitamos nuestros datos a incidencias delictivas registradas en 2020
library(dplyr)
datos <- datos[datos$Año == 2020, ]
datos <- select(datos, Estado, TipoDelito, SubtipoDelito, Modalidad, 
                BienJuridicoAfectado, 7:18)
write.csv(datos,'Incidencia_Delictiva_2020.csv', row.names = FALSE, fileEncoding = "UTF-8")
datos <- read.csv(file = "./Incidencia_Delictiva_2020.csv", encoding = "UTF-8")


#=============================== Descripcion de las variables ===============================================

str(datos)

###################################################################
##        Variable        #           Tipo           #   Escala  ##
###################################################################
##  Estado                #        Cualitativa       # Nominal   ##
##  TipoDelito            #        Cualitativa       # Ordinal   ##
##  SubtipoDelito         #        Cualitativa       # Ordinal   ##
##  Modalidad             #        Cualitativa       # Nominal   ##
##  BienJuridicoAfectado  #        Cualitativa       # Nominal   ##
##  Enero                 #  Cualitativa / Discreta  # Razón     ##
##  Febrero               #  Cualitativa / Discreta  # Razón     ##
##  Marzo                 #  Cualitativa / Discreta  # Razón     ##
##  Abril                 #  Cualitativa / Discreta  # Razón     ##
##  Mayo                  #  Cualitativa / Discreta  # Razón     ##
##  Junio                 #  Cualitativa / Discreta  # Razón     ##
##  Julio                 #  Cualitativa / Discreta  # Razón     ##
##  Agosto                #  Cualitativa / Discreta  # Razón     ##
##  Mayo            #  Cualitativa / Discreta  # Razón     ##
##  Octubre               #  Cualitativa / Discreta  # Razón     ##
##  Noviembre             #  Cualitativa / Discreta  # Razón     ##
##  Diciembre             #  Cualitativa / Discreta  # Razón     ##
###################################################################

#========================= Análisis descriptivo de variables cualitativas ====================================

# Tabla de frecuencias de la variable TipoDelito

absoluta_delito <- table(datos$TipoDelito)  #tabla de frecuencias absolutas

relativa_delito <- absoluta_delito / length(datos$TipoDelito) #tabla de frecuencias relativas

acumulada_a_delito <- cumsum(absoluta_delito)

acumulada_r_delito <- cumsum(relativa_delito)

(tabla_frec <- cbind(Absoluta = absoluta_delito, A_Acumulada = acumulada_a_delito, 
                     Relativa = relativa_delito, R_Acumulada = acumulada_r_delito))

# Análisis gráfico

library(ggplot2)

ggplot(as.data.frame(absoluta_delito), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", color="red3", fill="white") + 
  labs(title = "Diagrama de barras - Tipo de delito", subtitle = "Frecuencias Absolutas", 
       x = NULL, y = NULL) + coord_flip() 

ggplot(as.data.frame(relativa_delito), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", color="blue3", fill="white") + 
  labs(title = "Diagrama de barras - Tipo de delito", subtitle = "Frecuencias Relativas", 
       x = NULL, y = NULL) + coord_flip()

ggplot(as.data.frame(relativa_delito), aes(x=2, y=Freq, fill=names(relativa_delito))) +
  geom_bar(stat="identity", color="white") + coord_polar("y", start=0) +
  theme_void() + xlim(0.5, 2.5)

# Estadísticos de resumen

#Moda

library(modeest)

moda <- mfv(datos$TipoDelito) # Moda

cbind(Variable = names(datos)[3], Moda = moda)

#===========================Análisis descriptivo de variables cuantitativas===================================

#Tabla de frecuencias de la variable Mayo (Cantidad de delitos registrados en Mayo)

library(fdth)

range(datos$Mayo)

table(datos$Mayo)

length(datos$Mayo)

#Usando intervalos de clase
(tabla_mayo <- fdt(datos$Mayo,breaks="Sturges",start=0,end=3750,h=150,right=T))

#Podemos ver que a partir de registros de delitos mayores a 750 incidencias, las frecuencias se reducen 

# Análisis gráfico

library(plotly)

#Grafica de barras
ggplot(as.data.frame(tabla_mayo$table), aes(x=`Class limits`, y=f)) +
  geom_bar(stat="identity", color="green4", fill="white") + 
  labs(title = "Diagrama de barras - Delitos cometido en Mayo", subtitle = "Frecuencias Absolutas", 
       x = NULL, y = NULL) + coord_flip()

#Diagrama de puntos
plot_ly(datos, x = ~Mayo, type = 'scatter',
        mode = "markers", marker = list(color = "black")) %>% 
  layout(title = "Diagram de puntos",
         xaxis = list(title = "Delitos cometido en Mayo"))

#Diagrama de caja y brazos  
plot_ly(datos,x = ~Mayo, type = "box")

# Se observa que la mayoria de delitos se encuentran entre 0 y 20, pareciera que 
# el resto de los registros de delitos son valores atípicos. 
#Claramente la distribución de delitos es asimétrica hacia la derecha. 


#Veamos los delitos que corresponden algunos datos atipicos
select(datos[datos$Mayo > 1000, ], Mayo, TipoDelito, Estado)

#Veamos las graficas con un subconjunto de datos

#Diagrama de puntos
plot_ly(subset(datos, Mayo < 100), x = ~Mayo, type = 'scatter',
        mode = "markers", marker = list(color = "black")) %>% 
  layout(title = "Diagram de puntos",
         xaxis = list(title = "Delitos cometido en Mayo"))

#Diagrama de caja y brazos  
plot_ly(subset(datos, Mayo < 50),x = ~Mayo, type = "box")


# Medidas descriptivas

summary(datos$Mayo) 

mfv(datos$Mayo) #Moda

quantile(datos$Mayo, probs = c(0.20,0.40,0.60,0.80)) #Cuantiles

IQR(datos$Mayo) #Amplitud intercuartílica

var(datos$Mayo); sd(datos$Mayo) #Varianza

mean(abs(na.omit(datos$Mayo)-mean(na.omit(datos$Mayo))))   # error medio (media)

mean(abs(datos$Mayo-median(datos$Mayo)))  # error medio (mediana)

sd(datos$Mayo)/mean(datos$Mayo) #Coeficiente de variación

library(agricolae)

skewness(datos$Mayo) #Coeficiente de asimetría

kurtosis(datos$Mayo) #Coeficiente de curtosis

# Grafica de ojiva

h <- as.data.frame(tabla_mayo$table)

plot_ly(data=h,x= ~`Class limits` ,y= ~`cf(%)`, 
        marker = list(size = 10, color="red"),
        type = 'scatter', mode = 'lines')%>%
  layout(title = 'Ojiva (Dalitos Mayo)', 
         xaxis = list(title = 'Límite superior'), 
         yaxis = list(title = 'Pi'),
         shapes = list(
           # linea vertical 
           list(type = "line", x0 = 0, x1 = 0, 
                y0 = 0, y1 = 1, yref = "paper"),
           # linea horizontal 
           list(type = "line", x0 = 0, x1 = 50000, 
                y0 = 1, y1 = 1, xref = "paper")))

#=============================comparación y asociación entre variables======================================

#Comparamos la variable Mayo (cantidad de delitos registrados en mayo) con la variable Estado

#Diagrama de caja y brazos
plot_ly(datos,x = ~Mayo, y = ~Estado, type = "box")

#Parece que Campeche es el estado en donde se registraron menos delitos en el mes de Mayo, por otro lado,
#El Estado de Mexico es el estado que cuenta con mas registros de delitos


#Diagrama de puntos
plot_ly(datos, x = ~datos$Mayo, y = ~datos$TipoDelito, color = ~datos$Estado, type = 'scatter',
        mode = "markers")

#Nuevamente vemos como los registros de delitos se concentran en valores pequeños, ahora podemos ver
#con mas claridad el tipo de delito y estado en donde existen grandes cantidades de incidencias delictivas.

#Asociación entre variables cuantitativas

round(cor(datos[c(6:17)]),digits = 2) 

#Es claro que nuestras variables cuantitativas están asociadas, ya que los registros de delitos entre meses
#son muy parecidos. Ademas al ser registros consecutivos podemos ver la asociación con meses anteriores.

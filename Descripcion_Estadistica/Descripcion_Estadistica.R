datos <- read.csv(file = "./Incidencia_Delictiva_Ordenado.csv", encoding = "UTF-8")

#Limitamos nuestros datos a incidencias delictivas registradas en 2020
library(dplyr)
datos <- datos[datos$A�o == 2020, ]
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
##  Enero                 #  Cuantitativa / Discreta # Raz�n     ##
##  Febrero               #  Cuantitativa / Discreta # Raz�n     ##
##  Marzo                 #  Cuantitativa / Discreta # Raz�n     ##
##  Abril                 #  Cuantitativa / Discreta # Raz�n     ##
##  Mayo                  #  Cuantitativa / Discreta # Raz�n     ##
##  Junio                 #  Cuantitativa / Discreta # Raz�n     ##
##  Julio                 #  Cuantitativa / Discreta # Raz�n     ##
##  Agosto                #  Cuantitativa / Discreta # Raz�n     ##
##  Mayo                  #  Cuantitativa / Discreta # Raz�n     ##
##  Octubre               #  Cuantitativa / Discreta # Raz�n     ##
##  Noviembre             #  Cuantitativa / Discreta # Raz�n     ##
##  Diciembre             #  Cuantitativa / Discreta # Raz�n     ##
###################################################################

#========================= An�lisis descriptivo de variables cualitativas ====================================

# Tabla de frecuencias de la variable TipoDelito

absoluta_delito <- table(datos$TipoDelito)  #tabla de frecuencias absolutas

relativa_delito <- absoluta_delito / length(datos$TipoDelito) #tabla de frecuencias relativas

acumulada_a_delito <- cumsum(absoluta_delito)

acumulada_r_delito <- cumsum(relativa_delito)

(tabla_frec <- cbind(Absoluta = absoluta_delito, A_Acumulada = acumulada_a_delito, 
                     Relativa = relativa_delito, R_Acumulada = acumulada_r_delito))

# De acuerdo a la organizaci�n de nuestros datos, en la tabla de frecuencias aun no observamos los delitos
# con mayor registro de incidencias. Pero podemos observar la cantidad de modalidades en las que generalmente
# se cometen los diferentes delitos, es decir, la variedad de combinaciones de tipo de delito con subtipo 
# de delito, modalidad y bien jur�dico afectado.
# En nuestra tabla observamos claramente cinco delitos con mas modalidades, estos son, robo, homicidio,
# lesiones, secuestro y feminicidio.

# An�lisis gr�fico

library(ggplot2)

ggplot(as.data.frame(absoluta_delito), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", color="red3", fill="white") + 
  labs(title = "Diagrama de barras - Tipo de delito", subtitle = "Frecuencias Absolutas", 
       x = NULL, y = NULL) + coord_flip() 

ggplot(as.data.frame(relativa_delito), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", color="blue3", fill="white") + 
  labs(title = "Diagrama de barras - Tipo de delito", subtitle = "Frecuencias Relativas", 
       x = NULL, y = NULL) + coord_flip()

# Con las gr�ficas anteriores podemos afirmar los que ve�amos en nuestra tabla de frecuencias, se observan
# claramente los delitos con mas modalidades, ademas, podemos apreciar como todos los otros tipos de delitos
# tienen la misma cantad de modalidades.
# Es importante aclarar que por la forma en que se presentan nuestros datos, los registros de diferentes
# modalidades de delitos se repiten para cada estado de la rep�blica, contando con 32 estados, las
# frecuencias deben ser divididas entre 32 para tener las cantidades de modalidades exactas.

length(unique(datos$Estado))

ggplot(as.data.frame(relativa_delito), aes(x=2, y=Freq, fill=names(relativa_delito))) +
  geom_bar(stat="identity", color="white") + coord_polar("y", start=0) +
  theme_void() + xlim(0.5, 2.5)

# Estad�sticos de resumen

#Moda

library(modeest)

moda <- mfv(datos$TipoDelito) # Moda

cbind(Variable = names(datos)[3], Moda = moda)

#===========================An�lisis descriptivo de variables cuantitativas===================================

#Tabla de frecuencias de la variable Mayo (Cantidad de delitos registrados en Mayo)

library(fdth)

range(datos$Mayo)

table(datos$Mayo)

#Usando intervalos de clase
(tabla_mayo <- fdt(datos$Mayo,start=0,end=3750,h=150))

#Podemos ver que el 93 por ciento de nuestros valores se encuentran entre 0 y 150 registros de incidencias
# delictivas, a partir de registros de delitos mayores a 450 incidencias, las frecuencias se reducen.
# Es claro que tenemos una distribuci�n unimodal.

# An�lisis gr�fico

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

# En el diagrama de barras y el diagrama de puntos se observa claramente la distribuci�n de delitos 
# es asim�trica hacia la derecha. 
# Con el diagrama de caja y brazos podemos identificar que la mayor�a de delitos se encuentran entre 
# 0 y 20, pareciera que el resto de los registros de delitos son valores at�picos, podemos identificar tres
# que son mas extremos. 



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

summary(datos$Mayo) #Media y mediana

mfv(datos$Mayo) #Moda

quantile(datos$Mayo, probs = c(0.05,0.25,0.50,0.75,0.95)) #Cuantiles

IQR(datos$Mayo) #Amplitud intercuart�lica

sd(datos$Mayo) #Desviaci�n est�ndar

library(agricolae)

skewness(datos$Mayo) #Coeficiente de asimetr�a

# Con nuestras medidas descriptivas podemos obtener la siguiente informaci�n:
# La media es de 38.86, es decir, en el mes de Mayo de registraron en promedio 39 delitos, tenemos una 
# moda de 0 que representa la cantidad de filas en donde no se registraron incidencias de un tipo 
# especifico de delito, con los cuantiles podemos reafirmar lo anterior. El valor de la desviaci�n
# est�ndar y el coeficiente de asimetria nos da una idea de como est�n distribuidos nuestros datos y 
# podemos reafirmar lo que ve�amos en el an�lisis gr�fico.

# Grafica de ojiva

h <- fdt(datos$Mayo,start=0,end=3700,h=50)
h <- as.data.frame(h$table)

plot_ly(data=h,x= ~`Class limits` ,y= ~`cf(%)`, 
        marker = list(size = 5, color="red"),
        type = 'scatter', mode = 'lines')%>%
  layout(title = 'Ojiva (Delitos Mayo)', 
         xaxis = list(title = 'Intervalos'), 
         yaxis = list(title = 'Pi'),
         shapes = list(
           # linea vertical 
           list(type = "line", x0 = 0, x1 = 0, 
                y0 = 0, y1 = 1, yref = "paper"),
           # linea horizontal 
           list(type = "line", x0 = 0, x1 = 50000, 
                y0 = 1, y1 = 1, xref = "paper")))

quantile(datos$Mayo, c(.15, .60, .95))

# Debido a la frecuencia en los valores de nuestros datos, en la gr�fica de ojiva podemos identificar los
# porcentiles a patir de 0.85, el valor del porcentil 0.95 esta en el intervalo de 150 a 200, valor que 
# verificar calculando los porcentiles de manera directa en R.

#=============================comparaci�n y asociaci�n entre variables======================================

#Comparamos la variable Mayo (cantidad de delitos registrados en mayo) con la variable Estado

#Diagrama de caja y brazos
plot_ly(datos,x = ~Mayo, y = ~Estado, type = "box")

#Parece que Campeche es el estado en donde se registraron menos delitos en el mes de Mayo, por otro lado,
#El Estado de Mexico es el estado que cuenta con mas registros de delitos


#Diagrama de puntos
plot_ly(datos, x = ~datos$Mayo, y = ~datos$TipoDelito, color = ~datos$Estado, type = 'scatter',
        mode = "markers")

#Nuevamente vemos como los registros de delitos se concentran en valores peque�os, ahora podemos ver
#con mas claridad el tipo de delito y estado en donde existen grandes cantidades de incidencias delictivas.

#Asociaci�n entre variables cuantitativas

round(cor(datos[c(6:17)]),digits = 2) 

#Es claro que nuestras variables cuantitativas est�n asociadas, ya que los registros de delitos entre meses
#son muy parecidos. Ademas al ser registros consecutivos podemos ver la asociaci�n con meses anteriores.

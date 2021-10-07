datos <- read.csv(file = "./Incidencia_Delictiva_Ordenado.csv", encoding = "UTF-8")

#=============================== Descripcion de las variables ===============================================

str(datos)

###################################################################
##        Variable        #           Tipo           #   Escala  ##
###################################################################
##  Año                   #  Cuantitativa / Discreta # Intervalo ##
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
##  Septiembre            #  Cualitativa / Discreta  # Razón     ##
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

#Tabla de frecuencias de la variable Septiembre (Cantidad de delitos registrados en Septimbre)
#Usando intervalos de clase

library(fdth)

range(datos$Septiembre)

(tabla_cuant <- fdt(datos$Septiembre,breaks="Sturges",start=0,end=6500,h=250,right=T))

# Análisis gráfico

ggplot(as.data.frame(tabla_cuant$table), aes(x=`Class limits`, y=f)) +
  geom_bar(stat="identity", color="green4", fill="white") + 
  labs(title = "Diagrama de barras - Delitos cometido en Septiembre", subtitle = "Frecuencias Absolutas", 
       x = NULL, y = NULL) + coord_flip()

library(BHH2)

dotPlot(na.omit(datos$Septiembre),main="Diagrama de puntos",xlab="Delitos-Septiembre",pch=16)


library(plotly)
plot_ly(datos,x = ~Septiembre, type = "box")

# Se observa que la mayoria de delitos se encuantran entre 0 y 25, pareciera que 
# el resto de delitos son outliers. 
#Claramente la distribución de delitos es asimétrica hacia la derecha. 

# Medidas descriptivas

summary(datos$Septiembre) 

mfv(datos$Septiembre) #Moda

quantile(datos$Septiembre, probs = c(0.05,0.25,0.50,0.75,0.85,0.95,0.99)) #Cuantiles

IQR(datos$Septiembre) #Amplitud intercuartílica

var(datos$Septiembre); sd(datos$Septiembre) #Varianza

mean(abs(na.omit(datos$Septiembre)-mean(na.omit(datos$Septiembre))))   # error medio (media)

mean(abs(datos$Septiembre-median(datos$Septiembre)))  # error medio (mediana)

sd(datos$Septiembre)/mean(datos$Septiembre) #Coeficiente de variación

library(agricolae)

skewness(datos$Septiembre) #Coeficiente de asimetría

kurtosis(datos$Septiembre) #Coeficiente de curtosis

# Grafica de ojiva

#=============================comparación y asociación entre variables======================================


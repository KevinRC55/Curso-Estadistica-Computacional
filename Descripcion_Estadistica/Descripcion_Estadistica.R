datos <- read.csv(file = "./Incidencia_Delictiva_Ordenado.csv", encoding = "UTF-8")

#------------------------------------Descripcion de las variables-----------------------------------------------

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

# Tabla de frecuencias de la variable TipoDelito

(absoluta_delito <- table(datos$TipoDelito))  #tabla de frecuencias absolutas

relativa_delito <- tabla_delito / length(datos$TipoDelito) #tabla de frecuencias relativas

acumulada_a_delito <- cumsum(absoluta_delito)

acumulada_r_delito <- cumsum(relativa_delito)

tabla_frec <- cbind(Absoluta = absoluta_delito, A_Acumulada = acumulada_a_delito, 
      Relativa = relativa_delito, R_Acumulada = acumulada_r_delito)

tabla_frec

# Análisis gráfico

library(ggplot2)

ggplot(as.data.frame(absoluta_delito), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", color="red3", fill="white") + 
  labs(title = "Diagrama de barras - Tipo de delito", subtitle = "Frecuencias Absolutas", 
       x = NULL, y = NULL) + 
  coord_flip() 

ggplot(as.data.frame(relativa_delito), aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", color="blue3", fill="white") + 
  labs(title = "Diagrama de barras - Tipo de delito", subtitle = "Frecuencias Relativas", 
       x = NULL, y = NULL) + 
  coord_flip()

ggplot(as.data.frame(relativa_delito), aes(x=2, y=Freq, fill=names(relativa_delito))) +
  geom_bar(stat="identity", color="white") + coord_polar("y", start=0) +
  theme_void() + xlim(0.5, 2.5)

                 

# ========================================== Lectura de datos ================================================

rnd<- read.table("./aleatorios2.txt")

# =================================== Prueba de hipotesis de Kolmogorov-Smirnov ==============================

TablaKS <- function(muestra){
  n <- nrow(muestra) #Tamaño "n" de la muestra
  i <- 1:n #Posición que ocupa el numero aleatorio Xi
  rndi <- sort(muestra$V1)#Ordenar dichos números en orden acendente
  frndi <- i/n #Distribucion acumulada Fn(x) = i/n
  dif <- abs(rndi-frndi)#RNDi ??? F(RNDi)
  tbl <- data.frame("i"=i, "RNDi"=rndi, "F(RNDi)"=frndi, "RNDi-F(RNDi)"=dif)
  tbl
}

(tabla <- TablaKS(rnd))

# Dn(calculado) = max|Fn(Xi) ??? Xi|
(dn_c <- max(tabla$RNDi.F.RNDi.))

#valor en tablas para n=70 y alpha=0.02   
n <- nrow(rnd)
(dn_t <- 1.52/sqrt(n))  

# Si Dn(calculado) > Dn(tablas) se rechaza H0
# Como 0.32 > 0.18 se rechaza H0, es decir, la secuencia de numeros aleatorios no es uniforme

#Realizamos la prueba directamente en R
ks.test(rnd, "punif", 0, 1)



# ======================== Prueba de hipótesis de corridas por arriba y por abajo ======================

PruebaCorridas <- function(muestra){
  (sb<- as.integer(muestra>0.5)) #sucesion binaria
  #Corridas
  corridas <- rle(sb)
  corridas <- factor(corridas$lengths,levels=c(1:max(corridas$lengths)))
  corridas <- data.frame(table(corridas))
  #Columnas
  n <- nrow(muestra)
  i <- as.numeric(as.vector(corridas$corridas))
  fo <- as.numeric(corridas$Freq)
  fe <- (n-i+3)/2^(i+1)
  calc <- ((fe-fo)^2)/fe
  tbl <- data.frame("i"=i, "FE"=fe, "Fo"=fo, "(FE-FO)^2/FE"=calc)
  tbl
}

(tabla <- PruebaCorridas(rnd))

#X2(calculada)
(x2_c <- sum(tabla$X.FE.FO..2.FE))

#Valor en tablas para pi=9 alpha=0.02
(x2_t <- qchisq(0.98,9))

#46.12 > 19.67 entonces se rechaza independencia de los números aleatorios.
  
pchisq(46.12494, df=9, lower.tail = F)

#En R
library(snpar)

runs.test(rnd$V1, exact=T, alternative="two.sided")
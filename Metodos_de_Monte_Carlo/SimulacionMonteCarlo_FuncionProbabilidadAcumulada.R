#================================== Muestra aleatoria de tamaño 6000 ======================================

# Calculamos alfa y beta a partir de E[x]=24 y Var[X]=72
Parametros <- function(med, var){
  a <- med^2/var
  b <- var/med
  return(c(a,b))
}

media = 24
varianza = 72

(alpha <- Parametros(media, varianza)[1])
(beta <- Parametros(media, varianza)[2])

# Observamos gráficamente el comportamiento de la función Gamma con los parámetros cualculados
xx<- seq(0,50,length.out= 100)
plot(xx,dgamma(xx, shape=alpha, scale=beta), type="l", lwd=4, main="Gama(a=8,b=3)", col="green2")

t <- 15.95 # cuantil de interés
abline(v=t, col="red2")

legend(x = "topright", legend = c("t = 15.95"), fill = "red2")

# Generamos la muestra aleatoria
set.seed(5523)
n <- 6000
x <- rgamma(n, shape=alpha, scale=beta)
head(x); tail(x)

# ======================================= Método hit or miss ==============================================

g.ind <- as.integer(x>t)    # funcion indicadora
(theta.hat<- mean(g.ind))  # estimacion MC de la fda

head(cbind(x,g.ind)); tail(cbind(x,g.ind))   # comparamos los valores simulados con la indicadora

#====================================== Funcion directa de R ==============================================

# Comparamos con la funcion de R
cbind("Estimada"=theta.hat, "R"=pgamma(t, shape=alpha, scale=beta, lower.tail = F))

#============================== Estimacion de intervalo de confianza ======================================

## ERROR ESTANDAR EN EL ESTIMADOR DE LA INTEGRAL POR MC P(X > 15.95)
m <- 20000
x <- rgamma(n, shape=alpha, scale=beta)
g.ind <- x>t    # funcion indicadora
(theta.hat<- mean(g.ind))  # estimacion MC de la fda

(se.hat<- (1/m)*sqrt(sum((g.ind-theta.hat)^2))) 

# Intervalo de confianza para el estimador de la probabilidad acumulada
alfa<- 1-0.985   # ic 98.5%
c(theta.hat+qgamma(alfa/2, shape=alpha, scale=beta, lower.tail = T)*se.hat,
  theta.hat+qgamma(alfa/2, shape=alpha, scale=beta, lower.tail = F)*se.hat)   

# El valor verdadero de esta probabilidad está entre 0.8394542 y 0.8996866, nuestro valor
# verdadero calculado fue de 0.8273333

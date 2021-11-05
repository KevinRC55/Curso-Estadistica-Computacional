#=============================== Grafica de la función de densidad de probabilidad ==========================

x <- seq(0,20,0.01)

fx <- (2*x/exp(x))^2

plot(x, fx, type="l", col="blue2", lwd=3)

#======================================== Función envolvente ================================================

gx <- exp(-x)

lines(x, gx, type="l", col="red2",lwd=3)

legend(x = "topright", legend = c("F(x)","G(x)"),
       fill = c("blue2", "red2"))

#================================================= Valor de c =============================================

# Necesitamos f(x*)/g(x*) para encontrar el valor de c
h <- function(x){
  ((2*x/exp(x))^2)/exp(-x)
}

# Observamos gráficamente el cociente de funciones
plot(x, h(x), type="l", col="purple2", lwd=3)
# Podemos ver que el máximo es poco mayor a 2

# Maximizamos el cociente de las funciones con optimize para obtener el máximo
maximo = optimize(h,maximum = T, interval = c(0,20))
# Calculamos el valor de x*
xEstrella <- maximo$objective

# Encontramos el valor de c evaluando c = f(x*)-g(x*) 
(c <- h(xEstrella))


#======================================== Muestra aleatoria ===============================================

n <- 10000
k<- 0   # contador para aceptar valores
j<- 0  # contador de iteraciones necesarias para generar las "n" variables
y <- numeric(n)

while(k<n){
  u = runif(1)
  j = j+1
  x = rexp(1,rate=1) # v.a de g(x)~Exp(0,inf)
  if ((((2*x/exp(x))^2)/(c*exp(-x))) > u) { # Regla de aceptacion f(x)/cf(x)
    k = k+1 # acepto x
    y[k] = x
  }
}

j

# Se necesitan aprox. 21400 iteraciones para producir 10000 valores.

# Veamos las variables simuladas en la variable y

hist(y, prob = T, col = "orange2")
curve((2*x/exp(x))^2, add = T, col = "blue2", lwd=3)

#============================= Verificación estadística (Media y varianza) ==================================

# Calculamos el valor esperado, multiplicando por x nuestra función f(x) e integrando de 0 a infinito.

Ex <- function(x){
  x*((2*x/exp(x))^2)
}

espX <- integrate(Ex,0,9000)$value # Media teórica

#Calculamos la varianza como E[X^2] - E[x]^2

Ex2 <- function(x){
  (x^2)*((2*x/exp(x))^2)  
}

varX <- integrate(Ex2,0,9000)$value - (integrate(Ex,0,9000)$value)^2 # Varianza teórica

comparacion = cbind(Teóricos = c(espX, varX), Muestrales = c(mean(y), var(y)))
row.names(comparacion) = c("E[x]", "Var(x)"); comparacion

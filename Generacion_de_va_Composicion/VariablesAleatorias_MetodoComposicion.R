#==============================  función para la densidad de probabilidad ==================================

laplace <- function(x,a) (1/(2*a))*exp(-(abs(x)/a))

#==================================== Grafica de función de probabilidad de Laplace =========================
x<-seq(-10,10)

fa1 <- lapply(x, laplace, a=1)
plot(x,fa1,type="l", lwd=3, main="Distribución de Laplace", col="red2")

fa2 <- lapply(x, laplace, a=2)
lines(x,fa2,type="l", lwd=3, col="green2")

fa4 <- lapply(x, laplace, a=4)
lines(x,fa4,type="l", lwd=3, col="blue2")

legend(x = "topright", legend = c("a=1","a=2","a=4"),
       fill = c("red2", "green2", "blue2"))

#===================== Generacion de numeros aleatorios con distribución doble exponencial ==================
set.seed(2621654)

algoritmo <-function(a){
  u1 <- runif(1)
  u2 <- runif(1)
  ifelse(u1 < 0.5, -a*log(u2), a*log(u2))
}

library(purrr)

x <- rerun(5000, algoritmo(1))%>% flatten_dbl()

hist(x,prob=TRUE, ylim=c(0,0.5), main="Valores Generados", col="purple2")

x<-seq(-10,10)
fa1 <- lapply(x, laplace, a=1)
lines(x,fa1,type="l", lwd=3, col="red2")

#============================= Prueba de hipótesis de bondad de ajuste ====================================

library(ExtDist)

ks.test(x, alternative = "two.sided", "pLaplace", mu=0, b=1)

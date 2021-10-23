# =====================================Graficar la Función F(x)==============================================

library(dplyr)

f <- function(x){
  fx <- case_when(
    x >= 2 && x <= 3 ~ (x-2)/2,
    x > 3 && x <= 6 ~ (2-(x/3))/2
  )
}

x<-seq(2,6,0.01)

fx <- lapply(x, f)

plot(x,fx,type="l",col="green2")

# ========================== Producir números aleatorios con la distribución F(x) ===========================

triangular <- function(u){
  x <- case_when(
    u >= 0 && u<= 0.25 ~ 2+2*sqrt(u), 
    u > 0.25 && u <= 1 ~ 6-sqrt(12-(12*u)) 
  )
}

set.seed(55)
u <- runif(20000)

x.sim <- as.numeric(lapply(u, triangular)); 

#===================================== histograma de los valores simulados ==================================
library(RColorBrewer)

hist(x.sim, prob = T, col=brewer.pal(n = 9, name = "Greens"),main = "Histograma de los valores simulados",
     xlab = "x")

lines(x,fx,type="l",col="purple",lwd=4)

#======================================= E[X], Var(x) teóricos ==============================================

EX<- function(a,b,c) (a+b+c)/3
VX<- function(a,b,c) ((a^2)+(b^2)+(c^2)-(a*b)-(b*c)-(c*a))/18

comparacion = cbind(Teóricos = c(EX(2,3,6), VX(2,3,6)), Muestrales = c(mean(x.sim), var(x.sim)))
row.names(comparacion) = c("E[x]", "Var(x)"); comparacion

# ============================================ Directamente en R ============================================

# Usando rtri() de EnvStats

library(EnvStats)
?rtri

hist(rtri(20000, min = 2, max = 6, mode = 3), prob = T, ylim = c(0,0.5),
     col=brewer.pal(n = 9, name = "GnBu"), main = "Histograma de los valores simulados", xlab = "x")

lines(x,fx,type="l",col="purple",lwd=4)
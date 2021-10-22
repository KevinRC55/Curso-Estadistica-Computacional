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

set.seed(55)

inv_distri <- function(u){
  x <- case_when(
    u >= 0 && u<= 0.25 ~ 2+2*sqrt(u), 
    u > 0.25 && u <= 1 ~ 6-sqrt(12-(12*u)) 
  )
}

u <- runif(20000)

x.sim <- as.numeric(lapply(u, inv_distri)); 

#===================================== histograma de los valores simulados ==================================
library(RColorBrewer)

hist(x.sim, prob = T, col=brewer.pal(n = 9, name = "Greens"))

lines(x,fx,type="l",col="purple",lwd=4)

#======================================= E[X], Var(x) teóricos ==============================================

mean(x.sim); var(x.sim)   # valores muestrales


# ============================================ Directamente en R ============================================


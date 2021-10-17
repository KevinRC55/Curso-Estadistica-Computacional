lluvias <- read.table("./lluvias.dat",sep="\t",row.names = 1,header=T)

y <- lluvias$Precipitacion

#=========================== Valores de los parametros de la distribucion Wribull ==========================

# Para alpha
# Definimos una función para encontrar las raíces de alpha

ra <- function(a, x){
  return((var(x)/mean(x)^2)-(gamma(1+(2/a))/gamma(1+(1/a))^2)+1)
}

# Gráficamente

#Evaluamos nuestra función en el intervalo de (1,2)

intervalo<-seq(1,2,0.1)
evaluacion<-rep(0,length(intervalo))
for (i in 1:length(intervalo)) evaluacion[i] <- ra(intervalo[i],y)

library(plotly)

plot_ly(as.data.frame(cbind(intervalo, evaluacion)), x = ~intervalo, y = ~evaluacion, 
        type = "scatter", mode = 'lines+markers')

#Podemos ver un intervalo pequeño (1.9, 2) en donde se encuentra la raíz 

# Método numérico

# Podemos obtener las raíces de la función mediante la
#función de R uniroot() que ejecuta un algoritmo de bisección. Si proporcionamos
#un intervalo [a, b] tal que el signo de la evaluación en a sea diferente al signo en la evaluación en b, 
#uniroot() es capaz de encontrar el punto dentro de ese intervalo en el que la función se anula.

# Usemos el intervalo (1.9,2) encontrado gráficamente 

ra(1.9,y); ra(2,y)

(alpha.hat <- uniroot(ra, interval = c(1.9,2), x = y)$root)

# Para beta 

(beta.hat <- mean(y)/gamma(1+(1/alpha.hat)))

#================================ Funcion de (log)verosimilitud ============================================ 

log_weibull<- function(params) {
  logw=-sum(dweibull(y, params[1], params[2], log=TRUE))
  logw
}

#============================ Estimación de los parámetros ?? y ?? ============================================

(fit<- nlm(log_weibull,c(alpha.hat,beta.hat),hessian=T))

(alpha <- fit$estimate[1])
(beta <- fit$estimate[2])

#Usando MASS
library(MASS)
(fit2<- fitdistr(y, densfun="weibull",list(shape=alpha.hat,scale=beta.hat)))

# Comparamos los metodos 

alphas <- c(alpha.hat, alpha, fit2$estimate[1])
betas <- c(beta.hat, beta, fit2$estimate[2])
comp <- cbind("Alpha" = alphas, "??eta" = betas)
rownames(comp)<- c("Momentos","MLE (Verosimilitud)","MLE (fitdistr)"); comp

#=============================== Grafica con ajuste del Modelo Weibull =======================================

hist(y, pch=20, breaks=15, prob=TRUE,main="Ajuste",col = "green2")

curve(dweibull(x, alpha,beta),col="blue2",lwd=2,add=T)

#============================== Criterio ==================================================

# Modelo Exponencial
(fit3 <- fitdistr(y, densfun="exponential"))

k=1   # numero de parametros a estimar
n= length(y)
AIC_exp= 2*k-(2*fit3$loglik); AIC_exp
BIC_exp= log(n)*k-(2*fit3$loglik); BIC_exp


# Modelo Gamma
(fit4 <- fitdistr(y, densfun="gamma"))

k=2   # numero de parametros a estimar
AIC_gamma= 2*k-(2*fit4$loglik); AIC_gamma
BIC_gamma= log(n)*k-(2*fit4$loglik); BIC_gamma

# Modelo Weibull

k=2   # numero de parametros a estimar
AIC_weibull= 2*k-(2*fit2$loglik); AIC_weibull
BIC_weibull= log(n)*k-(2*fit2$loglik); BIC_weibull

criterios <- cbind("AIC" = c(AIC_exp,AIC_gamma,AIC_gamma),"BIC" = c(BIC_exp,BIC_gamma,BIC_weibull))
rownames(criterios) <- c("Exponencial","Gamma","Weibull"); criterios

# Gráficamente

hist(y, pch=20, breaks=15, prob=TRUE,main="Ajuste",col = "green2")

curve(dexp(x, fit3$estimate[1]),col="red2",lwd=2,add=T)
curve(dgamma(x, fit4$estimate[1],fit4$estimate[2]),col="yellow2",lwd=2,add=T)
curve(dweibull(x, fit2$estimate[1],fit2$estimate[2]),col="blue2",lwd=2,add=T)
legend(x = "topright", legend = c("Exponencial","Gamma","Weibull"),
       fill = c("red2", "yellow2","blue2"))

#El modelo preferido es el de menor AIC o BIC, podemos ver que el modelo Weibull y el modelo Gamma tienen 
# el mismo valor AIC, sin embargo el modelo Gamma tiene menor valor BIC. Ademas, si nos apoyamos en la gráfica 
# podemos ver que  el modelo gamma es el que mejor explica la distribución de lluvias en México.

####################################################################
# Practica No. 3                                                   #
####################################################################

install.packages("R2WinBUGS")
install.packages("mcmcplots")

library(R2WinBUGS)
library(coda)
library(boot)
library(mcmcplots)

#Directorio de WinBUGS
bugs.dir<-"D:/bin/WinBUGS14/"


####################################################################
# Ejemplo 4                                                        #
####################################################################
# Modelo
modelo <- function(){
  #Verosimilitud
  for(i in 1:n){
    x[i]~dnorm(mu,tau)
  }
  #Priori
  mu~dnorm(0,0.0001)
  tau~dgamma(0.01,0.01)
  sigma2<-1/tau
}

# Datos

datos=list(n=20,
           x=c(87,56,57,28,37,47,31,78,69,19,
               22,36,61,45,14,47,65,20,41,72)
)

# Parámetros
parametros <- c("mu","sigma2","tau")

# Inits
iniciales <-function(){list(mu=50,tau=0.0025)}


# Modelo en WinBUGS
write.model(modelo, "ejemplo1.bug")
#file.show("ejemplo1.bug")


fit1 <- bugs(data = datos,inits =  iniciales,
             parameters.to.save =  parametros,
             model.file="ejemplo1.bug",
             n.chains=3, n.iter=11000,
             n.burnin=1000,n.thin=1,
             #clearWD=TRUE,
             debug=FALSE,
             DIC = TRUE, digits=5,
             bugs.directory=bugs.dir
             #           ,program="OpenBUGS"
)
print(fit1,4)

#Diagnostico de Convergencia
plot(fit1)
mcmcplot(fit1)

#####################
# Gráfico de la cadena
ts.plot(fit1$sims.array[,,1],col=2:4)
ts.plot(fit1$sims.array[,,2],col=2:4)
# ...

#Gráficos de Autocorrelación
#Para alfa
acf(fit1$sims.array[,1,1],col=2)
acf(fit1$sims.array[,2,1],col=2)
# ...

#Gráficos de la función de densidad
plot(density(fit1$sims.matrix[,1]),
     type="l",col=2)
plot(density(fit1$sims.matrix[,2]),
     type="l",col=2)
# ...

# Usando la librería coda #

nchains=dim(fit1$sims.array)[2]

sim1=list()

for(h in 1:nchains){
  sim1[[h]]=as.mcmc(fit1$sims.array[,h,])
}
sim1=as.mcmc.list(sim1)
traceplot(sim1,ask=T)

#############################

sim1a=as.mcmc(fit1$sims.matrix)
HPDinterval(sim1a)
##############################




####################################################################
#                     Practica No.3
# Pregunta 1
####################################################################

#Descomentar para instalar los paquetes
#install.packages("R2WinBUGS")
#install.packages("mcmcplots")

library(R2WinBUGS)
library(coda)
library(boot)
library(mcmcplots)

#Directorio de WinBUGS
WINBUGS.DIR <- "D:/bin/WinBUGS14/"

#Nombre de archivo
NAME.FILE.BUG <- "Practica-03.bug"

#Ruta archivo
PATH.FILE <- "https://raw.githubusercontent.com/jgomezz/MscEstadisticaAplicada-UNALM-2018-2/master/MLG/Practica-03/molinos.csv"

molinos.data <- read.csv(PATH.FILE, header = TRUE)

head(molinos.data)

####################################################################
# MODELO LINEAL NORMAL :  Modelo 1: Eta_i= Beta_0 + Beta_1*x_i
####################################################################

# Se tiene que usar una variable intermedio para
# pasar los valores, se usa vectores
viento <- molinos.data$viento
corriente <- molinos.data$corriente
n <- nrow(molinos.data)

datos <- list("viento","corriente","n")

modelo <- function(){
  # verosimilitud
  for (i in 1:n) {
    mu[i] <- beta.0 + beta.1*viento[i];
    corriente[i] ~ dnorm(mu[i],tau);
  }
  # Las priori : estoy usando priori no informativos 
  # porque le estoy dando un rango amplio ( 0.0 en casi todos los modelos)
  beta.0 ~ dnorm(0.0,1.0E-4);      # 1er parametro  
  beta.1 ~ dnorm(0.0,1.0E-4);      # 2do parametro
  tau    ~ dgamma(1.0E-3,1.0E-3);  # 3er parametro , la presicion , uso gamma 

  # con valores pequeño para tener una varianza grande
  sigma2 <- 1/tau;
}

# Grabar archivo
write.model(modelo, NAME.FILE.BUG)

# Parametros
parametros <- c("beta.0","beta.1","tau","sigma2")

# Inicializa : asigna valores aleatorios para inicializarlos
iniciales <- function() { list(beta.0=rnorm(1), 
                          beta.1=rnorm(1), 
                          tau=rgamma(1,1,1))}

# Inferencia Bayesiana
fit1 <- bugs(data = datos,
             inits =  iniciales,
             parameters.to.save =  parametros,
             model.file= NAME.FILE.BUG,
             n.chains=2, 
             n.iter=20000,
             n.burnin=10000,
             n.thin=1,
             bugs.directory=WINBUGS.DIR,
             clearWD=TRUE, 
             debug=FALSE)

print(fit1,4)

#Diagnostico de Convergencia
plot(fit1)
mcmcplot(fit1)

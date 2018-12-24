
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
NAME.FILE.MOD1.BUG <- "Practica-03-mod1.bug"
NAME.FILE.MOD2.BUG <- "Practica-03-mod2.bug"
NAME.FILE.MOD3.BUG <- "Practica-03-mod3.bug"

#Ruta archivo
PATH.FILE <- "https://raw.githubusercontent.com/jgomezz/MscEstadisticaAplicada-UNALM-2018-2/master/MLG/Practica-03/molinos.csv"

#Lectura de datos
molinos.data <- read.csv(PATH.FILE, header = TRUE)
head(molinos.data)


# Se tiene que usar una variable intermedio para
# pasar los valores, se usa vectores
viento <- molinos.data$viento
corriente <- molinos.data$corriente
n <- nrow(molinos.data)

datos <- list("viento","corriente","n")

####################################################################
# Modelo Lineal Normal - Modelo 1 : Eta_i= Beta_0 + Beta_1*x_i
####################################################################

# Inferencia Clásica
fit1.model<-lm(molinos.data$corriente ~ molinos.data$viento)
summary(fit1.model)

# Aplicando Bayesianos
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
  tau    ~ dgamma(1.0E-3,1.0E-3);  # 3er parametro , la precisión , uso gamma 

  # con valores pequeño para tener una varianza grande
  sigma2 <- 1/tau;
}

# Grabar archivo
write.model(modelo, NAME.FILE.MOD1.BUG)

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
             model.file= NAME.FILE.MOD1.BUG,
             n.chains=2, 
             n.iter=20000,
             n.burnin=10000,
             n.thin=1,
             bugs.directory=WINBUGS.DIR,
             clearWD=TRUE, 
             debug=FALSE)

#Mostrar resultados de la simulación
print(fit1,4)

#Diagnostico de Convergencia
plot(fit1)

#Mostrar resultado por navegador de trace, density, autocorrelation
mcmcplot(fit1)


####################################################################
# Modelo Lineal Normal - Modelo 2 : Eta_i= Beta_0 + Beta_1*1/x_i
####################################################################

# Inferencia Clásica -
fit2.model<-lm(molinos.data$corriente ~ I(1/molinos.data$viento))
summary(fit2.model)

# Aplicando Bayesianos
modelo <- function(){
  # verosimilitud
  for (i in 1:n) {
    mu[i] <- beta.0 + beta.1*(1/viento[i]);
    corriente[i] ~ dnorm(mu[i],tau);
  }
  # Las priori : estoy usando priori no informativos 
  # porque le estoy dando un rango amplio ( 0.0 en casi todos los modelos)
  beta.0 ~ dnorm(0.0,1.0E-4);      # 1er parametro  
  beta.1 ~ dnorm(0.0,1.0E-4);      # 2do parametro
  tau    ~ dgamma(1.0E-3,1.0E-3);  # 3er parametro , la precisión , uso gamma 
  
  # con valores pequeño para tener una varianza grande
  sigma2 <- 1/tau;
}

# Grabar archivo
write.model(modelo, NAME.FILE.MOD2.BUG)

# Parametros
parametros <- c("beta.0","beta.1","tau","sigma2")

# Inicializa : asigna valores aleatorios para inicializarlos
iniciales <- function() { list(beta.0=rnorm(1), 
                               beta.1=rnorm(1), 
                               tau=rgamma(1,1,1))}

# Inferencia Bayesiana
fit2 <- bugs(data = datos,
             inits =  iniciales,
             parameters.to.save =  parametros,
             model.file= NAME.FILE.MOD2.BUG,
             n.chains=2, 
             n.iter=20000,
             n.burnin=10000,
             n.thin=1,
             bugs.directory=WINBUGS.DIR,
             clearWD=TRUE, 
             debug=FALSE)

#Mostrar resultados de la simulación
print(fit2,4)

#Diagnostico de Convergencia
plot(fit2)

#Mostrar resultado por navegador de trace, density, autocorrelation
mcmcplot(fit2)


####################################################################
# Modelo Lineal Normal - Modelo 3 : Eta_i= Beta_0 + Beta_1*log(x_i)
####################################################################

# Inferencia Clásica -
fit3.model<-lm(molinos.data$corriente ~ I(log(molinos.data$viento)))
summary(fit3.model)

# Aplicando Bayesianos
modelo <- function(){
  # verosimilitud
  for (i in 1:n) {
    mu[i] <- beta.0 + beta.1*log(viento[i]);
    corriente[i] ~ dnorm(mu[i],tau);
  }
  # Las priori : estoy usando priori no informativos 
  # porque le estoy dando un rango amplio ( 0.0 en casi todos los modelos)
  beta.0 ~ dnorm(0.0,1.0E-4);      # 1er parametro  
  beta.1 ~ dnorm(0.0,1.0E-4);      # 2do parametro
  tau    ~ dgamma(1.0E-3,1.0E-3);  # 3er parametro , la precisión , uso gamma 
  
  # con valores pequeño para tener una varianza grande
  sigma2 <- 1/tau;
}

# Grabar archivo
write.model(modelo, NAME.FILE.MOD3.BUG)

# Parametros
parametros <- c("beta.0","beta.1","tau","sigma2")

# Inicializa : asigna valores aleatorios para inicializarlos
iniciales <- function() { list(beta.0=rnorm(1), 
                               beta.1=rnorm(1), 
                               tau=rgamma(1,1,1))}

# Inferencia Bayesiana
fit3 <- bugs(data = datos,
             inits =  iniciales,
             parameters.to.save =  parametros,
             model.file= NAME.FILE.MOD3.BUG,
             n.chains=2, 
             n.iter=20000,
             n.burnin=10000,
             n.thin=1,
             bugs.directory=WINBUGS.DIR,
             clearWD=TRUE, 
             debug=FALSE)

#Mostrar resultados de la simulación
print(fit3,4)

#Diagnostico de Convergencia
plot(fit3)

#Mostrar resultado por navegador de trace, density, autocorrelation
mcmcplot(fit3)



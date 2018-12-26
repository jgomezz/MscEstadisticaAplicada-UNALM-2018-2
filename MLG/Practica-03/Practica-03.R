
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
NAME.FILE.MOD1.BUG <- "Practica03-modelo01.bug"
NAME.FILE.MOD2.BUG <- "Practica03-modelo02.bug"
NAME.FILE.MOD3.BUG <- "Practica03-modelo03.bug"

#Ruta archivo
PATH.FILE <- "https://raw.githubusercontent.com/jgomezz/MscEstadisticaAplicada-UNALM-2018-2/master/MLG/Practica-03/molinos.csv"

#Lectura de datos
molinos.data <- read.csv(PATH.FILE, header = TRUE)
head(molinos.data)

#Análisis descriptivo
plot(molinos.data$viento, molinos.data$corriente)

#Datos para el análisis
molinos.data.bugs <- list(viento = molinos.data$viento ,
                          corriente = molinos.data$corriente ,
                          N = nrow(molinos.data))

# Parametros
molinos.param.bugs <- c("alpha","beta1","tau","sigma2")


# Inicializa : asigna valores aleatorios para inicializarlos
molinos.inits.bugs <- function() {
                          list( alpha = rnorm(1), 
                                beta1 = rnorm(1), 
                                tau   = rgamma(1,1,1) )}

####################################################################
# Modelo Lineal Normal - Modelo 1 : Eta_i= Beta_0 + Beta_1*x_i
####################################################################

# Inferencia Clásica
molinos.model.1.clasic<-lm(molinos.data$corriente ~ molinos.data$viento)
summary(molinos.model.1.clasic)


# Aplicando Bayesianos
molinos.model.1.bugs <- function(){
  # verosimilitud
  for (i in 1:N) {
    mu[i] <- alpha + beta1*viento[i];
    corriente[i] ~ dnorm(mu[i],tau);
  }
  # Las priori : estoy usando priori no informativos 
  # porque le estoy dando un rango amplio ( 0.0 en casi todos los modelos)
  alpha ~ dnorm(0.0,1.0E-4);      # 1er parametro  
  beta1 ~ dnorm(0.0,1.0E-4);      # 2do parametro
  tau    ~ dgamma(1.0E-3,1.0E-3);  # 3er parametro , la precisión , uso gamma 

  # con valores pequeño para tener una varianza grande
  sigma2 <- 1/tau;
}

# Grabar archivo
write.model(molinos.model.1.bugs, NAME.FILE.MOD1.BUG)

# Inferencia Bayesiana
molinos.fit.model.1.bugs <- bugs(data = molinos.data.bugs,
                                 inits =  molinos.inits.bugs,
                                 parameters.to.save =  molinos.param.bugs,
                                 model.file= NAME.FILE.MOD1.BUG,
                                 n.chains=2, 
                                 n.iter=20000,
                                 n.burnin=10000,
                                 n.thin=1,
                                 bugs.directory=WINBUGS.DIR,
                                 clearWD=TRUE, 
                                 debug=FALSE)

#Mostrar resultados de la simulación
print(molinos.fit.model.1.bugs,4)

#Diagnostico de Convergencia
plot(molinos.fit.model.1.bugs)

#Mostrar resultado por navegador de trace, density, autocorrelation
mcmcplot(molinos.fit.model.1.bugs)

####################################################################
# Modelo Lineal Normal - Modelo 2 : Eta_i= Beta_0 + Beta_1*1/x_i
####################################################################

# Inferencia Clásica -
molinos.model.2.clasic<-lm(molinos.data$corriente ~ I(1/molinos.data$viento))
summary(molinos.model.2.clasic)

# Aplicando Bayesianos
molinos.model.2.bugs <- function(){
  # verosimilitud
  for (i in 1:N) {
    mu[i] <- alpha + beta1*(1/viento[i]);
    corriente[i] ~ dnorm(mu[i],tau);
  }
  # Las priori : estoy usando priori no informativos 
  # porque le estoy dando un rango amplio ( 0.0 en casi todos los modelos)
  alpha ~ dnorm(0.0,1.0E-4);      # 1er parametro  
  beta1 ~ dnorm(0.0,1.0E-4);      # 2do parametro
  tau    ~ dgamma(1.0E-3,1.0E-3);  # 3er parametro , la precisión , uso gamma 
  
  # con valores pequeño para tener una varianza grande
  sigma2 <- 1/tau;
}

# Grabar archivo
write.model(molinos.model.2.bugs, NAME.FILE.MOD2.BUG)

# Inferencia Bayesiana
molinos.fit.model.2.bugs <- bugs(data = molinos.data.bugs,
                                 inits =  molinos.inits.bugs,
                                 parameters.to.save =  molinos.param.bugs,
                                 model.file= NAME.FILE.MOD2.BUG,
                                 n.chains=2, 
                                 n.iter=20000,
                                 n.burnin=10000,
                                 n.thin=1,
                                 bugs.directory=WINBUGS.DIR,
                                 clearWD=TRUE, 
                                 debug=FALSE)

#Mostrar resultados de la simulación
print(molinos.fit.model.2.bugs,4)

#Diagnostico de Convergencia
plot(molinos.fit.model.2.bugs)

#Mostrar resultado por navegador de trace, density, autocorrelation
mcmcplot(molinos.fit.model.2.bugs)

####################################################################
# Modelo Lineal Normal - Modelo 3 : Eta_i= Beta_0 + Beta_1*log(x_i)
####################################################################

# Inferencia Clásica -
molinos.model.3.clasic<-lm(molinos.data$corriente ~ I(log(molinos.data$viento)))
summary(molinos.model.3.clasic)

# Aplicando Bayesianos
molinos.model.3.bugs <- function(){
  # verosimilitud
  for (i in 1:N) {
    mu[i] <- alpha + beta1*log(viento[i]);
    corriente[i] ~ dnorm(mu[i],tau);
  }
  # Las priori : estoy usando priori no informativos 
  # porque le estoy dando un rango amplio ( 0.0 en casi todos los modelos)
  alpha ~ dnorm(0.0,1.0E-4);      # 1er parametro  
  beta1 ~ dnorm(0.0,1.0E-4);      # 2do parametro
  tau    ~ dgamma(1.0E-3,1.0E-3);  # 3er parametro , la precisión , uso gamma 
  
  # con valores pequeño para tener una varianza grande
  sigma2 <- 1/tau;
}

# Grabar archivo
write.model(molinos.model.3.bugs, NAME.FILE.MOD3.BUG)

# Inferencia Bayesiana
molinos.fit.model.3.bugs <- bugs(data = molinos.data.bugs,
                                 inits =  molinos.inits.bugs,
                                 parameters.to.save =  molinos.param.bugs,
                                 model.file= NAME.FILE.MOD3.BUG,
                                 n.chains=2, 
                                 n.iter=20000,
                                 n.burnin=10000,
                                 n.thin=1,
                                 bugs.directory=WINBUGS.DIR,
                                 clearWD=TRUE, 
                                 debug=FALSE)

#Mostrar resultados de la simulación
print(molinos.fit.model.3.bugs,4)

#Diagnostico de Convergencia
plot(molinos.fit.model.3.bugs)

#Mostrar resultado por navegador de trace, density, autocorrelation
mcmcplot(molinos.fit.model.3.bugs)

# Validación del DIC en los modelos
df <- data.frame(
          modelo = c(" Modelo 1", 
                     " Modelo 2", 
                     " Modelo 3"),
          DIC = c(molinos.fit.model.1.bugs$DIC,
                  molinos.fit.model.2.bugs$DIC,
                  molinos.fit.model.3.bugs$DIC)
          )

df[order(df$DIC),]

####################################################################
# Predicción : Se elige el módelo 2 basado en el DIC
####################################################################

# Se obtiene la matriz de simulación
sims.matrix <- molinos.fit.model.2.bugs$sims.matrix

# Se obtiene los coeficientes de las variables predictoras
p.alpha <- sims.matrix[,"alpha"]
p.beta1 <- sims.matrix[,"beta1"]
p.sigma <- 1/sqrt(sims.matrix[,"tau"])
M       <- nrow(sims.matrix)

# Velocidad de viento es 5.8
viento <- 5.8 

# Se realiza la predicción
mu.new  <- p.alpha + p.beta1*(1/viento)
y.new   <- rnorm(M,mu.new,p.sigma)

summary(mu.new)
#hist(mu.new)
quantile(mu.new,probs=c(0.025,0.975))

summary(y.new)
#hist(y.new)
quantile(y.new,probs=c(0.025,0.975))

# Resultado
sprintf("Para un viento de %.2f se tiene una corriente de %.2f",viento, mean(y.new))

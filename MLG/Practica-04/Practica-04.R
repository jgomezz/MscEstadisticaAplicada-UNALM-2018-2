####################################################################
#                     Practica No.4
# Pregunta 
####################################################################

#install.packages("asaur")
#library(asaur)

#gastricXelox[23:27,]

#head(gastricXelox)
#head(pancreatic)

#
#install.packages("survival")
library(survival)
head(survexp.us)
str(survexp.us)

DAYS_BY_YEAR <- 365.25
#attach(survexp.us)
hazMale <- survexp.us[,"male","2004"]
hazFemale <- survexp.us[,"female","2004"]
tm <- c(0,
        1/DAYS_BY_YEAR,
        7/DAYS_BY_YEAR,
        28/DAYS_BY_YEAR,
        1:(dim(hazMale)-4))
tm
length(tm)
dim(hazMale)
dim(hazFemale)

plot(x = tm, y = hazMale, type = "l", col="blue",lwd=1)
lines(x = tm, y = hazFemale, type = "l", col="red",lwd=1)
legend("topleft",col=c("blue","red"),legend =c("hazMale","hazFemale"), lwd=1, bty = "n")


#plot(x = tm, y = hazFemale, type = "l")

# Calcula la integral como una suma de las áreas de los
# rectangulos
tm.diff <- diff(tm)
LEN <- length(tm.diff)
survMale <- exp(-cumsum(hazMale[1:LEN]*tm.diff)*DAYS_BY_YEAR)
survFemale <- exp(-cumsum(hazFemale[1:LEN]*tm.diff)*DAYS_BY_YEAR)

plot(x = tm[1:LEN], y = survMale, type = "l", col="blue",lwd=1)
#points(x = tm[1:LEN], y = survFemale, type = "l", col="green")
lines(x = tm[1:LEN], y = survFemale, type = "l", col="red",lwd=1)

legend("bottomleft",col=c("blue","red"),legend =c("survMale","survFemale"), lwd=1, bty = "n")

cbind(tm[1:LEN], tm.diff)

sum(survMale*tm.diff)
sum(survFemale*tm.diff)

##################################################################
#                       Exercises
##################################################################

##################################################################
# 2.1. Using the "survexp.us" data described in Example 2.2, plot
# the hazard functions for men and women in 1940 and 2000. Comment 
# on the change in mortality rates in children
##################################################################

# 1940
##################################################################
hazMale_1940 <- survexp.us[,"male","1940"]
hazFemale_1940 <- survexp.us[,"female","1940"]
tm_1940 <- c(0,
        1/DAYS_BY_YEAR,
        7/DAYS_BY_YEAR,
        28/DAYS_BY_YEAR,
        1:(dim(hazMale_1940)-4))

plot(x = tm, log = "y", y = hazFemale_1940, type = "l", col="blue", lwd=1, 
      ylab ="Hazard", xlab = "Age in years", 
      main ="Hazard for US males and females 1940", ylim=c(1e-07,1e-02))
lines(x = tm, y = hazMale_1940, type = "l", col="red",lwd=1)
legend("topleft",col=c("blue","red"),legend =c("Females","Males"), lwd=1, bty = "n")

# 2000
##################################################################

hazMale_2000 <- survexp.us[,"male","2000"]
hazFemale_2000 <- survexp.us[,"female","2000"]
tm_2000 <- c(0,
             1/DAYS_BY_YEAR,
             7/DAYS_BY_YEAR,
             28/DAYS_BY_YEAR,
             1:(dim(hazMale_2000)-4))

plot(x = tm, log = "y", y = hazFemale_2000, type = "l", col="blue", lwd=1, 
     ylab ="Hazard", xlab = "Age in years", 
     main ="Hazard for US males and females 2000", ylim=c(1e-07,1e-02))
lines(x = tm,  y = hazMale_2000, type = "l", col="red",lwd=1)
legend("topleft",col=c("blue","red"),legend =c("Females","Males"), lwd=1, bty = "n")


# 1940 - 2000
##################################################################

plot(x = tm, log = "y", y = hazFemale_1940, type = "l", col="blue", lwd=1, 
     ylab ="Hazard", xlab = "Age in years", 
     main ="Hazard for US males and females 1940 vs 2000", ylim=c(1e-07,1e-02))
lines(x = tm,  y = hazMale_1940, type = "l", col="red",lwd=1)
lines(x = tm,  y = hazFemale_2000, type = "l", col="green",lwd=1)
lines(x = tm,  y = hazMale_2000, type = "l", col="orange",lwd=1)
legend("topleft",col=c("blue","red","green","orange"),
       legend =c("Females 1940","Males 1940","Females 2000","Males 2000"), lwd=1, bty = "n")

##################################################################
# 2.2. Find the mean age of death separately for men and women for 1940
# and 2000.
##################################################################

# Se calcula la edad media de fallecimiento calculando el área
# debajo de la función de Hazard

# 1940
tm_1940.diff <- diff(tm_1940)
LEN <- length(tm_1940.diff)
survMale_1940 <- exp(-cumsum(hazMale_1940[1:LEN]*tm_1940.diff)*DAYS_BY_YEAR)
survFemale_1940 <- exp(-cumsum(hazFemale_1940[1:LEN]*tm_1940.diff)*DAYS_BY_YEAR)
sum(survMale_1940*tm_1940.diff)
sum(survFemale_1940*tm_1940.diff)

#2000
tm_2000.diff <- diff(tm_2000)
LEN <- length(tm_2000.diff)
survMale_2000 <- exp(-cumsum(hazMale_2000[1:LEN]*tm_2000.diff)*DAYS_BY_YEAR)
survFemale_2000 <- exp(-cumsum(hazFemale_2000[1:LEN]*tm_2000.diff)*DAYS_BY_YEAR)
sum(survMale_2000*tm_2000.diff)
sum(survFemale_2000*tm_2000.diff)


report <- data.frame(
  year = c("1940"," 2000"),
  male = c(sum(survMale_1940*tm_1940.diff),
           sum(survMale_2000*tm_2000.diff)),
  female = c(sum(survFemale_1940*tm_1940.diff),
             sum(survFemale_2000*tm_2000.diff))
)

report

##################################################################
# 2.3. The data set "survexp.usr" in the "survival" package is a four
# dimensional array of hazards in format similar to the "survexp.us"
# data set, with race (black or white) in the added dimension.Plot
# the hazard functions for black males and white males for 1940 and 2000
##################################################################

head(survexp.usr)
str(survexp.usr)
hazMaleWhite_1940 <- survexp.usr[,"male","white","1940"]
hazMaleWBlack_1940 <- survexp.usr[,"male","black","1940"]
hazMaleWhite_2000 <- survexp.usr[,"male","white","2000"]
hazMaleWBlack_2000 <- survexp.usr[,"male","black","2000"]

tm <- c(0,
         1/DAYS_BY_YEAR,
         7/DAYS_BY_YEAR,
         28/DAYS_BY_YEAR,
         1:(dim(hazMaleWBlack_2000)-4))  # Cualquier registro de los 4 se utiliza


plot(x = tm, log = "y", y = hazMaleWhite_1940, type = "l", col="blue", lwd=1, 
     ylab ="Hazard", xlab = "Age in years", 
     main ="Hazard for US males white and black in 1940 vs 2000", ylim=c(1e-07,1e-02))
lines(x = tm,  y = hazMaleWBlack_1940, type = "l", col="red",lwd=1)
lines(x = tm,  y = hazMaleWhite_2000, type = "l", col="green",lwd=1)
lines(x = tm,  y = hazMaleWBlack_2000, type = "l", col="orange",lwd=1)
legend("topleft",col=c("blue","red","green","orange"),
       legend =c("White males 1940","Black males 1940","White males 2000","Black males 2000"), lwd=1, bty = "n")

##################################################################
# 2.4. Consider the survival data in Exercise 1.1 Assuming that 
# these observations are from an exponential distribution, find LAMDA
# ESTIMATE and an estimate of var(lambda ESTIMATE)
##################################################################

# Data de exercise 1.1
#
#   patient survtime status
#     1       5       0
#     2       5       0
#     3       4       1
#     4       3       1
#     5       1       1
#
#
#
# recordando que 
#
#   d = sumatoria de phi - censoring indicator ( # de muertes)
#   V = sumatoria de ti (sumatoria de observaciones)
#
#   lambda estimada = d / V
#   var(lambda estimada) = d / ( V * V )
#

survival.data <- data.frame(patient = c(1,2,3,4,5),
                            survtime = c(5,5,4,3,1),
                            status = c(0,0,1,1,1))

d <- sum(survival.data$status)
V <- sum(survival.data$survtime)

lambda_estimado <- d/V
lambda_estimado

var_lambda_estimado <- d/V^2
var_lambda_estimado

##################################################################
# 2.5. Consider a survival distribution with constant hazard 
# lambda = 0.07 from t = 0 until t = 5 and then hazard lambda = 0.14
# for t > 5 ( This is known as a piecewise constant hazard). Plot
# this hazard function and the corresponding survival function
# for 0 < t < 10. What is the median survival time?
##################################################################


##################################################################
# Gráfica de la función de Hazard

##################################################################
ALPHA_EXP <- 1 # Distribucion Exponencial para la distribucion Weibull con ALPHA = 1
LAMBDA_1 <- 0.07 
LAMBDA_2 <- 0.14

weibHaz <- function(x, shape, scale) dweibull(x, shape = shape, scale = scale) / pweibull(x, shape = shape, scale = scale, lower.tail = F)

weibHazCompuesta <- function(x) {
  if( (x >= 0) && (x <= 5)  )
    weibHaz(x,shape=ALPHA_EXP, scale = 1/LAMBDA_1)
  else if ((x > 5) && (x <= 10) )
    weibHaz(x,shape=ALPHA_EXP, scale = 1/LAMBDA_2)
}

x1 <- seq(0, 10, by = 0.005)
y1 <- numeric(length = length(x1))

index <- 1
for(item in x1){
  y1[index] <- weibHazCompuesta(item)
  index <- index  + 1
}

plot(x1, y1, type = "l", col="blue",lwd=1, ylim=c(0,0.18), ylab ="Hazard", xlab = "Time", main ="Hazard function")

##################################################################
# Cálculo de la mediana
##################################################################
# La mediana se calcula cuando S(t_mediana) = 0.5
# Como S(t) = exp(-H(t))
# Entonces  exp(-H(t_mediana)) = 0.5 ==> H(t_mediana) = log(2) 
# 
# H(t_mediana) = log(2)
# H(t_mediana) = 0.6931472
#
# Pero H(t) es la integral de la función de Hazard, que vendria 
# hacer el área bajo la curva de Hazard
# 
# El área bajo la curva esta dado por lo siguiente :
#   
# H(t):  
#   0.07*t                0 <= t <= 5
#   0.07*5 + 0.14*(t-5)   5 <  t <= 10  
# 
# H(t):
#   0.07*t                0 <= t <= 5
#   0.35  + 0.14*(t-5)    5 <  t <= 10  
#   
# Se observa que el valor de la mediana esta en el segundo bloque
# porque 0.6931472 > 0.35
# 
# Entonces:  0.35 + 0.14*(t_mediana-5) = 0.6931472
       
t_mediana <-  (0.6931472-0.35)/0.14 + 5 
  
t_mediana

##################################################################
# Gráfica de la función de Supervivencia

##################################################################

# S(t) = exp(-H(t))  # Ecuación 2.2.1 pag 14
#
# Entonces
# 
# S(t):
#    exp(-0.07*t)             0 <= t <= 5
#    exp(-0.35-0.14*(t-5))    5 <  t <= 10
    
survCompuesta <- function(t) {
  if( (t >= 0) && (t <= 5)  )
    exp(-0.07*t)
  else if ((t > 5) && (t <= 10) )
    exp(-0.35-0.14*(t-5))
}  


x2 <- seq(0, 10, by = 0.005)
y2 <- numeric(length = length(x2))

index <- 1
for(item in x2){
  y2[index] <- survCompuesta(item)
  index <- index  + 1
}


plot(x2, y2, type = "l", col="blue",lwd=1, ylab ="Survival", xlab = "Time", main ="Survival function")


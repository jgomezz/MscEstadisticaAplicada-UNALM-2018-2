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




####################################################################
#                     Practica No.4
# Pregunta 
####################################################################

#install.packages("asaur")
#library(asaur)

#gastricXelox[23:27,]

head(gastricXelox)
head(pancreatic)

#
#install.packages("survival")
library(survival)
head(survexp.us)

DAYS_BY_YEAR <- 365.25
#attach(survexp.us)
x <- survexp.us
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

# Calcula la integral como una suma de las areas de los
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


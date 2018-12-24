
####################################################################
#                     Practica No.3
# Pregunta 1
####################################################################

install.packages("R2WinBUGS")
install.packages("mcmcplots")

library(R2WinBUGS)
library(coda)
library(boot)
library(mcmcplots)

#Directorio de WinBUGS
WINBUGS.DIR <-"D:/bin/WinBUGS14/"

#Ruta archivo
PATH.FILE <- "https://raw.githubusercontent.com/jgomezz/MscEstadisticaAplicada-UNALM-2018-2/master/MLG/Practica-03/molinos.csv"

molinos.data <- read.csv(PATH.FILE, header = TRUE)

head(molinos.data)


####################################################################
# MODELO LINEAL NORMAL :  Modelo 1: Eta_i= Beta_0 + Beta_1*x_i
####################################################################


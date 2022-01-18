##################################################
### PROG8430                                    ##
### Assignment2           ## 
##################################################
#                                               ##
##################################################
# Written by Uchenna Ilodigwe
# ID: 8801077
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory to an appropriate location
setwd("C:/Users/Uchenna/Desktop/Conestoga/DataAnalysis/WorkDirectory")

#options(scipen=12)

##################################################
### Install Libraries                           ##
##################################################
if(!require(tseries)){install.packages("tseries")}
library("tseries")           #"attaches" package 
if(!require(TTR)){install.packages("TTR")}
library("TTR")           #"attaches" package 
if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")           #"attaches" package 
if(!require(smooth)){install.packages("smooth")}
library("smooth")           #"attaches" package 


##################################################
### Read in Data                                ##
###########################################1.1#######################################################
Temperature_UI <-get(load("Woodstock_21F.Rdata"))

#Convert to a timeseries datatype
TempStudy_UI <-ts(Temperature_UI, frequency =12 , start=c(1988,1))




###########################################1.2.1#############################################
##############Summarize the temperature information (mean, etc.)
stat.desc(TempStudy_UI)



###########################################1.2.2#############################################
#####################Plot the time series data.
plot.ts(TempStudy_UI, main = "Average Temperature - Woodstock" , ylim = c(-15,25))


###########################################1.2.3#############################################
#####################Decompose the times series data in to the constituent components.

decompTemp_UI = decompose(TempStudy_UI, type = "additive")
decompTemp_UI
plot(decompTemp_UI)


###########################################1.2.4#############################################
#############################Determine if the time series is stationary.

adf.test(TempStudy_UI)

###########################################1.2.5#############################################
#################################### Deseasonalize the information and plot the result.

TempStudy_Seas_Adj_UI <-TempStudy_UI - decompTemp_UI$seasonal
plot.ts(TempStudy_Seas_Adj_UI, main = " Deseasonalized - Average Temperature - Woodstock" , ylim = c(-15,25))


#########################################2.1###################################################
##################Data Transformation
Temperature2_UI <-get(load("Ayr_21F.Rdata"))


#Convert to a timeseries datatype
TempStudy2_UI <-ts(Temperature2_UI, frequency =1 , start=c(1968))

####check first 6 observations
head(TempStudy2_UI)


#########################################2.2.1###################################################
##################Summarize the information
stat.desc(TempStudy2_UI)

#########################################2.2.2###################################################
##################plot the timeseries data

plot.ts(TempStudy2_UI, main = "Average Temperature - Ayr" , ylim = c(10,14))


#############################2.2.3#######################################33
#################Smooth the temperature chart using a moving average.

TempStudy2SMA10_UI <-SMA(TempStudy2_UI, n= 10)
plot.ts(TempStudy2SMA10_UI, main = "Temperature chart with moving average 10")

TempStudy2SMA10_UI <-SMA(TempStudy2_UI, n= 15)
plot.ts(TempStudy2SMA10_UI , main = "Temperature chart with moving average 15")


TempStudy2SMA10_UI <-SMA(TempStudy2_UI, n= 12)  ###BEST
plot.ts(TempStudy2SMA10_UI, main = "Temperature chart with moving average 12")



#######################2.2.4#######################################

#############################Determine if the time series is stationary.

adf.test(TempStudy2_UI)




#######################2.2.5#######################################
#Create an autocorrelation chart (using acf)
acf(TempStudy2_UI)


#######################3.1#######################################
############Create a simple moving average forecast of temperature in Ayr

move_avgUI <-sma(TempStudy2_UI)
move_avgUI

move_avgUI <-forecast(move_avgUI, h = 5,level = 0.75)  #h= Forecast years , levels = 75% prediction levels
move_avgUI
plot(move_avgUI, main = " simple moving Average Temperature Forecast in Ayr for 5 years ")


#######################3.2#######################################
############Create an exponentially smoothed forecast of temperature

ES_avgUI <-es(TempStudy2_UI)
ES_avgUI

ES_avgUI <-forecast(ES_avgUI, h = 5,level = 0.75)  #h= Forecast years , levels = 75% prediction levels
ES_avgUI
plot(ES_avgUI, main = "Exponetially Smoothed Forecast for 5 years")


#Time series

setwd('E:\\ResearchProject\\Jamal Sir\\Rabies')

#ARIMA 
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)
options(scipen = 999) ## To disable scientific notation
rabiesdata <- read.csv("IDH.csv")


# cor.test(rabiesdata$NVD, rabiesdata$ARB)

# Creating the plot
# plot(rabiesdata$NVD, rabiesdata$ARB, pch = 19, col = "lightblue")

# Regression line
# abline(lm(rabiesdata$NVD ~ rabiesdata$ARB), col = "red", lwd = 3)

# x <- lm(rabiesdata$NVD ~ rabiesdata$ARB)
# summary(x)
# 
# x <- glm(rabiesdata$NVD ~ rabiesdata$ARB, family = "poisson", offset())
# summary(x)

myts <- ts(rabiesdata$IDH, start=c(2011))
myts
auto.arima(myts)
Fit<-Arima(myts,order=c(1,2,1),lambda=0.9)
summary(Fit)



fcast <- forecast(Fit, h=10)

#R2
SSE <- sum((resid(Fit[1:12]))^2)
SST <- sum((rabiesdata$IDH[1:12] - mean(rabiesdata$IDH[1:12]))^2)
R_square <- 1 - SSE / SST
R_square

fcast$mean[fcast$mean < 0] <- 0


round(fcast$mean)
xx <- round(fcast$mean)
xx

yy <- round(fcast$lower)
yy

uu <- round(fcast$upper)
uu

z <- autoplot(fcast, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Number of human rabies cases") +ggtitle("Auto-Regressive Integrated Moving Average Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12)) + xlim(2010, 2032) 
z

#ARIMAX
library(forecast)
library(lubridate)
library(tseries)
library(fpp)
library(TSA)

options(scipen = 999)

xreg <- cbind(rabiesdata$NVD_pt, rabiesdata$ARB_pt)
colnames(xreg) <- c("NVD", "ARB")


modArima <- Arima(myts, xreg=xreg, order=c(1,0,1), lambda = 0.9)
modArima

summary(modArima)

coeftest(modArima)

coefci(modArima)



#R2
SSE <- sum((resid(modArima[1:12]))^2)
SST <- sum((rabiesdata$IDH[1:12] - mean(rabiesdata$IDH[1:12]))^2)
R_square <- 1 - SSE / SST
R_square

fcast <- forecast(modArima, xreg=xreg)

fcast$mean[fcast$mean < 0] <- 0

round(fcast$mean)
xx <- round(fcast$mean)
xx
yy <- round(fcast$lower)
yy

uu <- round(fcast$upper)
uu

y <- autoplot(fcast, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Number of human rabies cases") +ggtitle("Auto-Regressive Integrated Moving Average Model with Explanatory Variables")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12)) + xlim(2011, 2032) + ylim(-210,210)
y
summary(Fit)


library(lmtest)
library(vcov)
lmtest::coeftest(modArima)
coefci(modArima)


#ARIMAX
library(forecast)
library(lubridate)
library(tseries)
library(fpp)
library(TSA)

options(scipen = 999)


xreg <- cbind(rabiesdata$NVD_pt*1.5, rabiesdata$ARB_pt)
colnames(xreg) <- c("NVD", "ARB")

modArima <- Arima(myts, xreg=xreg, order=c(1,2,1), lambda = 0.99)
modArima
fcast <- forecast(modArima, xreg=xreg)
summary(modArima)


#R2
SSE <- sum((resid(modArima[1:12]))^2)
SST <- sum((rabiesdata$IDH[1:12] - mean(rabiesdata$IDH[1:12]))^2)
R_square <- 1 - SSE / SST
R_square

fcast$mean[fcast$mean < 0] <- 0

round(fcast$mean)
xx <- round(fcast$mean)
xx

yy <- round(fcast$lower)
yy

uu <- round(fcast$upper)
uu

y <- autoplot(fcast, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Number of human rabies cases") +ggtitle("Auto-Regressive Integrated Moving Average Model with Explanatory Variables (50% increase of MDV)")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12)) + xlim(2011, 2032) + ylim(-210,210)
y
summary(Fit)


library(lmtest)
library(vcov)
lmtest::coeftest(modArima)
coefci(modArima)




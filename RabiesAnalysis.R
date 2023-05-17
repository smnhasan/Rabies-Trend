library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
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
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)


setwd('E:\\Rabies')
rabies <- read.csv("Districtwise_animalbite.csv")
rabies
#Descriptive
describe(rabies$Others.animal.bites)
describe(rabies$Dog.bites)
describe(rabies$Total.bites)
describe(rabies$Male)
describe(rabies$Female)
describe(rabies$Under.15yrs)
describe(rabies$cat2)
describe(rabies$cat3)
describe(rabies$ARV)
describe(rabies$RIG)

rab <- rabies[2:11]

colnames(rab) <- c('Others Animal Bites', 
                    'Dog Bites', 
                    'Total Bites',
                    'Male',
                    'Female',
                    'Under 15 years',
                    'Category 2',
                   'Category 3',
                   'ARV',
                   'RIG')

ggpairs(rab, 
        columnLabels = gsub('.', ' ', colnames(rab), fixed = T), 
        labeller = label_wrap_gen(10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


RC <- read.csv("Rabies case_2019to2022.csv")
RC

tab <- table(RC$Vaccination,RC$Age_cat)
tab
prop.table(tab)*100
test <- fisher.test(tab)
test
chisq.test(RC$Vaccination,RC$Age_cat)

tab  <- table(RC$Sex, RC$Age_cat)
tab
prop.table(tab)*100
test <- fisher.test(tab)
test
chisq.test(RC$Age_cat, RC$Sex)

tab <- table(RC$Residence, RC$Age_cat)
tab
prop.table(tab)*100
test <- fisher.test(tab)
test
chisq.test(RC$Age_cat, RC$Residence)

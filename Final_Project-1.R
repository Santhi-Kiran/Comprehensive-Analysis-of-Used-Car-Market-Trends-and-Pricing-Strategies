
library(tidyverse)
library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(reshape2)
library(DT)
library(RColorBrewer)
library(data.table)
library(knitr)
library(caret)
library(stringr)
library(RANN)
library(data.table)
library(vtable)
library(scales)
library(ggpubr)
library(kableExtra)
library(psych)
library(gtsummary)
library(sjPlot)
library(car)
library(scatterplot3d)



German_Cars <- read_csv("C:/Users/Welcome/Documents/AL6010/Datasets/German_Cars.csv")
view(German_Cars)

# Correlation between hp and price

round(cor(German_Cars$hp,German_Cars$price),2)
cor.test(German_Cars$hp,German_Cars$price,method = "pearson")
hp_wt = subset(German_Cars,select = c("hp","price"))
corrplot::corrplot(cor(hp_wt),method = "number")
plot(German_Cars$hp,German_Cars$price,
     main = "Scatterplot",
     xlab ="Horse power ", 
     ylab ="Price ", 
     pch  = 19)
abline(lm(German_Cars$price~German_Cars$hp), col = "Blue")
text(paste("Correlation:", round(cor(German_Cars$hp,German_Cars$price), 2)), x = 100, y = 1000000)



# Price variation for auto matic cars and manula cars during the period from 2011 to 2021

Auto_Car  = filter(German_Cars,gear == "Automatic")
cor.test(Auto_Car$price,Auto_Car$year,method = "pearson")
plot(Auto_Car$price,Auto_Car$year,
     main = "Scatterplot Automatic cars",
     xlab ="Price ", 
     ylab ="Year ", 
     pch  = 19)
abline(lm(Auto_Car$year~Auto_Car$price), col = "Blue")
text(paste("Correlation:", round(cor(Auto_Car$price,Auto_Car$year), 2)), x = 1000000, y = 2020)

Manual_Car = filter(German_Cars,gear == "Manual")
cor.test(Manual_Car$price,Manual_Car$year,method = "pearson")
plot(Manual_Car$price,Manual_Car$year,
     main = "Scatterplot Manual cars",
     xlab ="Price ", 
     ylab ="Year ", 
     pch  = 19)
abline(lm(Manual_Car$year~Manual_Car$price), col = "Blue")
text(paste("Correlation:", round(cor(Manual_Car$price,Manual_Car$year), 2)), x = 300000, y = 2020)

# Correlation between Year and Mileage


round(cor(German_Cars$year,German_Cars$mileage),2)
cor.test(German_Cars$year,German_Cars$mileage,method = "pearson")
yr_mileage = subset(German_Cars,select = c("year","mileage"))
corrplot::corrplot(cor(yr_mileage),method = "number")

plot(German_Cars$year,German_Cars$mileage,
     main = "Scatterplot",
     xlab ="year ", 
     ylab ="Mileage ", 
     pch  = 19)
abline(lm(German_Cars$mileage~German_Cars$year), col = "Blue")
text(paste("Correlation:", round(cor(German_Cars$year,German_Cars$mileage), 2)), x = 2020, y = 8e+05)


# Correlation chart 
All_num = subset(German_Cars,select = c('mileage','price','year'))
cor(All_num)
corrplot::corrplot(cor(All_num),method = "number")


# Regression
#Price VS Mileage

Reg_1 = lm(price~mileage,data = German_Cars)
summary(Reg_1)
tab_model(Reg_1)
avPlots(Reg_1)


# Price Vs Mileage+hp

Reg_2 = lm(price~mileage+hp,data = German_Cars)
summary(Reg_2)
tab_model(Reg_2)
avPlots(Reg_2)

# Price Vs Mileahe+hp+year

Reg_3 = lm(price~mileage+hp+year,data = German_Cars)
summary(Reg_3)
tab_model(Reg_3)
avPlots(Reg_3)


# Price Vs hp

Reg_4 = lm(price~hp,data = German_Cars)
summary(Reg_4)
tab_model(Reg_4)
avPlots(Reg_4)


# Mileage vs hp

Reg_5 = lm(mileage~hp,data = German_Cars)
summary(Reg_5)
tab_model(Reg_5)
scatterplot(hp~mileage, data = German_Cars,smooth = FALSE,
            grid = FALSE)

# Manual cars price vs hp+mileage+year

Reg_6 = lm(price~hp+mileage+year, data = Manual_Car)
summary(Reg_6)
tab_model(Reg_6)
avPlots(Reg_6)
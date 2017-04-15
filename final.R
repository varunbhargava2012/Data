setwd('C:/Users/Varun/Desktop/Udemy')
library(dplyr)
library(foreign)
library(car)
library(corrplot)
library(visreg)
library(DAAG)
library(fmsb)
library(plyr)
library(MASS)
data = read.arff('dataset_2209_stock.arff')
head(data, 5)
summary(data)
plot(data[, c(1:10)], pch=16, col='brown')
#mod 0
mod0 <- lm(company10~., data=data)
summary(mod0)
VIF(mod0)
#mod 1 with scaling
data1 = data.frame(scale(data))
mod1 <- lm(company10~., data=data1)
summary(mod1)
VIF(mod1)

MSE <- mean((resid(mod1))^2)
coef(mod1)
press(mod1)
confint(mod1)

#mod2 is without scaling but with centering
data2 = data.frame(scale(data, center=TRUE, scale=FALSE))
mod2 <- lm(company10~., data=data2)
summary(mod2)
VIF(mod2)
#they all give same values so going forward with normal data
#deleting company3 as not significant
mod0 <- lm(company10~company1+company2+company4+company5+company6+company7+company8+company9, data=data)
summary(mod0)
VIF(mod0)
#data3=select(data,company1,company2,company4,company5,company6,company7,company8,company9,company10)
#Residual Analysis
MSE <- mean((resid(mod0))^2)
coef(mod0)
press<-press(mod0)
confint(mod0)
VIF(mod0)
#normal plot of residuals
qqnorm(resid(mod0), pch=16)
qqline(resid(mod0))
#constant variance checking
plot(fitted(mod0), resid(mod0), pch=16)
plot(fitted(mod0), rstudent(mod0), pch=16)
Pred_R_Sq <- 1-((press(mod0)/sum(anova(mod0)$'Sum Sq')))

#BOX COX

bc<-boxCox(mod0,
       lambda = seq(-2, 2, 1/10), plotit=TRUE)
lambda = bc$x[which.max(bc$y)]
data_t=data
data_t$company10=data_t$company10^lambda
#fitting a new model
mod_t <- lm(company10~company1+company2+company4+company5+company6+company7+company8+company9, data=data_t)
summary(mod_t)
VIF(mod_t)
MSE_t <- mean((resid(mod_t))^2)
coef(mod_t)
press_t<-press(mod_t)
#new normal plot of residuals
qqnorm(resid(mod_t), pch=16)
qqline(resid(mod_t))
# new constant variance checking
plot(fitted(mod_t), resid(mod_t), pch=16)
plot(fitted(mod_t), rstudent(mod_t), pch=16)
Pred_R_Sq_t <- 1-((press(mod_t)/sum(anova(mod_t)$'Sum Sq')))



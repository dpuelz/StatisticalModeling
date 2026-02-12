# The file saratoga_houses.csv contains data on house sales in Saratoga County.
# Build a model to predict sale prices from the number of bedrooms, bathrooms, and
# an interaction between age and livingArea

# 1. Use autoplot() to construct diagnostic plots of the regression assumptions.
#    Do you see any problems?
# 2. Are there any potentially influential points? Do these datapoints seem to
#    impact the fitted model, either in terms of the estimated coefficients or 
#    predictions?

library(ggplot2)
library(ggfortify)
library(car)

data = read.csv("../data/saratoga_houses.csv")

fit = lm(price~bedrooms + bathrooms + age*livingArea,data)
summary(fit)
autoplot(fit,label.repel = TRUE)

datanew = data[c(-1686,-1195,-1279,-702),]
fitnew = lm(price~bedrooms + bathrooms + age*livingArea,datanew)
summary(fitnew)
autoplot(fitnew,label.repel = TRUE)

plot(fitnew$coefficients,fit$coefficients,pch=19,col='navy')
abline(0,1,col='red')


autoplot(fit,label.repel = TRUE)
vif(fit)

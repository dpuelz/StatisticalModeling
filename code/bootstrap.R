## Bootstrapping example

library(mosaic)

creatinine = read.csv("../data/creatinine.csv", header=TRUE)
head(creatinine)

### Bootstrapping the sample mean

# Look at the mean creatinine clearance rate
mean(creatinine$creatclear)

# OK, 125.25 +/- what?
# Bootstrap to get a standard error
boot1 = do(5000)*{
  mean(resample(creatinine)$creatclear)
}

hist(boot1$result)
sd(boot1$result)

# formula for standard error? se = sqrt(s^2/n)
s = sd(creatinine$creatclear)
n = nrow(creatinine)
se_closedform = sqrt(s^2/n)
se_closedform


# Interpetation: our sample mean is probably off from the true population mean by about 0.95 units

#### Bootstrapping the OLS estimator
plot(creatclear~age, data=creatinine,
     pch=19, col='grey', bty='n',
     ylab="creatinine score", xlab="Age")

lm1 = lm(creatclear~age, data=creatinine)
abline(lm1, lwd=2, col='blue')
coef(lm1)

# Bootstrap
boot1 = do(1000)*lm(creatclear~age, data=resample(creatinine))
head(boot1)

hist(boot1$Intercept)
hist(boot1$age)

# bootstrapped standard errors
sd(boot1$Intercept)
sd(boot1$age)
summary(lm1)

# confidence intervals
confint(boot1)


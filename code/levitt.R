rm(list=ls())
library(mosaic)

##### Working with on original Levitt data
#' The response variable, Y , is per capita crime rates (violent crime, property crime, and murders) by state, from 1985 to 1997 (inclusive). The treatment variable, Z, is the “effective” abortion rate. This metric is an averaged abortion rate, weighted by criminal age at the time of arrest (to account for the fact that crimes committed by criminals should be associated with abortion rates at the time of their births).
##########################################
Original = read.table("../data/levitt_ex.dat",fill=TRUE,header=TRUE)
n=dim(Original)[1]

## Remove DC, Alaska and Hawaii
ind1 = (1:n)[Original$statenum==9]
ind2 = (1:n)[Original$statenum==2]
ind3 = (1:n)[Original$statenum==12]

ind = c(ind1,ind2,ind3)

ind1 = (1:n)[Original$year>97]
ind2 = (1:n)[Original$year<85]

ind = c(ind,ind1,ind2)
ind = unique(ind)

Data = Original[-ind,]
ind = complete.cases(Data)
Data = Data[ind,]

Data[,2] = Data[,2]-84

###

state.f = factor(Data$statenum)
states_dummies = model.matrix(~state.f)

year.f = factor(Data$year)
year_dummies = model.matrix(~year.f)


Y_M = Data$lpc_murd
D_M = Data$efamurd 

Y_P = Data$lpc_prop
D_P = Data$efaprop

Y_V = Data$lpc_viol
D_V = Data$efaviol

Controls = Data[,-c(1:9)]
year = Data[,2]

Interactions = Controls*as.numeric(year)
Interactions2 = Controls*(as.numeric(year)^2)
Interactions3 = states_dummies*as.numeric(year)
Interactions4 = states_dummies*as.numeric(year^2)

XO = as.matrix(cbind(Controls,states_dummies[,2:48],year_dummies[,2:13]))
XO = scale(XO)
X = as.matrix(cbind(Controls,states_dummies[,2:48],year_dummies[,2:13],Interactions,Interactions2,Interactions3[,2:48],Interactions4[,2:48]))
X = scale(X)
X = X[,-71]

# OLS confidence intervals for the causal effect (small model)
confint(lm(Y_V~D_V+XO))[2,]
confint(lm(Y_P~D_P+XO))[2,]
confint(lm(Y_M~D_M+XO))[2,]

# OLS confidence intervals for the causal effect (BIG model)
CE_V_OLS = confint(lm(Y_V~D_V+X))[2,]
CE_P_OLS = confint(lm(Y_P~D_P+X))[2,]
CE_M_OLS = confint(lm(Y_M~D_M+X))[2,]

# OLS c
CE_V_OLSsmallmodel = confint(lm(Y_V~D_V+XO))[2,]

## lasso-based selection (naive - large model)
library(glmnet)
boot = do(100)*{
  rowindices = 1:nrow(X)
  iixx = sample(rowindices,size=length(rowindices),replace=TRUE)
  Xmod = X[iixx,]; Dmod = D_P[iixx]; Ymod = Y_V[iixx]
  
  # fitting the model!
  fit_reg = cv.glmnet(x=cbind(Dmod,Xmod),y=Ymod,family="gaussian",alpha=0,penalty.factor	=c(0,rep(1,ncol(Xmod))),nlambda=20)
  as.numeric(coef(fit_reg,s="lambda.1se")[2]) 
}
causal_effect = unlist(boot)
hist(causal_effect)
CE_V_naive = confint(causal_effect,level = 0.95)


## lasso-based selection (the right way! - large model)
library(glmnet)
boot = do(100)*{
  rowindices = 1:nrow(X)
  iixx = sample(rowindices,size=length(rowindices),replace=TRUE)
  Xmod = X[iixx,]; Dmod = D_P[iixx]; Ymod = Y_V[iixx]
  
  # fitting the treatment model
  fit_D = cv.glmnet(x=Xmod,y=Dmod,family="gaussian",alpha=0,nlambda=20)
  Dhat = predict(fit_D,newx=Xmod,s="lambda.1se")
  
  # fitting the outcome model
  fit_reg = cv.glmnet(x=cbind(Dmod-Dhat,Xmod),y=Ymod,family="gaussian",alpha=0,penalty.factor	=c(0,rep(1,ncol(Xmod))),nlambda=20)
  as.numeric(coef(fit_reg,s="lambda.1se")[2]) 
}

causal_effect = unlist(boot)
hist(causal_effect)
CE_V_best = confint(causal_effect,level = 0.95)

CE_V_OLSsmallmodel
CE_V_best
CE_V_OLS
CE_V_naive

# plot to visualize
yval = c(1,1,2,2,3,3,4,4)
xval = unlist(c(CE_V_OLSsmallmodel,CE_V_OLS,CE_V_naive,CE_V_best))

plot(xval,yval,col='white',bty='n',xlab='causal effect',main='Effect of abortion rate on violent crime')
legend('topleft',legend=c('OLS-small','OLS-large','Reg-naive','Reg-best'),col=c('red','blue','orange','green'),lwd=4)
abline(v=0,lty=2)
lines(xval[1:2],yval[1:2],lwd=10,col='red')
lines(xval[3:4],yval[3:4],lwd=10,col='blue')
lines(xval[5:6],yval[5:6],lwd=10,col='orange')
lines(xval[7:8],yval[7:8],lwd=10,col='green')



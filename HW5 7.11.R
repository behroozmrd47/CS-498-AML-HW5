#Installing necessary packages and setting up working directory
library(glmnet)
rm(list=ls())
setwd("C:/Users/Behrooz/Google Drive/CS/Machine Learning/HW5/RPack")


#Reading in dataset 
AllD <- read.csv('abalone.data',header = F)
#Developing feature dataset
df <- AllD[,1:8] 
df[,1] <- as.numeric(df[,1])
df.mat <- as.matrix(df)
#Developing age dataset
dl <- AllD[,9]+1.5
dl.log <- log(dl)


#7.11.a
lm.a<- lm(formula =  dl ~ df.mat[,2]+df.mat[,3]+df.mat[,4]+
            df.mat[,5]+df.mat[,6]+df.mat[,7]+df.mat[,8])
#R-squared
summary(lm.a)$r.squared
#Standardized Residuals
lm.a.stdres = rstandard(lm.a)
plot(lm.a[["fitted.values"]], lm.a.stdres, ylab="Standardized Residuals", xlab="Fitted Age", ylim=c(-6,+6),
     main="Fitted Age vs  Std. Residuls W/O Sex") 
abline(0, 0) # the horizon
#plot (lm.a)


#7.11.b
lm.b <- lm(formula =  dl ~ df.mat[,1]+df.mat[,2]+df.mat[,3]+
             df.mat[,4]+df.mat[,5]+df.mat[,6]+df.mat[,7]+df.mat[,8])
#R-squared
summary(lm.b)$r.squared
#Standardized Residuals
lm.b.stdres = rstandard(lm.b)
plot(lm.b[["fitted.values"]], lm.b.stdres, ylab="Standardized Residuals", xlab="Fitted Age", ylim=c(-6,+6),
     main="Fitted Age vs  Std. Residuls With Sex") 
abline(0, 0) # the horizon
#plot (lm.b)


#7.11.c
lm.c<- lm(formula =  dl.log ~ df.mat[,2]+df.mat[,3]+
          df.mat[,4]+df.mat[,5]+df.mat[,6]+df.mat[,7]+df.mat[,8])
#R-squared
summary(lm.c)$r.squared
#Standardized Residuals
lm.c.stdres = rstandard(lm.c) 
plot(lm.c[["fitted.values"]], lm.c.stdres, ylab="Standardized Residuals", xlab="Fitted Log of Age", ylim=c(-6,+6),
     main="Fitted Log of Age vs  Std. Residuls W/O Sex") 
abline(0, 0) # the horizon
#plot (lm.c)
#Calculating the residuals in original coordinate system
pred.c <- exp(lm.c[["fitted.values"]])
resid.c <- dl-pred.c
plot(x = pred.c, y = resid.c, xlab="Fitted Values", 
     ylab="Residuals (Original Coordinate)", xlim=c(-5,25),pch=19)
abline(a=0,b=0)


#7.11.d
lm.d <- lm(formula =  dl.log ~ df.mat[,1]+df.mat[,2]+df.mat[,3]+
             df.mat[,4]+df.mat[,5]+df.mat[,6]+df.mat[,7]+df.mat[,8])
#R-squared
summary(lm.d)$r.squared
#Standardized Residuals
lm.d.stdres = rstandard(lm.d) 
plot(lm.d[["fitted.values"]], lm.d.stdres, ylab="Standardized Residuals", xlab="Fitted Log of Age", ylim=c(-6,+6),
     main="Fitted Log of Age vs Std. Residuls With Sex") 
abline(0, 0) # the horizon
#plot (lm.d)
#Calculating the residuals in original coordinate system
pred.d <- exp(lm.d[["fitted.values"]])
resid.d <- dl-pred.d
plot(x = pred.d, y = resid.d, xlab="Fitted Values", 
     ylab="Residuals (Original Coordinate)", pch=19)
abline(a=0,b=0)


#7.11.f
cv.lambda.9.a = cv.glmnet(df.mat[,2:8],dl,alpha = 0)
plot(cv.lambda.9.a)
cv.lambda.9.a$lambda.min

cv.lambda.9.b = cv.glmnet(df.mat,dl,alpha = 0)
plot(cv.lambda.9.b)
cv.lambda.9.b$lambda.min
# R-squared for regularized 7.11.b
pred <- predict(cv.lambda.9.b, newx = df.mat, s = "lambda.min")
sst <- sum((dl - mean(dl))^2)
sse <- sum((pred - dl)^2)
rsq <- 1 - sse / sst
rsq

cv.lambda.9.c = cv.glmnet(df.mat[,2:8],dl.log,alpha = 0)
plot(cv.lambda.9.c)
cv.lambda.9.c$lambda.min

cv.lambda.9.d = cv.glmnet(df.mat,dl.log,alpha = 0)
plot(cv.lambda.9.d)
cv.lambda.9.d$lambda.min
# R-squared for regularized 7.11.d 
pred <- exp(predict(cv.lambda.9.d, newx = df.mat, s = "lambda.min"))
sst <- sum((dl - mean(dl))^2)
sse <- sum((pred - dl)^2)
rsq <- 1 - sse / sst
rsq

#End

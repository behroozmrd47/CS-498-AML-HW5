#Installing necessary packages and setting up working directory
library(glmnet)
rm(list=ls())
setwd("C:/Users/Bajes01842/Google Drive/CS/Machine Learning/HW5/RPack")


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
lm.a<- lm(formula =  dl ~ df.mat[,2]+df.mat[,3]+df.mat[,4]+df.mat[,5]+df.mat[,6]+df.mat[,7]+df.mat[,8])
summary(lm.a)$r.squared
plot (lm.a)


#7.11.b
lm.b <- lm(formula =  dl ~ df.mat[,1]+df.mat[,2]+df.mat[,3]+df.mat[,4]+df.mat[,5]+df.mat[,6]+df.mat[,7]+df.mat[,8])
summary(lm.b)$r.squared
plot (lm.b)


#7.11.c
lm.c<- lm(formula =  dl.log ~ df.mat[,2]+df.mat[,3]+df.mat[,4]+df.mat[,5]+df.mat[,6]+df.mat[,7]+df.mat[,8])
summary(lm.c)$r.squared
plot (lm.c)
pred.c <- exp(predict(lm.c, newx = df.mat[,2:8]))
resid.c <- dl-pred.c
plot(x = pred.c, y = resid.c, xlab="Fitted Values", 
     ylab="Residuals (Original Coordinate)", pch=19)
abline(a=0,b=0)


#7.11.d
lm.d <- lm(formula =  dl.log ~ df.mat[,1]+df.mat[,2]+df.mat[,3]+df.mat[,4]+df.mat[,5]+df.mat[,6]+df.mat[,7]+df.mat[,8])
summary(lm.d)$r.squared
plot (lm.d)
pred.d <- exp(predict(lm.d, newx = df.mat))
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
# R squared
pred <- exp(predict(cv.lambda.9.d, newx = df.mat, s = "lambda.min"))
sst <- sum((dl - mean(dl))^2)
sse <- sum((pred - dl)^2)
rsq <- 1 - sse / sst
rsq

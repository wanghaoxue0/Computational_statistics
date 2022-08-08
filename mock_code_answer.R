# solution for mock exam in 2022 computational statistics
# by Haoxue Wang@copyright
# question 8 and 9
set.seed(33)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
y <- 0.3*x1 + 0.5*x2 + 0.6*x3 + rnorm(100, 0, 1)
DataFrame <- data.frame(y, x1, x2, x3)
fit<-lm(y~.,data = DataFrame)
out1 <-predict(fit,data.frame(x1=1,x2=1,x3=1),level=0.95,interval = "c")
out1[3]-out1[2]
out2 <- predict(fit,data.frame(x1=1,x2=1,x3=1),level=0.95,interval = "p")
out2[3]-out2[2]

# question 12 and 13
set.seed(11)
predict=rep(NA,100)
for (i in 1:100){
  n =50
  x <- rnorm(n, 0, 1)
  y <- -2 + 1.5*x + rnorm(n, 0, 1)
  fit <- lm(y~poly(x,2))
  predict[i] <- predict(fit,data.frame(x=2))
}
mean <- mean(predict)
true <- -2 + 1.5*2
bias = 0.88675-true

# question 20 and 23
load("cars.dat")
repair <- cars_df$repair
cost <- cars_df$cost
fit <- lm(repair~I(cost)+I(cost^2))
summary(fit)

set.seed(22)
generate.bootstrap.data <- function(data){
  x <- cars_df$cost
  y <-  2.1 + 2.2*x-0.2*x^2+rnorm(300,mean=0,sd=sqrt(0.6))
  return(data.frame(x=x, y=y))
}

B = 3000
thetastar = rep(NA, B)
for(i in 1:B){
  dset = generate.bootstrap.data(df)
  fit = lm(y~I(x)+I(x^2), data=dset)
  thetastar[i] = fit$coefficients[3]
}
sd(thetastar)

# second method using residual
set.seed(22)
generate.bootstrap.data <- function(data){
  x <- cars_df$cost
  residual <- as.numeric(cars_df$repair-I(2.1+2.2*x-0.2*x^2))
  residuals <- sample(residual,length(x),replace = TRUE)
  y <-  2.1 + 2.2*x-0.2*x^2+residuals
  return(data.frame(x=x, y=y))
}

B = 3000
thetastar = rep(NA, B)
for(i in 1:B){
  dset = generate.bootstrap.data(df)
  fit = lm(y~I(x)+I(x^2), data=dset)
  thetastar[i] = fit$coefficients[3]
}
sd(thetastar)

# question 27-30
load("runners.dat")
summary(runners_df)
runners_df$club<-factor(runners_df$club)
fit <- lm(distance~.,data = runners_df)
predict(fit,data.frame(club=factor(3),height=180,weight=80))-predict(fit,data.frame(club=factor(1),height=180,weight=80))
summary(fit)


fit <- lm(distance~height+weight,data = runners_df)
residual <- fit$residuals
sum(residual^2)

# question 29
set.seed(22)
B = 5000
  perm_test_statistic = rep(NA, B)
for(i in 1:B){
  new_distance = sample(runners_df$distance,replace = FALSE)
  fit <- lm(new_distance~runners_df$height+runners_df$weight)
  residual <- fit$residuals
  perm_test_statistic[i] = sum(residual^2)
}

standard_deviation = sd(perm_test_statistic)

pval <- mean(simulated_RSS>=1975.0)

# question 33-38
load('square.dat')
pval<- mean(new_test_statistic>=5)

# question 40-43
load('position.dat')
  n = 1000
  nfolds = 10
  fold_indicator = cut(1:n,breaks = nfolds,labels = FALSE) # create labels denoting which point is in which fold
  sse = 0 # here we will store the result for the sum of squared errors
for(i in 1:nfolds){
  train = position_df[fold_indicator!=i,]
  test = position_df[fold_indicator==i,]
    fit <- lm(position~poly(time,2),data = train)
    predicted_positions <- predict(fit,data.frame(time=test$time))
    sse = sse + sum((test$position-predicted_positions)^2)
}
mse = sse/1000
mse


sse<-0
mse<-rep(NA,10)
for (j in 1:10) {
  for(i in 1:nfolds){
    sse<-0
    train = position_df[fold_indicator!=i,]
    test = position_df[fold_indicator==i,]
    fit <- lm(position~poly(time,degree=j),data = train)
    predicted_positions <- predict(fit,data.frame(time=test$time))
    sse = sse + sum((test$position-predicted_positions)^2)
  }
  mse[j] = sse/100
}
which.min(mse)


#question 51-53
library(glmnet)
library(ISLR)
data(Hitters)
Hitters=na.omit(Hitters)
x = model.matrix(Salary~., data=Hitters)
x = x[,-1]
y = Hitters$Salary
# define grid of lambda
grid.lambda=10^seq(from = 10, to = -2, length=100)
# define train and test
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2, replace=FALSE)
test=(-train)
# training data
x.train <- x[train,]
y.train <- y[train]
# test data
x.test <- x[test,]
y.test <- y[test]
fit<-glmnet(x.train,y.train,alpha = 1,lambda = 100)
coef(fit)

set.seed(2020)
# using cross validation
cvfit<-cv.glmnet(x.train,y.train,alpha = 1,nfolds=10,lambda = grid.lambda)
best_lambda <- cvfit$lambda[which.min(cvfit$cvm)]
best_fit <- glmnet(x.train,y.train,lambda = best_lambda,alpha = 1)
pred <- predict(best_fit,x.test)
mse <- mean((y.test-pred)^2)

# for ridge regression
set.seed(2020)
# using cross validation
cvfit<-cv.glmnet(x.train,y.train,alpha = 0,nfolds=10,lambda = grid.lambda)
best_lambda <- cvfit$lambda[which.min(cvfit$cvm)]
best_fit <- glmnet(x.train,y.train,lambda = best_lambda,alpha = 0)
pred <- predict(best_fit,x.test)
mse <- mean((y.test-pred)^2)


# question 60-62

library(randomForest)
library(MASS)
data(Boston)
# splitting the data into training and testing data
set.seed(1)
subset<-sample(1:nrow(Boston), size = 0.5*nrow(Boston), replace = FALSE)
Boston.train<-Boston[subset,-14]
Boston.test<-Boston[-subset,-14]
y.train<-Boston[subset,14]
y.test<-Boston[-subset,14]

fit <- randomForest(Boston.train,y.train,ntree = 500, mtry = 13)
mean((y.train-fit$predicted)^2)

pred<-predict(fit,data.frame(Boston.test))
mean((y.test-pred)^2)

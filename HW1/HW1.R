# Set working directory
setwd("~/polisci490/HW1")

# Load packages
packages <- c("dplyr","ggplot2","broom", "glmnet", "e1071")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)


#1) ################  Tidyverse! ################

# Compare glm and glmnet
glm(mpg ~ ., data = mtcars) %>% coef()
glmnet::glmnet(x = as.matrix(mtcars[, -1]), y = mtcars$mpg,
               alpha = 1, lambda = 1) %>% coef()

# Compare glm and glmnet with tidy
glm(mpg ~ ., data = mtcars) %>% broom::tidy()
glmnet::glmnet(x = as.matrix(mtcars[, -1]), y = mtcars$mpg,
               alpha = 1, lambda = 1) %>% broom::tidy()

#2) ################  Regression ################


# Load data
sick_data <- read.csv("sick_data.csv")

### OLS ###


#a) Estimate y ~ x1 + x2 + u
ols.reg <- lm(as.numeric(result)-1 ~ temp + bp, data = sick_data)
ols.reg %>% tidy() %>% knitr::kable()

#b) Get predicted values. y_hat > 0.5 = positive for disease. y_hat < 0.5 = negative for disease.
y_hat <- fitted(ols.reg)

ols.reg.predict <- sick_data$result
ols.reg.predict[y_hat >= 0.5]= "Positive"
ols.reg.predict[y_hat < 0.5]= "Negative"

# Confusion matrix
table(ols.reg.predict, sick_data$result)
mean(ols.reg.predict == sick_data$result)

#c) Generate the equation of the line where y_hat = 0.5 as a function of blood pressure and temperature
# Equation is given by 0.5 = B0 + B1X1 + B2X2
intercept <- (0.5 - coef(ols.reg)[1]) / coef(ols.reg)[3]
slope <- -1 * coef(ols.reg)[2] / coef(ols.reg)[3]
line_0.5 <- function(x) {
  intercept + slope*x
}

#d) Display the blood pressure and temperature data on a single scatterplot, using either color or shape to distinguish between positive and negative results. Add the line you calculated from the previous step.

p <- ggplot(sick_data, aes(temp, bp))
p + geom_point(aes(color = factor(result))) + stat_function(fun = line_0.5) + labs(title = "Ordinary Least Squares")

### Logit ###

#a) Estimate y ~ x1 + x2 + u
logit.reg <- glm(result ~ temp + bp,data= sick_data ,family = "binomial")
logit.reg %>% tidy() %>% knitr::kable()

#b) Get predicted values. y_hat > 0.5 = positive for disease. y_hat < 0.5 = negative for disease.
y_hat <- fitted(logit.reg)

logit.reg.predict <- sick_data$result
logit.reg.predict[y_hat >= 0.5]= "Positive"
logit.reg.predict[y_hat < 0.5]= "Negative"

# Confusion matrix
table(logit.reg.predict, sick_data$result)
mean(logit.reg.predict == sick_data$result)

#c) Generate the equation of the line where y_hat = 0.5 as a function of blood pressure and temperature
# Equation is given by 0.5 = B0 + B1X1 + B2X2
intercept <- (0.5 - coef(logit.reg)[1]) / coef(logit.reg)[3]
slope <- -1 * coef(logit.reg)[2] / coef(logit.reg)[3]
line_0.5 <- function(x) {
  intercept + slope*x
}

#d) Display the blood pressure and temperature data on a single scatterplot, using either color or shape to distinguish between positive and negative results. Add the line you calculated from the previous step.

p <- ggplot(sick_data, aes(temp, bp))
p + geom_point(aes(color = factor(result))) + stat_function(fun = line_0.5)

### Ridge ###
ridge.reg <- glmnet(x = as.matrix(sick_data[, -1]), y = sick_data$result, alpha = 0, lambda = 1, family = "binomial")
ridge.reg %>% tidy() %>% knitr::kable()

#b) Get predicted values. y_hat > 0.5 = positive for disease. y_hat < 0.5 = negative for disease.
y_hat <- predict(ridge.reg, newx = as.matrix(sick_data[, -1]))

ridge.reg.predict <- sick_data$result
ridge.reg.predict[y_hat >= 0.5]= "Positive"
ridge.reg.predict[y_hat < 0.5]= "Negative"

# Confusion matrix
table(ridge.reg.predict, sick_data$result)
mean(ridge.reg.predict == sick_data$result)

#c) Generate the equation of the line where y_hat = 0.5 as a function of blood pressure and temperature
# Equation is given by 0.5 = B0 + B1X1 + B2X2
intercept <- (0.5 - coef(ridge.reg)[1]) / coef(ridge.reg)[3]
slope <- -1 * coef(ridge.reg)[2] / coef(ridge.reg)[3]
line_0.5 <- function(x) {
  intercept + slope*x
}

#d) Display the blood pressure and temperature data on a single scatterplot, using either color or shape to distinguish between positive and negative results. Add the line you calculated from the previous step.

p <- ggplot(sick_data, aes(temp, bp))
p + geom_point(aes(color = factor(result))) + stat_function(fun = line_0.5)


#3) ################  Regularization/Selection ################

#a) Load the data, and plot the dependent variable y.
widget_data <- read.csv("widget_data.csv")
plot(widget_data$y)

### Ridge ###

#b) Use glmnet() from the glmnet package to estimate a ridge regression with a sequence of labda from 0.01 to 100
lambdas <- 10^seq(2, -2, length =100)
ridge.reg.widget <- glmnet(x = as.matrix(widget_data[, -1]), y = widget_data$y, alpha = 0, lambda = lambdas)



#c) Use tidy from the broom package to extract the data from the regression into a useable format and use ggplot2 to plot the coefficient estimates as lambda changes.

# Drop intercepts
ridge.plot <- filter(ridge.reg.widget %>% tidy() , term != "(Intercept)")

p <- ggplot(ridge.plot, aes(lambda, estimate, color=term))
p + geom_line()


#d) Use cross validation with cv.glmnet to pick the value of lambda that will minimize mean squared error, and give the coefficients you get when using that lambda 
ridge.five.fold <- cv.glmnet(x = as.matrix(widget_data[,-1]), y = widget_data$y, alpha = 0, lambda = lambdas, nfolds = 5)

optimal.lambda <- ridge.five.fold$lambda.min
optimal.lambda
ridge.five.fold %>% coef()

### Lasso ###

#b) Use glmnet() from the glmnet package to estimate a lasso regression with a sequence of labda from 0.01 to 100
lambdas <- 10^seq(2, -2, length =100)
lasso.reg.widget <- glmnet(x = as.matrix(widget_data[, -1]), y = widget_data$y, alpha = 1, lambda = lambdas)



#c) Use tidy from the broom package to extract the data from the regression into a useable format and use ggplot2 to plot the coefficient estimates as lambda changes.

# Drop intercepts
lasso.plot <- filter(lasso.reg.widget %>% tidy() , term != "(Intercept)")

p <- ggplot(lasso.plot, aes(lambda, estimate, color=term))
p + geom_line()

#d) Use cross validation with cv.glmnet to pick the value of lambda that will minimize mean squared error, and give the coefficients you get when using that lambda 
lasso.five.fold <- cv.glmnet(x = as.matrix(widget_data[,-1]), y = widget_data$y, alpha = 1, lambda = lambdas, nfolds = 5)

optimal.lambda <- lasso.five.fold$lambda.min
lasso.five.fold %>% coef()

#e) Ridge vs Lasso. Discuss the difference.

#4) ################  Classification ################

# Load the data
pol_data <- read.csv("pol_data.csv")

# Set seed

set.seed(123)

#a) Split the data into 2/3 training and 1/3 test data.
smp <- sample(300, 200)

train <- pol_data[smp, ]
test <- pol_data[-smp, ]

### Naive Bayes ###

#b) Estimate each model using the training data only.
NB <- naiveBayes(group ~ ., data = train)
summary(NB)

#c) Use the model to predict the outcome in the test data.
ypredict <- predict(NB, test)
ypredict

#d) Create a table of the predicted classes against the real classes.
# Confusion matrix
table(ypredict, test$group)
mean(ypredict == test$group)

### SVM ###

#b) Estimate each model using the training data only.

tune.out <- tune(svm, group ~ ., data = train, kernel="linear", ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

tune.out$best.model


#c) Use the model to predict the outcome in the test data.
ypredict <- predict(tune.out$best.model, test)
ypredict

#d) Create a table of the predicted classes against the real classes.
# Confusion matrix
table(ypredict, test$group)
mean(ypredict == test$group)



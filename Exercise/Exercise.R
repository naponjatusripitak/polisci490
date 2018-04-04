# Set working directory
setwd("~/polisci490/Exercise")

# Load packages
packages <- c("xml2","rvest", "dplyr", "tm", "tidytext", "ggplot2", "SnowballC", "stats", "MASS", "nnet")

load.packages <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}

lapply(packages, load.packages)


USArrests <- USArrests


log.iris <- log(iris[,1:4])
iris.species <- iris[,5]

USArrest.PCA <- prcomp(USArrests, center=TRUE, scale = TRUE)
print(USArrest.PCA)

plot(USArrest.PCA, type="l")

library(devtools)
install_github("ggbiplot", "vqv")


library(ggbiplot)

states <- rownames(USArrest.PCA)

p <- ggbiplot(USArrest.PCA, obs.scale=1, var.scale=1, groups=states, ellipse=TRUE, circle=TRUE, labels=rownames(USArrests))
p <- p + scale_color_discrete(name='') + theme_bw()


#####

Boston <- Boston

sample <- .5*nrow(Boston)

set.seed(789)

data.split <- sample(seq_len(nrow(Boston)), size=sample)

training <- Boston[data.split,]
test <- Boston[-data.split,]

reg1 <- lm(medv ~., data=training)

predict.reg1 <- predict(reg1, newdata=test[, -14])

mean((test$medv - predict.reg1)^2)
##
y <- nnet::class.ind(Boston$medv)

x <- Boston
x$medv <- NULL
colmins <- apply(Boston, 2, min)
colmaxs <- apply(Boston, 2, max)


BostonScale <- as.data.frame(cbind(y, scale(x, center = colmins,scale = colmaxs - colmins)))

wineScale[1:3, 1:6]

wineScale <- as.data.frame(cbind(y, scale(x, center = colmins,scale = colmaxs - colmins)))wineScale[1:3, 1:6]






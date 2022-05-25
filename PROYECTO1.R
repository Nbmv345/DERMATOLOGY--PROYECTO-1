derma <- read.csv("dermatology.csv", header=T, sep='\t')

derma$Age <- as.integer(derma$Age)
derma$Disease <- as.factor(derma$Disease)

attach(derma)
plot(derma$Disease, derma$Age, col = rgb(0.2,0.4,0.6,0.4),
xlab="Enfermedad", ylab="Edad", main = "Enfermedad y Edad")

derma$Disease1 <- ifelse(Disease==1,1,0)

detach(derma)
attach(derma)

costo1 <- function(X, Disease1, theta) {
sum(X%*%theta - Disease1)^2/(2*length(Age))}

theta <- matrix(as.double((c(0,0)), nrow=2))
iteraciones <- 200
alpha <- .01
costo_historial <- double(iteraciones) 
theta_historial <- list(iteraciones) 
X <- cbind(1,matrix(Age))

for (i in 1:iteraciones) {
error <- (X %*% theta - Disease1)
delta <- t(X) %*% error/length(Disease1)
theta <- theta - alpha*delta
costo_historial[i] <- costo1(X, Disease1, theta)
theta_historial[[i]] <- theta }

print(theta)


#[,1]
#[1,] -3.519192e+290
#[2,] -1.294364e+292

Disease1 = -3.519192e+290 - 1.294364e+292*Age


set.seed(123)
dermaprueba <- sample(2, nrow(derma), prob = c(0.7,0.3),
replace = T)
dermaTrain <- derma [dermaprueba == 1,]
dermaTest <- derma [dermaprueba == 2,]

library(randomForest)
dermaForest <- randomForest(Disease~ ., data = dermaTrain, na.action=na.roughfix)
print(dermaForest)


library(ggplot2)
library(factoextra)

clusters <- kmeans(derma[, 2:5], centers = 4)
print(clusters)

#--------------------------------------
# HOMEWORK 1 - STATISTICAL MODELS 2026
#--------------------------------------

library(ISLR2)
dataset <- ISLR2::Auto

model1 <- lm(mpg ~ horsepower, data=Auto)
model1

#1
plot(dataset$horsepower,dataset$mpg,xlab="horsepower",ylab="mpg",main="Auto dataset", color="darkgrey", pch=20, cex=0.5)
abline(model1, color=3,lwd=2, col=2)

model1
summary(model1)


#2
y_hat <- predict(model1, newdata = data.frame(horsepower = Auto$horsepower))
ssr <- sum((y_hat-mean(Auto$mpg))**2)
sst <- sum((Auto$mpg-mean(Auto$mpg))**2)
R_sq <- ssr/sst
if (isTRUE(all.equal(R_sq,summary(model1)$r.squared))) {
  print(paste("OK:", R_sq,summary(model1)$r.squared))
}

#3 and 4
x_grid <- seq(min(Auto$horsepower), max(Auto$horsepower), length.out=100)
plot(dataset$horsepower,dataset$mpg,xlab="horsepower",ylab="mpg",main="Auto dataset", color="darkgrey", pch=20, cex=0.5)
for (rank in 1:4) {
    model2 <- lm(mpg ~ poly(horsepower, rank), data=Auto)
    print(paste("Rank:", rank, "R^2:", summary(model2)$r.squared))
    predictions2 <- predict(model2, newdata=data.frame(horsepower=x_grid))
    lines(x_grid, predictions2, col=rank+1, lwd=2)
    
}
legend("topright", 
       legend = paste("Degree", 1:4),  
       col = 2:5,                     
       lty = 1,                       
       lwd = 2)

#5
# Degree 1
fit1 <- lm(mpg ~ horsepower, data = dataset)
plot(fit1, which = 1)

# Degree 2
fit2 <- lm(mpg ~ poly(horsepower, 2), data = dataset)
plot(fit2, which = 1)

# Degree 3
fit3 <- lm(mpg ~ poly(horsepower, 3), data = dataset)

# Degree 4
fit4 <- lm(mpg ~ poly(horsepower, 4), data = dataset)

#Test
anova(fit1, fit2, fit3, fit4)
 
par(mfrow = c(1, 2), oma = c(0, 0, 1, 0))
plot(fit1, which = 1, main="Degree 1", sub.caption = "") 
plot(fit2, which = 1, main="Degree 2", sub.caption = "")
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

title("Model Diagnostic Plots", outer = TRUE, line = -1.5, cex.main = 1.5)





crime.data <- read.table("crime.txt", header = TRUE)

# problem 1

png("pairs.png")
pairs(crime.data)
dev.off()

# problem 2

#install.packages("corrplot")

library(corrplot)
cor(crime.data)

png("corr.png")
corrplot(cor(crime.data))
dev.off()

# problem 4

model <- lm(Rate ~ ., data = crime.data)
summary(model)
co <- coef(model)
eq <- paste0("y = ", round(co[1], 2), " + ",
	round(co[2], 2), "Age + ",
	round(co[3], 2), "S + ",
	round(co[4], 2), "Ed + ",
	round(co[5], 2), "Ex0 + ",
	round(co[6], 2), "Ex1 + ",
	round(co[7], 3), "LF + ",
	round(co[8], 4), "M + ",
	round(co[9], 3), "N + ",
	round(co[10], 4), "NW + ",
	round(co[11], 2), "U1 + ",
	round(co[12], 2), "U2 + ",
	round(co[13], 2), "W + ",
	round(co[14], 2), "X")
eq

new.data <- data.frame(Age = 150, S = 1, Ed = 90, Ex0 = 50, Ex1 = 60, LF = 500, M = 950, N = 30, NW = 300, U1 = 100, U2 = 40, W = 400, X = 200)
predicted.value <- predict(model, newdata = new.data)
predicted.value

# problems 5-7

model2 <- lm(Rate ~ Ex1 + X + Ed + Age + U2, data = crime.data)
summary(model2)
co <- coef(model2)
eq <- paste0("y = ", round(co[1], 2), " + ",
	round(co[2], 2), "Ex1 + ",
	round(co[3], 2), "X + ",
	round(co[4], 2), "Ed + ",
	round(co[5], 2), "Age + ",
	round(co[6], 2), "U2")
eq

new.data <- data.frame(Age = 150, S = 1, Ed = 90, Ex0 = 50, Ex1 = 60, LF = 500, M = 950, N = 30, NW = 300, U1 = 100, U2 = 40, W = 400, X = 200)
predicted.value <- predict(model2, newdata = new.data)
predicted.value

model3 <- lm(Rate ~ Ex0 + LF + M + N + NW, data = crime.data)
summary(model3)
co <- coef(model3)
eq <- paste0("y = ", round(co[1], 2), " + ",
	round(co[2], 2), "Ex0 + ",
	round(co[3], 3), "LF + ",
	round(co[4], 4), "M + ",
	round(co[5], 3), "N + ",
	round(co[6], 4), "NW")
eq

new.data <- data.frame(Age = 150, S = 1, Ed = 90, Ex0 = 50, Ex1 = 60, LF = 500, M = 950, N = 30, NW = 300, U1 = 100, U2 = 40, W = 400, X = 200)
predicted.value <- predict(model3, newdata = new.data)
predicted.value

# problem 8

png("qqplot.png")
qqnorm(residuals(model3))
qqline(residuals(model3), col = "red")
dev.off()

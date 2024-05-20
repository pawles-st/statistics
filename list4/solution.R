crime.data <- read.table("crime.txt", header = TRUE)

# problem 1

plot(crime.data$Rate, crime.data$S)
pairs(crime.data)

# problem 2

library(corrplot)
cor(crime.data)
corrplot(cor(crime.data))

# problem 3

model <- lm(crime.data$Rate ~ ., data = crime.data)
summary(model)

help(lm)

var.data <- read.table("lab2.csv", header = TRUE, sep = ",")

# problem 1

jpeg("hist1.jpeg")
no.classes <- ceiling((max(var.data$X1) - min(var.data$X1)) / 2 / IQR(var.data$X1) * length(var.data$X1)^(1/3))
hist(var.data$X1, main = "histogram dla X1", xlab = "wartość zmiennej", ylab = "gęstość", breaks = no.classes, prob = TRUE)
pts <- seq(min(var.data$X1), max(var.data$X1), length = 50)
#distrib1 <- dnorm(pts, mean = 0, sd = 1)
distrib2 <- dnorm(pts, mean = 0.2, sd = 0.9)
#lines(pts, distrib1, col = 2, lwd = 3)
lines(pts, distrib2, col = 3, lwd = 3)
lines(density(var.data$X1))
dev.off()

jpeg("hist2.jpeg")
#no.classes <- ceiling((max(var.data$X2) - min(var.data$X2)) / 2 / IQR(var.data$X2) * length(var.data$X2)^(1/3))
hist(var.data$X2, main = "histogram dla X2", xlab = "wartość zmiennej", ylab = "gęstość", breaks = 25, prob = TRUE)
pts <- seq(min(var.data$X2), max(var.data$X2), length = 50)
distrib1 <- dgamma(pts, shape = 1, scale = 0.5) # exponential
lines(pts, distrib1, col = 2, lwd = 3)
#distrib2 <- dgamma(pts, shape = 1.5, scale = 0.4)
#lines(pts, distrib2, col = 3, lwd = 3)
lines(density(var.data$X2))
dev.off()

jpeg("hist3.jpeg")
no.classes <- ceiling((max(var.data$X3) - min(var.data$X3)) / 2 / IQR(var.data$X3) * length(var.data$X3)^(1/3))
hist(var.data$X3, main = "histogram dla X3", xlab = "wartość zmiennej", ylab = "gęstość", breaks = no.classes, prob = TRUE)
pts <- seq(min(var.data$X3), max(var.data$X3), length = 50)
distrib <- dgamma(pts, shape = 3.5, scale = 0.6)
lines(pts, distrib, col = 2, lwd = 3)
lines(density(var.data$X3))
dev.off()

jpeg("hist4.jpeg")
no.classes <- ceiling((max(var.data$X4) - min(var.data$X4)) / 2 / IQR(var.data$X4) * length(var.data$X4)^(1/3))
hist(var.data$X4, main = "histogram dla X4", xlab = "wartość zmiennej", ylab = "gęstość", breaks = no.classes, prob = TRUE)
pts <- seq(min(var.data$X4), max(var.data$X4), length = 50)
distrib <- dnorm(pts, mean = 0.41, sd = 0.15)
lines(pts, distrib, col = 2, lwd = 3)
lines(density(var.data$X4))
dev.off()

jpeg("hist5.jpeg")
no.classes <- ceiling((max(var.data$X5) - min(var.data$X5)) / 2 / IQR(var.data$X5) * length(var.data$X5)^(1/3))
hist(var.data$X5, main = "histogram dla X5", xlab = "wartość zmiennej", ylab = "gęstość", breaks = no.classes, prob = TRUE)
pts <- seq(min(var.data$X5), max(var.data$X5), length = 50)
distrib <- dunif(pts, min = 3.0, max = 3.5)
lines(pts, distrib, col = 2, lwd = 3)
lines(density(var.data$X5))
dev.off()

# problem 2

sample.size = 1000
normal.sample = rnorm(sample.size, mean = 1, sd = 2)
exp.sample = rexp(sample.size, rate = 2)
beta.sample = rbeta(sample.size, shape1 = 1, shape2 = 1)

q.normal.sample = sort(normal.sample)
q.exp.sample = sort(exp.sample)
q.beta.sample = sort(beta.sample)

quantiles = seq(0, sample.size - 1, by = 1) / (sample.size - 1)
q.normal = qnorm(quantiles, mean = 1, sd = 2)
q.exp = qexp(quantiles, rate = 2)
q.beta = qbeta(quantiles, shape1 = 1, shape2 = 1)

jpeg("qq-normal-normal.jpeg")
qqplot(q.normal, normal.sample, main = "QQPlot: teoretyczny rozkład normalny/\npróbka z rozkładu normalnego", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

jpeg("qq-exp-normal.jpeg")
qqplot(q.exp, normal.sample, main = "QQPlot: teoretyczny rozkład wykładniczy/\npróbka z rozkładu normalnego", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

jpeg("qq-beta-normal.jpeg")
qqplot(q.beta, normal.sample, main = "QQPlot: teoretyczny rozkład beta/\npróbka z rozkładu normalnego", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

jpeg("qq-normal-exp.jpeg")
qqplot(q.normal, exp.sample, main = "QQPlot: teoretyczny rozkład normalny/\npróbka z rozkładu wykładniczego", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

jpeg("qq-exp-exp.jpeg")
qqplot(q.exp, exp.sample, main = "QQPlot: teoretyczny rozkład wykładniczego/\npróbka z rozkładu wykładniczego", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

jpeg("qq-beta-exp.jpeg")
qqplot(q.beta, exp.sample, main = "QQPlot: teoretyczny rozkład beta/\npróbka z rozkładu wykładniczego", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

jpeg("qq-normal-beta.jpeg")
qqplot(q.normal, beta.sample, main = "QQPlot: teoretyczny rozkład normalny/\npróbka z rozkładu beta", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

jpeg("qq-exp-beta.jpeg")
qqplot(q.exp, beta.sample, main = "QQPlot: teoretyczny rozkład wykładniczy/\npróbka z rozkładu beta", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

jpeg("qq-beta-beta.jpeg")
qqplot(q.beta, beta.sample, main = "QQPlot: teoretyczny rozkład beta/\npróbka z rozkładu beta", xlab = "Kwantyle teoretyczne", ylab = "Kwantyle próbkowe")
abline(0, 1)
dev.off()

# problem 3

sample.size <- 200
sample.1 <- rnorm(sample.size, mean = 0, sd = 1)
sample.2 <- rnorm(sample.size, mean = 0, sd = 2)
sample.3 <- rnorm(sample.size, mean = 1, sd = 1)
sample.4 <- rnorm(sample.size, mean = 1, sd = 3)

samples <- data.frame(sample.1, sample.2, sample.3, sample.4)

jpeg("norm-box.jpeg")
boxplot(samples, main = "Boxplot dla próbek z rozkładów normalnych")
dev.off()

summary(sample.1)

help(grid)

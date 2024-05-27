# problem 1

sample.cdf <- function(s, x) {
	return(sum(s <= x))
}

sample.size <- 100

png("norm_cdf.png")
norm.sample <- rnorm(sample.size)
x.values <- seq(min(norm.sample), max(norm.sample), by = 0.01)
plot(x.values, sapply(x.values, sample.cdf, s = norm.sample) / sample.size, main = "Porównanie dystrybuanty rozkładu normalnego\nz dystrybuantą empiryczną próby", ylab = "wartość dystrybuanty", xlab = "punkt", type = "l")
lines(x.values, pnorm(x.values), lwd = 3, col = 2)
dev.off()

png("exp_cdf.png")
exp.sample <- rexp(sample.size)
x.values <- seq(min(exp.sample), max(exp.sample), by = 0.01)
plot(x.values, sapply(x.values, sample.cdf, s = exp.sample) / sample.size, main = "Porównanie dystrybuanty rozkładu wykładniczego\nz dystrybuantą empiryczną próby", ylab = "wartość dystrybuanty", xlab = "punkt", type = "l")
lines(x.values, pexp(x.values), lwd = 3, col = 2)
dev.off()

# problem 2

sample.size <- 100
no.reps <- 1000
alpha <- 0.05
eps <- sqrt(log(2 / alpha) / 2 / sample.size)

norm.confidence <- 0
exp.confidence <- 0

for (i in 1:no.reps) {
	norm.sample <- rnorm(sample.size)
	norm.ecdf <- ecdf(norm.sample)
	if (all(abs(norm.ecdf(norm.sample) - pnorm(norm.sample)) <= eps)) {
		norm.confidence <- norm.confidence + 1
	}

	exp.sample <- rexp(sample.size)
	exp.ecdf <- ecdf(exp.sample)
	if (all(abs(exp.ecdf(exp.sample) - pexp(exp.sample)) <= eps)) {
		exp.confidence <- exp.confidence + 1
	}
}
norm.confidence
exp.confidence

# problem 3

sample.size <- 500

norm.sample <- rnorm(sample.size)
silverman.bw <- 0.9 * min(sd(norm.sample, IQR(norm.sample) / 1.34)) * sample.size^(-1/5)
x.values <- seq(min(norm.sample), max(norm.sample), by = 0.1)

png("kernel.png")
plot(x.values, dnorm(x.values), type = "l", lwd = 3, main = "Porównanie estymatorów jądrowych\nw zależności od szerokości pasma", xlab = "punkt", ylab = "wartość gęstości")
d1 <- density(norm.sample, bw = 1)
lines(d1, col = 2)
d2 <- density(norm.sample, bw = 0.5)
lines(d2, col = 3)
d3 <- density(norm.sample, bw = 0.2)
lines(d3, col = 4)
d4 <- density(norm.sample, bw = 0.1)
lines(d4, col = 5)
d5 <- density(norm.sample, bw = 0.05)
lines(d5, col = 6)
d6 <- density(norm.sample, bw = silverman.bw)
lines(d6, col = 7, lwd = 3)
legend(x = "topright", legend = c("h = 1", "h = 0.5", "h = 0.2", "h = 0.1", "h = 0.05", "Silverman"), lty = c(1, 1, 1, 1, 1, 1), col = c(2, 3, 4, 5, 6, 7))
dev.off()

# problem 4

sample.size <- 500

rmix <- function(n) {
	mix.sample <- c()
	for (i in 1:n) {
		r <- runif(1)
		if (r < 0.4) {
			mix.sample <- c(mix.sample, rnorm(1))
		} else if (r < 0.8) {
			mix.sample <- c(mix.sample, rnorm(1, mean = 2, sd = 1))
		} else {
			mix.sample <- c(mix.sample, rnorm(1, mean = 4, sd = 2))
		}
	}
	return(mix.sample)
}

mix.sample <- rmix(sample.size)

x.values <- seq(min(mix.sample), max(mix.sample), by = 0.1)
png("mix.png")
hist(mix.sample, breaks = "Freedman-Diaconis", main = "Porównanie estymatora histogramowego i jądrowego", xlab = "punkt", ylab = "wartość gęstości", probability = TRUE)
lines(x.values, 0.4 * dnorm(x.values) + 0.4 * dnorm(x.values, mean = 2, sd = 1) + 0.2 * dnorm(x.values, mean = 4, sd = 2), type = "l", lwd = 3)
silverman.bw <- 0.9 * min(sd(mix.sample, IQR(mix.sample) / 1.34)) * sample.size^(-1/5)
d <- density(mix.sample, bw = silverman.bw)
lines(d, col = 3, lwd = 3)
dev.off()

help("hist")

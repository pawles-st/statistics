# problem 1

sample.cdf <- function(s, x) {
	return(sum(s <= x))
}

sample.size = 100

png("norm_cdf.png")
norm.sample <- rnorm(sample.size)
x.values <- seq(min(norm.sample), max(norm.sample), by = 0.01)
plot(x.values, sapply(x.values, sample.cdf, s = norm.sample) / sample.size, main = "Porównanie dystrybuanty rozkładu normalnego\nz dystrybuantą empiryczną próby", ylab = "wartość dystrybuanty", xlab = "punkt")
lines(x.values, pnorm(x.values), lwd = 3, col = 2)
dev.off()

png("exp_cdf.png")
exp.sample <- rexp(sample.size)
x.values <- seq(min(exp.sample), max(exp.sample), by = 0.01)
plot(x.values, sapply(x.values, sample.cdf, s = exp.sample) / sample.size, main = "Porównanie dystrybuanty rozkładu wykładniczego\nz dystrybuantą empiryczną próby", ylab = "wartość dystrybuanty", xlab = "punkt")
lines(x.values, pexp(x.values), lwd = 3, col = 2)
dev.off()

help("rexp")

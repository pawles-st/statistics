p <- 0.5
alpha <- 0.05
n.min <- 10
n.max <- 100
n.range <- n.min:n.max

z <- qnorm(1 - alpha / 2)

# Wald CI

coverage.pbb <- rep(0, n.max)

for (n in n.range) {
	for (k in 0:n) {
		p.hat <- k / n
		if (abs(p.hat - p) <= z * sqrt(p.hat * (1 - p.hat) / n)) {
			coverage.pbb[n] = coverage.pbb[n] + dbinom(k, size = n, prob = p)
		}
	}
}

coverage.pbb <- coverage.pbb[n.range]
coverage.pbb <- round(coverage.pbb, digits = 3)

png("wald.png")
plot(n.range, coverage.pbb, type = "l")
abline(a = 1 - alpha, b = 0, col = "red", lwd = 3)
dev.off()

# Wilson CI

coverage.pbb <- rep(0, n.max)

for (n in n.range) {
	for (k in 0:n) {
		p.hat <- k / n
		if (abs(p.hat - p) <= z * sqrt(p * (1 - p) / n)) {
			coverage.pbb[n] = coverage.pbb[n] + dbinom(k, size = n, prob = p)
		}
	}
}

coverage.pbb <- coverage.pbb[n.range]
coverage.pbb <- round(coverage.pbb, digits = 3)

png("wilson.png")
plot(n.range, coverage.pbb, type = "l")
abline(a = 1 - alpha, b = 0, col = "red", lwd = 3)
dev.off()

# Agresti-Coull CI

coverage.pbb <- rep(0, n.max)

for (n in n.range) {
	for (k in 0:n) {
		p.hat <- k / n
		k.tilde <- k + z^2 / 2
		n.tilde <- n + z^2
		p.tilde <- k.tilde / n.tilde
		q.tilde <- 1 - p.tilde
		if (abs(p.hat - p) <= z * sqrt(p.tilde * q.tilde / n.tilde)) {
			coverage.pbb[n] = coverage.pbb[n] + dbinom(k, size = n, prob = p)
		}
	}
}

coverage.pbb <- coverage.pbb[n.range]
coverage.pbb <- round(coverage.pbb, digits = 3)

png("agresti-coull.png")
plot(n.range, coverage.pbb, type = "l")
abline(a = 1 - alpha, b = 0, col = "red", lwd = 3)
dev.off()

# likelihood quotient CI

coverage.pbb <- rep(0, n.max)

for (n in n.range) {
	for (k in 0:n) {
		likelihood.quotient <- p^k * (1 - p)^(n - k) / (k / n)^k / (1 - k / n)^(n - k)
		if (-2 * log(likelihood.quotient) <= z^2) {
			coverage.pbb[n] = coverage.pbb[n] + dbinom(k, size = n, prob = p)
		}
	}
}

coverage.pbb <- coverage.pbb[n.range]
coverage.pbb <- round(coverage.pbb, digits = 3)

png("agresti-coull.png")
plot(n.range, coverage.pbb, type = "l")
abline(a = 1 - alpha, b = 0, col = "red", lwd = 3)
dev.off()



help("")

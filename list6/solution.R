PH0 <- function(w, m, n) {
	N <- n + m
	K <- combn(N, n)
	result <- 0
	for(i in 1:choose(N, n)){
		if(sum(K[, i]) >= w){
			result <- result + 1
		}
	}
	return(result / choose(N,n))
}

PH0.normal <- function(w, m, n) {
	q_p0 <- ( w - 0.5 * n * (n + m + 1) ) / ( sqrt( m * n * (N + 1) / 12) )
	return(1 - pnorm(q_p0))
}

PH0.normal2 <- function(w, m, n) {
	q_p0 <- ( w - 0.5 * n * (n + m + 1) - 0.5 ) / ( sqrt(m * n * (N + 1) / 12) )
	return(1 - pnorm(q_p0))
}

m <- 5
n.values <- c(5, 8, 10)
alpha.values <- c(0.1, 0.05, 0.01, 0.005)
for (n in n.values) {
	for (alpha in alpha.values) {
		N <- n + m
		z <- qnorm(1 - alpha)
		w <- z * sqrt(m * n * (N + 1) / 12) + 0.5 * n * (N + 1)
		print(paste0("(", n, ",", m, "), alpha = ", alpha, ": ", PH0(w, m, n)))
	}
}

m <- 3
n <- 6
w.values <- c(9, 12, 15, 18, 21)
for (w in w.values) {
	print(paste0("a0, w = ", w, ": ", PH0(w, m, n)))
	print(paste0("a1, w = ", w, ": ", PH0.normal(w, m, n)))
	print(paste0("a2, w = ", w, ": ", PH0.normal2(w, m, n)))
}

m <- 6
n <- 6
w.values <- c(27, 33, 39, 45, 51)
for (w in w.values) {
	print(paste0("a0, w = ", w, ": ", PH0(w, m, n)))
	print(paste0("a1, w = ", w, ": ", PH0.normal(w, m, n)))
	print(paste0("a2, w = ", w, ": ", PH0.normal2(w, m, n)))
}

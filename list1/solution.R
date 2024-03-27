edu.data <- read.table("lab1_dane.txt", dec = ",", header = TRUE)

# problem 1

type.count <- table(edu.data$typ)
percentages <- paste0(round(100 * type.count / sum(type.count), 2), "%")
dev.new(width = 10, height = 10)
pie(type.count, labels = percentages, col = rainbow(6), pin = c(50, 50))
legend("topleft", legend = c("kierownik", "sprzedawca/marketing", "urzędnik", "obsługa", "wolny zawód", "inne"), fill = rainbow(6))

# problem 2

edu.level <- function(e) {
	if (e <= 8) {
		return(1);
	} else if (e <= 12) {
		return(2);
	} else {
		return(3);
	}
}

dollars.to.pln = 3.99

pension <- function(dph) {
	return(dph * dollars.to.pln * 8 * 22 / 1000)
}

edu.data$wykszt <- sapply(edu.data$edu, edu.level)
edu.data$pensja <- sapply(edu.data$zarobki, pension)

# problem 3-4

pension.summary <- function(d) {
	pension <- data.frame(matrix(ncol = 9, nrow = 1))
	colnames(pension) <- c("mean", "median", "q1", "q3", "iqr", "var", "std", "min", "max")
	pension$mean <- mean(d$pensja)
	pension$median <- median(d$pensja)
	pension$q1 <- quantile(d$pensja, probs = 0.25)
	pension$q3 <- quantile(d$pensja, probs = 0.75)
	pension$iqr <- pension$q3 - pension$q1
	pension$var <- var(d$pensja)
	pension$std <- sqrt(pension$var)
	pension$min <- min(d$pensja)
	pension$max <- max(d$pensja)
	return(pension)
}

total.summary <- pension.summary(edu.data)
classes.summary <- rbind(pension.summary(edu.data[edu.data$wykszt == 1,]), pension.summary(edu.data[edu.data$wykszt == 2,]), pension.summary(edu.data[edu.data$wykszt == 3,]))
total.summary
classes.summary

boxplot(pensja ~ wykszt, edu.data)

# problem 5

pts <- seq(total.summary$min, total.summary$max, length = 50)

gamma.dist <- dgamma(pts, shape = 3.5, scale = 1.5)

hist(edu.data$pensja, prob = TRUE)
lines(pts, gamma.dist, col = 2, lwd = 3)
lines(density(edu.data$pensja))

fd.classes <- ceiling((total.summary$max - total.summary$min) / (2 * total.summary$iqr / nrow(edu.data) ^ (1/3)))

hist(edu.data$pensja, breaks = fd.classes, prob = TRUE)
lines(pts, gamma.dist, col = 2, lwd = 3)
lines(density(edu.data$pensja))

# problem 6

edu.data
race.edu <- table(edu.data$rasa, edu.data$wykszt)
race.edu <- prop.table(race.edu, margin = 1)
race.edu

help("boxplot")


edu.data <- read.table("lab1_dane.txt", dec = ",", header = TRUE)

types.count <- table(edu.data$typ)
percentages <- paste0(round(100 * types.count / sum(types.count), 2), "%")
pie(types.count, labels = percentages, col = rainbow(6))
legend("topleft", legend = c("kierownik", "sprzedawca/marketing", "urzędnik", "obsługa", "wolny zawód", "inne"), fill = rainbow(6))

edu.data["wykszt"] = c(rep(0, nrow(edu.data)))
edu.data[edu.data$edu <= 8]

help("legend")


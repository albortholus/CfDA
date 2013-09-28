D <- read.csv("../hw1/hw1_data.csv")
# 1
names(D)

# 2
D[c(1:2), ]

# 3
nr <- nrow(D)
nr

# 4
D[c((nr - 1):nr), ]

# 5
D[47, "Ozone"]

# 6
length(D[is.na(D[, "Ozone"]), "Ozone"])

# 7
mean(D[!is.na(D[, "Ozone"]), "Ozone"])

# 8
mean(D[!is.na(D[,"Ozone"]) & D[,"Ozone"] > 31 & D[,"Temp"] > 90, "Solar.R"])

# 9
mean(D[D[, "Month"] == 6, "Temp"])

# 10
max(D[D[, "Month"] == 5 & !is.na(D[, "Ozone"]), "Ozone"])

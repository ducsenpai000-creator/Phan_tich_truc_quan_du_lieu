set.seed(42)

# 1. Dữ liệu
customers = data.frame(
  Age = c(rnorm(70, 25, 4), rnorm(60, 45, 5), rnorm(70, 65, 6)),
  Income = c(rnorm(70, 30, 8), rnorm(60, 70, 10), rnorm(70, 45, 8)),
  Spending = c(rnorm(70, 20, 5), rnorm(60, 80, 12), rnorm(70, 40, 8))
)

# 2. K-Means
km = kmeans(customers, centers = 3, nstart = 25)
customers$Cluster = km$cluster

par(mfrow = c(1, 2), mar = c(2, 2, 1, 1), mgp = c(1, 0.1, 0), cex = 0.6)

# Biểu đồ 1
plot(customers$Age, customers$Income,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, xlab = "Age", ylab = "Inc", main = "A vs I")
points(km$centers[, 1:2], pch = 4, cex = 1.5, lwd = 2)

# Biểu đồ 2
plot(customers$Income, customers$Spending,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, xlab = "Inc", ylab = "Spe", main = "I vs S")
points(km$centers[, 2:3], pch = 4, cex = 1.5, lwd = 2)

set.seed(42)

# Tạo dữ liệu khách hàng
customers = data.frame(
  Age = c(rnorm(70, 25, 4), rnorm(60, 45, 5), rnorm(70, 65, 6)),
  Income = c(rnorm(70, 30, 8), rnorm(60, 70, 10), rnorm(70, 45, 8)),
  Spending = c(rnorm(70, 20, 5), rnorm(60, 80, 12), rnorm(70, 40, 8))
)

# K-Means Clustering
# Ở đây ta chọn 3 cụm
km = kmeans(customers, centers = 3, nstart = 25)
customers$Cluster = km$cluster

# Cấu hình hiển thị 2 biểu đồ cùng lúc
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

# Chạy tiếp các lệnh plot của bạn ở đây...
# Biểu đồ 1: Tuổi vs Thu nhập
plot(customers$Age, customers$Income,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.3,
     xlab = "Tuổi", ylab = "Thu nhập (triệu/tháng)",
     main = "Age vs Income")
# Vẽ tâm của các cụm (Centroids)
points(km$centers[, 1:2], pch = 4, cex = 3, lwd = 3)

# Biểu đồ 2: Thu nhập vs Chi tiêu
plot(customers$Income, customers$Spending,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.3,
     xlab = "Thu nhập", ylab = "Chi tiêu",
     main = "Income vs Spending")
# Vẽ tâm của các cụm (Centroids)
points(km$centers[, 2:3], pch = 4, cex = 3, lwd = 3)
# Định nghĩa các hàm bổ trợ (Vì R không có sẵn các hàm này)
# 1. Chỉnh hợp: A(n, k) = n! / (n-k)!
chinh_hop = function(n, k) {
  return(factorial(n) / factorial(n - k))
}

# 2. Tổ hợp lặp: C_lap(n, k) = C(n + k - 1, k)
to_hop_lap = function(n, k) {
  return(choose(n + k - 1, k))
}

# Bài tập 1: Phân loại
cat("\n--- Bài tập 1: ---\n")

# 1. Hoán vị: P(7)
cat("1. Sắp xếp 7 quyển sách:", factorial(7), "\n")

# 2. Hoán vị lặp: 11 ký tự, M:2, A:2, T:2
result_2 = factorial(11) / (factorial(2) * factorial(2) * factorial(2))
cat("2. Hoán vị lặp (MATHEMATICS):", result_2, "\n")

# 3. Chỉnh hợp lặp: 10^5
cat("3. Mật khẩu 5 chữ số (có lặp):", 10^5, "\n")

# 4. Tổ hợp: C(20, 4)
cat("4. Chọn 4 từ 20 học sinh:", choose(20, 4), "\n")

# 5. Tổ hợp lặp: C_lap(5, 3)
cat("5. Chọn 3 kẹo từ 5 loại:", to_hop_lap(5, 3), "\n")


# Bài tập 2: Mật khẩu
cat("\n--- Bài tập 2 - Mật khẩu: ---\n")
cat("1. Chỉ số, có lặp (10^8):", 10^8, "\n")
cat("2. Chỉ số, không lặp (A 10 chập 8):", chinh_hop(10, 8), "\n")
cat("3. Chữ+số (36 ký tự), có lặp (36^8):", 36^8, "\n")


# Bài tập 3: Xếp người
cat("\n--- Bài tập 3 - Xếp người: ---\n")
cat("1. Xếp 5 người ngang:", factorial(5), "\n")
cat("2. A và B cạnh nhau (xem là 1 khối):", factorial(4) * 2, "\n")
cat("3. A và B KHÔNG cạnh nhau:", factorial(5) - (factorial(4) * 2), "\n")


# Bài tập 4: Phân phối (Stars and Bars)
# Bài toán chia k vật giống nhau cho n người, mỗi người ít nhất 1: C(k-1, n-1)
cat("\n--- Bài tập 4 - Phân phối bi: ---\n")
cat("Số cách chia 10 bi cho 3 người (mỗi người >= 1):", choose(9, 2), "\n")


# Bài tập 5: Tổ hợp lặp
cat("\n--- Bài tập 5 - Tổ hợp lặp: ---\n")
cat("1. Mua 8 quả từ 4 loại:", to_hop_lap(4, 8), "\n")
cat("2. Số nghiệm nguyên không âm x1+x2+x3=10:", to_hop_lap(3, 10), "\n")
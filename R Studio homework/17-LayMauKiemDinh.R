
# Bài tập 1: Khoảng tin cậy (CI)
cat("\n--- BÀI TẬP 1: KHOẢNG TIN CẬY ---\n")
n = 25; xbar = 78; s = 8
se = s / sqrt(n)

# 1. 95% CI (t-score với df = 24)
t_95 = qt(0.975, df = n-1)
ci_95 = c(xbar - t_95 * se, xbar + t_95 * se)
cat("1. 95% CI là: [", round(ci_95[1], 2), ",", round(ci_95[2], 2), "]\n")

# 2. 99% CI
t_99 = qt(0.995, df = n-1)
ci_99 = c(xbar - t_99 * se, xbar + t_99 * se)
cat("2. 99% CI là: [", round(ci_99[1], 2), ",", round(ci_99[2], 2), "]\n")

# 3. Ý nghĩa: Chúng ta tin tưởng 95% rằng điểm trung bình thực sự của TOÀN BỘ 
# sinh viên nằm trong khoảng từ 74.7 đến 81.3.


# Bài tập 2: One-sample t-test
cat("\n--- BÀI TẬP 2: ONE-SAMPLE T-TEST ---\n")
bulb_data = c(980, 1020, 990, 1010, 1005, 995, 1015, 985, 1000, 1010,
              995, 1005, 1000, 990, 1015, 1010, 995, 1000, 1005, 1010)

test2 = t.test(bulb_data, mu = 1000)
print(test2)
# Nhận xét: Nếu p-value > 0.05, tuyên bố của nhà máy là có cơ sở.


# Bài tập 3: 2 mẫu độc lập
cat("\n--- BÀI TẬP 3: INDEPENDENT T-TEST ---\n")
lop_A = c(75, 80, 72, 85, 78, 82, 76, 79, 81, 77)
lop_B = c(82, 85, 80, 88, 83, 86, 84, 87, 85, 83)

test3 = t.test(lop_A, lop_B, var.equal = TRUE)
cat("P-value so sánh 2 lớp:", test3$p.value, "\n")
# Nhận xét: p-value rất nhỏ (< 0.05) => Lớp B có điểm cao hơn rõ rệt.


# Bài tập 4: Cặp tương ứng
cat("\n--- BÀI TẬP 4: PAIRED T-TEST ---\n")
truoc = c(70, 75, 68, 80, 72, 77, 69, 74, 76, 71)
sau = c(68, 72, 66, 77, 70, 74, 67, 71, 73, 69)

test4 = t.test(truoc, sau, paired = TRUE)
cat("P-value hiệu quả giảm cân:", test4$p.value, "\n")
# Nhận xét: p-value < 0.05 => Chế độ ăn có hiệu quả làm giảm cân.


# Bài tập 5:Kiểm định tính độc lập
cat("\n--- BÀI TẬP 5: CHI-SQUARE ---\n")
# Tạo bảng tần số (Contingency Table)
film_table = matrix(c(40, 30, 30, 20, 50, 30), 
                    nrow = 2, byrow = TRUE)
rownames(film_table) = c("Nam", "Nữ")
colnames(film_table) = c("Hành động", "Tâm lý", "Hài")

test5 = chisq.test(film_table)
print(test5)
# Nhận xét: Nếu p-value < 0.05 => Giới tính và sở thích phim có liên quan.
# Bài tập 1: Phân phối Nhị thức
n = 20
p = 0.25

# 1. Xác suất trả lời đúng đúng 5 câu P(X = 5)
cau1_1 = dbinom(5, size = n, prob = p)

# 2. Xác suất trả lời đúng ít nhất 10 câu P(X >= 10)
# Tương đương: 1 - P(X <= 9)
cau1_2 = pbinom(9, size = n, prob = p, lower.tail = FALSE) 
# Hoặc: 1 - pbinom(9, n, p)

# 3. Số câu đúng kỳ vọng E[X] = n * p
cau1_3 = n * p


# Bài tập 2:Phân phối Poisson
lambda = 12

# 1. Xác suất có đúng 10 khách P(X = 10)
cau2_1 = dpois(10, lambda)

# 2. Xác suất có nhiều hơn 15 khách P(X > 15)
cau2_2 = ppois(15, lambda, lower.tail = FALSE)

# 3. Xác suất có từ 10-15 khách P(10 <= X <= 15)
# Tương đương: P(X <= 15) - P(X <= 9)
cau2_3 = ppois(15, lambda) - ppois(9, lambda)


#Bài tập 3:Phân phối Chuẩn
mu = 75
sigma = 10

# 1. Xác suất sinh viên đạt trên 85 điểm P(X > 85)
cau3_1 = pnorm(85, mean = mu, sd = sigma, lower.tail = FALSE)

# 2. Xác suất đạt từ 65-85 điểm P(65 <= X <= 85)
cau3_2 = pnorm(85, mu, sigma) - pnorm(65, mu, sigma)

# 3. Điểm tối thiểu để vào top 20% (Tức là nằm ở bách phân vị thứ 80)
cau3_3 = qnorm(0.80, mean = mu, sd = sigma)

# 4. Tính Z-score của điểm 90: $Z = \frac{X - \mu}{\sigma}$
cau3_4 = (90 - mu) / sigma


# Bài tập 4:Định lý ggiới hạn Trung tâm
mu_pop = 45
sigma_pop = 8
n_sample = 36

# Độ lệch chuẩn của trung bình mẫu : $SE = \frac{\sigma}{\sqrt{n}}$
se = sigma_pop / sqrt(n_sample)

# 1. Xác suất trung bình thời gian > 47 phút P(X_bar > 47)
cau4_1 = pnorm(47, mean = mu_pop, sd = se, lower.tail = FALSE)

# 2. Xác suất trung bình thời gian < 43 phút P(X_bar < 43)
cau4_2 = pnorm(43, mean = mu_pop, sd = se)
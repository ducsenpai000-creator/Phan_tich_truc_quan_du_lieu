# 1. Tính diện tích hình chữ nhật
tinh_dien_tich_hcn = function(dai, rong) {
  dien_tich = dai * rong
  return(dien_tich)
}

# 2. Tính chu vi hình tròn
tinh_chu_vi_tron = function(ban_kinh) {
  chu_vi = 2 * pi * ban_kinh
  return(chu_vi)
}

# 3. Chuyển Celsius sang Fahrenheit: $F = C \times \frac{9}{5} + 32$
c_to_f = function(celsius) {
  fahrenheit = celsius * 9/5 + 32
  return(fahrenheit)
}


# 1. Kiểm tra số chẵn/lẻ (Yêu cầu số nguyên)
kiem_tra_chan_le = function(n) {
  if (!is.numeric(n) || n %% 1 != 0) {
    return("Lỗi: Vui lòng nhập một số nguyên!")
  }
  if (n %% 2 == 0) return("Chẵn") else return("Lẻ")
}

# 2. Tính điểm trung bình (Validate 0-10 và loại NA)
tinh_diem_tb = function(vector_diem) {
  # Loại bỏ NA
  vector_clean = na.omit(vector_diem)
  
  # Kiểm tra điều kiện điểm từ 0 đến 10
  if (any(vector_clean < 0 | vector_clean > 10)) {
    stop("Lỗi: Điểm số phải nằm trong khoảng từ 0 đến 10!")
  }
  
  return(mean(vector_clean))
}



# 1. Tính toán tổng quan
thong_ke_tong_quan = function(x) {
  res = list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    range = diff(range(x, na.rm = TRUE))
  )
  return(res)
}

# 2. Tính hoán vị $P(n, r) = \frac{n!}{(n-r)!}$
hoan_vi = function(n, r) {
  if (r > n) return(0)
  return(factorial(n) / factorial(n - r))
}

# 3. Tính tổ hợp $C(n, r) = \frac{n!}{r! \times (n-r)!}$
to_hop = function(n, r) {
  if (r > n) return(0)
  return(factorial(n) / (factorial(r) * factorial(n - r)))
}



# 1. Tìm các số nguyên tố từ 1 đến n
tim_so_nguyen_to = function(n) {
  if (n < 2) return(NULL)
  primes = c()
  for (i in 2:n) {
    if (all(i %% (2:sqrt(i)) != 0) || i == 2) {
      primes = c(primes, i)
    }
  }
  return(primes)
}

# 2. Tạo tam giác Pascal với n hàng
tam_giac_pascal = function(n) {
  for (i in 0:(n-1)) {
    row = c()
    for (k in 0:i) {
      row = c(row, to_hop(i, k))
    }
    print(row)
  }
}

# 3. Phân loại sinh viên (Kèm GPA 4.0)
phan_loai_sv = function(diem) {
  if (diem >= 9) {
    rank = "Xuất sắc"; gpa = 4.0
  } else if (diem >= 8) {
    rank = "Giỏi"; gpa = 3.5
  } else if (diem >= 6.5) {
    rank = "Khá"; gpa = 3.0
  } else if (diem >= 5) {
    rank = "Trung bình"; gpa = 2.0
  } else {
    rank = "Yếu"; gpa = 1.0
  }
  return(list(XepLoai = rank, GPA_4.0 = gpa))
}



# 1. Tính lương ròng (Giả định thuế 10% nếu lương > 15tr)
tinh_luong_rong = function(luong_cb, phu_cap, ngay_lam, gio_tang_ca) {
  luong_ngay = luong_cb / 26
  luong_tang_ca = (luong_ngay / 8) * 1.5 * gio_tang_ca
  tong_thu_nhap = (luong_ngay * ngay_lam) + phu_cap + luong_tang_ca
  
  # Tính thuế giả định
  thue = if(tong_thu_nhap > 15000000) tong_thu_nhap * 0.1 else 0
  luong_rong = tong_thu_nhap - thue
  return(luong_rong)
}

# 2. Chuẩn hóa điểm thi (Min-Max Scaling 0-100)
chuan_hoa_diem = function(vector_diem) {
  d_min = min(vector_diem, na.rm = TRUE)
  d_max = max(vector_diem, na.rm = TRUE)
  # Công thức: $\frac{x - min}{max - min} \times 100$
  score_norm = (vector_diem - d_min) / (d_max - d_min) * 100
  return(score_norm)
}

# 3. Phân tích dữ liệu sinh viên
phan_tich_sv = function(df) {
  cat("--- Thống kê mô tả ---\n")
  print(summary(df))
  cat("\n--- Sinh viên có điểm cao nhất ---\n")
  print(df[which.max(df$diem), ])
  cat("\n--- Điểm trung bình theo nhóm tuổi ---\n")
  print(aggregate(diem ~ tuoi, data = df, mean))
}

# TEST 
# tinh_dien_tich_hcn(5, 10)
# tim_so_nguyen_to(20)
# phan_loai_sv(8.5)
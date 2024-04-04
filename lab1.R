data <- c(299,
          287,
          292,
          280,
          291,
          288,
          291,
          290,
          287,
          287,
          281,
          284,
          279,
          290,
          286,
          278,
          286)
print(data)

# Hãy tính giá trị trung bình và khoảng tin cậy 95% của mẫu

# Tính giá trị trung bình
mean(data)
# Tính khoảng tin cậy 95% của mẫu

t.test(data, conf.level = 0.95)


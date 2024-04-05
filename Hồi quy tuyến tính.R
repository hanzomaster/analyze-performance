fit <- lm(readtime ~ filesize, data = rf)

plot(density(residuals(fit)))

grid()
abline(fit, col = 'red')
summary(fit)

library(forecast)

forecast(fit, newdata = data.frame(filesize = 1000))

confint(fit)

predict(fit, rf, interval = 'confidence')

predict(fit, rf, interval = 'prediction')

# Hồi quy tuyến tính đa biến
library(datasets)
data(mtcars)

?mtcars

mtcars$vs <- as.factor(mtcars$vs)

plot(mtcars[, c('mpg', 'wt', 'vs')],
     pch = 20,
     cex = 0.8,
     col = mtcars$vs)

fit2 <- lm(data = mtcars, mpg ~ wt + vs)

summary(fit2)

confint(fit2, conf.level = 0.95)

install.packages('car')

library(car)

scatterplot(data = mtcars, mpg ~ wt | vs, smooth = F)

summary(fit3 <- lm(data = mtcars, mpg ~ wt * vs))

# Phân tích mối quan hệ và sự tương quan (nếu có) của biến phụ thuộc mpg và hai
# biến độc lập wt, am. Lưu ý chuyển đổi am thành factor
mtcars$am <- as.factor(mtcars$am)

scatterplot(data = mtcars, mpg ~ wt | am, smooth = F)

summary(fit4 <- lm(data=mtcars, mpg~wt*am))

# Làm ngoài
head(ToothGrowth)
?ToothGrowth

summary(lm(data = ToothGrowth, len ~ supp + dose))

scatterplot(data = ToothGrowth, len ~ dose | supp, smooth = F)

head(mtcars)

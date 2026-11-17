library(readr)
library(ggplot2)
library(car)
library(tidyverse)
library(MASS)
library(broom)
library(dplyr)
library(knitr)

dirty_data <- read_csv("C:/Users/Tom/Documents/dirty_data.csv")
head(dirty_data, 10)
str(dirty_data)

new_data <- dirty_data[, c("season", "order_price", "coupon_discount", "delivery_charges",
                           "order_total", "distance_to_nearest_warehouse", "is_expedited_delivery",
                           "is_happy_customer")]

head(new_data, 10)

colSums(is.na(new_data))
colSums(new_data == "", na.rm = TRUE)

unique(new_data$season)

new_data$season <- tools::toTitleCase(tolower(trimws(new_data$season)))

unique(new_data$season)

Q1 <- quantile(new_data$order_price, 0.25, na.rm = TRUE)
Q3 <- quantile(new_data$order_price, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers_price <- new_data$order_price < lower_bound | new_data$order_price > upper_bound
cat("\nSố lượng dòng ngoại lai order_price bị loại bỏ:", sum(outliers_price, na.rm = TRUE), "\n")

new_data <- new_data[!outliers_price, ]

new_data$tyle <- new_data$distance_to_nearest_warehouse / new_data$delivery_charges

Q1_tyle <- quantile(new_data$tyle, 0.25, na.rm = TRUE)
Q3_tyle <- quantile(new_data$tyle, 0.75, na.rm = TRUE)
IQR_tyle <- Q3_tyle - Q1_tyle

lowerbound_tyle <- Q1_tyle - 1.5 * IQR_tyle
upperbound_tyle <- Q3_tyle + 1.5 * IQR_tyle

distance_error_index <- which(new_data$tyle < lowerbound_tyle | new_data$tyle > upperbound_tyle)

cat("\nNgưỡng dưới (Lower):", lowerbound_tyle)
cat("\nNgưỡng trên (Upper):", upperbound_tyle, "\n")
cat("Phát hiện", length(distance_error_index), "dòng có tỷ lệ bất hợp lý ở cả 2 chiều.\n")

head(new_data, 20)

new_data$distance_to_nearest_warehouse[distance_error_index] <- new_data$distance_to_nearest_warehouse[distance_error_index] / 100

new_data$tyle <- NULL

order_total_temp <- new_data$order_price *(1 - new_data$coupon_discount / 100) + new_data$delivery_charges
error_data <- abs(new_data$order_total - order_total_temp) > 0
sum(error_data)

new_data$order_total[error_data] <- order_total_temp[error_data]
sum(abs(new_data$order_total - order_total_temp) > 0)

new_data$season <- factor(new_data$season,levels = c("Spring", "Summer", "Autumn", "Winter"))

bang_mua <- table(new_data$season)
bang_ty_le_mua <- prop.table(bang_mua) * 100
bang_tong_hop_mua <- cbind('Số lượng (n)' = bang_mua, 'Tỷ lệ (%)' = round(bang_ty_le_mua, 2))
print("--- BẢNG 1: THỐNG KÊ ĐƠN HÀNG THEO MÙA ---")
print(bang_tong_hop_mua)
barplot(bang_mua, 
        main = "Biểu đồ số lượng đơn hàng theo mùa", 
        xlab = "Mùa", 
        ylab = "Số lượng", 
        col = "lightblue",
        ylim = c(0, max(bang_mua) * 1.2))

describe_function <- function(x) {
  c(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE)
  )
}
describe_table <- apply(
  new_data[, c("order_total", "order_price", "delivery_charges")], 2,   describe_function)
rownames(describe_table) <- c("Mean", "Median", "SD", "Min", "Max")
print("--- BẢNG THỐNG KÊ MÔ TẢ ĐỊNH LƯỢNG TỔNG HỢP ---")
print(round(describe_table, 2))

bang_cheo <- table(new_data$season,new_data$is_happy_customer)
ty_le_hai_long <- round(prop.table(bang_cheo, margin = 1)[,"TRUE"] * 100,2)
bang_season_happy <- cbind(bang_cheo,"Tỷ lệ hài lòng (%)" = ty_le_hai_long)
print(bang_season_happy)

bang_cheo_1 <- table(new_data$is_expedited_delivery,new_data$is_happy_customer)
ty_le_hai_long_1 <- round(prop.table(bang_cheo_1, margin = 1)[,"TRUE"] * 100,2)
bang_is_expedited_delivery_happy <- cbind(bang_cheo_1,"Tỷ lệ hài lòng (%)" = ty_le_hai_long_1)
print(bang_is_expedited_delivery_happy)

hist(new_data$order_total, 
     main = "Biểu đồ phân phối Giá trị đơn hàng", 
     xlab = "Giá trị đơn hàng", 
     ylab = "Tần số", 
     col = "lightgreen", 
     border = "black",
     breaks = 20)
hist(new_data$order_price, 
     main = "Biểu đồ phân phối Giá trị sản phẩm", 
     xlab = "Giá trị sản phẩm", 
     ylab = "Tần số", 
     col = "lightblue", 
     border = "black",
     breaks = 20)
hist(new_data$delivery_charges, 
     main = "Biểu đồ phân phối phí giao hàng", 
     xlab = "Phí giao hàng", 
     ylab = "Tần số", 
     col = "lightyellow", 
     border = "black",
     breaks = 20)


boxplot(order_total ~ is_happy_customer, data = new_data, 
        main = "Biểu đồ Boxplot giá trị đơn hàng & Hài lòng", 
        xlab = "Khách hàng hài lòng (FALSE / TRUE)", 
        ylab = "Giá trị đơn hàng", 
        col = c("salmon", "skyblue"))

ggplot(new_data,aes(x = is_expedited_delivery,fill = is_happy_customer)) +geom_bar(position = "fill") +scale_y_continuous(labels = scales::percent)

cat("\n--- 2.2.1 ƯỚC LƯỢNG KHOẢNG CHO TỶ LỆ HÀI LÒNG ---\n")
n <- nrow(new_data)
f <- mean(new_data$is_happy_customer)
z <- qnorm(0.975)
se <- sqrt(f * (1 - f) / n)
data.frame(n=n,f=f,z=z, se=se)
Lower <- f - z * se
Upper <- f + z * se
data.frame(Ty_le_hai_long =f,Lower = Lower,Upper = Upper)

cat("\n2.2.2 KIỂM ĐỊNH SO SÁNH TỶ LỆ HÀI LÒNG\n")
x1 <- bang_cheo_1["TRUE", "TRUE"]   
n1 <- sum(bang_cheo_1["TRUE", ])    
p1 <- x1 / n1                       
x2 <- bang_cheo_1["FALSE", "TRUE"]  
n2 <- sum(bang_cheo_1["FALSE", ])   
p2 <- x2 / n2
data.frame(x1=x1, n1=n1,p1=p1,x2=x2,n2=n2,p2=p2)
p_pool <- (x1 + x2) / (n1 + n2)
SE <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
Z_stat <- (p1 - p2) / SE
p_value <- 2 * (1 - pnorm(abs(Z_stat)))
SE_ci <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
margin_of_error <- qnorm(0.975) * SE_ci
ci_lower <- (p1 - p2) - margin_of_error
ci_upper <- (p1 - p2) + margin_of_error
cat("Tỷ lệ hài lòng giao nhanh (p1):", round(p1, 4), "\n")
cat("Tỷ lệ hài lòng giao thường (p2):", round(p2, 4), "\n")
cat("Giá trị thống kê Z:", round(Z_stat, 4), "\n")
cat("P-value:", p_value, "\n")
cat("Khoảng tin cậy 95% của sự chênh lệch: \n")
data.frame(ci_lower=ci_lower, ci_upper=ci_upper)

new_data$coupon_discount_factor <- as.factor(new_data$coupon_discount)  

anova_order <- aov(order_total ~ season * coupon_discount_factor,data = new_data)
residuals_order <- residuals(anova_order)

cat("\n[1] ĐÁNH GIÁ TÍNH ĐỘC LẬP:\n")
par(mfrow = c(1, 1))
plot(residuals_order, type = "p", pch = 16, col = "darkgray",
     main = "Kiểm tra Tính độc lập: Phân tán phần dư",
     ylab = "Phần dư (Residuals)", xlab = "Thứ tự quan sát (Index)")
abline(h = 0, col = "red", lwd = 2, lty = 2)
dw_result <- car::durbinWatsonTest(anova_order)
print(dw_result)

cat("\n[2] ĐÁNH GIÁ TÍNH CHUẨN (Shapiro-Wilk Test):\n")
print(shapiro.test(residuals_order))

qqnorm(residuals_order, main = "Q-Q Plot: Kiểm tra Tính chuẩn của phần dư")
qqline(residuals_order, col = "blue", lwd = 2)


cat("\n[3] ĐÁNH GIÁ TÍNH ĐỒNG NHẤT PHƯƠNG SAI (Levene Test):\n")

cat("- Kiểm tra cho nhóm Mùa (season):\n")
print(leveneTest(order_total ~ season, data = new_data))

cat("- Kiểm tra cho nhóm Chiết khấu (coupon_discount):\n")
print(leveneTest(order_total ~ coupon_discount_factor, data = new_data))

cat("- Kiểm tra chéo (Mùa x Chiết khấu):\n")
print(leveneTest(order_total ~ interaction(season, coupon_discount_factor), data = new_data))

boxplot(
  order_total ~ season,
  data = new_data,
  col = "lightblue",
  main = "order_total by Season",
  xlab = "Season",
  ylab = "order_total"
)

boxplot(
  order_total ~ coupon_discount_factor,
  data = new_data,
  col = "lightgreen",
  main = "order_total by Coupon Discount",
  xlab = "Coupon Discount",
  ylab = "order_total"
)

boxplot(
  order_total ~ interaction(season, coupon_discount_factor),
  data = new_data,
  col = "lightpink",
  main = "order_total by Season and Coupon Discount",
  xlab = "",
  ylab = "order_total",
  las = 2
)

anova_order <- aov(order_total ~ season * coupon_discount_factor, data = new_data)

summary(anova_order)

tukey_discount <- TukeyHSD(anova_order, which = "coupon_discount_factor")
print(tukey_discount)
plot(
  tukey_discount,
  las = 1,
  col = "darkblue"
)

set.seed(123) 
new_data$season <- relevel(new_data$season, ref = "Autumn")
train_index <- sample(1:nrow(new_data), size = round(0.8 * nrow(new_data)))
train_data  <- new_data[train_index, ]
test_data   <- new_data[-train_index, ]

summary(glm(is_happy_customer ~ order_price, data = train_data, family = binomial(link = "logit")))
summary(glm(is_happy_customer ~ delivery_charges, data = train_data, family = binomial(link = "logit")))
summary(glm(is_happy_customer ~ coupon_discount, data = train_data, family = binomial(link = "logit")))
summary(glm(is_happy_customer ~ distance_to_nearest_warehouse, data = train_data, family = binomial(link = "logit")))
summary(glm(is_happy_customer ~ season, data = train_data, family = binomial(link = "logit")))
summary(glm(is_happy_customer ~ is_expedited_delivery, data = train_data, family = binomial(link = "logit")))
summary(glm(is_happy_customer ~ order_total, data = train_data, family = binomial(link = "logit")))

full_model <- glm(
  is_happy_customer ~ order_price + coupon_discount + delivery_charges + 
    distance_to_nearest_warehouse + season + 
    is_expedited_delivery + order_total,
  data = train_data, family = binomial(link = "logit") 
)

best_model <- stepAIC(full_model, direction = "both", trace = TRUE)

model_2vars <- glm(is_happy_customer ~ coupon_discount + delivery_charges + distance_to_nearest_warehouse + season + is_expedited_delivery + order_total, data = train_data, family = binomial(link = "logit"))

coef_table_2vars <- tidy(model_2vars, exponentiate = TRUE) %>%
  mutate(
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 4),
    p.value = round(p.value, 4)
  ) %>%
  rename(
    `Biến / Hệ số` = term,
    `Odds Ratio (Estimate)` = estimate,
    `Sai số chuẩn (Std. Error)` = std.error,
    `Giá trị z (z-value)` = statistic,
    `Giá trị p (p-value)` = p.value
  )

kable(coef_table_2vars, caption = "Bảng: Các hệ số hồi quy logistic của mô hình 2 biến")

vephandu <- par(mfrow = c(2, 2))
plot(best_model)

coefficients_values <- coef(best_model)

print(coefficients_values)

beta_0 <- coefficients_values["(Intercept)"]
beta_coupon <- coefficients_values["coupon_discount"]
beta_delivery_charges <- coefficients_values["delivery_charges"]
beta_distance <- coefficients_values["distance_to_nearest_warehouse"]
beta_seasonSpring <- coefficients_values["seasonSpring"]
beta_seasonSummer <- coefficients_values["seasonSummer"]
beta_seasonWinter <- coefficients_values["seasonWinter"]
beta_expedited <- coefficients_values["is_expedited_deliveryTRUE"]

cat("Hệ số chặn (Beta 0):", beta_0, "\n")
cat("Hệ số coupon_discount:", beta_coupon, "\n")
cat("Hệ số delivery_charges:", beta_delivery_charges, "\n")
cat("Hệ số distance_to_nearest_warehouse:", beta_distance, "\n")
cat("Hệ số seasonSpring:", beta_seasonSpring, "\n")
cat("Hệ số seasonSummer:", beta_seasonSummer, "\n")
cat("Hệ số seasonWinter:", beta_seasonWinter, "\n")
cat("Hệ số is_expedited_deliveryTRUE:", beta_expedited, "\n")

regression_table <- tidy(best_model, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    term = c(
      "Intercept",
      "coupon_discount",
      "delivery_charges",
      "distance_to_nearest_warehouse",
      "seasonSpring",
      "seasonSummer",
      "seasonWinter",
      "is_expedited_deliveryTRUE"
    )
  )
print(regression_table)



plot_data <- tidy(best_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_label = case_when(
      term == "coupon_discount" ~ "Mức giảm giá (coupon_discount)",
      term == "delivery_charges" ~ "Phí vận chuyển (delivery_charges)",
      term == "distance_to_nearest_warehouse" ~ "Khoảng cách kho (distance_to_nearest_warehouse)",
      term == "seasonSpring" ~ "Mùa Xuân (seasonSpring)",
      term == "seasonSummer" ~ "Mùa Hạ (seasonSummer)",
      term == "seasonWinter" ~ "Mùa Đông (seasonWinter)",
      term == "is_expedited_deliveryTRUE" ~ "Giao hàng nhanh (is_expedited_delivery)"
    )
  )

ggplot(plot_data, aes(x = estimate, y = term_label)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "blue", linewidth = 0.8) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(
    title = "Biểu đồ khoảng tin cậy 95% của Odds Ratio",
    x = "Tỷ suất chênh lệch (Odds Ratio)",
    y = "Biến độc lập"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 9)
  )

vif_values <- vif(best_model)
print(vif_values)

cooks_d <- cooks.distance(best_model)
threshold <- 4 / nrow(train_data)
influential_obs <- which(cooks_d > threshold)
cat("Ngưỡng phát hiện (4/n):", threshold, "\n")
cat("Số lượng điểm có ảnh hưởng lớn:", length(influential_obs), "\n")

plot(cooks_d, type = "h", main = "Biểu đồ Khoảng cách Cook (Cook's Distance)",
     ylab = "Cook's distance", xlab = "Chỉ số quan sát (Observation Index)")
abline(h = threshold, col = "red", lty = 2) 

influential_indices <- which(cooks_d > threshold)
if (length(influential_indices) > 0) {
  text(influential_indices, cooks_d[influential_indices], 
       labels = influential_indices, pos = 3, col = "red", cex = 0.8)
}

# 計算 Logit 信賴區間
# 計算 Logit 變換
logit_transform <- function(p) {
  return(log(p / (1 - p)))
}

# 計算 Logit 的標準誤
logit_se <- function(p, n) {
  return(sqrt(1 / (n * p * (1 - p))))
}

# 計算 Logit 空間的信賴區間
logit_conf_interval <- function(logit_p, se, conf.level = 0.95) {
  alpha <- 1 - conf.level
  z_value <- qnorm(1 - alpha / 2)  # 取得 z 分位數
  logit_low <- logit_p - z_value * se
  logit_high <- logit_p + z_value * se
  return(c(logit_low, logit_high))
}

# 轉換 Logit 值回機率空間
logit_to_prob <- function(logit_value) {
  return(exp(logit_value) / (1 + exp(logit_value)))
}

# 計算 Logit 信賴區間的主函數
logit_ci <- function(k, n, conf.level = 0.95) {
  if (k < 0 | n <= 0 | k > n) stop("k 必須在 0 到 n 之間")
  
  # 計算估計的成功率
  p_hat <- k / n
  
  # 針對 p_hat = 0 或 1 進行調整，避免 log(0) 問題
  if (p_hat == 0) p_hat <- 1 / (2 * n)
  if (p_hat == 1) p_hat <- 1 - 1 / (2 * n)
  
  # 計算 logit 變換值
  logit_p <- logit_transform(p_hat)
  
  # 計算標準誤
  se <- logit_se(p_hat, n)
  
  # 計算 Logit 空間的信賴區間
  logit_bounds <- logit_conf_interval(logit_p, se, conf.level)
  
  # 轉換回機率空間
  p_low <- logit_to_prob(logit_bounds[1])
  p_high <- logit_to_prob(logit_bounds[2])
  
  return(c(lower = p_low, estimate = p_hat, upper = p_high))
}

N <- 100000
main <- function() {
    for (i in 1:20) {
        for (j in 1:N * 20) {
            logit_ci(k = i, n = j)
        }
    }
}
# 測試
print(system.time(
    main()
))
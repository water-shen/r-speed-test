# 計算 Logit 信賴區間
logit_ci <- function(k, n, conf.level = 0.95) {
    # 檢查數據合理性
    if (k < 0 | n <= 0 | k > n) stop("k 必須在 0 到 n 之間")

    # 估計成功機率 p_hat
    p_hat <- k / n

    # 針對 p_hat = 0 或 1 進行調整，避免 log(0) 問題
    if (p_hat == 0) p_hat <- 1 / (2 * n)
    if (p_hat == 1) p_hat <- 1 - 1 / (2 * n)

    # 計算 logit 值
    logit_p <- log(p_hat / (1 - p_hat))

    # 標準誤 (SE) 計算
    se_logit <- sqrt(1 / (n * p_hat * (1 - p_hat)))

    # 標準常態分布的 z 值
    alpha <- 1 - conf.level
    z_value <- qnorm(1 - alpha / 2)

    # 計算 logit 空間的信賴區間
    logit_low <- logit_p - z_value * se_logit
    logit_high <- logit_p + z_value * se_logit

    # 轉換回機率空間
    p_low <- exp(logit_low) / (1 + exp(logit_low))
    p_high <- exp(logit_high) / (1 + exp(logit_high))

    # 返回結果
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

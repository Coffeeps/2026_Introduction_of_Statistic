# ============================================================
#  量化分析：統計學入門（第二講）— R 資料集與語法對照
#  對應講義 Unit 08–13
#  在 RStudio 中逐段執行（Ctrl+Enter）
# ============================================================
# 支援系統字型：macOS (Heiti TC) & Windows (Microsoft JhengHei)
# ============================================================

# ── 安裝必要套件（第一次使用時執行）──────────────────────
install.packages(c("ggplot2", "dplyr", "tidyr", "patchwork",
                   "scales", "broom", "effsize"))

# ── 環境設定 ─────────────────────────────────────────────
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

curr_os <- Sys.info()["sysname"]
my_font <- switch(curr_os,
                  "Darwin"  = "Heiti TC",
                  "Windows" = "Microsoft JhengHei",
                  "sans")

theme_set(theme_minimal(base_size = 13, base_family = my_font))
update_geom_defaults("text",  list(family = my_font))
update_geom_defaults("label", list(family = my_font))

cat("目前系統：", curr_os, "，已載入字體：", my_font, "\n")

# ============================================================
# UNIT 08：機率基礎 — 常態分配與 Z 分數
# ============================================================

# ── 8-1 常態分配曲線 ──
x <- seq(-4, 4, length.out = 300)
y <- dnorm(x)
df_norm <- data.frame(x = x, y = y)

ggplot(df_norm, aes(x, y)) +
  geom_line(linewidth = 1.2, color = "#1a1814") +
  geom_area(data = subset(df_norm, x >= -1 & x <= 1),
            aes(x, y), fill = "#1a6b6b", alpha = 0.4) +
  geom_area(data = subset(df_norm, (x >= -2 & x < -1) | (x > 1 & x <= 2)),
            aes(x, y), fill = "#c8e0e0", alpha = 0.5) +
  annotate("text", x = 0,    y = 0.18, label = "68%",
           color = "white", fontface = "bold", size = 5, family = my_font) +
  annotate("text", x = -1.5, y = 0.06, label = "13.5%",
           color = "#1a6b6b", size = 3.5, family = my_font) +
  annotate("text", x =  1.5, y = 0.06, label = "13.5%",
           color = "#1a6b6b", size = 3.5, family = my_font) +
  scale_x_continuous(breaks = -3:3,
                     labels = c("μ-3σ","μ-2σ","μ-σ","μ","μ+σ","μ+2σ","μ+3σ")) +
  labs(title = "常態分配與 68-95-99.7 法則",
       x = "", y = "機率密度")

# ── 8-2 Z 分數計算 ──
# 例：班級平均 70，標準差 10，某同學得 80
mu <- 70; sigma <- 10; x_val <- 80
z <- (x_val - mu) / sigma
cat("Z 分數 =", z, "\n")
cat("解讀：該同學成績高於平均", z, "個標準差\n")
cat("高於此分數的比例：", round((1 - pnorm(z)) * 100, 1), "%\n")

# ── 8-3 p 值的直觀理解 ──
# 若 H0: μ=70，觀察到 X̄=75，n=25，s=10
xbar <- 75; mu0 <- 70; s <- 10; n <- 25
t_stat <- (xbar - mu0) / (s / sqrt(n))
p_val  <- 2 * pt(-abs(t_stat), df = n - 1)   # 雙尾
cat("\nt 統計量 =", round(t_stat, 3),
    "\np 值 =",    round(p_val, 4),
    "\n結論：", ifelse(p_val < 0.05, "拒絕 H0（顯著）", "無法拒絕 H0"), "\n")

# ============================================================
# UNIT 09：抽樣方法與抽樣分配 — 中央極限定理
# ============================================================

# ── 9-1 模擬中央極限定理（從右偏母體重複抽樣）──
set.seed(42)
n_sim <- 5000  # 模擬次數

means_n10  <- replicate(n_sim, mean(rexp(10,  rate = 1)))
means_n50  <- replicate(n_sim, mean(rexp(50,  rate = 1)))
means_n200 <- replicate(n_sim, mean(rexp(200, rate = 1)))

df_clt <- data.frame(
  means = c(means_n10, means_n50, means_n200),
  n     = rep(c("n = 10", "n = 50", "n = 200"), each = n_sim)
)
df_clt$n <- factor(df_clt$n, levels = c("n = 10", "n = 50", "n = 200"))

ggplot(df_clt, aes(x = means, fill = n)) +
  geom_histogram(binwidth = 0.04, color = "white", alpha = 0.85) +
  facet_wrap(~n, ncol = 3) +
  scale_fill_manual(values = c("#e8c5bb", "#c8e0e0", "#1a6b6b")) +
  labs(title = "中央極限定理：樣本平均數的抽樣分配",
       subtitle = "母體為右偏的指數分配，但 n 大時 X̄ 趨近常態",
       x = "樣本平均數 X̄", y = "次數") +
  theme(legend.position = "none")

# ── 9-2 標準誤示範：n 增大，SE 縮小 ──
cat("\n=== 標準誤（SE = σ/√n）===\n")
sigma_pop <- 20
for (n_val in c(25, 100, 400, 1000)) {
  se <- sigma_pop / sqrt(n_val)
  ci_half <- 1.96 * se
  cat(sprintf("n = %4d  →  SE = %5.2f  →  95%% CI 寬度 ≈ ±%.2f\n",
              n_val, se, ci_half))
}

# ============================================================
# UNIT 10：統計估計與信賴區間
# ============================================================

# ── 10-1 手動計算 95% 信賴區間 ──
set.seed(42)
scores_ci <- round(rnorm(50, mean = 75, sd = 10))

n_ci    <- length(scores_ci)
xbar_ci <- mean(scores_ci)
s_ci    <- sd(scores_ci)
se_ci   <- s_ci / sqrt(n_ci)
t_crit  <- qt(0.975, df = n_ci - 1)           # t 臨界值

ci_lower <- xbar_ci - t_crit * se_ci
ci_upper <- xbar_ci + t_crit * se_ci

cat("\n=== 95% 信賴區間 ===\n")
cat("樣本平均數 X̄  =", round(xbar_ci, 2), "\n")
cat("樣本標準差 s  =", round(s_ci, 2), "\n")
cat("樣本數 n      =", n_ci, "\n")
cat("標準誤 SE     =", round(se_ci, 2), "\n")
cat("t 臨界值      =", round(t_crit, 3), "\n")
cat("95% CI        = [", round(ci_lower, 2), ",", round(ci_upper, 2), "]\n")

# ── 10-2 用 t.test() 快速計算（結果應與上面相同）──
result_ci <- t.test(scores_ci)
cat("\n=== 用 t.test() 驗證 ===\n")
cat("95% CI：[", round(result_ci$conf.int, 2), "]\n")

# ── 10-3 視覺化：不同樣本數的 CI 寬度 ──
set.seed(7)
ci_demo <- do.call(rbind, lapply(c(25, 50, 100, 200, 400), function(n_val) {
  x_tmp  <- rnorm(n_val, mean = 75, sd = 10)
  res    <- t.test(x_tmp)
  data.frame(n     = n_val,
             xbar  = mean(x_tmp),
             lower = res$conf.int[1],
             upper = res$conf.int[2])
}))

ggplot(ci_demo, aes(x = factor(n), y = xbar, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 75, color = "#b8341b",
             linetype = "dashed", linewidth = 1) +
  geom_pointrange(color = "#1a6b6b", linewidth = 1, size = 0.7) +
  annotate("text", x = 0.6, y = 75.4, label = "真實 μ = 75",
           color = "#b8341b", hjust = 0, size = 3.5, family = my_font) +
  labs(title = "樣本數對 95% 信賴區間寬度的影響",
       x = "樣本數 n", y = "95% CI",
       caption = "n 越大，CI 越窄，估計越精確")

# ============================================================
# UNIT 11：假設檢定深入 — 效果量
# ============================================================

# ── 11-1 Cohen's d 效果量 ──
library(effsize)

set.seed(42)
scores_full <- data.frame(
  student_id = 1:50,
  tutoring   = sample(c("有補習", "未補習"), 50, replace = TRUE),
  score      = c(round(rnorm(25, mean = 78, sd = 9)),
                 round(rnorm(25, mean = 72, sd = 11)))
)
scores_full$score <- pmax(pmin(scores_full$score, 100), 40)

tutoring_yes <- scores_full %>% filter(tutoring == "有補習") %>% pull(score)
tutoring_no  <- scores_full %>% filter(tutoring == "未補習") %>% pull(score)

cat("\n=== t 檢定 + 效果量 ===\n")
t_res <- t.test(tutoring_yes, tutoring_no)
d_res <- cohen.d(tutoring_yes, tutoring_no)

cat("t =", round(t_res$statistic, 3), "\n")
cat("p =", round(t_res$p.value, 4), "\n")
cat("Cohen's d =", round(abs(d_res$estimate), 3),
    "→", ifelse(abs(d_res$estimate) >= 0.8, "大效果",
         ifelse(abs(d_res$estimate) >= 0.5, "中效果", "小效果")), "\n")

# ── 11-2 統計檢定力（Power）示範 ──
# 若 d = 0.5（中效果），α = 0.05，n = 50，檢定力是多少？
power_result <- power.t.test(n = 50, delta = 0.5, sd = 1,
                              sig.level = 0.05, type = "two.sample")
cat("\n=== 統計檢定力 ===\n")
cat("效果量 d = 0.5，n = 50，α = 0.05\n")
cat("統計檢定力（Power）=", round(power_result$power, 3), "\n")
cat("解讀：若真實效果存在，有", round(power_result$power*100, 1),
    "% 的機率能偵測到\n")

# ── 11-3 要達到 80% 檢定力，需要多少樣本？──
n_needed <- power.t.test(delta = 0.5, sd = 1, sig.level = 0.05,
                          power = 0.80, type = "two.sample")
cat("\n達到 80% 檢定力所需樣本數（每組）=",
    ceiling(n_needed$n), "\n")

# ============================================================
# UNIT 12：相關分析與簡單線性迴歸
# ============================================================

# ── 12-1 建立迴歸用資料集 ──
set.seed(2024)
n_reg <- 100
reg_data <- data.frame(
  edu_years  = round(runif(n_reg, 9, 18)),
  work_years = round(runif(n_reg, 0, 25)),
  gender     = sample(c(0, 1), n_reg, replace = TRUE)   # 0=男, 1=女
)
reg_data$salary <- 12 + 2.5 * reg_data$edu_years +
                       1.2 * reg_data$work_years -
                       3.8 * reg_data$gender + rnorm(n_reg, 0, 8)
reg_data$gender_label <- ifelse(reg_data$gender == 0, "男性", "女性")

# ── 12-2 相關分析 ──
cat("\n=== Pearson 相關分析 ===\n")
cor_result <- cor.test(reg_data$edu_years, reg_data$salary)
cat("r  =", round(cor_result$estimate, 3), "\n")
cat("p  =", round(cor_result$p.value,  4), "\n")
cat("95% CI of r: [", round(cor_result$conf.int, 3), "]\n")
cat("解讀：教育年數與月薪呈",
    ifelse(cor_result$estimate > 0, "正", "負"), "相關，",
    ifelse(abs(cor_result$estimate) >= 0.6, "強", "中度"), "相關強度\n")

# ── 12-3 散佈圖 + 迴歸線 ──
ggplot(reg_data, aes(x = edu_years, y = salary)) +
  geom_point(alpha = 0.5, color = "#1a6b6b", size = 2) +
  geom_smooth(method = "lm", color = "#b8341b",
              fill = "#e8c5bb", alpha = 0.2, se = TRUE) +
  labs(title = "教育年數與月薪的散佈圖（含迴歸線與 95% CI）",
       x = "教育年數", y = "月薪（萬元）",
       caption = paste0("r = ", round(cor_result$estimate, 3),
                        "，p < 0.001"))

# ── 12-4 簡單線性迴歸 ──
model_simple <- lm(salary ~ edu_years, data = reg_data)
cat("\n=== 簡單線性迴歸：salary ~ edu_years ===\n")
print(summary(model_simple))

cat("\n--- 逐行解讀 ---\n")
coef_s <- summary(model_simple)$coefficients
cat("截距 b0 =", round(coef_s[1,1], 3),
    "（當 edu_years = 0 時的預測月薪，通常無實質意義）\n")
cat("斜率 b1 =", round(coef_s[2,1], 3),
    "（教育年數每增加 1 年，月薪平均增加", round(coef_s[2,1], 2), "萬元）\n")
cat("R²      =", round(summary(model_simple)$r.squared, 4),
    "（教育年數解釋了月薪", round(summary(model_simple)$r.squared*100, 1), "% 的變異）\n")

# ── 12-5 殘差診斷圖（在 RStudio 中執行會顯示 4 張圖）──
par(mfrow = c(2, 2))
plot(model_simple)
par(mfrow = c(1, 1))   # 還原為單圖
cat("\n殘差診斷說明：\n")
cat("圖1（Residuals vs Fitted）：殘差應隨機散布，無明顯曲線→確認線性假設\n")
cat("圖2（Q-Q Plot）          ：點應貼近對角線→確認殘差常態性\n")
cat("圖3（Scale-Location）    ：殘差散布應均勻→確認等變異性\n")
cat("圖4（Cook's Distance）   ：無特別大的點→確認無強影響點\n")

# ============================================================
# UNIT 13：複迴歸分析
# ============================================================

# ── 13-1 多元線性迴歸 ──
model_multi <- lm(salary ~ edu_years + work_years + gender, data = reg_data)
cat("\n=== 複迴歸：salary ~ edu_years + work_years + gender ===\n")
print(summary(model_multi))

cat("\n--- 係數解讀（其他條件相同的情況下）---\n")
coef_m <- summary(model_multi)$coefficients
cat("edu_years  b =", round(coef_m["edu_years", 1], 3),
    "→ 教育年數每多 1 年，月薪平均多", round(coef_m["edu_years", 1], 2), "萬元\n")
cat("work_years b =", round(coef_m["work_years", 1], 3),
    "→ 年資每多 1 年，月薪平均多", round(coef_m["work_years", 1], 2), "萬元\n")
cat("gender     b =", round(coef_m["gender", 1], 3),
    "→ 女性比男性月薪平均少", abs(round(coef_m["gender", 1], 2)), "萬元\n")
cat("Adjusted R² =", round(summary(model_multi)$adj.r.squared, 4),
    "（三個變數共同解釋",
    round(summary(model_multi)$adj.r.squared * 100, 1), "% 的月薪變異）\n")

# ── 13-2 比較簡單迴歸與複迴歸 ──
cat("\n=== 模型比較 ===\n")
cat("簡單迴歸 R²       =", round(summary(model_simple)$r.squared, 4), "\n")
cat("複迴歸 Adjusted R²=", round(summary(model_multi)$adj.r.squared, 4), "\n")
cat("加入工作年資與性別後，解釋力提升\n")

# ── 13-3 係數圖（含 95% 信賴區間）──
library(broom)
tidy_model <- tidy(model_multi, conf.int = TRUE)

ggplot(tidy_model[-1, ],          # 移除截距
       aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high,
           color = ifelse(p.value < 0.05, "顯著", "不顯著"))) +
  geom_pointrange(size = 0.7, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "#b8341b", linewidth = 1) +
  scale_color_manual(values = c("顯著" = "#1a6b6b", "不顯著" = "#7a756e"),
                     name = NULL) +
  scale_y_discrete(labels = c("edu_years" = "教育年數",
                               "work_years" = "工作年資",
                               "gender" = "性別（女=1）")) +
  labs(title = "複迴歸係數圖（含 95% 信賴區間）",
       subtitle = "虛線為 0，若 CI 跨越 0 則未達顯著",
       x = "係數估計值（β）", y = "自變數")

# ── 13-4 虛擬變數示範：職業類別 ──
set.seed(303); n_dummy <- 80
career_levels <- c("軍公教", "私部門管理", "私部門基層", "農林漁牧", "其他")
dummy_data <- data.frame(
  career    = sample(career_levels, n_dummy, replace = TRUE),
  edu_years = round(runif(n_dummy, 9, 18))
)
career_effect <- c("軍公教" = 8, "私部門管理" = 15,
                   "私部門基層" = 3, "農林漁牧" = -2, "其他" = 0)
dummy_data$salary <- 20 + 2.0 * dummy_data$edu_years +
                     career_effect[dummy_data$career] + rnorm(n_dummy, 0, 6)

cat("\n=== 虛擬變數迴歸（以「其他」為參照組）===\n")
model_dummy <- lm(salary ~ edu_years +
                    relevel(factor(career), ref = "其他"),
                  data = dummy_data)
print(summary(model_dummy))

cat("\n--- 虛擬變數係數解讀 ---\n")
cat("每個職業係數代表：相較於「其他」職業，在控制教育年數之後，\n")
cat("各職業月薪平均高出（或低於）的萬元數。\n")

# ── 13-5 多元共線性診斷（VIF）──
library(car)
cat("\n=== 多元共線性診斷（VIF）===\n")
vif_values <- vif(model_multi)
print(round(vif_values, 2))
cat("\nVIF < 5：無嚴重共線性\nVIF 5–10：需注意\nVIF > 10：嚴重共線性，需處理\n")

cat("\n✓ 第二講 R 語法全部執行完畢。\n")

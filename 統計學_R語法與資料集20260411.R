# ============================================================
#  量化分析：統計學入門 — R 資料集與語法對照
#  對應講義各單元練習
#  在 RStudio 中逐段執行（Ctrl+Enter）
# ============================================================
# 支援系統：macOS (Heiti TC) & Windows (Microsoft JhengHei)
# ============================================================
###### 安裝必要套件（第一次使用時執行）
install.packages(c("ggplot2", "dplyr", "psych", "car", "moments"))
# 色碼表參考網址：https://www.toodoo.com/db/color.html
# ============================================================

# ── 1. 環境設定 ───────────────────────────────
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

# 自動判定作業系統字體
curr_os <- Sys.info()["sysname"]
my_font <- switch(curr_os,
                  "Darwin"  = "Heiti TC",           # Mac 專用
                  "Windows" = "Microsoft JhengHei", # Windows 專用
                  "sans")                           # 其他

# 設定主題與文字預設字體
theme_set(theme_minimal(base_size = 13, base_family = my_font))
update_geom_defaults("text",  list(family = my_font))
update_geom_defaults("label", list(family = my_font))

cat("目前系統：", curr_os, "，已載入字體：", my_font, "\n")

# ============================================================
# DATASET 1：成績資料集（Unit 06 集中趨勢與離散趨勢）
# ============================================================
set.seed(42)
scores <- data.frame(
  student_id = 1:50,
  gender     = sample(c("男", "女"), 50, replace = TRUE, prob = c(0.5, 0.5)),
  tutoring   = sample(c("有補習", "未補習"), 50, replace = TRUE, prob = c(0.5, 0.5)),
  score      = c(
    round(rnorm(25, mean = 78, sd = 9)),   # 有補習組
    round(rnorm(25, mean = 72, sd = 11))   # 未補習組
  )
)
scores$score <- pmax(pmin(scores$score, 100), 40)

# ── 1-1 集中趨勢 ──
cat("=== 集中趨勢 ===\n")
cat("平均數 Mean  :", mean(scores$score), "\n")
cat("中位數 Median:", median(scores$score), "\n")
mode_val <- as.numeric(names(sort(table(scores$score), decreasing = TRUE)[1]))
cat("眾數   Mode  :", mode_val, "\n")

# ── 1-2 離散趨勢 ──
cat("\n=== 離散趨勢 ===\n")
cat("變異數 Variance:", var(scores$score), "\n")
cat("標準差 SD      :", sd(scores$score),  "\n")
cat("全距   Range   :", range(scores$score), "\n")
cat("\n=== 完整摘要 ===\n")
print(summary(scores$score))

# ── 1-3 次數分配表 ──
cat("\n=== 次數分配表（成績區間） ===\n")
breaks <- c(40, 50, 60, 70, 80, 90, 100)
freq_table <- cut(scores$score, breaks = breaks, right = TRUE,
                  labels = c("40-50","51-60","61-70","71-80","81-90","91-100"))
print(table(freq_table))

# ── 1-4 直方圖 ──
ggplot(scores, aes(x = score)) +
  geom_histogram(binwidth = 5, fill = "#FFC0CB", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean(score)), color = "#c4870a", linewidth = 1.2, linetype = "dashed") +
  geom_vline(aes(xintercept = median(score)), color = "#1a6b6b", linewidth = 1.2, linetype = "dotted") +
  annotate("text", x = mean(scores$score) + 1, y = 8, label = paste("Mean =", round(mean(scores$score),1)),
           color = "#c4870a", hjust = 0, size = 3.5, family = my_font) +
  annotate("text", x = median(scores$score) - 1, y = 9, label = paste("Median =", median(scores$score)),
           color = "#1a6b6b", hjust = 1, size = 3.5, family = my_font) +
  labs(title = "成績分配直方圖", x = "成績", y = "人數")

# ============================================================
# DATASET 2：滿意度資料集（Unit 05 次數分配表）
# ============================================================
set.seed(7)
satisfaction <- data.frame(
  student_id = 1:50,
  rating     = sample(1:5, 50, replace = TRUE, prob = c(0.06, 0.14, 0.30, 0.36, 0.14))
)
rating_labels <- c("1 非常不滿意", "2 不滿意", "3 普通", "4 滿意", "5 非常滿意")

cat("\n=== 滿意度次數分配表 ===\n")
freq <- table(factor(satisfaction$rating, levels = 1:5, labels = rating_labels))
rel_freq  <- prop.table(freq) * 100
cum_freq  <- cumsum(freq)
cum_pct   <- cumsum(rel_freq)

result_sat <- data.frame(
  次數     = as.numeric(freq),
  相對次數 = paste0(round(rel_freq, 1), "%"),
  累積次數 = as.numeric(cum_freq),
  累積百分比 = paste0(round(cum_pct, 1), "%")
)
rownames(result_sat) <- rating_labels
print(result_sat)
cat("合計:", sum(freq), "\n")

# 長條圖 (圖二的前身)
ggplot(satisfaction, aes(x = factor(rating, labels = rating_labels))) +
  geom_bar(fill = c("#c62828","#ef9a9a","#ede7db","#80cbc4","#00695c"), color = "white") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.4, size = 4, family = my_font) +
  labs(title = "線上學習滿意度分配長條圖", x = "滿意度", y = "人數") +
  theme(axis.text.x = element_text(angle = 10, hjust = 1))

# ============================================================
# DATASET 3：單樣本 t 檢定（Unit 07）
# ============================================================
set.seed(101)
class_scores <- round(rnorm(30, mean = 74, sd = 8))
class_scores <- pmax(pmin(class_scores, 100), 50)

cat("\n=== 單樣本 t 檢定 ===\n")
cat("樣本平均數:", mean(class_scores), "\n")
cat("樣本標準差:", round(sd(class_scores), 2), "\n")
cat("樣本數 n  :", length(class_scores), "\n\n")

t_result <- t.test(class_scores, mu = 70, alternative = "two.sided")
print(t_result)

cat("\n--- 解讀 ---\n")
cat("t 值:", round(t_result$statistic, 3), "\n")
cat("df  :", t_result$parameter, "\n")
cat("p 值:", round(t_result$p.value, 4), "\n")
if (t_result$p.value < 0.05) { cat("結論：p < 0.05，拒絕 H₀\n") } else { cat("結論：p ≥ 0.05，無法拒絕 H₀\n") }

# ============================================================
# DATASET 4：獨立樣本 t 檢定（Unit 07）
# ============================================================
tutoring_yes <- scores %>% filter(tutoring == "有補習") %>% pull(score)
tutoring_no  <- scores %>% filter(tutoring == "未補習") %>% pull(score)

cat("\n=== 獨立樣本 t 檢定 ===\n")
t_ind <- t.test(tutoring_yes, tutoring_no, var.equal = FALSE)
print(t_ind)

# 箱形圖（Box plot）
summary_bar <- scores %>% group_by(tutoring) %>%
  summarise(mean_score = round(mean(score), 1), se = sd(score) / sqrt(n()), .groups = "drop")

ggplot(scores, aes(x = tutoring, y = score, fill = tutoring)) +
  geom_boxplot(alpha = 0.75, width = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c("有補習" = "#b8341b", "未補習" = "#1a6b6b")) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "white") +
  labs(title = "有補習 vs 未補習成績比較",
       subtitle = paste0("t = ", round(t_ind$statistic, 2), "，p = ", round(t_ind$p.value, 3)),
       x = "補習狀況", y = "期末成績") + theme(legend.position = "none")

# ============================================================
# DATASET 5：成對樣本 t 檢定（Unit 07）
# ============================================================
set.seed(55); n_paired <- 20
anxiety_data <- data.frame(
  student_id = 1:n_paired,
  before     = round(runif(n_paired, min = 5, max = 9)),
  after      = NA
)
anxiety_data$after <- pmax(1, anxiety_data$before - round(rnorm(n_paired, mean = 1.8, sd = 0.8)))
anxiety_data$diff  <- anxiety_data$after - anxiety_data$before

cat("\n=== 成對樣本 t 檢定 ===\n")
cat("平均差值 d̄  :", round(mean(anxiety_data$diff), 2), "\n")
t_paired <- t.test(anxiety_data$after, anxiety_data$before, paired = TRUE)
print(t_paired)

anxiety_long <- anxiety_data %>%
  select(student_id, before, after) %>%
  pivot_longer(cols = c(before, after), names_to = "time", values_to = "anxiety") %>%
  mutate(time = factor(time, levels = c("before","after"), labels = c("課前","課後")))

ggplot(anxiety_long, aes(x = time, y = anxiety, group = student_id)) +
  geom_line(alpha = 0.3, color = "#7a756e") +
  geom_point(aes(color = time), size = 2.5, alpha = 0.8) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 2, color = "#b8341b") +
  scale_color_manual(values = c("課前" = "#c4870a", "課後" = "#1a6b6b")) +
  labs(title = "課前 vs 課後統計焦慮分數", x = "測量時間點", y = "焦慮分數") + theme(legend.position = "none")

# ============================================================
# DATASET 6：單因子 ANOVA（Unit 07）
# ============================================================
set.seed(99)
anova_data <- data.frame(
  method = rep(c("傳統講授", "翻轉教室", "線上自學"), each = 20),
  score  = c(round(rnorm(20, mean = 70, sd = 8)), round(rnorm(20, mean = 77, sd = 8)), round(rnorm(20, mean = 73, sd = 9)))
)
anova_data$score <- pmax(pmin(anova_data$score, 100), 40)

cat("\n=== 單因子 ANOVA ===\n")
aov_result <- aov(score ~ method, data = anova_data)
print(summary(aov_result))
cat("\n事後比較 Tukey HSD：\n")
print(TukeyHSD(aov_result))

ggplot(anova_data, aes(x = method, y = score, fill = method)) +
  geom_boxplot(alpha = 0.75, width = 0.5) +
  geom_jitter(width = 0.12, alpha = 0.4, size = 1.5) +
  scale_fill_manual(values = c("傳統講授" = "#e8c5bb", "翻轉教室" = "#1a6b6b", "線上自學" = "#f5e6c5")) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#b8341b") +
  labs(title = "三種教學法的期末成績比較（ANOVA）", x = "教學法", y = "成績") + theme(legend.position = "none")

# ============================================================
# DATASET 7：卡方檢定（Unit 07）
# ============================================================
chi_data <- data.frame(
  gender  = c(rep("男", 100), rep("女", 100)),
  support = c(rep("支持", 55), rep("不支持", 45), rep("支持", 69), rep("不支持", 31))
)
cat("\n=== 卡方檢定 ===\n")
cross_tab <- table(chi_data$gender, chi_data$support)
print(chisq.test(cross_tab))

chi_pct <- chi_data %>% group_by(gender, support) %>% summarise(n = n(), .groups = "drop") %>% group_by(gender) %>% mutate(pct = n / sum(n) * 100)

ggplot(chi_pct, aes(x = gender, y = pct, fill = support)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = paste0(round(pct, 0), "%")), position = position_fill(vjust = 0.5), size = 4.5, color = "white", fontface = "bold", family = my_font) +
  scale_fill_manual(values = c("支持" = "#1a6b6b", "不支持" = "#b8341b")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "性別與同性婚姻支持度（卡方檢定）堆疊圖", x = "性別", y = "百分比")

# ============================================================
# DATASET 8：線性迴歸（Unit 07）
# ============================================================
set.seed(2024); n_reg <- 100
reg_data <- data.frame(
  edu_years  = round(runif(n_reg, 9, 18)),
  work_years = round(runif(n_reg, 0, 25)),
  gender     = sample(c(0, 1), n_reg, replace = TRUE)
)
reg_data$salary <- 12 + 2.5*reg_data$edu_years + 1.2*reg_data$work_years - 3.8*reg_data$gender + rnorm(n_reg, 0, 8)
model <- lm(salary ~ edu_years + work_years + gender, data = reg_data)

cat("\n=== 多元線性迴歸 ===\n")
print(summary(model))
cat("\n--- 係數解讀 ---\n")
print(round(as.data.frame(summary(model)$coefficients), 4))

reg_data$gender_label <- ifelse(reg_data$gender == 0, "男性", "女性")
ggplot(reg_data, aes(x = edu_years, y = salary, color = gender_label)) +
  geom_point(alpha = 0.5, size = 2) + geom_smooth(method = "lm") +
  scale_color_manual(values = c("男性" = "#1a6b6b", "女性" = "#b8341b")) +
  labs(title = "教育年數與月薪的關係（迴歸分析）", x = "教育年數", y = "月薪", color = "性別")

# ============================================================
# DATASET 9：虛擬變數（Unit 07 迴歸補充）
# ============================================================
set.seed(303); n_dummy <- 80
career_levels <- c("軍公教", "私部門管理", "私部門基層", "農林漁牧", "其他")
dummy_data <- data.frame(career = sample(career_levels, n_dummy, replace = TRUE), edu_years = round(runif(n_dummy, 9, 18)))
career_effect <- c("軍公教"=8, "私部門管理"=15, "私部門基層"=3, "農林漁牧"=-2, "其他"=0)
dummy_data$salary <- 20 + 2.0*dummy_data$edu_years + career_effect[dummy_data$career] + rnorm(n_dummy, 0, 6)

cat("\n=== 職業類別虛擬變數迴歸 ===\n")
print(summary(lm(salary ~ edu_years + relevel(factor(career), ref="其他"), data = dummy_data)))

ggplot(dummy_data, aes(x = reorder(career, salary, median), y = salary, fill = career)) +
  geom_boxplot(alpha = 0.75, width = 0.55) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#b8341b") +
  scale_fill_manual(values = c("軍公教"="#c8e0e0","私部門管理"="#1a6b6b","私部門基層"="#e8c5bb","農林漁牧"="#f5e6c5","其他"="#ede7db")) +
  labs(title = "各職業類別月薪分布", x = "職業類別", y = "月薪") + theme(legend.position = "none")

# ============================================================
# DATASET 10：視覺化範例(圖一 ~ 圖十)
# ============================================================

# 圖一：圓餅圖（Pie Chart）
pie_data <- anova_data %>%
  count(method) %>%
  mutate(pct = n / sum(n) * 100,
         label = paste0(method, "\n", n, "人 (", round(pct, 0), "%)"))

ggplot(pie_data, aes(x = "", y = pct, fill = method)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.8) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), 
            size = 4, fontface = "bold", color = "white", family = my_font) +
  scale_fill_manual(values = c("傳統講授" = "#b8341b", "翻轉教室" = "#1a6b6b", "線上自學" = "#c4870a")) +
  labs(title = "圖一：三種教學法的樣本分布（圓餅圖）", 
       caption = "注意：類別超過5個或差異微小時，不建議使用圓餅圖") +
  theme_void(base_family = my_font) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))

# 圖二：長條圖（Bar Chart）
ggplot(summary_bar, aes(x = tutoring, y = mean_score, fill = tutoring)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.15) +
  geom_text(aes(label = mean_score), vjust = -1.2, size = 4.5, fontface = "bold", family = my_font) +
  scale_fill_manual(values = c("有補習" = "#b8341b", "未補習" = "#1a6b6b")) +
  labs(title = "圖二：有補習 vs 未補習的平均成績（長條圖）")

# 圖三：分組長條圖（Grouped Bar Chart）
gender_summary <- scores %>% group_by(gender, tutoring) %>% summarise(mean_score = round(mean(score), 1), .groups = "drop")
ggplot(gender_summary, aes(x = gender, y = mean_score, fill = tutoring)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = mean_score), position = position_dodge(width = 0.7), vjust = -0.5, family = my_font) +
  scale_fill_manual(values = c("有補習"="#b8341b", "未補習"="#1a6b6b")) +
  labs(title = "圖三：性別 × 補習狀況的平均成績")

# 圖四：直方圖（Histogram）— 多組對比
ggplot(scores, aes(x = score, fill = tutoring)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.75, position = "identity") +
  facet_wrap(~tutoring, ncol = 1) +
  scale_fill_manual(values = c("有補習" = "#b8341b", "未補習" = "#1a6b6b")) +
  labs(title = "圖四：兩組成績分配直方圖（分面顯示）")

# 圖五：箱形圖基本款（Box Plot）
ggplot(anova_data, aes(x = method, y = score, fill = method)) +
  geom_boxplot(alpha = 0.75, width = 0.5) +
  scale_fill_manual(values = c("傳統講授" = "#e8c5bb", "翻轉教室" = "#1a6b6b", "線上自學" = "#f5e6c5")) +
  labs(title = "圖五：三種教學法成績的箱形圖")

# 圖六：箱形圖 + 資料點）
ggplot(anova_data, aes(x = method, y = score, fill = method)) +
  geom_boxplot(alpha = 0.5, width = 0.45, outlier.shape = NA) +
  geom_jitter(aes(color = method), width = 0.15, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
  labs(title = "圖六：箱形圖 + 原始資料點 (Jitter)")

# 圖七：小提琴圖（Violin Plot）
ggplot(anova_data, aes(x = method, y = score, fill = method)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.12, fill = "white", alpha = 0.9) +
  labs(title = "圖七：小提琴圖 (Violin Plot)")

# 圖八：箱形圖 — 分佈說明
one_group <- anova_data %>% filter(method == "翻轉教室")
q1 <- quantile(one_group$score, 0.25); med <- median(one_group$score); q3 <- quantile(one_group$score, 0.75)
ggplot(one_group, aes(x = "翻轉教室", y = score)) +
  geom_boxplot(fill = "#c8e0e0", color = "#1a6b6b", width = 0.35) +
  annotate("text", x = 1.32, y = q1, label = paste0("Q1 = ", q1), family = my_font) +
  annotate("text", x = 1.32, y = med, label = paste0("中位數 = ", med), family = my_font, color = "red", fontface="bold") +
  annotate("text", x = 1.32, y = q3, label = paste0("Q3 = ", q3), family = my_font) +
  labs(title = "圖八：箱形圖各部位說明")

# 圖九：圓餅圖 vs 長條圖 對比
job_demo <- data.frame(職業 = c("軍公教", "私部門", "農林漁牧", "其他"), 人數 = c(15, 45, 10, 30)) %>% mutate(pct = 人數/sum(人數)*100)
p_pie <- ggplot(job_demo, aes(x="", y=pct, fill=職業)) + geom_bar(stat="identity") + coord_polar(theta="y") + theme_void(base_family = my_font)
p_bar <- ggplot(job_demo, aes(x=職業, y=人數, fill=職業)) + geom_bar(stat="identity") + theme(legend.position="none")
p_pie + p_bar + plot_annotation(title = "圖九：圓餅圖 vs 長條圖對比", theme = theme(plot.title = element_text(family = my_font, hjust=0.5)))

# 圖十：滿意度資料 — 堆疊長條圖
rating_labels <- c("非常不滿意","不滿意","普通","滿意","非常滿意")
sat_pct <- data.frame(group = rep(c("實體", "線上"), each=5), rating = rep(rating_labels, 2), pct = c(5,10,25,40,20, 10,20,30,25,15))
ggplot(sat_pct, aes(x = group, y = pct, fill = rating)) +
  geom_bar(stat = "identity", position = "fill", width = 0.55) +
  geom_text(aes(label = paste0(pct, "%")), position = position_fill(vjust = 0.5), family = my_font, color = "white") +
  scale_fill_manual(values = c("#c62828","#ef9a9a","#ede7db","#80cbc4","#00695c")) +
  labs(title = "圖十：實體 vs 線上課程滿意度分布") + theme(legend.position = "bottom")

cat("\n 執行完畢。\n")

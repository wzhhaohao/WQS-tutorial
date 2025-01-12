## R_prog_24.6
# 文件名：diagnostics.R

# 载入数据
df = read.csv("./data.csv")
continous = c("RIDAGEYR", "BMXBMI", "INDFMPIR", "PAD680")  # 连续变量列表

# 为什么用 Kruskal-Wallis 检验替代 t 检验？
# Kruskal-Wallis 检验适用于不满足 t 检验假设的情况
# 通过正态性、方差齐性以及异常值的检查，判断是否满足 t 检验的要求

# 正态性检验
# 1. 绘制直方图和密度图，直观观察变量分布是否接近正态分布
for (var in continous) {
  hist(df[[var]], main = paste("Histogram of", var), xlab = var)  # 绘制直方图
  plot(density(df[[var]], na.rm = TRUE), main = paste("Density plot of", var))  # 绘制密度图
}

# 2. 利用 Q-Q 图检查变量是否接近正态分布
for (var in continous) {
  qqnorm(df[[var]], main = paste("Q-Q Plot of", var))  # 绘制 Q-Q 图
  qqline(df[[var]], col = "red")  # 添加参考线
}

# 方差齐性检验
# 使用 Levene 检验评估不同组间方差是否相等，这是 t 检验的前提之一
library(car)
for (var in continous) {
  print(var)  # 打印变量名称
  print(leveneTest(as.formula(paste(var, "~ Sarcopenia")), data = df))  # 运行 Levene 检验
}

# 异常值检查
# 通过四分位距（IQR）计算上下界，识别并统计异常值的数量
for (var in continous) {
  Q1 = quantile(df[[var]], 0.25, na.rm = TRUE)  # 计算第 1 四分位数（25% 分位数）
  Q3 = quantile(df[[var]], 0.75, na.rm = TRUE)  # 计算第 3 四分位数（75% 分位数）
  IQR = Q3 - Q1  # 计算四分位距
  lower_bound = Q1 - 1.5 * IQR  # 下界
  upper_bound = Q3 + 1.5 * IQR  # 上界
  outliers = df[[var]][df[[var]] < lower_bound | df[[var]] > upper_bound]  # 筛选异常值
  cat(var, ": Found", length(outliers), "outliers\n")  # 打印异常值的数量
}
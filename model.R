# import packages
library(tableone)
library(splines)
library(dplyr)
library(corrplot)
library(gWQS)

# import data
df = read.csv("./data.csv")
# str(df)
# summary(df)

# 疾病
df.Non.sarcopenia = filter(df, Sarcopenia == 0)
df.Sarcopenia = filter(df, Sarcopenia == 1)

# 描述性统计
# 变量整理
continous = c("RIDAGEYR", "BMXBMI", "INDFMPIR", "PAD680")
category = c("RIAGENDR", "DMDEDUC2", "DMDMARTL", "ALQ", "SMQ020", "DIQ010", "BPQ020")
metals = c("LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN")

# 数据类型转换
df$Sarcopenia = as.factor(df$Sarcopenia)
# df[continous] = lapply(df[continous], as.integer)
df[category] = lapply(df[category], as.factor)
# df[metals] = lapply(df[metals], as.integer)

vars = c("RIDAGEYR", "RIAGENDR", "BMXBMI", "DMDEDUC2", "DMDMARTL", "INDFMPIR", "ALQ", "SMQ020", "PAD680", "DIQ010", "BPQ020")


# 对连续型变量变量进行Kruskal-Wallis 检验
# 这里我们使用 kruskal.test() 函数进行检验。
# Kruskal-Wallis 检验
kruskal_results <- lapply(continous, function(var) {
  kruskal.test(as.formula(paste(var, "~ Sarcopenia")), data = df)
})
names(kruskal_results) <- continous

# 打印结果
for (var in continous) {
    cat("============================================================\n")   
    cat(var, ":\n")
    print(kruskal_results[[var]])
    cat("\n")
}

# 对分类变量进行卡方检验    

# tableone
table1 = CreateTableOne(vars = vars, strata = "Sarcopenia", df, factorVars = category, addOverall = TRUE)

table1_df <- as.data.frame(print(table1, showAllLevels = TRUE))
write.csv(table1_df, "table1.csv")

 
# 对5种重金属之间进行相关性检验
correlation = round(cor(df[,metals], method = "pearson"),3)
correlation

pdf("correlation.pdf", width = 10, height = 10)
corrplot(correlation, method = "color", order = "AOE",type = "full", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 1, tl.cex = 1)
dev.off()

# 四分位数转化
df = df %>%   
  mutate(
    PB = cut(LBXBPB, breaks = quantile(LBXBPB, probs = seq(0, 1, 0.25)), include.lowest = TRUE, labels = c("1", "2", "3", "4")),
    CD = cut(LBXBCD, breaks = quantile(LBXBCD, probs = seq(0, 1, 0.25)), include.lowest = TRUE, labels = c("1", "2", "3", "4")),
    HG = cut(LBXTHG, breaks = quantile(LBXTHG, probs = seq(0, 1, 0.25)), include.lowest = TRUE, labels = c("1", "2", "3", "4")),
    SE = cut(LBXBSE, breaks = quantile(LBXBSE, probs = seq(0, 1, 0.25)), include.lowest = TRUE, labels = c("1", "2", "3", "4")),
    MN = cut(LBXBMN, breaks = quantile(LBXBMN, probs = seq(0, 1, 0.25)), include.lowest = TRUE, labels = c("1", "2", "3", "4"))
  )


# logistics regression

# model fuunction
logis_model = function(exposure, var = NULL, outcome = "Sarcopenia") {
  if(length(exposure) > 1) {
    exposure = paste(exposure, collapse = " + ")
  }

  if (!is.null(var)) {
    var_formula = paste(var, collapse = " + ")
    model_formula = as.formula(paste(outcome, "~", exposure, "+", var_formula))
  } else {
    model_formula = as.formula(paste(outcome, "~", exposure))
  }

  foumual = as.formula(paste("Sarcopenia ~", exposure))


  model = glm(foumual, data = df, family = binomial)  
  coef = coefficients(model)
  OR = exp(coef)
    confint.model = confint(model)
  OR.CI = exp(confint.model)
  
  result = data.frame(
    OR = round(OR, 2),
    CI = paste0("(", round(OR.CI[,1], 2), ", ", round(OR.CI[,2], 2), ")"),
    p = round(summary(model)$coefficients[,4], 3))

  return(result)
}

# p for trend function
ptrend = function(temp_data = df, exposure, var = NULL, outcome = "Sarcopenia") {
  temp_data[exposure] = lapply(temp_data[exposure], as.numeric)
  exposure_formula = paste(exposure, collapse = " + ")

  if (!is.null(var)) {
    var_formula = paste(var, collapse = " + ")
    model_formula = as.formula(paste(outcome, "~", exposure_formula, "+", var_formula))
  }else {
     model_formula = as.formula(paste(outcome, "~", exposure_formula))
  }

  model = glm(model_formula, data = temp_data, family = binomial)

  p_value = summary(model)$coefficients[exposure, "Pr(>|z|)"]

  result = data.frame(
    metals = exposure,
    p_value = ifelse(p_value < 0.001, "<0.001", round(p_value, 3)))

  return(result)
}


# crude model for each metal
logis_model(exposure = "PB")
ptrend(df, exposure = "PB")

logis_model(exposure = "CD")
ptrend(df, exposure = "CD")

logis_model(exposure = "HG")
ptrend(df, exposure = "HG")

logis_model(exposure = "SE")
ptrend(df, exposure = "SE")

logis_model(exposure = "MN")
ptrend(df, exposure = "MN")


# adjusted model 1
vars = c("RIDAGEYR", "RIAGENDR", "BMXBMI", "DMDEDUC2", "DMDMARTL", "INDFMPIR", "ALQ", "SMQ020", "PAD680", "DIQ010", "BPQ020")
logis_model(exposure = "PB", var = vars)
ptrend(df, exposure = "PB", var = vars)

logis_model(exposure = "CD", var = vars)
ptrend(df, exposure = "CD", var = vars)

logis_model(exposure = "HG", var = vars)
ptrend(df, exposure = "HG", var = vars)

logis_model(exposure = "SE", var = vars)
ptrend(df, exposure = "SE", var = vars)

logis_model(exposure = "MN", var = vars)
ptrend(df, exposure = "MN", var = vars)


# adjusted model 2
Qmetal = c("PB", "CD", "HG", "SE", "MN")
logis_model(exposure = Qmetal, var = vars)
ptrend(df, exposure = Qmetal, var = vars)



# WQS model
wqs_model = gwqs(
  formula = Sarcopenia ~ wqs + RIDAGEYR + RIAGENDR + BMXBMI + DMDEDUC2 + 
            DMDMARTL + INDFMPIR + ALQ + SMQ020 + PAD680 + DIQ010 + BPQ020, 
  data = df,                   # 数据集
  mix_name = metals,             # 混合物变量
  b = 100,                     # Bootstrap 迭代次数
  q = 4,                         # 将混合物划分为四分位数
  validation = 0.6,              # 40% 用于训练，60% 用于验证
  b1_pos = NULL,                 # 同时计算正向和负向 WQS 指数
  b_constr = FALSE,             # 不对权重方向进行约束
  family = "binomial",           # 使用逻辑回归（因变量为二分类变量）
  seed = 123,                    # 设置随机种子以便结果可复现
  plots = TRUE,                  # 生成模型相关图形
  tables = TRUE                  # 生成模型相关表格
)


# 正向 WQS 模型
start_time <- Sys.time()
wqs_model_pos <- gwqs(
  formula = Sarcopenia ~ wqs + RIDAGEYR + RIAGENDR + BMXBMI + DMDEDUC2 + 
            DMDMARTL + INDFMPIR + ALQ + SMQ020 + PAD680 + DIQ010 + BPQ020, 
  data = df,
  mix_name = metals,
  b = 100, 
  q = 4, 
  validation = 0.6,
  b1_pos = TRUE,      # 正向 WQS 指数
  b_constr = FALSE,
  family = "binomial",
  seed = 123,
  plots = TRUE,
  tables = TRUE
)
summary(wqs_model_pos)
end_time <- Sys.time()
print(end_time - start_time)


# 负向 WQS 模型
wqs_model_neg <- gwqs(
  formula = Sarcopenia ~ wqs + RIDAGEYR + RIAGENDR + BMXBMI + DMDEDUC2 + 
            DMDMARTL + INDFMPIR + ALQ + SMQ020 + PAD680 + DIQ010 + BPQ020, 
  data = df,
  mix_name = metals,
  b = 100, 
  q = 4, 
  validation = 0.6,
  b1_pos = FALSE,     # 负向 WQS 指数
  b_constr = FALSE,
  family = "binomial",
  seed = 123,
  plots = TRUE,
  tables = TRUE
)
summary(wqs_model_neg)

# 提取权重结果
weights_pos <- wqs_model_pos$final_weights  # 正向权重
weights_neg <- wqs_model_neg$final_weights  # 负向权重

# 合并权重
list_weights <- list(pos = weights_pos, neg = weights_neg)
list_weights

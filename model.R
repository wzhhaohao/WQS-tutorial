# import packages
library(tableone)
library(splines)
library(dplyr)
library(corrplot)
library(gWQS)
library(ggplot2)
library(epiDisplay)

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
crude_PB_model = logis_model(exposure = "PB")
crude_PB_ptrend = ptrend(df, exposure = "PB")

crude_CD_model = logis_model(exposure = "CD")
crude_PCD_ptrend = ptrend(df, exposure = "CD")

crude_HG_model = logis_model(exposure = "HG")
crude_HG_ptrend = ptrend(df, exposure = "HG")

crude_SE_model = logis_model(exposure = "SE")
crude_SE_ptrend = ptrend(df, exposure = "SE")

crude_MN_model = logis_model(exposure = "MN")
crude_MN_ptrend = ptrend(df, exposure = "MN")


# adjusted model 1
vars = c("RIDAGEYR", "RIAGENDR", "BMXBMI", "DMDEDUC2", "DMDMARTL", "INDFMPIR", "ALQ", "SMQ020", "PAD680", "DIQ010", "BPQ020")
model1_PB = logis_model(exposure = "PB", var = vars)
model1_PB_ptrend = ptrend(df, exposure = "PB", var = vars)

model1_CD = logis_model(exposure = "CD", var = vars)
model1_CD_ptrend = ptrend(df, exposure = "CD", var = vars)

model1_HG = logis_model(exposure = "HG", var = vars)
model1_HG_ptrend = ptrend(df, exposure = "HG", var = vars)

model1_SE = logis_model(exposure = "SE", var = vars)
model1_SE_ptrend = ptrend(df, exposure = "SE", var = vars)

model1_MN = logis_model(exposure = "MN", var = vars)
model1_MN_ptrend = ptrend(df, exposure = "MN", var = vars)


# adjusted model 2
Qmetal = c("PB", "CD", "HG", "SE", "MN")
model2 = logis_model(exposure = Qmetal, var = vars)
model2_ptrend = ptrend(df, exposure = Qmetal, var = vars)



# WQS model
# wqs_model = gwqs(
#   formula = Sarcopenia ~ wqs + RIDAGEYR + RIAGENDR + BMXBMI + DMDEDUC2 + 
#             DMDMARTL + INDFMPIR + ALQ + SMQ020 + PAD680 + DIQ010 + BPQ020, 
#   data = df,                   # 数据集
#   mix_name = metals,             # 混合物变量
#   b = 100,                     # Bootstrap 迭代次数
#   q = 4,                         # 将混合物划分为四分位数
#   validation = 0.6,              # 40% 用于训练，60% 用于验证
#   b1_pos = NULL,                 # 同时计算正向和负向 WQS 指数
#   b_constr = FALSE,             # 不对权重方向进行约束
#   family = "binomial",           # 使用逻辑回归（因变量为二分类变量）
#   seed = 123,                    # 设置随机种子以便结果可复现
#   plots = TRUE,                  # 生成模型相关图形
#   tables = TRUE                  # 生成模型相关表格
# )


# 正向 WQS 模型
start_time <- Sys.time()
wqs_model_pos <- gwqs(
  formula = Sarcopenia ~ wqs + RIDAGEYR + RIAGENDR + BMXBMI + DMDEDUC2 + 
            DMDMARTL + INDFMPIR + ALQ + SMQ020 + PAD680 + DIQ010 + BPQ020, 
  data = df,
  mix_name = metals,
  b = 10000, 
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

coef = coefficients(wqs_model_pos)
OR = exp(coef)
confint.model = confint(wqs_model_pos)
OR.CI = exp(confint.model)

result = data.frame(
  OR = round(OR, 2),
  CI = paste0("(", round(OR.CI[,1], 2), ", ", round(OR.CI[,2], 2), ")"),
  p = round(summary(wqs_model_pos)$coefficients[,4], 3))
print(result)

end_time <- Sys.time()
print(end_time - start_time)


# # 负向 WQS 模型
# wqs_model_neg <- gwqs(
#   formula = Sarcopenia ~ wqs + RIDAGEYR + RIAGENDR + BMXBMI + DMDEDUC2 + 
#             DMDMARTL + INDFMPIR + ALQ + SMQ020 + PAD680 + DIQ010 + BPQ020, 
#   data = df,
#   mix_name = metals,
#   b = 100, 
#   q = 4, 
#   validation = 0.6,
#   b1_pos = FALSE,     # 负向 WQS 指数
#   b_constr = FALSE,
#   family = "binomial",
#   seed = 123,
#   plots = TRUE,
#   tables = TRUE
# )
# summary(wqs_model_neg)

# # 提取权重结果
# weights_pos <- wqs_model_pos$final_weights  # 正向权重
# weights_neg <- wqs_model_neg$final_weights  # 负向权重

# 合并权重
# list_weights <- list(pos = weights_pos, neg = weights_neg)
# list_weights


pdf("./barplot.pdf", width = 8, height = 6)
gwqs_barplot(wqs_model_pos)
dev.off()


# 另外一种画图
w_ord = order(wqs_model_pos$final_weights$mean_weight)
mean_weight = wqs_model_pos$final_weights$mean_weight[w_ord]
mix_name = factor(wqs_model_pos$final_weights$mix_name[w_ord], 
                   levels = wqs_model_pos$final_weights$mix_name[w_ord])


data_plot <- data.frame(mean_weight, mix_name)



barplot1 = ggplot(data_plot, aes(x = mix_name, y = mean_weight, fill = mix_name)) + 
  geom_bar(stat = "identity", color = "black") +  # 边框颜色为黑色
  scale_fill_brewer(palette = "Set3") +  # 使用 R 的预置配色方案 Set3
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(color = 'black'),
        legend.position = "none") +
  coord_flip() +
  geom_hline(yintercept = 1 / length(metals), linetype = "dashed", color = "red") + 
  geom_text(aes(label = round(mean_weight, 2)),  # 标注数值，保留两位小数
          hjust = -0.2,  # 控制标注在条形外侧，负值为外侧
          color = "black",  # 数值颜色
          size = 3)   # 数值字体大小 

barplot1
ggsave("./barplot1.png", width = 8, height = 6)

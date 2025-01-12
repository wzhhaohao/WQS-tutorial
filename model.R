#R_prog_24.4 - R_prog_24.11
# 文件名：model.R
# 载入包
library(tableone)
library(splines)
library(dplyr)
library(corrplot)
library(gWQS)
library(ggplot2)
library(epiDisplay)
source("utils.R")

# 设置种子
set.seed(1)

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

df = df %>%
  rename(
    Pb = LBXBPB,
    Cd = LBXBCD,
    Hg = LBXTHG,
    Se = LBXBSE,
    Mn = LBXBMN
  )

metals = c("Pb", "Cd", "Hg", "Se", "Mn")

# 数据类型转换
df$Sarcopenia = as.factor(df$Sarcopenia)
df[category] = lapply(df[category], as.factor)
vars = c("RIDAGEYR", "RIAGENDR", "BMXBMI", "DMDEDUC2", "DMDMARTL", "INDFMPIR", "ALQ", "SMQ020", "PAD680", "DIQ010", "BPQ020")

# 对连续型变量变量进行Kruskal-Wallis 检验
# 这里我们使用 kruskal.test() 函数进行检验。
# Kruskal-Wallis 检验
kruskal_results = lapply(continous, function(var) {
  kruskal.test(as.formula(paste(var, "~ Sarcopenia")), data = df)
})
names(kruskal_results) = continous

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
table1_df = as.data.frame(print(table1, showAllLevels = TRUE))
write.csv(table1_df, "table1.csv")

# 对5种重金属之间进行相关性检验
correlation = round(cor(df[,metals], method = "pearson"),3)
correlation

png("correlation.png", width = 10, height = 10, units = "in", res = 300)
corrplot(correlation, method = "color", order = "AOE",type = "full", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 1, tl.cex = 1)
dev.off()

# logistics regression
# crude model for each metal
crude_PB_model = logis_model(exposure = "Pb")
crude_PB_ptrend = ptrend(df, exposure = "Pb")

crude_CD_model = logis_model(exposure = "Cd")
crude_PCD_ptrend = ptrend(df, exposure = "Cd")

crude_HG_model = logis_model(exposure = "Hg")
crude_HG_ptrend = ptrend(df, exposure = "Hg")

crude_SE_model = logis_model(exposure = "Se")
crude_SE_ptrend = ptrend(df, exposure = "Se")

crude_MN_model = logis_model(exposure = "Mn")
crude_MN_ptrend = ptrend(df, exposure = "Mn")

# adjusted model 1
vars = c("RIDAGEYR", "RIAGENDR", "BMXBMI", "DMDEDUC2", "DMDMARTL", "INDFMPIR", "ALQ", "SMQ020", "PAD680", "DIQ010", "BPQ020")
model1_PB = logis_model(exposure = "Pb", variable = vars)
model1_PB_ptrend = ptrend(df, exposure = "Pb", variable = vars)

model1_CD = logis_model(exposure = "Cd", variable = vars)
model1_CD_ptrend = ptrend(df, exposure = "Cd", variable = vars)

model1_HG = logis_model(exposure = "Hg", variable = vars)
model1_HG_ptrend = ptrend(df, exposure = "Hg", variable = vars)

model1_SE = logis_model(exposure = "Se", variable = vars)
model1_SE_ptrend = ptrend(df, exposure = "Se", variable = vars)

model1_MN = logis_model(exposure = "Mn", variable = vars)
model1_MN_ptrend = ptrend(df, exposure = "Mn", variable = vars)

# adjusted model 2
model2 = logis_model(exposure = metals, variable = vars)
model2_ptrend = ptrend(df, exposure = metals, variable = vars)


# WQS model
# crude WQS model
crude_wqs_model_pos = gwqs(
  formula = Sarcopenia ~ wqs, 
  data = df,
  mix_name = metals,
  b = 10000, 
  q = 4, 
  validation = 0.6,
  b1_pos = TRUE,      # 正向 WQS 指数 负向改为FALSE
  b_constr = FALSE,
  family = "binomial",
  seed = 1,
  plots = TRUE,
  tables = TRUE
)
###################################################################################
# 调整后的模型
# 正向 WQS 模型
start_time = Sys.time()
wqs_model_pos = gwqs(
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
  seed = 1,
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

end_time = Sys.time()
print(end_time - start_time)

# 负向 WQS 模型
wqs_model_neg = gwqs(
  formula = Sarcopenia ~ wqs + RIDAGEYR + RIAGENDR + BMXBMI + DMDEDUC2 + 
            DMDMARTL + INDFMPIR + ALQ + SMQ020 + PAD680 + DIQ010 + BPQ020, 
  data = df,
  mix_name = metals,
  b = 10000, 
  q = 4, 
  validation = 0.6,
  b1_pos = FALSE,     # 负向 WQS 指数
  b_constr = FALSE,
  family = "binomial",
  seed = 1,
  plots = TRUE,
  tables = TRUE
)

# 调整过的正向权重 画图
w_ord = order(wqs_model_pos$final_weights$mean_weight)
mean_weight = wqs_model_pos$final_weights$mean_weight[w_ord]
mix_name = factor(wqs_model_pos$final_weights$mix_name[w_ord], 
                   levels = wqs_model_pos$final_weights$mix_name[w_ord])
data_plot = data.frame(mean_weight, mix_name)

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

# import packages
library(tableone)
library(splines)
library(dplyr)

# import data
data = read.csv("./第二篇/data.csv")
# str(data)
# summary(data)

# 疾病
data.Non.sarcopenia = filter(data, Sarcopenia == 0)
data.Sarcopenia = filter(data, Sarcopenia == 1)

# 描述性统计
# 变量整理
continous = c("RIDAGEYR", "BMXBMI", "INDFMPIR", "PAD680")
category = c("RIAGENDR", "DMDEDUC2", "DMDMARTL", "ALQ", "SMQ020", "DIQ010", "BPQ020")
metals = c("LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN")

# 数据类型转换
data$Sarcopenia = as.factor(data$Sarcopenia)
data[continous] = lapply(data[continous], as.integer)
data[category] = lapply(data[category], as.factor)
data[metals] = lapply(data[metals], as.integer)

vars = c("RIDAGEYR", "RIAGENDR", "BMXBMI", "DMDEDUC2", "DMDMARTL", "INDFMPIR", "ALQ", "SMQ020", "PAD680", "DIQ010", "BPQ020")


# 对连续型变量变量进行Kruskal-Wallis 检验
# 这里我们使用 kruskal.test() 函数进行检验。
# Kruskal-Wallis 检验
kruskal_results <- lapply(continous, function(var) {
  kruskal.test(as.formula(paste(var, "~ Sarcopenia")), data = data)
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
table1 = CreateTableOne(vars = vars, strata = "Sarcopenia", data, factorVars = category, addOverall = TRUE)

table1_df <- as.data.frame(print(table1, showAllLevels = TRUE))
write.csv(table1_df, "table1.csv")

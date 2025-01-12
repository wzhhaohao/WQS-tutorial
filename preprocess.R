#R_prog_24.2-3
# 文件名：preprocess.R
# 1. 加载必要包与函数
library(purrr)
library(dplyr)
source("utils.R")   # 导入工具函数模块

# 2. 读取并合并各文件数据
DEMO = read_xpts("./DEMO/")
PBCD = read_xpts("./PBCD/")
ALQ  = read_xpts("./ALQ/")
BMX  = read_xpts("./BMX/")
BPQ  = read_xpts("./BPQ/")
DXX  = read_xpts("./DXX/")
PAQ  = read_xpts("./PAQ/")
SMQ  = read_xpts("./SMQ/")
DIQ  = read_xpts("./DIQ/")

merged_data = list(DEMO, PBCD, ALQ, BMX, BPQ, DXX, PAQ, SMQ, DIQ) %>%
  reduce(full_join, by = "SEQN")

# 3. 选取感兴趣变量并查看行数
vars_interest = c(
  "SEQN", "RIDAGEYR", "RIAGENDR", "DMDEDUC2", "DMDMARTL", "INDFMPIR",
  "LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN",
  "DXDLALE", "DXDRALE", "DXDLLLE", "DXDRLLE",
  "ALQ101", "ALQ111", "SMQ020", "PAD680", "DIQ010", "BPQ020", "BMXBMI"
)
data = merged_data[, vars_interest]

# 4. 年龄>=18
n = nrow(data)
data = data %>%
  filter(RIDAGEYR >= 18)
cat("删除了", n - nrow(data), "行年龄<18的数据\n")

# 5. 重金属数据处理
n = nrow(data)
summary(data[, c("LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN")])
data = data %>%
  mutate(
    # 条件筛选并 log10 转换
    LBXBPB = ifelse(LBXBPB >= 0.25, log10(LBXBPB), NA_real_),
    LBXBCD = ifelse(LBXBCD >= 0.16, log10(LBXBCD / 10), NA_real_),
    LBXTHG = ifelse(LBXTHG >= 0.16, log10(LBXTHG / 10), NA_real_),
    LBXBSE = ifelse(LBXBSE >= 30,  log10(LBXBSE / 10),  NA_real_),
    LBXBMN = ifelse(LBXBMN >= 1.06, log10(LBXBMN / 10), NA_real_)
  )

# 6. 骨密度与肌少症指标
data = data %>%
  mutate(
    ASM = (DXDLALE + DXDRALE + DXDLLLE + DXDRLLE) / 1000,
    ASMI = ASM / BMXBMI,
    Sarcopenia = ifelse(
      (RIAGENDR == 1 & ASMI < 0.789) | (RIAGENDR == 2 & ASMI < 0.512),
      1, 0
    )
  )

# 7. 处理婚姻状况（DMDMARTL）
data = data %>%
  mutate(
    DMDMARTL = case_when(
      DMDMARTL == 1              ~ 1,
      DMDMARTL == 5              ~ 2,
      DMDMARTL %in% c(2, 3, 4, 6) ~ 3,
      TRUE ~ NA_real_
    )
  )

# 8. 处理教育水平（DMDEDUC2）
data = data %>%
  mutate(
    DMDEDUC2 = case_when(
      DMDEDUC2 %in% c(1, 2) ~ 1,
      DMDEDUC2 == 3         ~ 2,
      DMDEDUC2 %in% c(4, 5) ~ 3,
      TRUE                  ~ NA_real_
    )
  )

# 9. 处理饮酒 (ALQ101, ALQ111) 并生成 ALQ
data = data %>%
  mutate(
    ALQ101 = ifelse(
      is.na(ALQ101), NA,
      ifelse(ALQ101 == 1, 1, ifelse(ALQ101 %in% c(9, 7), NA, 0))
    ),
    ALQ111 = ifelse(
      is.na(ALQ111), NA,
      ifelse(ALQ111 == 1, 1, ifelse(ALQ111 %in% c(9, 7), NA, 0))
    ),
    ALQ = ifelse(
      is.na(ALQ101) & is.na(ALQ111), NA,
      pmax(ALQ101, ALQ111, na.rm = TRUE)
    )
  ) %>%
  select(-ALQ101, -ALQ111)

# 10. 吸烟数据 (SMQ020)
data = data %>%
  mutate(
    SMQ020 = case_when(
      SMQ020 == 2 ~ 1,
      SMQ020 == 1 ~ 2,
      TRUE        ~ NA_real_
    )
  )

# 11. Diabetes 与 Hypertension
data = data %>%
  mutate(
    DIQ010 = case_when(
      DIQ010 == 1 ~ 1,
      DIQ010 == 2 ~ 2,
      TRUE        ~ NA_real_
    ),
    BPQ020 = case_when(
      BPQ020 == 1 ~ 1,
      BPQ020 == 2 ~ 2,
      TRUE        ~ NA_real_
    )
  )

# 12. 久坐时间 (PAD680)
data = data %>%
  mutate(
    PAD680 = ifelse(PAD680 >= 0 & PAD680 <= 1200, PAD680, NA_real_)
  )

# 13. 查看各重要变量缺失情况
cat("heavy metal 缺失值:", n - sum(complete.cases(data[, c("LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN")])), "\n")
cat("BMI 缺失值:",         n - sum(complete.cases(data[, "BMXBMI"])), "\n")
cat("DXA 缺失值:",         n - sum(complete.cases(data[, c("DXDLALE", "DXDRALE", "DXDLLLE", "DXDRLLE")])), "\n")
cat("婚姻 缺失值:",         n - sum(complete.cases(data[, "DMDMARTL"])), "\n")
cat("教育 缺失值:",         n - sum(complete.cases(data[, "DMDEDUC2"])), "\n")
cat("PIR 缺失值:",          n - sum(complete.cases(data[, "INDFMPIR"])), "\n")
cat("ALQ 缺失值:",          n - sum(complete.cases(data[, "ALQ"])), "\n")
cat("吸烟 缺失值:",         n - sum(complete.cases(data[, "SMQ020"])), "\n")
cat("Diabetes 缺失值:",     n - sum(complete.cases(data[, "DIQ010"])), "\n")
cat("Hypertension 缺失值:", n - sum(complete.cases(data[, "BPQ020"])), "\n")
cat("sedentary 缺失值:",    n - sum(complete.cases(data[, "PAD680"])), "\n")

# 14. 删除含缺失值的行
data = na.omit(data)
print(table(data$Sarcopenia))

# 15. 清理环境 & 保存结果
rm(DEMO, PBCD, ALQ, BMX, BPQ, DXX, PAQ, SMQ, DIQ, vars_interest, merged_data)
write.csv(data, file = "./data.csv", row.names = FALSE)
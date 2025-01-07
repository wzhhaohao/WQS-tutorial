# import packages
library(dplyr)
library(purrr)
source("./read_xpts.R")

# Definition of na.summary function
na.summary = function(data) {
    cat("nrow:", nrow(data)," \n")
    for (i in 1:ncol(data)) {
        cat(names(data)[i], ": ", sep = "")
        cat(sum(is.na(data[, i])), "\n")
    }
    cat("Rows with NA: ", sum(apply(data, 1, function(row) any(is.na(row)))), "\n")
    cat("remain nrow: ", nrow(data) - sum(apply(data, 1, function(row) any(is.na(row)))), "\n")
    rm(i)
    cat("============================================================\n")
}

# read data
DEMO = read_xpts("./第二篇/DEMO/")
PBCD = read_xpts("./第二篇/PBCD/")
ALQ = read_xpts("./第二篇/ALQ/")
BMX = read_xpts("./第二篇/BMX/")
BPQ = read_xpts("./第二篇/BPQ/")
DXX = read_xpts("./第二篇/DXX/")
PAQ = read_xpts("./第二篇/PAQ/")
SMQ = read_xpts("./第二篇/SMQ/")
DIQ = read_xpts("./第二篇/DIQ/")

# 逐步合并多个数据框
merged_data <- list(DEMO, PBCD, ALQ, BMX, BPQ, DXX, PAQ, SMQ, DIQ) %>%
  reduce(full_join, by = "SEQN")

# na.summary(merged_data)

# choose variables of interest
interst = c("SEQN", "RIDAGEYR", "RIAGENDR", "DMDEDUC2", "DMDMARTL", "INDFMPIR",
    "LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN",
    "DXDLALE", "DXDRALE", "DXDLLLE", "DXDRLLE",
    "ALQ101", "ALQ111", "SMQ020", "PAD680", "DIQ010", "BPQ020", "BMXBMI")
data = merged_data[, interst]

# 数据预处理

# 筛选年龄18以上的
n = nrow(data)
data = data %>% filter(RIDAGEYR >= 18)
na.summary(data)
cat("删除了", n - nrow(data), "18岁以下的人数\n")



# 处理"ALQ101", "ALQ111"这两个变量
n = nrow(data)
data <- data %>%
  mutate(
    # 转换 ALQ101
    ALQ101 = ifelse(
      is.na(ALQ101), NA, 
      ifelse(ALQ101 == 1, 1, ifelse(ALQ101 %in% c(9, 7), NA, 0))
    ),
    # 转换 ALQ111
    ALQ111 = ifelse(
      is.na(ALQ111), NA, 
      ifelse(ALQ111 == 1, 1, ifelse(ALQ111 %in% c(9, 7), NA, 0))
    ),
    # 计算最终的 ALQ
    ALQ = ifelse(
      is.na(ALQ101) & is.na(ALQ111), NA, 
      pmax(ALQ101, ALQ111, na.rm = TRUE)
    )
  ) %>%
    filter(!is.na(ALQ)) %>%
    select(-ALQ101, -ALQ111) 
cat("删除了", n - nrow(data), "行ALQ的缺失数据\n")





# 处理教育数据：将1和2变为1 将3变为2 将4和5变成3 对于拒绝回答和不知道的以及缺失的排除 DMDEDUC2
table(data$DMDEDUC2)
data = data %>%
    mutate(
        DMDEDUC2 = case_when(
        DMDEDUC2 %in% c(1, 2) ~ 1,
        DMDEDUC2 == 3 ~ 2,
        DMDEDUC2 %in% c(4, 5) ~ 3,
        TRUE ~ NA_real_
))
table(data$DMDEDUC2)

# 处理婚姻数据：1是1 5变2 其他变3 missing的排除 DMDMARTL
table(data$DMDMARTL)
data = data %>%
    mutate(
        DMDMARTL = case_when(
        DMDMARTL == 1 ~ 1,
        DMDMARTL == 5 ~ 2,
        is.na(DMDMARTL) ~ NA_real_,
        TRUE ~ 3      
))
table(data$DMDMARTL)

# 处理吸烟数据
table(data$SMQ020)
data = data %>%
    mutate(
        SMQ020 = case_when(
        SMQ020 == 2 ~ 1,
        SMQ020 == 1 ~ 2,
        TRUE ~ NA_real_   
))
table(data$SMQ020)

# 处理sedentary time
table(data$PAD680)
data = data %>%
    mutate(
        PAD680 = ifelse(PAD680>=0 & PAD680<=1200, PAD680, NA_real_) 
)
table(data$PAD680)

# 处理Diabetes 和 Hypertension
table(data$DIQ010)
table(data$BPQ020)
data = data %>%
    mutate(
        DIQ010 = case_when(
        DIQ010 == 1 ~ 1,
        DIQ010 == 2 ~ 2,
        TRUE ~ NA_real_   
    ),
    BPQ020 = case_when(
        BPQ020 == 1 ~ 1,
        BPQ020 == 2 ~ 2,
        TRUE ~ NA_real_   
    )
)
table(data$DIQ010)
table(data$BPQ020)

# 处理重金属数据
summary(data[, c("LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN")])
data = data %>%
    mutate(
        LBXBPB = ifelse(LBXBPB >= 0.25, LBXBPB, NA_real_),
        LBXBCD = ifelse(LBXBCD >= 0.16, LBXBCD, NA_real_),
        LBXTHG = ifelse(LBXTHG >= 0.16, LBXTHG, NA_real_),
        LBXBSE = ifelse(LBXBSE >= 30, LBXBSE, NA_real_),
        LBXBMN = ifelse(LBXBMN >= 1.06, LBXBMN, NA_real_)
    )
summary(data[, c("LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN")])

# 处理骨密度数据
summary(data[, c("DXDLALE", "DXDRALE", "DXDLLLE", "DXDRLLE")])
data = data %>%
    mutate(
        ASM = (DXDLALE + DXDRALE + DXDLLLE + DXDRLLE) / 1000,
        ASMI = ASM / BMXBMI,
        Sarcopenia = ifelse((RIAGENDR == 1 & ASMI < 0.789) | (RIAGENDR == 2 & ASMI < 0.512) , 1, 0)
    )



data = na.omit(data)
table(data$Sarcopenia)

# 清楚不需要的变量
rm(DEMO, PBCD, ALQ, BMX, BPQ, DXX, PAQ, SMQ, DIQ, interst, merged_data)

# 对重金属数据进行单位统一
data = data %>%
    mutate(
        LBXBCD = LBXBCD / 10,
        LBXTHG = LBXTHG / 10,
        LBXBSE = LBXBSE / 10,
        LBXBMN = LBXBMN / 10
    )

summary(data)

# 保存数据
write.csv(data, file = "./第二篇/data.csv", row.names = FALSE)

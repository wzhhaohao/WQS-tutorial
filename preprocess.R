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
DEMO = read_xpts("./DEMO/")
PBCD = read_xpts("./PBCD/")
ALQ = read_xpts("./ALQ/")
BMX = read_xpts("./BMX/")
BPQ = read_xpts("./BPQ/")
DXX = read_xpts("./DXX/")
PAQ = read_xpts("./PAQ/")
SMQ = read_xpts("./SMQ/")
DIQ = read_xpts("./DIQ/")

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

data.copy = data

# 数据预处理
# 第一part
# 筛选年龄18以上的
n = nrow(data)
data = data %>% filter(RIDAGEYR >= 18)
na.summary(data)
cat("删除了", n - nrow(data), "18岁以下的人数\n")


# 处理重金属数据
n = nrow(data)
summary(data[, c("LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN")])
data = data %>%
    mutate(
        LBXBPB = ifelse(LBXBPB >= 0.25, LBXBPB, NA_real_),
        LBXBCD = ifelse(LBXBCD >= 0.16, LBXBCD, NA_real_),
        LBXTHG = ifelse(LBXTHG >= 0.16, LBXTHG, NA_real_),
        LBXBSE = ifelse(LBXBSE >= 30, LBXBSE, NA_real_),
        LBXBMN = ifelse(LBXBMN >= 1.06, LBXBMN, NA_real_)
    ) %>%
    filter(if_all(c("LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN"), ~ !is.na(.x)))
cat("删除了", n - nrow(data), "行heavy metal缺失数据\n")

# BMI数据处理
n = nrow(data)
data = data %>%
    filter(!is.na(BMXBMI))
cat("删除了", n - nrow(data), "行BMI缺失数据\n")

# DXA数据处理
n = nrow(data)
data = data %>%
    filter(!is.na(DXDLALE), !is.na(DXDRALE), !is.na(DXDLLLE), !is.na(DXDRLLE))
cat("删除了", n - nrow(data), "行DXA缺失数据\n")


# 处理骨密度数据
data = data %>%
    mutate(
        ASM = (DXDLALE + DXDRALE + DXDLLLE + DXDRLLE) / 1000,
        ASMI = ASM / BMXBMI,
        Sarcopenia = ifelse((RIAGENDR == 1 & ASMI < 0.789) | (RIAGENDR == 2 & ASMI < 0.512) , 1, 0)
    )


# 第二part
# 处理婚姻数据：1是1 5变2 其他变3 missing的排除 DMDMARTL
table(data$DMDMARTL)
n = nrow(data)
data = data %>%
    mutate(
        DMDMARTL = case_when(
        DMDMARTL == 1 ~ 1,
        DMDMARTL == 5 ~ 2,
        DMDMARTL %in% c(2, 3, 4, 6) ~ 3,
        TRUE ~ NA_real_  
)) %>%
    filter(!is.na(DMDMARTL))
table(data$DMDMARTL)
cat("删除了", n - nrow(data), "行婚姻数据缺失数据\n")

# 处理教育数据：将1和2变为1 将3变为2 将4和5变成3 对于拒绝回答和不知道的以及缺失的排除 DMDEDUC2
table(data$DMDEDUC2)
n = nrow(data)
data = data %>%
    mutate(
        DMDEDUC2 = case_when(
        DMDEDUC2 %in% c(1, 2) ~ 1,
        DMDEDUC2 == 3 ~ 2,
        DMDEDUC2 %in% c(4, 5) ~ 3,
        TRUE ~ NA_real_
)) %>%
    filter(!is.na(DMDEDUC2))

table(data$DMDEDUC2)
cat("删除了", n - nrow(data), "行教育数据缺失数据\n")

# 对重金属数据进行单位统一 并以10为底进行log变换
data = data %>%
    mutate(
        LBXBCD = log10(LBXBCD / 10),
        LBXTHG = log10(LBXTHG / 10),
        LBXBSE = log10(LBXBSE / 10),
        LBXBMN = log10(LBXBMN / 10),
        LBXBPB = log10(LBXBPB)
    )
    


# 处理PIR数据：缺失的排除 INDFMPIR
n = nrow(data)
data = data %>%
    filter(!is.na(INDFMPIR))
cat("删除了", n - nrow(data), "行PIR缺失数据\n")



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

# 处理吸烟数据
n = nrow(data)
data = data %>%
    mutate(
        SMQ020 = case_when(
        SMQ020 == 2 ~ 1,
        SMQ020 == 1 ~ 2,
        TRUE ~ NA_real_   
)) %>%
    filter(!is.na(SMQ020))
cat("删除了", n - nrow(data), "行吸烟数据缺失数据\n")


# 处理Diabetes 和 Hypertension
n = nrow(data)
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

# 打印缺失值数量
cat("Diabetes 缺失值数量:", sum(is.na(data$DIQ010)), "\n")
cat("Hypertension 缺失值数量:", sum(is.na(data$BPQ020)), "\n")

# 过滤掉缺失值
data = data %>%
  filter(!is.na(DIQ010), !is.na(BPQ020))

# 处理sedentary time
n = nrow(data)
data = data %>%
    mutate(
        PAD680 = ifelse(PAD680>=0 & PAD680<=1200, PAD680, NA_real_) 
) %>%
    filter(!is.na(PAD680))
cat("删除了", n - nrow(data), "行sedentary time缺失数据\n")


# 冗余设计
data = na.omit(data)
table(data$Sarcopenia)
na.summary(data)

# 清楚不需要的变量
rm(DEMO, PBCD, ALQ, BMX, BPQ, DXX, PAQ, SMQ, DIQ, interst, merged_data)

# 保存数据
write.csv(data, file = "./data.csv", row.names = FALSE)

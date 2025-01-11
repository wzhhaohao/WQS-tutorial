#R_prog_24.1
# 文件名：utils.R
# 加载必要的包
library(foreign)

# 批量读取函数
read_xpts = function(path) { 
    # 获取指定路径下所有以 .xpt 结尾的文件
    files = list.files(path, pattern = "\\.xpt$", full.names = TRUE)
    # 逐个读取 XPT 文件，将其转换为数据框，并合并所有数据框
    data = lapply(files, function(file) as.data.frame(read.xport(file))) %>% bind_rows()
    # 去除基于 SEQN（唯一标识符）的重复行，保留最后一次出现的记录
    data = data[!duplicated(data$SEQN, fromLast = TRUE), ]
    return(data)
}
                  
# 数据结构函数（缺失值）
na.summary = function(data) {
    cat("nrow:", nrow(data), "\n")
    # 统计缺失值
    for (i in 1:ncol(data)) {
        cat(names(data)[i], ": ", sep = "")
        cat(sum(is.na(data[, i])), "\n")
    }
    cat("Rows with NA: ", sum(apply(data, 1, function(row) any(is.na(row)))), "\n")
    cat("remain nrow: ", nrow(data) - sum(apply(data, 1, function(row) any(is.na(row)))), "\n")
    # 清理变量 i，避免潜在的污染
    rm(i)
    cat("============================================================\n")
}

# 快速逻辑回归建模函数
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

  model = glm(model_formula, data = df, family = binomial)
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
                                                
# 趋势分析函数
ptrend = function(temp_data = df, exposure, var = NULL, outcome = "Sarcopenia") {
  temp_data[exposure] = lapply(temp_data[exposure], as.numeric)
  exposure_formula = paste(exposure, collapse = " + ")
  
  if (!is.null(var)) {
    var_formula = paste(var, collapse = " + ")
    model_formula = as.formula(paste(outcome, "~", exposure_formula, "+", var_formula))
  } else {
    model_formula = as.formula(paste(outcome, "~", exposure_formula))
  }
  
  model = glm(model_formula, data = temp_data, family = binomial)
  p_value = summary(model)$coefficients[exposure, "Pr(>|z|)"]
  
  result = data.frame(
    metals = exposure,
    p_value = ifelse(p_value < 0.001, "<0.001", round(p_value, 3)))
  return(result)
}
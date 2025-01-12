#R_prog_24.1
# 文件名：utils.R
# 加载必要的包
library(foreign)
library(dplyr)
library(broom)

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
    cat("============================================================\n")
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
logis_model = function(data = df, exposure, variable = NULL, outcome = "Sarcopenia") {
  
  # 辅助函数：给定公式，返回模型及结果
  run_glm_and_extract = function(formula, data, type) {
    model = glm(formula, data = data, family = binomial)
    tidy_model = broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
    
    exposure_pattern = ifelse(type == "linear", paste0("^", exposure, collapse = "|"), paste0("^", toupper(exposure), collapse = "|"))
    sub_tidy = tidy_model %>%
      filter(grepl(exposure_pattern, term))
    
    result = as.data.frame(sub_tidy) %>%
      mutate(
        metal = term,
        "OR(95%CI)" = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
        p_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
      ) %>%
      dplyr::select(metal, "OR(95%CI)", p_value)
  }
  
  exposure_linear = paste(exposure, collapse = " + ")
  if (!is.null(variable)) {
    variable_formula = paste(variable, collapse = " + ")
    formula_linear = as.formula(paste(outcome, "~", exposure_linear, "+", variable_formula))
  } else {
    formula_linear = as.formula(paste(outcome, "~", exposure_linear))
  }
  
  result_linear = run_glm_and_extract(formula_linear, data, type = "linear")

  # 分位数分析：将 exposure 划分为 Q1~Q4
  E = toupper(exposure)
  E_quantile = paste(E, collapse = " + ")
  for (exp in exposure) {
    new_col_name =  toupper(exp)
    
    data[[new_col_name]] = cut(
      data[[exp]],
      breaks = quantile(data[[exp]], probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste0("Q", 1:4)
    )
  }
  if (!is.null(variable)) {
    formula_grouped = as.formula(paste(outcome, "~", E_quantile, "+", variable_formula))
  } else {
    formula_grouped = as.formula(paste(outcome,  "~", E_quantile))
  }
  
  result_grouped = run_glm_and_extract(formula_grouped, data, "Q")
  
  return(list(Linear_Analysis = result_linear, Grouped_Analysis = result_grouped))
}
                                                
# 趋势分析函数
ptrend = function(data, exposure, variables = NULL, outcome = "Sarcopenia", Q = 4) {
  # 使用 dplyr::across 创建分位数分箱后的变量
  data = data %>%
    mutate(across(
      all_of(exposure),
      ~ as.numeric(cut(
        .x,
        breaks = quantile(.x, probs = seq(0, 1, length.out = Q + 1), na.rm = TRUE),
        include.lowest = TRUE,
        labels = 1:Q,
        right = TRUE
      )),
      .names = "{toupper(.col)}"  # 新变量名为大写的暴露变量名
    ))
  
  # 构建回归公式
  E_quantile = paste(toupper(exposure), collapse = " + ")
  
  if (!is.null(variables)) {
    variables = as.character(variables)
    formula_terms = paste(c(E_quantile, paste(variables, collapse = " + ")), collapse = " + ")
  } else {
    formula_terms = E_quantile
  }
  
  model_formula = as.formula(paste(outcome, "~", formula_terms))
  model = glm(model_formula, data = data, family = binomial)
  tidy_model = broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  exposure_pattern = paste0("^", toupper(exposure), collapse = "|")
  
  # 使用正则表达式筛选相关的系数
  sub_tidy = tidy_model %>%
    filter(grepl(exposure_pattern, term))
  
  result = as.data.frame(sub_tidy) %>%
    mutate(
      metal = term,
      `OR(95%CI)` = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
      p_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    dplyr::select(metal, `OR(95%CI)`, p_value)
  
  return(result)
}
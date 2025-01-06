library(gWQS)
library(ggplot2)
library(epiDisplay)

data = read.csv("./data.csv")

name = names(data)[3:11]




data$age = as.factor(ifelse(data$RIDAGEYR>=60, 1, 0))
data$RIAGENDR = as.factor(data$RIAGENDR)
data$RIDRETH1 = as.factor(data$RIDRETH1)
data$DMDMARTL = as.factor(data$DMDMARTL)
data$DMDEDUC2 = as.factor(data$DMDEDUC2)
# data$phy = as.factor(data$phy)
data$ALQ = as.factor(data$ALQ)


results1 = gwqs(OA ~ wqs + age + RIAGENDR + RIDRETH1 + DMDEDUC2 + INDFMPIR + BMXBMI + ALQ + DMDMARTL + LBXCOT , mix_name = name,
                 data = data, q = 4, validation = 0.6, b = 10, b1_pos = TRUE, b1_constr = FALSE, family = "binomial", seed = 1003)

summary(results1)
gwqs_barplot(results1)
gwqs_scatterplot(results1)
gwqs_fitted_vs_resid(results1)
ptbp = results1$final_weights
round(ptbp$mean_weight,4)
gwqs_summary_tab(results1)
ptbp
OR_ptb = logistic.display(results1$fit)
OR_ptb

# preprocess.R
# This script is used to preprocess the data for the analysis.
library(dplyr)
library(Hmisc)
source("./read_xpts.R")

# Read the data
MCQ = read_xpts("./MCQ")
UM = read_xpts("./UM")
demo = read_xpts("./DEMO")
ALQ = read_xpts("./ALQ")
BMI = read_xpts("./BMI")
COT = read_xpts("./LBXCOT")

# process the MCQ data
OA.ques = c("SEQN", "MCQ160A", "MCQ190", "MCQ195")
existing_columns = intersect(OA.ques, names(MCQ))
OA = MCQ[, existing_columns]
rm(MCQ)
has_MCQ160A = OA$MCQ160A == 1
has_MCQ190_or_MCQ195 = OA$MCQ190 == 2 | OA$MCQ195 == 1
OA["OA"] = ifelse(has_MCQ160A & has_MCQ190_or_MCQ195, 1, 0)
OA = OA[, c("SEQN", "OA")]

# process the UM data
metals = c("URXUBA", "URXUCD", "URXUCO", "URXUCS", "URXUMO", "URXUPB", "URXUSB", "URXUTL", "URXUTU")
UM9 = UM[, c("SEQN", metals)]
UM9 = UM9 %>%
  mutate(across(all_of(metals), ~ impute(., fun = median)))  # # using median to impute missing values

# process the demo data
extract = c("SEQN", "RIAGENDR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "DMDMARTL", "RIDAGEYR")
demo = demo[, extract]
demo = demo %>%
  mutate(across(all_of(c("INDFMPIR", "DMDMARTL")), ~ impute(., fun = median)))  # # using median to impute missing values

# process the ALQ data
alcohlo = ALQ[, c("SEQN", "ALQ100", "ALQ101")]
alcohlo["ALQ"] <- alcohlo %>%
  mutate(
    ALQ = case_when(
      ALQ100 == 1 | ALQ101 == 1 ~ 1,  # 任意为 1
      ALQ100 == 2 | ALQ101 == 2 ~ 2,  # 任意为 2
      TRUE ~ NA_real_                # 其他情况为 NA
    )
  ) %>%
  pull(ALQ)
alcohlo = alcohlo[, c("SEQN", "ALQ")]

# process the BMI data
BMI = BMI[, c("SEQN", "BMXBMI")]
BMI = BMI %>%
  mutate(across(all_of("BMXBMI"), ~ impute(., fun = median)))  # using median to impute missing values

# process the COT data
COT = COT[, c("SEQN", "LBXCOT")]
COT = COT %>%
  mutate(across(all_of("LBXCOT"), ~ impute(., fun = median)))  # using median to impute missing values

# Merge the data
merged = merge(OA, UM9, by = "SEQN")
merged = merge(merged, COT, by = "SEQN")
merged = merge(merged, BMI, by = "SEQN")
merged = merge(merged, demo, by = "SEQN")
merged = merge(merged, alcohlo, by = "SEQN")
rm(OA, UM, UM9, alcohlo, demo, BMI, COT, ALQ)

merged = na.omit(merged)
summary(merged)
# Save the data
write.csv(merged, "./data.csv", row.names = FALSE)
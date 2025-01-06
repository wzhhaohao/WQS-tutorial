# import packages
library(dplyr)
source("./read_xpts.R")

# read data
demo = read_xpts("./第一篇/DEMO/")
PBCD = read_xpts("./第一篇/PBCD/")

PBCD_G = read.xport("第二篇/PBCD/PBCD_G.xpt")
PBCD_H = read.xport("第二篇/PBCD/PBCD_H.xpt")
PBCD_I = read.xport("第二篇/PBCD/PBCD_I.xpt")
PBCD_J = read.xport("第二篇/PBCD/PBCD_J.xpt")

PBCD = list(PBCD_G, PBCD_H, PBCD_I, PBCD_J) %>% bind_rows()

# 重金属测量
metals = c("SEQN", "LBXBPB", "LBXBCD", "LBXTHG", "LBXBSE", "LBXBMN")

print(head(PBCD))









library(foreign)

# Read XPT files
read_xpts = function(path) {
    files = list.files(path, pattern = "\\.xpt$", full.names = TRUE)
    data.list = lapply(files, function(file) as.data.frame(read.xport(file)))

    # if (reduce) {
    # common_columns = Reduce(intersect, lapply(data.list, names))
    # merged_data = do.call(rbind, lapply(data.list, function(df) {
    #     df[, common_columns, drop = FALSE]
    # }))
    #     return(merged_data)
    # }

    data = bind_rows(data.list)


    data = data[!duplicated(data$SEQN, fromLast = TRUE), ]
    return(data)
}

# demo = read_xpts("./DEMO", reduce = TRUE)
# ncol(demo)
# summary(demo)
# # extract = c("SEQN", "RIAGENDR", "RIDRETH1", "DMDEDUC2", "INDFMPIR", "BMXBMI", "alq101", "DMDMARTL", "LBXCOT", "phy", "RIDAGEYR")
# # existing_columns = intersect(extract, names(demo))
# # x = demo[, existing_columns, drop = FALSE]
# # head(x)

# MCQ = Reduce(function(x, y) merge(x, y, all = TRUE), read_xpts("./MCQ"))

# ncol(MCQ)
# summary(MCQ)

# OA.ques = c("SEQN", "MCQ160A", "MCQ180A", "MCQ190", "MCQ195", "MCQ250D")
# existing_columns = intersect(OA.ques, names(MCQ))
# OA = MCQ[, existing_columns]
# OA["OA"] = ifelse(OA$MCQ160A & (OA$MCQ190 == 2 | OA$MCQ195 == 1), 1, 0)
# ncol(OA)
# summary(OA)

# ALQ = read_xpts("./ALQ", reduce = TRUE)
# ALQ = ALQ[, c("SEQN", "ALQ110")]
# ALQ["ALQ"] = ifelse(ALQ$ALQ110 == 1, 1, 0)


# UM = read_xpts("./UM", reduce = TRUE)
# summary(UM)

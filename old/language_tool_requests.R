# install.packages("curl")
# curl --data "language=en-US&text=a simple test" http://localhost:8081/v2/check

df = read.csv(file="../data/data_pm.csv")
df_short <- df[!is.na(df$TOTAL_REVIEW_USEFUL),]    ## remove rows that don't have null helpful vote
texts <- as.character(df_short$REVIEW_FULL_TEXT)

df_readability <- read.csv("../data/readblty.csv")


## cor(as.numeric(Y2),text_errors / df_readability$num_of_words) = -0.1705424
## check which length measure correlates the best
text_errors <- detect_text_errors(texts)

library(tm)
library(RWeka)
df = read.csv(file="../data/data_pm.csv")
df_short <- df[!is.na(df$TOTAL_REVIEW_USEFUL),]    ## remove rows that don't have null helpful vote

texts <- as.character(df_short$REVIEW_FULL_TEXT)  

txt <- VectorSource(texts[1:1500])
txt.corpus <- Corpus(txt)
txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
txt.corpus <- tm_map(txt.corpus, stripWhitespace)
txt.corpus <- tm_map(txt.corpus, stemDocument)
txt.corpus <- Corpus(VectorSource(txt.corpus))

Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

dtm_bigram <- DocumentTermMatrix(txt.corpus,control=list(tokenize = Tokenizer, weighting = function(x) weightTfIdf(x)))
dtm_matrix <- data.matrix(dtm_bigram)

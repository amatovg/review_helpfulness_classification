######## DEPRECATED, INCLUDED IN data_prep.R #######


df_rwr <- read.csv("../data/reviewer.csv")
    df <- read.csv("../data/data_pm.csv")

top1 <- ifelse(df_rwr$top1=="<NA>",0,1) 
top1[is.na(top1)]<-0


top10 <- ifelse(df_rwr$top10=="<NA>",0,1) 
top10[is.na(top10)]<-0

top50 <- ifelse(df_rwr$top50=="<NA>",0,1) 
top50[is.na(top50)]<-0

top500 <- ifelse(df_rwr$top500=="<NA>",0,1) 
top500[is.na(top500)]<-0

top1000 <- ifelse(df_rwr$top1000=="<NA>",0,1) 
top1000[is.na(top1000)]<-0

vine <- ifelse(df_rwr$vine=="<NA>",0,1) 
vine[is.na(vine)]<-0

top <- c()
for(i in 1:length(top1)){
  if (top1[i]==1|top10[i]==1|top50[i]==1|top500[i]==1|top1000[i]==1|vine[i]==1){
    top[i] = 1
  }else{
    top[i] = 0
  }
}

write.csv(data.frame(top1,top10,top50,top500,top1000,vine,top),"../data/top_reviewer.csv")
#############################################################################
prev_help <-c()
num_prev_review <-c()
for (i in 1:nrow(df)){
  date_temp <- as.numeric(as.Date(df$REVIEW_POSTED_DATE[i]))
  reviewer_temp <- as.character(df$REVIEWER_NAME[i])
  num_prev_review[i] <- nrow(df[(as.numeric(as.Date(df$REVIEW_POSTED_DATE)) < date_temp) & (as.character(df$REVIEWER_NAME)==reviewer_temp),,drop=FALSE])
  temp <- colMeans(df[(as.numeric(as.Date(df$REVIEW_POSTED_DATE)) < date_temp) & (as.character(df$REVIEWER_NAME)==reviewer_temp),c("TOTAL_REVIEW_USEFUL","TOTAL_REVIEW_RESPONSES"),drop=FALSE])
  prev_help[i] <- (temp[1]^2)/temp[2]
  if(i%%100==0)
    print (i)
}

prev_help[is.na(prev_help)] <- 0

period <- as.numeric(as.Date(df_short$REVIEW_POSTED_DATE))
period <- max(period)-period

library(NLP)
library(tm)
library(SnowballC)

txt <- as.character(df[,21])
txt <- VectorSource(txt)
txt.corpus <- Corpus(txt)
txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
txt.corpus <- tm_map(txt.corpus, stripWhitespace) 
txt.corpus <- tm_map(txt.corpus, stemDocument)
txt.corpus <- Corpus(VectorSource(txt.corpus))
dtm <- DocumentTermMatrix(txt.corpus)

## inspect(dtm)[1:10,101:110]
dtm.data.frame <- as.data.frame(inspect(dtm))

rating <- df_short$STAR_RATING
dtm.table <- as.data.frame(dtm.data.frame)

result <- svd(dtm.table)


library(randomForest)
set.seed(12356)
rf <- randomForest(x=data.frame(result$u[,1:25],df$STAR_RATING,num_prev_review,prev_help,period,top1,top10,top50,top500,top1000,vine,df_rwr$rwr_rank,df_rwr$help_vote,df_rwr$vote,num_of_words,FOG,FK,ARI,CLI),y=log(((df$TOTAL_REVIEW_USEFUL^2)/df$TOTAL_REVIEW_RESPONSES)+1))
importance(rf)
varImpPlot(rf)
############################################################
fit_1 <- lm(y=log(((df$TOTAL_REVIEW_USEFUL^2)/df$TOTAL_REVIEW_RESPONSES)+1) ~ df$STAR_RATING + num_prev_review + prev_help + period + top1+ top10 +top50 + top500 + top1000 + df_rwr$rwr_rank + df_rwr$help_vote +df_rwr$vote)


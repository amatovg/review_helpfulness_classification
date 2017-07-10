library("RWeka")
library("tm")
library(topicmodels)
library(svd)
library(corpcor)
library(irlba)
library(slam)
library(Matrix)


#####################
## 1. Read in Data

#####################

#txt_abbr <- read.csv('C:/Users/mjmeire/Documents/SAS_data/Text_mining/inputtextmodels_abbr.csv',header=TRUE, sep=",")
#txt_syn <- read.csv('C:/Users/mjmeire/Documents/SAS_data/Text_mining/inputtextmodels_syn.csv',header=TRUE, sep=",")
txt_stemmed <- read.csv('C:/Users/mjmeire/Documents/SAS_data/Text_mining/inputtextmodels_stemmed.csv',header=TRUE, sep=",")
txt_dep <- read.csv('C:/Users/mjmeire/Documents/SAS_data/Text_mining/inputtextmodels_dependent.csv',header=TRUE, sep=",")
other_var <- read.csv('C:/Users/mjmeire/Documents/SAS_data/Text_mining/extra_info_statuses2.csv',header=TRUE, sep=",")


#####################
## 2. Apply 5*2 fold CV

#####################

#-repeated cross validation (e.g., 5 times 2 fold, 1 times 2 fold)
txt_dep[,"dependent"] <- as.factor(txt_dep[,"dependent"] )
other_var[,"dependent"] <- as.factor(other_var[,"dependent"] )

set.seed(2)

partition <- function(y,p=0.5,times) {
  
  #STEP 1: split up 0 and 1
  class1_ind <- which(txt_dep[,"dependent"]==as.integer(levels(txt_dep[,"dependent"])[1]))
  class2_ind <- which(txt_dep[,"dependent"]==as.integer(levels(txt_dep[,"dependent"])[2]))
  
  l <- list()
  for (i in 1:times){
    
    #STEP 2: take subsamples for both 0 and 1
    class1_ind_train <- sample(class1_ind, floor(0.5*table(txt_dep[,"dependent"])[1]),replace=FALSE)
    class2_ind_train <- sample(class2_ind, floor(0.5*table(txt_dep[,"dependent"])[2]),replace=FALSE)
    
    class1_ind_test <- class1_ind[!class1_ind %in% class1_ind_train]
    class2_ind_test <- class2_ind[!class2_ind %in% class2_ind_train]
    
    class1_ind_train2 <- sample(class1_ind_train,length(class2_ind_train),replace=TRUE)
    class1_ind_test2 <- sample(class1_ind_test,length(class2_ind_test),replace=TRUE)
    
    #STEP 3: combine 0 and 1 for both train and test
    
    l[[i]] <- list(train=c(class1_ind_train2,class2_ind_train),test=c(class1_ind_test2,class2_ind_test))
  }
  return(l)
}
# Partition for full datasets

partition_txt <- partition(txt_dep[,"dependent"],times=5)

txt_stemmed_l <- list()
txt_stemmed_l[[5]] <- list()


for (a in 1:5){
  for (b in 1:2){
    txt_stemmed_l[[a]][[b]]<- txt_stemmed[sort(partition_txt[[a]][[b]]),]
  }
}


#####################
## 4. Create Term document matrices
#####################

#### First, create Term document matrices (which will be used as input to SVD) for the unigrams


# First unigram, then bigram
unigramF_stemmed <- list()
unigramF_stemmed[[5]] <- list()

Ngram <- function(inputset1,inputset2,mindegree,maxdegree){
  outputlist <- list()
  
  
  # training
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mindegree, max = maxdegree))
  tr <- DocumentTermMatrix(inputset1, control = list(tokenize = Tokenizer,
                                                     weighting =
                                                       function(x)
                                                         weightTf(x),
                                                     RemoveNumbers=TRUE,
                                                     removePunctuation=TRUE,
                                                     stripWhitespace= TRUE))
  # test
  test <- DocumentTermMatrix(inputset2, control = list(tokenize = Tokenizer,
                                                       weighting =
                                                         function(x)
                                                           weightTf(x),
                                                       RemoveNumbers=TRUE,
                                                       removePunctuation=TRUE,
                                                       stripWhitespace= TRUE))
  
  # Apply sparseness reduction
  # also reducing the number of documents (respondends) because there will be rows which will not have values anymore
  
  tr <- removeSparseTerms(tr,0.9999)
  
  # Reform the test DTM to have the same terms as the training case
  Intersect <- test[,intersect(colnames(test), colnames(tr))]
  diffCol <- tr[,setdiff(colnames(tr),colnames(test))]
  newCols <- as.simple_triplet_matrix(matrix(0,nrow=test$nrow,ncol=diffCol$ncol))
  newCols$dimnames <- diffCol$dimnames
  testNew<-cbind(Intersect,newCols)
  testNew<- testNew[,colnames(tr)]
  
  ## Convert term document matrices to common sparse matrices to apply efficient SVD algorithm
  
  dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}
  
  outputlist<- list(train=dtm.to.sm(tr),test=dtm.to.sm(testNew))
  
  
  
  return(outputlist)
}

for (i in 1:5){
  for (ii in 1:2){
    txt_stemmed_l[[i]][[ii]] <-Corpus(DataframeSource(as.data.frame(txt_stemmed_l[[i]][[ii]])))
  }
}

for (i in 1:5){
  for (ii in 1:2){
    iii <- 3-ii
    print(paste(i , " ",ii))
    unigramF_stemmed[[i]][[ii]] <-Ngram(txt_stemmed_l[[i]][[ii]],txt_stemmed_l[[i]][[iii]],1,1)
  }
}
##########################
# 5. Perform SVD

##########################

## Apply SVD

svdUniList_stemmed<- list()



SVD_all <- function(inputset,N1,N2,k){
  outputlist <- list()
  
  for (i in 1:N1){
    outputlist[[i]]<-list()
    
    for (ii in 1:N2){
      trainer <- irlba(t(inputset[[i]][[ii]][[1]]), nu=k, nv=k)
      tester <- as.data.frame(as.matrix(inputset[[i]][[ii]][[2]] %*% trainer$u %*%  solve(diag(trainer$d))))
      
      outputlist[[i]][[ii]]<- list(train = as.data.frame(trainer$v), test= tester)
      
    }
  }
  return(outputlist)
}



svdUniList_stemmed <- SVD_all(unigramF_stemmed,5,2,100)






#######################################################################

## original review data
df = read.csv(file="C:/Users/yue/Dropbox/YUFEI_MICHEL/2_Data/data_pm.csv")

library(NLP)
library(tm)
library(SnowballC)
##library(e1071)
##library(Matrix)
##library(SparseM)
##library(irlba)


 ## helpness as dependent varible, bag of words and star rating as predcitive variable

df_short <- df[!is.na(df[,24]),]

helpfulness <- df_short[,24]

txt <- as.character(df_short[,21])
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

## star rating
rating <- df_short$STAR_RATING
## extreme rating (1 star, 5 star)
extreme_review <- ifelse(rating>1&rating<5,0,1)

dtm.table <- as.data.frame(dtm.data.frame)

result <- svd(dtm.table)

write.csv(result$u[,1:25],"../2_Data/svd_top25.csv")
## svd in tm package 
## number of reviews from the same reviewer
## covariate 

## random forest
library(randomForest)

set.seed(12356)
rf <- randomForest(x=result$u[,1:25],y=as.factor(ifelse(helpfulness==0,0,1)))
pr <- predict(rf,type="prob")[,2]

library(AUC)
auc <- auc(roc(pr,as.factor(ifelse(helpfulness==0,0,1))))
auc

## all 3953, auc = 0.721
## first 1000, auc = 0.671.
## first 500, auc = 0.693
## first 250, auc = 0.712
## first 100, auc = 0.722
## first 50, auc = 0.724
## first 25, auc = 0.732
## first 10, auc = 0.714

plot(roc(pr,as.factor(ifelse(helpfulness==0,0,1))))

prev_help <-c()
num_prev_review <-c()
for (i in 1:nrow(df_short)){
  date_temp <- as.numeric(as.Date(df_short$REVIEW_POSTED_DATE[i]))
  reviewer_temp <- as.character(df_short$REVIEWER_NAME[i])
  num_prev_review[i] <- nrow(df_short[(as.numeric(as.Date(df_short$REVIEW_POSTED_DATE)) < date_temp) & (as.character(df_short$REVIEWER_NAME)==reviewer_temp),,drop=FALSE])
  temp <- colMeans(df_short[(as.numeric(as.Date(df_short$REVIEW_POSTED_DATE)) < date_temp) & (as.character(df_short$REVIEWER_NAME)==reviewer_temp),c("TOTAL_REVIEW_USEFUL","TOTAL_REVIEW_RESPONSES"),drop=FALSE])
  prev_help[i] <- (temp[1]^2)/temp[2]
  print (i)
}

prev_help[is.na(prev_help)] <- 0

period <- as.numeric(as.Date(df_short$REVIEW_POSTED_DATE))
period <- max(period)-period

write.csv(data.frame(prev_help,num_prev_review,period),"../2_Data/reviewer_product.csv")


set.seed(12356)
rf <- randomForest(x=data.frame(result$u[,1:25],df_short$STAR_RATING,num_prev_review,prev_help,period),y=log(((df_short$TOTAL_REVIEW_USEFUL^2)/df_short$TOTAL_REVIEW_RESPONSES)+1))
importance(rf)
varImpPlot(rf)

## literature review

## Q1 Which articles are there?
## Q2 If not, what else?

###############################################################
#######################adding more variables###################
###############################################################

## ref1. Korfiatis 2012
## introducing Guuning-Fog Index(FOG), Flesch-Kincaid Reading Ease(FK), 
## Automated Readability Index(ARI) and Coleman-Liau Index(CLI) 

## FOG
## FOG = 0.4*(Words/Sentence + 100*(ComplexWords/Words)), ComplexWords = Words have more than 2 syllables

library(qdap)
library(NLP)
library(tm)
library(openNLP)

text <- as.character(df_short[,21]) ## convert text of level type to character type

for(i in 1:length(text)){
  outfile <- paste("C:/review/",i,".txt",sep="")
  write.csv(text[i][2,],outfile,row.names=FALSE)
}

for(i in 1:length(text)){
  sentence <- sent_detect_nlp(text[i])
  outfile <- paste("../2_Data/sentence/",i,".txt",sep="")
  write.csv(sentence,outfile,row.names=FALSE)
}

sentence <- lapply(text,sent_detect_nlp) ## split text into sentences

num_of_sent<-c()
num_of_words <- c()
FOG <- c()
FK <-c()
ARI <-c()
CLI <- c()

for (i in 1:length(sentence)){
    num_of_sent <- length(unlist(sentence[i]))  ## count total sentences
          words <- unlist(strsplit(unlist(sentence[i]), "\\,+|\\.+|\\s+|!+|\\?+|\\*+|\\-+|:|;")) ## split sentence into words
          words <- words[words!=""]  ## remove empty strings in words
          words <- iconv(words, "latin1", "ASCII", sub="") ## remove unrecognizable symbol, i.e., issue caused by http hyperlink
num_of_words[i] <- length(words)  ## count total words
     sylb_count <- syllable_sum(words) ## count syllables of word  
    num_of_sylb <- sum(sylb_count[!is.na(sylb_count)])  ## count total syllables
    num_of_comp <- length(which(syllable_sum(words)>2)) ## count total words with >2 syllables 
     char_count <- character_count(words,missing=0,apostrophe.remove=TRUE,digit.remove=TRUE,count.space=FALSE)   ## count characters of word
    num_of_char <- sum(char_count) ## count total characters
         FOG[i] <- 0.4*((num_of_words[i]/num_of_sent) + 100*(num_of_comp/num_of_words[i]))
         FK[i]  <- 0.39*(num_of_words[i]/num_of_sent) + 11.8*(num_of_sylb/num_of_words[i])
         ARI[i] <- 4.71*(num_of_char/num_of_words[i]) + 0.5*(num_of_words[i]/num_of_sent) - 21.43
         CLI[i] <- 5.89*(num_of_char/num_of_words[i]) - 0.3*(num_of_sent/num_of_words[i]) - 15.8
         print (i)
} 

write.csv(data.frame(num_of_sent,num_of_words,FOG,FK,ARI,CLI),"C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/readblty_1.csv")
      df9 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/readblty.csv")
     df99 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/avg_star.csv")
    df999 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/top_reviewer.csv")
num_of_words <- df9[,2]
top <- df999[,8]
FOG <- df9[,3]
FK  <- df9[,4]
ARI <- df9[,5]
CLI <- df9[,6]
  avg_star<- df99[,2]
extremity <- (df_short$STAR_RATING - avg_star)/avg_star

set.seed(12356)
rf_1 <- randomForest(x=data.frame(result$u[,1:25],df_short$STAR_RATING,extremity,num_prev_review,prev_help,period,num_of_words,FOG,FK,ARI,CLI,top),y=log(((df_short$TOTAL_REVIEW_USEFUL^2)/df_short$TOTAL_REVIEW_RESPONSES)+1))

fit_1 <- lm(log(((df_short$TOTAL_REVIEW_USEFUL^2)/df_short$TOTAL_REVIEW_RESPONSES)+1) ~ df_short$STAR_RATING + num_prev_review + prev_help + period + FOG + FK + ARI + CLI+num_of_words+top)


importance(rf_1)
varImpPlot(rf_1)


## subjectivity based on TextBlob written in Python
sent_subj<-readLines("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/review.csv")

review_id <-c()
subjectivity <- c()
sentiment<-c()

sss <- strsplit(sent_subj, ",")
for (i in 1:length(sent_subj)){
     review_id[i] <- substr(sss[[i]][1],3,7)
  subjectivity[i] <- as.numeric(sss[[i]][2])
     sentiment[i] <- as.numeric(gsub('.{2}$', '', sss[[i]][3]))
}  

dff <- cbind(as.numeric(review_id),as.numeric(subjectivity),as.numeric(sentiment))
df_subj <- dff[order(dff[,1]),]
write.csv(df_subj,"C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/subj_semt.csv")

library(Hmisc)
describe(dff[,2])
####################################################################################################################################
## review subjectivity: subj,semt
## review readability: FOG,FK,ARI,CLI
## review extremity: extremity, extreme_review, star_rating
## review framing: num_of_words
## review recency: period
## bag of words: top25_svd, 
## review engagement: num_prev_review, prev_help
## reviewer info: reviewer_ranking,top_reviewer(top1,top10,top50,top500,top1000,vine)

df1 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/reviewer.csv")
df2 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/top_reviewer.csv")
df3 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/subj_semt.csv")
df4 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/svd_top25.csv")
df5 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/readblty.csv")
df6 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/data_pm.csv")
df7 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/reviewer_product.csv")
df8 <- read.csv("C:/Users/ricky/Dropbox/YUFEI_MICHEL/2_Data/avg_star.csv")

 reviewer_rank <- df1[,4]                    ## reviewer info: rank of reviewer
 helpful_vote  <- df1[,5]                    ## reviewer info: total helpful votes for the reviewer  
         vote  <- df1[,6]                    ## reviewer info: total votes for the reviewer
 top_reviewer  <- df2[,8]                    ## reviewer info: top reviewer
 subjectivity  <- df3[,3]                    ## review subjectivity: review subjectivity generated by TextBlob
    sentiment  <- df3[,4]                    ## review subjectivity: review sentiment generated by TextBlok
          svd  <- df4[,2:26]                 ## bag of words: top25 svd vectors generated from document-term matrix
 num_of_words  <- df5[,2]                    ## review framing: length of review by word
          FOG  <- df5[,3]                    ## readability         
           FK  <- df5[,4]                    ## readability
          ARI  <- df5[,5]                    ## readability
          CLI  <- df5[,6]                    ## readability 
review_useful  <- df6$TOTAL_REVIEW_USEFUL    ## response variable: helpful votes for the review
 review_votes  <- df6$TOTAL_REVIEW_RESPONSES ## response variable: votes for the review
      period   <- df7[,4]                    ## review recency: period since review posted, till the most recent review posted
num_prev_helpful <- df7[,2]                  ## reviewer engagement(in the product group): number of previous helpful votes for the reviewer
num_prev_review  <- df7[,3]                  ## reviewer engagement(in the product group): number of previous votes for the reviewer
     avg_star  <- df[,2]                     ## average star rating of the product

         star  <- df6$STAR_RATING            ## review extremity: star rating  
star_extremity <- (star - avg_star)/avg_star ## review extremity: star extremity
extreme_review <- ifelse(star>1&star<5,0,1)  ## review extremity: extreme review (star = c(1,5) or not)

df_predictors <- data.frame(reviewer_rank,helpful_vote,vote,top_reviewer,subjectivity,sentiment,svd,num_of_words,FOG,FK,ARI,CLI,period,num_prev_helpful,num_prev_review,avg_star,star,star_extremity,extreme_review)

##y <- log(((review_useful^2)/review_votes)+1)
y <- review_useful/review_votes

df_total <- data.frame(y,df_predictors,dtm.data.frame)

library(randomForest)
set.seed(12356)
rf <- randomForest(x=df_predictors,y)
importance(rf)
varImpPlot(rf)

varUsed(randomForest(x=df_predictors,y,ntree=100))

bound <- floor((nrow(df_total)/2))                      ## define % of training and test set
set.seed(12356)
 df_temp <- df_total[sample(nrow(df_total)), ]          ## sample rows 
df_train <- df_temp[1:bound, ]                          ## get training set
 df_test <- df_temp[(bound+1):nrow(df_temp), ]          ## get test set
 
rfcv(df_train[,-1],df_train[,1], cv.fold=5, scale="log", step=0.5) ## cross validation
 

        rf <- randomForest(df_train[,-1],df_train[,1],prox=TRUE)
     rf_pr <- predict(rf,df_test[,-1])

 plot(pred,df_test[,1])
 
 table(df_test[,1],test_pred)
 summary(lm(df_test[,1]~test_pred))
 
 
 
 
 set.seed(12356)
 rf <- randomForest(x=df_predictors,y=as.factor(ifelse(review_useful==0,0,1)))
 pr <- predict(rf,type="prob")[,2]
 varImpPlot(rf)

 library(AUC)
 auc <- auc(roc(pr,as.factor(ifelse(review_useful==0,0,1))))
 auc
 
 ##> auc
 ##[1] 0.8042538
 
 plot(roc(pr,as.factor(ifelse(review_useful==0,0,1))))
 
 pred_data <- as.matrix(df_predictors)
 

library(xgboost)
##y=as.factor(ifelse(review_useful==0,0,1))
df_total <- data.frame(y,df_predictors,dtm.data.frame)
bound <- floor((nrow(df_total)/2))                      ## define % of training and test set
set.seed(12356)
df_temp <- df_total[sample(nrow(df_total)), ]          ## sample rows 
df_train <- df_temp[1:bound, ]                          ## get training set
df_test <- df_temp[(bound+1):nrow(df_temp), ]     


set.seed(12356)
xgb <- xgboost(as.matrix(df_train[,-1]),df_train[,1],nrounds=1000)
xgb_pr <- predict(xgb,as.matrix(df_test[,-1]))


library(e1071)
svm_model <- svm(as.matrix(df_train[,-1]),df_train[,1])
svm_pr <- predict(svm_model,as.matrix(df_test[,-1]))


library(Metrics)
rmse(df_test[,1],xgb_pr)
## 0.3257462
rmse(df_test[,1],rf_pr)
## 0.3156442
rmse(df_test[,1],svm_pr)
## 0.3525892
rmse(df_test[,1],lm_pr)
## 0.4189512

lm_model <- lm(df_train[,1]~as.matrix(df_train[,-1]))
lm_pr    <- predict(lm_model,as.data.frame(df_test[,-1]))

auc <- auc(roc(pr,as.factor(ifelse(review_useful==0,0,1))))
auc

 install.packages("modEvA", repos = "http://R-Forge.R-project.org")
 

 
 
 
 
 library(modEvA)
 plotGLM(obs=df_test[,1],pred=test_pred, xlab = "Logit (Y)", ylab = "Predicted probability", main = "Model plot")
 
         
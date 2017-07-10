source("data_prep_functions.R")

## original review data
df <- read.csv(file="data/data_pm.csv")

df <- df[!is.na(df$TOTAL_REVIEW_USEFUL),]    ## remove rows that don't have null helpful vote
invalid_index <- c(641,2899) # see bottom of file for the reason of their removal (invalid votes data)
df <- df[-invalid_index,]

texts <- as.character(df$REVIEW_FULL_TEXT)  

##############                                 ######################
##############   PART 0: Dependent variables   ######################
##############                                 ######################

THRESHOLD = 0.6  ## Value comes from http://pages.stern.nyu.edu/~aghose/helpfulness_print.pdf (Ghose, Ipeirotis)

review_useful  <- df$TOTAL_REVIEW_USEFUL    ## response variable: helpful votes for the review
review_votes   <- df$TOTAL_REVIEW_RESPONSES ## response variable: votes for the review

Y1 <- log(((review_useful^2)/review_votes)+1)  ## dependent variable 1
Y2 <- review_useful/review_votes               ## dependent variable 2
Y3 <- as.factor(Y2 > 0)             ## dependent variable 3
Y4 <- as.factor(Y2 > THRESHOLD)     ## dependent variable 4

## write the dependent variables
df_all_dependent_vars <- data.frame(review_useful,review_votes,Y1,Y2,Y3,Y4)
write.csv(df_all_dependent_vars,"data/all_dep_vars.csv", row.names = FALSE)

##summary(Y1)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.0000  0.4055  0.6931  0.8845  1.0990  6.7250 

## summary(Y2)
## Min.    1st Qu. Median  Mean    3rd Qu. Max. 
## 0.000   0.500   1.000   0.732   1.000   1.000 

## summary(Y3)
## 0    1 
## 705 3247 

## summary(Y4)
## 0    1 
## 1160 2792 

#set.seed(126)
#train.index <- createDataPartition(Y4, p = .6, list = FALSE)   ## 60/40 split between train and testset
#write(train.index,"data/train_index.txt")
#train.index <- scan(file="data/train_index.txt") 

set.seed(126)
ynum <- as.numeric(Y4) -1
tr_indices <- createDataPartition(ynum, p = .75, list = FALSE) ##indexes of the whole train set
Y4_tr <- ynum[tr_indices]
te_indices <- setdiff(1:length(ynum),tr_indices) ## indexes of the whole test set
Y4_te <- ynum[te_indices]
val_indices <- createDataPartition(Y4_tr, p = .33333333, list = FALSE) ## indexes from the validation part of the train set
Y4_val <- Y4_tr[val_indices]
tr_tr_indices <- setdiff(1:length(Y4_tr),val_indices) ## indexes from the true train set
Y4_tr_tr <- Y4_tr[tr_tr_indices]
write(tr_indices,"data/tr_indices.txt") ## 75% from whole dataset
write(te_indices,"data/te_indices.txt") ## 25% from whole dataset
write(val_indices,"data/val_indices.txt") # 25% from whole dataset 
write(tr_tr_indices,"data/tr_tr_indices.txt") # 50% from whole dataset

##############                                         ##################
##############   PART 1. Compute factors as features   ##################
##############                                         ##################

## read review text and prepare text content as document term matrix(dtm)

dtm_tr <- create_dtm(texts[tr_indices]) #compute the SVD only for the train set
dtm_te <- create_dtm(texts[te_indices])

tr_texts <- texts[tr_indices]
dtm_val <- create_dtm(tr_texts[val_indices])
dtm_tr_tr <- create_dtm(tr_texts[tr_tr_indices])

# res <- factors_variance(dtm_tr)

svd_tr <- prcomp(dtm_tr)$x
svd_te <- prcomp(dtm_te)$x
svd_val <- prcomp(dtm_val)$x
svd_tr_tr <- prcomp(dtm_tr_tr)$x
write.csv(svd_tr,"data/svd_tr.csv", row.names = FALSE)        ## write data to file
write.csv(svd_te,"data/svd_te.csv", row.names = FALSE)        ## write data to file
write.csv(svd_val,"data/svd_val.csv", row.names = FALSE)        ## write data to file
write.csv(svd_tr_tr,"data/svd_tr_tr.csv", row.names = FALSE)        ## write data to file

##############                                 ######################
##############  PART 2: Metadata as features   ######################
##############                                 ######################

## computing the amount of prev reviews and the average of the hfness ratio in the dataset of reviewers, this per review
avg_prod_rating <- read.csv("data/avg_star.csv")$avg_star[-invalid_index]

df_meta <- prev_reviews_summary(df,avg_prod_rating)

write.csv(df_meta,"data/metadata.csv", row.names = FALSE)        ## write data to file

##############                                 ######################
##############  PART 3: Readability features   ######################
##############                                 ######################

## computing all readability and length features. Is slow.
df_readability <- compute_readability(texts)

write.csv(df_readability,"data/readblty.csv", row.names = FALSE)     ## write data to file


##############                                 ######################
##############  PART 4: Subjectivity features  ######################
##############                                 ######################

## Subjectivity analysis based on TextBlob, written in Python

## Preferably move this all to python itself, reduces the reading/writing of files.

textblob_results <- readLines("data/review.csv")    ## The input file is generated as list by Python

df_subj <- parse_textblob_results(textblob_results)
df_subj <- df_subj[-invalid_index,]

write.csv(df_subj,"data/subj_semt.csv", row.names = FALSE)             ## write data to file

##############                                 ######################
##############    PART 5: Reviewer features    ######################
##############                                 ######################

## Process the parsed reviewer rank information into features
df_reviewer <- read.csv("data/reviewer.csv")

df_reviewer_feats <- compute_reviewer_feats(df_reviewer[-invalid_index,],review_useful,review_votes)

invalid_check <- any(df_reviewer_feats$vote < 0 | df_reviewer_feats$help_vote > df_reviewer_feats$vote)
if(invalid_check){
  print("There is still erratic data present.")
}

write.csv(df_reviewer_feats,"data/reviewer_feats.csv", row.names = FALSE) 

## all top reviewer features are dropped due to not being useful, the aggregate is_top is more useful

##############                                 ######################
##############        COMBINE EVERYTHING       ######################
##############                                 ######################


#continued in reporting_performance.R


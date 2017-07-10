#install.packages("corrplot")
library(corrplot)

source("../model_functions.R")

feats <- read.csv("../data/all_features.csv")
dependent_vars <- read.csv("../data/dep_vars.csv")

y1 <- dependent_vars$Y1  ## dependent variable 1
y2 <- dependent_vars$Y2  ## dependent variable 2
y3 <- as.factor(dependent_vars$Y3)  ## dependent variable 3
y4 <- as.factor(dependent_vars$Y4)  ## dependent variable 4

## plot the correlation between features + print ordered correlations with the label vector
M <- cor(data.matrix(data.frame(y2,feats)))
corr_vector <- M[1,abs(M[1,])>0.03]
data.frame(sort(corr_vector))
corrplot(M,order="AOE")

##initial tests
perform_tests(feats, y4, print_info = TRUE)

#####                   #####
##### extremity feature #####
#####                   #####

##compute correlations for comparison
cor(as.numeric(y4),feats$extremity)
cor(as.numeric(y4),(feats$rating - feats$avg_prod_rating)/feats$rating)
cor(as.numeric(y4), abs(feats$rating - feats$avg_prod_rating)) ##higher absolute correlation but negative

##replace the extremity with its new values
old_extremity <- feats$extremity
feats$extremity <- abs(feats$rating - feats$avg_prod_rating)
perform_tests(feats, y4, print_info = TRUE) ## better results for xgboost!


#####                     #####
##### ratio of past votes #####
#####                     #####

cor(as.numeric(y4),feats$prev_help_ratio)
cor(as.numeric(y2),feats$prev_help_ratio) ##correlation doubled but still very low
cor(as.numeric(y4),feats$help_vote)
cor(as.numeric(y4),feats$vote)

cor(as.numeric(y4),ifelse(feats$vote == 0,0,feats$help_vote/feats$vote))
cor(as.numeric(y4),log((feats$help_vote+1)/(feats$vote-feats$help_vote+1))) ##higher correlation than the normal ratio but no significant performance increase
cor(as.numeric(y4),feats$hf_ratio)
cor(feats$prev_help_ratio,feats$hf_ratio) 

old_ratio <- feats$hf_ratio
feats$hf_ratio <- log((feats$help_vote+1)^2/(feats$vote-feats$help_vote+1))
perform_tests(feats, y4, print_info = TRUE)

source("../model_functions.R")

train.index <- scan(file="../data/train_index.txt") 

df_dependent_vars <- read.csv("../data/dep_vars.csv")
y1 <- df_dependent_vars$Y1  ## dependent variable 2

features <- read.csv("../data/all_features.csv")
svd_tr <- read.csv("../data/optimal_svd_100_tr.csv")
svd_te <- read.csv("../data/optimal_svd_100_te.csv") 

num_vectors <- 10
Xtr <- data.frame(svd_tr[,1:num_vectors],features[train.index,])
Xte <- data.frame(svd_te[,1:num_vectors],features[-train.index,])

for(TH in c(0,1,2,3,4,5,6)){
  print(TH)
  y5 <- as.factor(as.numeric(y1) > TH)
  Ytr <- y5[train.index]
  Yte <- y5[-train.index]
  mean(as.numeric(Ytr)-1)
  perform_tests(Xtr,Xte,Ytr,Yte, print_info = FALSE) ## threshold prediction: helpful or not, y
}


TH <- 3
print(TH)
y5 <- as.factor(as.numeric(y1) > TH)
Ytr <- y5[train.index]
Yte <- y5[-train.index]
mean(as.numeric(Ytr)-1)
perform_tests(Xtr,Xte,Ytr,Yte, print_info = TRUE) 
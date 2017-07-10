source("../model_functions.R")

train.index <- scan(file="../data/train_index.txt") 

df_dependent_vars <- read.csv("../data/dep_vars.csv")
y2 <- df_dependent_vars$Y2  ## dependent variable 2

features <- read.csv("../data/all_features.csv")
svd_tr <- read.csv("../data/optimal_svd_100_tr.csv")
svd_te <- read.csv("../data/optimal_svd_100_te.csv") 

num_vectors <- 10
Xtr <- data.frame(svd_tr[,1:num_vectors],features[train.index,])
Xte <- data.frame(svd_te[,1:num_vectors],features[-train.index,])

for(TH in c(0.1,0.2,0.4,0.5,0.6,0.8,0.9)){
  print(TH)
  y4 <- as.factor(y2 > TH)
  Ytr <- y4[train.index]
  Yte <- y4[-train.index]
  perform_tests(Xtr,Xte,Ytr,Yte, print_info = FALSE) ## threshold prediction: helpful or not, y4
}

## OUTPUT ##

# [1] 0.1
# 
# Random Forest
# AUC:		0.663
# ACC:		0.861
# Prec:		0.797
# Recl:		0.346
# F1:		0.482
# 
# SVM
# AUC:		0.598
# ACC:		0.844
# Prec:		0.824
# Recl:		0.207
# F1:		0.331
# 
# xgboost
# AUC:		0.785
# ACC:		0.858
# Prec:		0.741
# Recl:		0.369
# F1:		0.493
# 
# Logistic Regression
# AUC:		0.781
# ACC:		0.704
# Prec:		0.356
# Recl:		0.722
# F1:		0.477
# 
# Naive Bayes
# AUC:		0.626
# ACC:		0.533
# Prec:		0.254
# Recl:		0.773
# F1:		0.382
# 
# RF  has an AUC of 0.663
# SVM has an AUC of 0.598
# XGB has an AUC of 0.785
# LR  has an AUC of 0.781
# NB  has an AUC of 0.626
# 
# [1] 0.2
# 
# Random Forest
# AUC:		0.674
# ACC:		0.861
# Prec:		0.789
# Recl:		0.371
# F1:		0.505
# 
# SVM
# AUC:		0.634
# ACC:		0.840
# Prec:		0.684
# Recl:		0.301
# F1:		0.418
# 
# xgboost
# AUC:		0.791
# ACC:		0.854
# Prec:		0.720
# Recl:		0.384
# F1:		0.501
# 
# Logistic Regression
# AUC:		0.788
# ACC:		0.695
# Prec:		0.359
# Recl:		0.752
# F1:		0.486
# 
# Naive Bayes
# AUC:		0.626
# ACC:		0.534
# Prec:		0.259
# Recl:		0.775
# F1:		0.389
# 
# RF  has an AUC of 0.674
# SVM has an AUC of 0.634
# XGB has an AUC of 0.791
# LR  has an AUC of 0.788
# NB  has an AUC of 0.626
# 
# [1] 0.4
# 
# Random Forest
# AUC:		0.695
# ACC:		0.850
# Prec:		0.774
# Recl:		0.425
# F1:		0.549
# 
# SVM
# AUC:		0.657
# ACC:		0.822
# Prec:		0.651
# Recl:		0.369
# F1:		0.471
# 
# xgboost
# AUC:		0.798
# ACC:		0.840
# Prec:		0.733
# Recl:		0.404
# F1:		0.521
# 
# Logistic Regression
# AUC:		0.780
# ACC:		0.680
# Prec:		0.377
# Recl:		0.752
# F1:		0.502
# 
# Naive Bayes
# AUC:		0.613
# ACC:		0.512
# Prec:		0.277
# Recl:		0.791
# F1:		0.410
# 
# RF  has an AUC of 0.695
# SVM has an AUC of 0.657
# XGB has an AUC of 0.798
# LR  has an AUC of 0.780
# NB  has an AUC of 0.613
# 
# [1] 0.5
# 
# Random Forest
# AUC:		0.693
# ACC:		0.786
# Prec:		0.671
# Recl:		0.479
# F1:		0.559
# 
# SVM
# AUC:		0.672
# ACC:		0.743
# Prec:		0.550
# Recl:		0.508
# F1:		0.528
# 
# xgboost
# AUC:		0.746
# ACC:		0.769
# Prec:		0.618
# Recl:		0.485
# F1:		0.544
# 
# Logistic Regression
# AUC:		0.750
# ACC:		0.606
# Prec:		0.401
# Recl:		0.794
# F1:		0.533
# 
# Naive Bayes
# AUC:		0.584
# ACC:		0.523
# Prec:		0.340
# Recl:		0.725
# F1:		0.463
# 
# RF  has an AUC of 0.693
# SVM has an AUC of 0.672
# XGB has an AUC of 0.746
# LR  has an AUC of 0.750
# NB  has an AUC of 0.584
# 
# [1] 0.6
# 
# Random Forest
# AUC:		0.690
# ACC:		0.766
# Prec:		0.624
# Recl:		0.505
# F1:		0.558
# 
# SVM
# AUC:		0.672
# ACC:		0.739
# Prec:		0.561
# Recl:		0.510
# F1:		0.534
# 
# xgboost
# AUC:		0.771
# ACC:		0.778
# Prec:		0.664
# Recl:		0.490
# F1:		0.564
# 
# Logistic Regression
# AUC:		0.747
# ACC:		0.618
# Prec:		0.418
# Recl:		0.773
# F1:		0.543
# 
# Naive Bayes
# AUC:		0.575
# ACC:		0.517
# Prec:		0.344
# Recl:		0.713
# F1:		0.464
# 
# RF  has an AUC of 0.690
# SVM has an AUC of 0.672
# XGB has an AUC of 0.771
# LR  has an AUC of 0.747
# NB  has an AUC of 0.575
# 
# [1] 0.8
# 
# Random Forest
# AUC:		0.690
# ACC:		0.682
# Prec:		0.539
# Recl:		0.718
# F1:		0.616
# 
# SVM
# AUC:		0.654
# ACC:		0.717
# Prec:		0.652
# Recl:		0.435
# F1:		0.522
# 
# xgboost
# AUC:		0.728
# ACC:		0.709
# Prec:		0.603
# Recl:		0.531
# F1:		0.565
# 
# Logistic Regression
# AUC:		0.725
# ACC:		0.583
# Prec:		0.451
# Recl:		0.807
# F1:		0.579
# 
# Naive Bayes
# AUC:		0.542
# ACC:		0.506
# Prec:		0.387
# Recl:		0.667
# F1:		0.490
# 
# RF  has an AUC of 0.690
# SVM has an AUC of 0.654
# XGB has an AUC of 0.728
# LR  has an AUC of 0.725
# NB  has an AUC of 0.542
# 
# [1] 0.9
# 
# Random Forest
# AUC:		0.659
# ACC:		0.640
# Prec:		0.527
# Recl:		0.745
# F1:		0.617
# 
# SVM
# AUC:		0.662
# ACC:		0.699
# Prec:		0.648
# Recl:		0.497
# F1:		0.562
# 
# xgboost
# AUC:		0.731
# ACC:		0.658
# Prec:		0.551
# Recl:		0.666
# F1:		0.603
# 
# Logistic Regression
# AUC:		0.716
# ACC:		0.567
# Prec:		0.468
# Recl:		0.818
# F1:		0.596
# 
# Naive Bayes
# AUC:		0.537
# ACC:		0.539
# Prec:		0.426
# Recl:		0.526
# F1:		0.471
# 
# RF  has an AUC of 0.659
# SVM has an AUC of 0.662
# XGB has an AUC of 0.731
# LR  has an AUC of 0.716
# NB  has an AUC of 0.537

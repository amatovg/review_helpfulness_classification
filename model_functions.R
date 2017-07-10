## loading ML libraries
library(caret)
library(randomForest)
library(e1071)  ## SVM, Naive Bayes
library(xgboost)
library(AUC)
detach("package:AUC",unload=TRUE)
library(Metrics) ##Important to load Metrics BEFORE AUC to avoid masking the AUC.auc function
library(AUC)


xgb_auc_cmp <- function(X_tr,X_te,Y_tr,Y_te,model=FALSE){
  dtest <- xgb.DMatrix(data=scale(X_te[,]),label=as.numeric(Y_te)-1)
  dfinaltrain <- xgb.DMatrix(data=scale(X_tr[,]),label=as.numeric(Y_tr)-1)
  xgb_model <- xgb.train(data=dfinaltrain,nrounds=100,nthread=3,max_depth=6,objective="binary:logistic",eta=0.05,verbose = FALSE)
  xgb_pr <- predict(xgb_model,dtest)
  xgb_roc <- roc(xgb_pr,Y_te)
  xgb_auc <- auc(xgb_roc)
  if(model){
    return(xgb_model)
  } else{
    return(xgb_auc)
  }
}


report_performance <- function(predictions,labels){
  #purpose is to give all key statistics
  rocc <- roc(predictions,labels)
  auc <- auc(rocc)
  if(!is.factor(predictions)){
    predictions <- predictions >= 0.5 #cut off threshold
  }
  mat <- confusionMatrix(predictions,labels,mode="prec_recall")

  acc <- mat$overall["Accuracy"]
  recall <- mat$byClass["Recall"]
  precision <- mat$byClass["Precision"]
  f1 <- mat$byClass["F1"]
  
  cat(sprintf("AUC:\t\t%.3f", auc))
  cat(sprintf("\nACC:\t\t%.3f", acc))
  cat(sprintf("\nPrec:\t\t%.3f", precision))
  cat(sprintf("\nRecl:\t\t%.3f", recall))
  cat(sprintf("\nF1:\t\t%.3f\n", f1 ))
  
  return(c(auc,acc,recall,precision,f1))
}

factors_variance <- function(x){
  #used to plot the amt of variance explained by principal components of a dataset
  pc_tmp <- prcomp(x)
  norm_var <- pc_tmp$sdev^2 / sum(pc_tmp$sdev^2)
  print(head(norm_var),10)
  cum_var <- cumsum(norm_var)
  print(head(cum_var,10))
  plot(norm_var , pch=16, type="o", col="blue", xlab = "Principal components",ylab="Explained percentage of the variance",xlim=c(1,30),ylim=c(0,1))
  lines(cum_var , pch=13, type="o", col="red")
  return(pc_tmp)
}

perform_tests = function(X_tr,X_te,Y_tr,Y_te, classification = TRUE, print_info = FALSE){
#this function is not used any more

  #cat(sprintf("Tests started of %s\nSummary:\n", capture.output(print(substitute(labels)))))
  #print(summary(labels))
  #labels_tr <- as.factor(as.integer(as.logical(Y_tr)))
  #labels_te <- as.factor(as.integer(as.logical(Y_te)))
  
  
  #cat(sprintf("Baseline ACC:\t%.3f\n", mean(as.logical(Y_tr))))
  # Baseline = 0.706
  
  ## random forest model
  
  good_feats <- c("rank","num_of_chars","period","chars_entropy","alternative_extremity","extremity","PC19","PC17","alternative_avg_ratings","pct_uppercase","same_rating_reviews","sentiment","stddev_ratings")
  feat_mask <- (colnames(X_tr) %in% good_feats)
  
  cat("\nRandom Forest\n")
  set.seed(666)
  rf_model <-randomForest(X_tr,Y_tr,mtry=4*sqrt(ncol(X_tr)))
  rf_pr <- predict(rf_model,X_te)

  if(classification){
    rf_auc <- auc(roc(rf_pr,Y_te))
    report_performance(predictions = rf_pr,labels = Y_te)
  } else {
    rf_rmse <- rmse(Y_te,rf_pr)
  }

  if(print_info){
    varImpPlot(rf_model)
  }

  ## svm model
  cat("\nSVM\n")
  set.seed(666)
  svm_model <- svm(scale(X_tr),Y_tr, kernel = "linear", cost = 20, tolerance = 0.01) 
  svm_pr <- predict(svm_model,scale(X_te))

  if(classification){
    svm_auc <- auc(roc(svm_pr,Y_te))
    report_performance(predictions = svm_pr,labels = Y_te)
  } else {
    svm_rmse <- rmse(Y_te,svm_pr)
  }
  
  ## xgboost model
  cat("\nxgboost\n")
  set.seed(666)
  Mtr <- as.matrix(sapply(X_tr,as.numeric))
  Mte <- as.matrix(sapply(X_te,as.numeric))
  if(classification){
    xgb_model <- xgboost(Mtr,label = as.numeric(Y_tr)-1,nrounds=100,objective="binary:logistic",verbose=FALSE)
    xgb_pr <- predict(xgb_model,Mte,outputmargin=F)
    xgb_auc <- auc(roc(xgb_pr,Y_te))
    report_performance(xgb_pr,Y_te)
  } else {
    xgb_model <- xgboost(Mtr,label = as.numeric(Y_tr)-1,nrounds=1000, verbose = FALSE)
    xgb_pr <- predict(xgb_model,Mte,outputmargin=F)
    xgb_rmse <- rmse(Y_te,xgb_pr)
  }
  
  if(print_info){
    names <- dimnames(X_tr)[[2]]
    importance_matrix <- xgb.importance(names, model = xgb_model)
    print(importance_matrix[1:30,])
  }
  
  ## logistic regression model
  cat("\nLogistic Regression\n")
  set.seed(666)
  if(classification){
    lr_model <- glm(Y_tr ~., family=binomial(link='logit'), data=data.frame(scale(X_tr[,feat_mask])))
    lr_pr <- predict(lr_model,data.frame(scale(X_te[,feat_mask])))
    lr_auc <- auc(roc(lr_pr,Y_te))
    #na_features <- c("avg_sentence_length","l2_plus","l10_plus","pct_open_class_words","subjectivity_2","senti_diffs_per_ref")
    #feat_mask <- !(colnames(X_tr) %in% na_features)
    # sign_feats <- c("V1","V10","period","rating","is_moderateTRUE","alternative_eX_tremity","ARI","text_errors","pct_nouns","pct_wh_words","polarity","hf_ratio","rank")
    # feat_mask <- (colnames(X_tr) %in% sign_feats)
    #lr_model <- glm(Y_tr ~., family=binomial(link='logit'), data=data.frame(scale(X_tr[,feat_mask])))
    #lr_pr <- predict(lr_model,data.frame(scale(X_te[,feat_mask])))
    #lr_auc <- auc(roc(lr_pr,Y_te))
    report_performance(lr_pr,Y_te)
    if(print_info){
      print(summary(lr_model))
      #pvalues <- coef(summary(lr_model))[,4]
      # sign_feats <- names(pvalues[pvalues < 0.1])
      
    }
  } else {
    glm_model <- glm(Y_tr ~., data=X_tr)
    glm_pr <- predict(glm_model,X_te)
    glm_rmse <- rmse(Y_te,glm_pr)
  }

  if(classification){
    ##NB only takes factors as input for boolean vector
    for(idx in 1:length(X_te)){
      column <- X_te[,idx]
      if(typeof(column) == "logical"){
        X_te[,idx] <- factor(column)
      }
    }
    #X_te$is_moderate <- factor(X_te$is_moderate)
    #X_te$is_top <- factor(X_te$is_top)
    cat("\nNaive Bayes\n")
    ## naive bayes model
    set.seed(666)
    nb_model <- naiveBayes(X_tr,Y_tr)
    nb_pr <- predict(nb_model,X_te)
    nb_auc <- auc(roc(nb_pr,Y_te))
    report_performance(nb_pr,Y_te)
    
    ## auc = 0.6443377
    
    ## TODO: add accuracy, precision and recall where applicable
    cat(sprintf("\nRF  has an AUC of %.3f\n", rf_auc))
    cat(sprintf("SVM has an AUC of %.3f\n", svm_auc))
    cat(sprintf("XGB has an AUC of %.3f\n", xgb_auc))
    cat(sprintf("LR  has an AUC of %.3f\n", lr_auc))
    cat(sprintf("NB  has an AUC of %.3f\n\n", nb_auc))
  } else {
    cat(sprintf("\nRF  has an RMSE of %.3f\n", rf_rmse))
    cat(sprintf("SVM has an RMSE of %.3f\n", svm_rmse))
    cat(sprintf("XGB has an RMSE of %.3f\n", xgb_rmse))
    cat(sprintf("GLM  has an RMSE of %.3f\n\n", glm_rmse))
  }

}
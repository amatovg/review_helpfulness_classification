source("model_functions.R")

library(Cairo)
library(lattice)
library(corrplot)

tr_indices <- scan(file="data/tr_indices.txt") 
te_indices <- scan(file="data/te_indices.txt") 
val_indices <- scan(file="data/val_indices.txt") 
tr_tr_indices <- scan(file="data/tr_tr_indices.txt") 

df_all_dependent_vars <- read.csv("data/dep_vars.csv")

df_meta <- read.csv("data/metadata.csv")
df_readability <- read.csv("data/readblty.csv")
df_subj <- read.csv("data/subj_semt.csv")
df_reviewer_feats <- read.csv("data/reviewer_feats.csv")
features <- data.frame(df_meta,df_readability,df_subj,df_reviewer_feats) 

#first create these files by executing PART 1 of data_prep.R 
#these files were too large to include in the repository
svd_tr <- read.csv("data/svd_tr.csv")        
svd_te <- read.csv("data/svd_te.csv")       
svd_val <- read.csv("data/svd_val.csv")       
svd_tr_tr <- read.csv("data/svd_tr_tr.csv")     

Y <- as.factor(df_all_dependent_vars$Y4)
Y_tr <- Y[tr_indices]
Y_te <- Y[te_indices]
Y_val <- Y[val_indices]
Y_tr_tr <- Y[tr_tr_indices]

num_factors <- 20
X_tr <- data.frame(svd_tr[,1:num_factors],features[tr_indices,])
X_te <- data.frame(svd_te[,1:num_factors],features[te_indices,])
X_val <- data.frame(svd_val[,1:num_factors],features[val_indices,])
X_tr_tr <- data.frame(svd_tr_tr[,1:num_factors],features[tr_tr_indices,])


#perform_tests(X_tr,X_te,Y_tr,Y_te, print_info = FALSE) ## threshold prediction: helpful or not, y4


##### xgboost optimalisation ######
dval <- xgb.DMatrix(data=scale(X_val[,]),label=as.numeric(Y_val)-1)
dtrain <- xgb.DMatrix(data=scale(X_tr_tr[,]),label=as.numeric(Y_tr_tr)-1)
dtest <- xgb.DMatrix(data=scale(X_te[,]),label=as.numeric(Y_te)-1)
dfinaltrain <- xgb.DMatrix(data=scale(X_tr[,]),label=as.numeric(Y_tr)-1)

# watchlist_val <- list(train=dtrain,test=dval)
# bst <- xgb.train(data=dtrain,nrounds=100,nthread=3,max_depth=6,watchlist=watchlist_val,objective="binary:logistic",eta=0.05,verbose = FALSE)
# xgb_pr <- predict(bst,dval)
# xgb_roc <- roc(xgb_pr,Y_val)
# xgb_auc <- auc(xgb_roc)
# print(xgb_auc)

##final round
xgb_model <- xgb.train(data=dfinaltrain,nrounds=100,nthread=3,max_depth=6,objective="binary:logistic",eta=0.05,verbose = FALSE)
xgb_pr <- predict(xgb_model,dtest)
xgb_roc <- roc(xgb_pr,Y_te)
xgb_auc <- auc(xgb_roc)
print(xgb_auc)

############# logistic regression ##############

good_feats <- c("rank","num_of_chars","period","chars_entropy","alternative_extremity","PC1",
                "alternative_avg_ratings","pct_uppercase","same_rating_reviews","sentiment","stddev_ratings")

feat_mask <- (colnames(X_tr) %in% good_feats)

# lr_model <- glm(Y_tr_tr ~., family=binomial(link='logit'), data=data.frame(scale(X_tr_tr[,feat_mask])))
# lr_pr <- predict(lr_model,data.frame(scale(X_val[,feat_mask])))
# lr_roc <- roc(lr_pr,Y_val)
# lr_auc <- auc(lr_roc)
# print(lr_auc)

lr_model <- glm(Y_tr ~., family=binomial(link='logit'), data=data.frame(scale(X_tr[,feat_mask])))
lr_pr <- predict(lr_model,data.frame(scale(X_te[,feat_mask])))
lr_roc <- roc(lr_pr,Y_te)
lr_auc <- auc(lr_roc)
print(lr_auc)

############# svm ##################

# svm_model <- svm(scale(X_tr_tr[,feat_mask]),Y_tr_tr, kernel = "linear", cost = 20, tolerance = 0.01) # default SVM has accuracy of 0.771 but much lower recall, F1 and AUC
# svm_pr <- predict(svm_model,scale(X_val[,feat_mask]))
# svm_roc <- roc(svm_pr,Y_val)
# svm_auc <- auc(svm_roc)
# print(svm_auc)

svm_model <- svm(scale(X_tr[,feat_mask]),Y_tr, kernel = "linear", cost = 20, tolerance = 0.01) 
svm_pr <- predict(svm_model,scale(X_te[,feat_mask]))
svm_roc <- roc(svm_pr,Y_te)
svm_auc <- auc(svm_roc)
print(svm_auc)

############# random forest ##################
# rf_model <-randomForest(X_tr_tr,Y_tr_tr,mtry=4*sqrt(ncol(X_tr_tr)),ntree=1000)
# rf_pr <- predict(rf_model,X_val,type="prob")
# rf_roc <- roc(rf_pr[,2],Y_val)
# rf_auc <- auc(rf_roc)
# print(rf_auc)


rf_model <-randomForest(X_tr,Y_tr,mtry=4*sqrt(ncol(X_tr)),ntree=1000)
rf_pr <- predict(rf_model,X_te,"prob")[,2]
rf_roc <- roc(rf_pr,Y_te)
rf_auc <- auc(rf_roc)
print(rf_auc)

############ NB #################

# nb_model <- naiveBayes(X_tr_tr[,feat_mask],Y_tr_tr)
# nb_pr <- predict(nb_model,X_val[,feat_mask])
# nb_roc <- roc(nb_pr,Y_val)
# nb_auc <- auc(nb_roc)
# print(nb_auc)

nb_model <- naiveBayes(X_tr[,feat_mask],Y_tr)
nb_pr <- predict(nb_model,X_te[,feat_mask])
nb_roc <- roc(nb_pr,Y_te)
nb_auc <- auc(nb_roc)
print(nb_auc)

###################### PLOTTING ########################
#latex folder
directory = "/home/amato/Dropbox/School/UGent/Bedrijfseconomie/Thesis_v2/thesis_reviews_book"
mysize=1.5
colors_blue <- c("#084594", "#4292C6", "#9ECAE1", "#C6DBEF", "#F7FBFF")
colors_spread <- c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
cs <- colors_spread

############################### METRICS computation ##########

res_metrics <- matrix(, nrow = 5, ncol = 5)
res_metrics[1,] <- report_performance(xgb_pr,Y_te)
res_metrics[2,] <-  report_performance(lr_pr,Y_te)
res_metrics[3,] <-  report_performance(rf_pr,Y_te)
res_metrics[4,] <-  report_performance(svm_pr,Y_te)
res_metrics[5,] <-  report_performance(nb_pr,Y_te)
df_metrics <- data.frame(res_metrics)
colnames(df_metrics) <- c("AUC","Accuracy","Precision","Recall","F1-score")
rownames(df_metrics) <- c("xgboost","LR","RF","SVM","NB")
d <- data.matrix(df_metrics)

######################## PLOT METRICS IN BARPLOT ###############

Cairo(file=sprintf("%s/fig/1_metrics_plot.png",directory), 
      bg="white",
      type="png",
      units="in", 
      width=8, 
      height=8, 
      pointsize=12, 
      dpi=300)
barplot(d, beside = TRUE, ylim=c(0,1), legend.text = rownames(d),
        col=colors_blue,
  args.legend = list(x = "topright", bty="n",cex=1.2),
  cex.lab=mysize, cex.axis=mysize, cex.main=mysize, cex.sub=mysize,cex=1.3)
title("Evaluation metrics for the 5 classifiers",cex.main=2)
dev.off()

################ ROC CURVES ################ 
Cairo(file=sprintf("%s/fig/2_ROC_curves.png",directory), 
      bg="white",
      type="png",
      units="in", 
      width=8, 
      height=8, 
      pointsize=12, 
      dpi=300)
plot(c(0,1),c(0,1),type="n", xlab = "False Positive Rate (FPR)",ylab="True Positive Rate (TPR)",
     cex.lab=mysize, cex.axis=mysize, cex.main=mysize, cex.sub=mysize)
title("ROC curves from the 5 classifiers",cex.main=2)
lines(xgb_roc$fpr,xgb_roc$tpr,col=cs[1],lwd=2.5)
lines(lr_roc$tpr,x=lr_roc$fpr,col=cs[2],lwd=2.5)
lines(rf_roc$fpr,rf_roc$tpr,col=cs[3],lwd=2.5)
lines(svm_roc$tpr,x=svm_roc$fpr,col=cs[4],lwd=2.5)
lines(nb_roc$fpr,nb_roc$tpr,col=cs[5],lwd=2.5)
lines(c(0,1),c(0,1),col="black",lwd=1)
legend(0.65,0.35,
       c("xgboost",
         "LR",
         "RF",
         "SVM",
         "NB"),
       lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2),
       col=colors_spread,
       cex=1.2)
dev.off()


############## PLOT IMPORTANCE OF FEATURES ##############

code_feat_names <- colnames(X_te)
code_feat_names[which(code_feat_names == "period")] <- "age"
code_feat_names[which(code_feat_names == "same_rating_reviews")] <- "conformity"
code_feat_names[which(code_feat_names == "polarity")] <- "polarity2"
code_feat_names[which(code_feat_names == "sentiment")] <- "polarity"
xgb_impt15 <- xgb.importance(feature_names = code_feat_names, xgb_model)[1:15,]

Cairo(file=sprintf("%s/fig/3_xgb_importance.png",directory), 
      bg="white",
      type="png",
      units="in", 
      width=8, 
      height=8, 
      pointsize=12, 
      dpi=300)
xgb.plot.importance(xgb_impt15, cex=1,xlim=c(0,0.2),xlab="Gain")
dev.off()

################# Importance of featyres ##############
print(xgb_impt15$Feature)

rf_impt <- importance(rf_model)
mask <- which(rf_impt > 17) #top15 feats
rf_impt_15 <- rf_impt[mask]
rf_names_15 <- rownames(rf_impt)[mask]
names(rf_impt_15) <- rf_names_15
print(names(sort(rf_impt_15,decreasing=TRUE)))
varImpPlot(rf_model)

#check the importance 

many_good_feats <- c("rank","num_of_chars","period","chars_entropy","alternative_extremity","PC1",
                     "alternative_avg_ratings","pct_uppercase","same_rating_reviews","sentiment","stddev_ratings",
                     "vote", "rating","hf_ratio" ,"DC","extremity","PC19","PC17","PC20","PC18","PC6")
many_feat_mask <- (colnames(X_tr) %in% many_good_feats)
alt_lr_model <- glm(Y_tr ~., family=binomial(link='logit'), data=data.frame(scale(X_tr[,many_feat_mask])))
alt_lr_pr <- predict(alt_lr_model,data.frame(scale(X_te[,many_feat_mask])))
alt_lr_roc <- roc(alt_lr_pr,Y_te)
print(auc(alt_lr_roc))
summary(alt_lr_model)


################## plotting of factors variance  #######################

if(FALSE){ ##only  execute when data_prep.R wasn't executed yet

  df <- read.csv(file="data/data_pm.csv")
  tr_indices <- scan(file="data/tr_indices.txt")
  df <- df[!is.na(df$TOTAL_REVIEW_USEFUL),]    ## remove rows that don't have null helpful vote
  invalid_index <- c(641,2899) # see bottom of file for the reason of their removal (invalid votes data)
  df <- df[-invalid_index,]
  texts <- as.character(df$REVIEW_FULL_TEXT)
  dtm_tr <- create_dtm(texts[tr_indices]) #compute the SVD only for the train set
  pc_tmp <- factors_variance(dtm_tr)
}

norm_var <- pc_tmp$sdev^2 / sum(pc_tmp$sdev^2)
cum_var <- cumsum(norm_var)

Cairo(file=sprintf("%s/fig/factors_variance_importance.png",directory), 
      bg="white",
      type="png",
      units="in", 
      width=8, 
      height=8, 
      pointsize=12, 
      dpi=300)
plot(norm_var , pch=16,lwd=2, type="o", col="blue", xlab = "Principal components",ylab="Explained percentage of the variance",
     xlim=c(0,30),ylim=c(0,0.5),cex.lab=mysize, cex.axis=mysize, cex.main=mysize, cex.sub=mysize)
lines(cum_var , pch=13,lwd=2, type="o", col="red")
legend(0,0.5,
       c("Explained variance per factor",
         "Cumulative explained variance"),
       pch=c(16,13),
       col=c("blue","red"),
       cex=1.5)
dev.off()
norm_var[1]
length(which(cum_var < 0.5))


################# impact of amount of principal components ################


num_factors_v <- c(0,5,10,20,30,40,50,75,100,200,300,400,500)
AUCs <- c()
for(nmf in num_factors_v){
  X_tr <- data.frame(svd_tr[,1:nmf],features[tr_indices,])
  X_te <- data.frame(svd_te[,1:nmf],features[te_indices,])
  dtest <- xgb.DMatrix(data=scale(X_te[,]),label=as.numeric(Y_te)-1)
  dfinaltrain <- xgb.DMatrix(data=scale(X_tr[,]),label=as.numeric(Y_tr)-1)
  xgb_model <- xgb.train(data=dfinaltrain,nrounds=100,nthread=3,max_depth=6,objective="binary:logistic",eta=0.05,verbose = FALSE)
  xgb_pr <- predict(xgb_model,dtest)
  xgb_roc <- roc(xgb_pr,Y_te)
  xgb_auc <- auc(xgb_roc)
  print(xgb_auc)
  AUCs <- append(AUCs,xgb_auc)
}

Cairo(file=sprintf("%s/fig/4_factors_impact_results.png",directory), 
      bg="white",
      type="png",
      units="in", 
      width=8, 
      height=8, 
      pointsize=12, 
      dpi=300)
plot(num_factors_v,AUCs, pch=5,lwd=2, type="b", col="blue", xlab = "Number of principal components used as features",ylab="Area Under Curve (AUC)",
     axes = FALSE,cex.lab=mysize, cex.axis=mysize, cex.main=mysize, cex.sub=mysize)
abline(v=20,col="red",lwd=2)
axis(side = 1, at = c(0,num_factors_v),cex.axis=mysize)
axis(side = 2, at = c(0.72,0.73,0.74,0.75,0.76,0.77,0.78))
text(x=40,y=0.723,labels = "20",cex=mysize,col="red")
text(x=85,y=0.775,labels = "AUC = 0.775",cex=mysize,col="blue")
legend(0,0.5,
       c("Explained variance per factor",
         "Cumulative explained variance"),
       pch=c(16,13),
       col=c("blue","red"),
       cex=1.5)
dev.off()
################# impact of the feature categories ################

feat_cats <- c("META","PROD","READ","STYL","LING","SUBJ","LENG","ENTR","RVWR","TBOW")

#META - Review 
meta_feats    <- c("rating","is_moderate","period","extremity","alternative_extremity","same_rating_reviews")
#PROD - Product
prod_feats    <- c("avg_prod_rating","alternative_avg_ratings","stddev_ratings")
#READ - Readability Metrics
read_feats    <- c("FOG","FK","ARI","CLI","FRE","SMOG","DC","text_errors","avg_sentence_length")
#STYL - Text Characteristics
styl_feats    <- c("pct_caps_lock_words","pct_uppercase","num_of_digits","num_excl_marks","num_ques_marks",
                   "num_punctuation","num_commas","l1","l2","l2_plus","l2_l9","l10_plus")
#LING - Linguistic
ling_feats    <- c("num_unique_words","num_stop_words","pct_adjectives","pct_adverbs","pct_nouns","pct_verbs",
                   "pct_wh_words","pct_compar_words","pct_open_class_words")
#SUBJ - Subjectivity
subj_feats    <- c("subjectivity","sentiment","anger","anticipation","disgust","fear","joy","negative",
                   "positive","sadness","surprise","trust","polarity","subjectivity_2","senti_diffs_per_ref")
#LENG - Length
leng_feats    <- c("num_of_chars","num_of_words","num_of_sents","num_of_sylb")
#ENTR - Entropy
entr_feats    <- c("words_entropy","chars_entropy","length_entropy")
#RVWR - Reviewer
rvwr_feats    <- c("help_vote","vote","hf_ratio","prev_help_ratio","num_prev_review","rank","is_top","hof","vine")
#TBOW - Text (BOW)
tbow_feats    <- c(1:20)
for(i in 1:length(tbow_feats)){
  nmb <- tbow_feats[i]
  tbow_feats[i] <- paste(c("PC",nmb), collapse = '')
}

feat_names <- list()
feat_names[[1]] <- meta_feats
feat_names[[2]] <- prod_feats
feat_names[[3]] <- read_feats
feat_names[[4]] <- styl_feats
feat_names[[5]] <- ling_feats
feat_names[[6]] <- subj_feats
feat_names[[7]] <- leng_feats
feat_names[[8]] <- entr_feats
feat_names[[9]] <- rvwr_feats
feat_names[[10]] <- tbow_feats

fcats_tr <- list()
fcats_te <- list()
for(i in 1:10){
  fcats_tr[[i]] <- X_tr[,feat_names[[i]]]
  fcats_te[[i]] <- X_te[,feat_names[[i]]]
}

comb <- do.call(cbind,fcats_tr)
setdiff(colnames(X_tr),colnames(comb))
setdiff(colnames(comb),colnames(X_tr))

setdiff(colnames(features),colnames(x_tr))
setdiff(colnames(x_tr),colnames(features))

top_auc <- xgb_auc_cmp(X_tr,X_te,Y_tr,Y_te)

AUCs_out <- c()
for(i in 1:10){
  x_tr <- data.frame(do.call(cbind,fcats_tr[-i]))
  x_te <- data.frame(do.call(cbind,fcats_te[-i]))
  xgb_auc <- xgb_auc_cmp(x_tr,x_te,Y_tr,Y_te)
  cat(feat_cats[i])
  print(xgb_auc)
  AUCs_out <- append(AUCs_out,xgb_auc)
}

AUCs_in <- c()
for(i in 1:10){
  x_tr <- fcats_tr[[i]]
  x_te <- fcats_te[[i]]
  xgb_auc <- xgb_auc_cmp(x_tr,x_te,Y_tr,Y_te)
  cat(feat_cats[i])
  print(xgb_auc)
  AUCs_in<- append(AUCs_in,xgb_auc)
}

#fix names for plot
fcats_out_names <- c()
for(i in 1:length(feat_cats)){
  nm <- feat_cats[i]
  fcats_out_names[i] <- paste(c("F \\ ",nm), collapse = '')
}
#blue_colours_10 <- c("#0000ff","#1919ff","#3232ff","#4c4cff","#6666ff","#7f7fff","#9999ff","#b2b2ff","#ccccff","#e5e5ff")

write.csv(data.frame(AUCs_out,AUCs_in),"results/AUC_cats.csv",row.names = FALSE)

############################ BARPLOT OF THE AUC #####################

Cairo(file=sprintf("%s/fig/5_feats_cat_excluded.png",directory), 
      bg="white",
      type="png",
      units="in", 
      width=8, 
      height=8, 
      pointsize=12, 
      dpi=300)
barplot(1-AUCs_out,ylab="1 - Area Under Curve (1-AUC)",
        names.arg=fcats_out_names,
        las=3,ylim=c(0,0.35),xpd=FALSE,col=c("darkblue"),
        cex.lab=1.2, cex.axis=mysize, cex.main=mysize, cex.sub=mysize)
abline(h=1-top_auc,col="red",lwd=2)
text(x=5,y=0.26,labels = "1-AUC = 0.225",cex=mysize,col="red")
box()
dev.off()

Cairo(file=sprintf("%s/fig/6_feats_cat_only.png",directory), 
      bg="white",
      type="png",
      units="in", 
      width=8, 
      height=8, 
      pointsize=12, 
      dpi=300)
barplot(AUCs_in,ylab="Area Under Curve (AUC)",
        names.arg=feat_cats,
        las=3,ylim=c(0,0.9),xpd=FALSE,col=c("darkgreen"),
        cex.lab=1.2, cex.axis=mysize, cex.main=mysize, cex.sub=mysize)
abline(h=top_auc,col="red",lwd=2)
text(x=5,y=0.805,labels = "AUC = 0.775",cex=mysize,col="red")
box()
dev.off()


################### INTRA FEATURE CATEGORY ####################


feat_names[[1]][which(feat_names[[1]] == "period")] <- "age"
feat_names[[1]][which(feat_names[[1]] == "same_rating_reviews")] <- "conformity"
feat_names[[1]][which(feat_names[[1]] == "alternative_extremity")] <- "altExtremity"
feat_names[[6]][which(feat_names[[6]] == "polarity")] <- "polarity2"
feat_names[[6]][which(feat_names[[6]] == "sentiment")] <- "polarity"

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

pcs_list <- list()
for(i in 1:10){
  x_tr <- fcats_tr[[i]]
  x_te <- fcats_te[[i]]
  xgb_model <- xgb_auc_cmp(x_tr,x_te,Y_tr,Y_te,model = TRUE)
  xgb_imp <- xgb.importance(feature_names = feat_names[[i]],xgb_model)
  print(xgb_imp)
  
  #importance plot
  Cairo(file=sprintf("%s/fig/7_%d_xgb_importance.png",directory,i), 
        bg="white",
        type="png",
        units="in", 
        width=8, 
        height=8, 
        pointsize=12, 
        dpi=300)
  xgb.plot.importance(xgb_imp, cex=1.1,xlab="Gain",mar=c(5,5,5,5), oma=c(3,4,0,0))
  dev.off()
  
  # corr plot
  colnames(x_tr) <- feat_names[[i]]
  Y <- Y_tr
  M <- cor(data.matrix(data.frame(Y,x_tr)))
  Cairo(file=sprintf("%s/fig/8_%d_feats_correlation.png",directory,i), 
        bg="white",
        type="png",
        units="in", 
        width=8, 
        height=8, 
        pointsize=12, 
        dpi=300)
  corrplot(M, method="color", col=col(200),  
           diag=FALSE, # tl.pos="d", 
           type="upper", order="hclust", 
          # title=sprintf("Visualised correlation matrix of %s and review helpfulness Y",feat_cats[i]), 
           addCoef.col = "black",mar = c(0,0,1,0)
  )
  dev.off()
  
  #features variance plot
  pc_tmp <- prcomp(x_tr)
  norm_var <- pc_tmp$sdev^2 / sum(pc_tmp$sdev^2)
  cum_var <- cumsum(norm_var)
  pcs_list <- append(pcs_list,pc_tmp)
  
  Cairo(file=sprintf("%s/fig/9_%d_PCs_variation.png",directory,i), 
        bg="white",
        type="png",
        units="in", 
        width=8, 
        height=8, 
        pointsize=12, 
        dpi=300)
  plot(norm_var , pch=16, lwd=2,type="o", col="blue", xlab = "Principal components",ylab="Explained percentage of the variance",ylim=c(0,1), cex=1.5,cex.axis=1.5,cex.lab=1.5)
  lines(cum_var , pch=13, lwd=2,type="o", col="red")
  dev.off()
}



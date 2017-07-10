## Installing all required packages

##for scraping.R
#run sudo apt-get install libssl-dev
install.packages("openssl")
install.packages("httr")
install.packages("rvest")

##for model.R
install.packages("Metrics")
install.packages("randomForest")
install.packages("e1071")
install.packages("xgboost")
install.packages("AUC")
install.packages("caret")

##for data_prep.R
install.packages("NLP")
install.packages("tm")
install.packages("SnowballC")
install.packages("rJava")
install.packages("openNLP")
install.packages("jsonlite")
install.packages("RCurl")
install.packages("koRpus") ##requires install of treetagger on the system
install.packages("stringi")
install.packages("irlba")

## qdap installation ##

## run in terminal sudo apt-get update && sudo apt-get install libxml2-dev
install.packages("XML")

## run in terminal sudo apt-get install libcurl4-gnutls-dev
install.packages("qdapTools")
install.packages("gender")

## download https://github.com/trinker/qdap/tarball/master and run R CMD INSTALL <tarball>
install.packages("qdap")

## run in terminal sudo apt-get install libcairo2-dev
install.packages("Cairo")
install.packages("corrplot")



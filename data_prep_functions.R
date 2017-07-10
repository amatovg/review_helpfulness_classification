## loading required libraries to prepare SVD vectors
library(caret)
library(NLP)
library(tm)
library(SnowballC)
## loading AUC to evaluate predictive accurary on the binary helpfulness variable
library(AUC)
## loading random forest to evaluate how many SVD vectors are needed for predictive modeling
library(randomForest) 
##efficient implementation of SVD
library(irlba)
## loading NLP related libraries
library(qdap)
library(tm)
library(rJava)
library(openNLP)
library(koRpus) 
##install.packages("koRpus")
## set.kRp.env(TT.cmd="/usr/bin/TreeTagger/cmd/tree-tagger-english", lang="en")
library(RCurl)
library(tm)
library(jsonlite)
library(stringi)



## process text and create the Document Term Matrix
create_dtm = function(texts){
  txt <- VectorSource(texts) 
  txt.corpus <- Corpus(txt) 
  txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))  
  txt.corpus <- tm_map(txt.corpus, removeNumbers)
  txt.corpus <- tm_map(txt.corpus, removePunctuation)
  txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
  txt.corpus <- tm_map(txt.corpus, stripWhitespace) 
  txt.corpus <- tm_map(txt.corpus, stemDocument)
  dtm <- DocumentTermMatrix(txt.corpus,control=list(weighting = function(x) weightTfIdf(x)))
  dtm <- data.matrix(dtm)
  return(dtm)
}

## compute the U matrix in the SVD with the highest AUC through app

prev_reviews_summary = function(df,avg_prod_rating){
  prev_help_ratio <- c()             ## previous helpfulness ratio of the reviewer for all the reviews in the data set
  num_prev_review <- c()             ## previous amount of reviews for the reviewer in the data set
  same_rating_reviews <- c()          ## compute how many reviews of the same product have the same score
  alternative_avg_ratings <- c()   ## average of product ratings
  stddev_ratings <- c()   ## stddev of product ratings

  ## calculate how long a review had been posted till the most recent review was posted in the data set
  period <- as.numeric(as.Date(df$REVIEW_POSTED_DATE))  
  period <- max(period)-period      
  #period <- log(period+1)
  
  ## average star rating of the product
  rating  <- df$STAR_RATING                 ## star rating  
  is_moderate <- (rating == 3)                    ## review extremity: moderate review (rating = 3 or not)
  
  extremity <- abs(rating - avg_prod_rating)      ## review extremity: rating extremity
  
  for (i in 1:nrow(df)){
    posted_date <- as.numeric(as.Date(df$REVIEW_POSTED_DATE[i]))
    reviewer_name <- as.character(df$REVIEWER_NAME[i])
    num_prev_review[i] <- nrow(df[(as.numeric(as.Date(df$REVIEW_POSTED_DATE)) <= posted_date) 
                                        & (as.character(df$REVIEWER_NAME) == reviewer_name)
                                        , ,drop=FALSE])
    review_votes <- colMeans(df[(as.numeric(as.Date(df$REVIEW_POSTED_DATE)) < posted_date) 
                                      & (as.character(df$REVIEWER_NAME) == reviewer_name),
                                      c("TOTAL_REVIEW_USEFUL","TOTAL_REVIEW_RESPONSES"),drop=FALSE])
    
    same_rating_reviews[i] <- nrow(df[(as.numeric(as.Date(df$REVIEW_POSTED_DATE)) <= posted_date) 
                                   & (df$SITE_PRODUCT_NAME == df$SITE_PRODUCT_NAME[i])
                                   & df$STAR_RATING == rating[i]
                                   , ,drop=FALSE])
    alternative_avg_ratings[i] <- mean(df[(as.numeric(as.Date(df$REVIEW_POSTED_DATE)) <= posted_date) 
                                          & (df$SITE_PRODUCT_NAME == df$SITE_PRODUCT_NAME[i])
                                          , ,drop=FALSE]$STAR_RATING)
    stddev_ratings[i] <- sd(df[(as.numeric(as.Date(df$REVIEW_POSTED_DATE)) <= posted_date) 
                                 & (df$SITE_PRODUCT_NAME == df$SITE_PRODUCT_NAME[i])
                                 , ,drop=FALSE]$STAR_RATING)
    
    prev_help_ratio[i] <- review_votes[1]/review_votes[2] 
    if(i%%100 == 0){
      cat(sprintf("Reviewer metadata of review %d prepared\n", i))
    }
  }
  prev_help_ratio[is.na(prev_help_ratio)] <- 0  ## remove null values
  stddev_ratings[is.na(stddev_ratings)] <- 0
  alternative_extremity <- abs(rating - alternative_avg_ratings)  
  
  df_meta <- data.frame(prev_help_ratio,period,rating,is_moderate,
                        same_rating_reviews,alternative_avg_ratings,alternative_extremity,stddev_ratings,num_prev_review,
                        avg_prod_rating,extremity)
  
  return (df_meta)
}

## computing all readability and length features. Is slow.
## Instead of exclusively using the tools from koRpus we noticed that qdap outperforms koRpus on some fields
## also in some cases our own implementation outperformed the standard tools
compute_readability = function(texts){
  
  #wordlist downloaded from http://www.readabilityformulas.com/articles/dale-chall-readability-word-list.php
  DC_word_list <- sort(scan("list/DaleChallEasyWordList.txt",what=character(),quiet = TRUE))
  stop_words_list <- stopwords(kind="en")
  emotion_lexicon <- read.csv("list/nrc_emoticon_lexicon_processed.csv",row.names = 1)
  #https://gist.github.com/jamiew/1112488
  # bad_word_list <- read.csv("list/bad_words_list.txt",sep = ":",header = FALSE)[1]
  
  ## needed for error checking through LanguageTool
  url <- "http://localhost:8081/v2/check"
  
  start.time <- Sys.time()
  len <- length(texts)

  num_of_sents <- rep(1,len)            ## define number of sentences from a review
  num_of_words <- rep(1,len)         ## define number of words from a review
  num_of_chars <- rep(0,len)
  num_of_sylb  <- rep(0,len)
  
  pct_difficult_words <- rep(0,len)
  num_of_digits <- rep(0,len)
  num_unique_words <- rep(0,len)
  num_stop_words <- rep(0,len)
  num_punctuation <- rep(0,len)
  num_commas <- rep(0,len)
  pct_uppercase <- rep(0,len)
  
  pct_adjectives <- rep(0,len)
  pct_adverbs <- rep(0,len)
  pct_nouns <- rep(0,len)
  pct_verbs <- rep(0,len)
  pct_wh_words <- rep(0,len)
  pct_compar_words <- rep(0,len)
  
  FOG <- rep(0,len)                  ## define readability index FOG
  FK <- rep(0,len)                    ## define readability index FK  
  ARI <-rep(0,len)                   ## define readability index ARI   
  CLI <- rep(0,len)                  ## define readability index CLI
  FRE <- rep(0,len)                  ## define readability index FRE
  SMOG <- rep(0,len)
  DC <- rep(0,len)
  
  text_errors <-rep(0,len)
  avg_sentence_length <- rep(0,len)
  words_entropy <- rep(0,len)
  chars_entropy <- rep(0,len)
  length_entropy <- rep(0,len)
  
  l1 <- rep(0,len)
  l2 <- rep(0,len)
  l2_plus <- rep(0,len)
  l2_l9 <- rep(0,len)
  l10_plus <- rep(0,len)
  
  num_emotion_words <- data.frame(matrix(0,ncol = length(emotion_lexicon), nrow = length(texts)))
  colnames(num_emotion_words) <- colnames(emotion_lexicon)
  
  #num_bad_words <- rep(0,len)
  
  #text_errors <- detect_text_errors(texts)
  
  for (i in 1:length(texts)){
    text <- texts[i]
    
    ## to avoid issues with external functions not being able to handle non-ASCII characters
    text <- iconv(text, "latin1", "ASCII", sub="e")
    
    ##################### check review sanity ###################
    
    ##check whether the text contains any string
    if(!grepl(pattern = "[A-Za-z]",x = text)){
      num_of_chars[i] <- nchar(text)
      next() ##skip the entire rest of the loop
    }
    
    #################### process the text ########################
  
    tf <- tempfile()
    write(tolower(text),tf)
    tagged.text <- treetag(tf, treetagger="manual", lang="en", TT.options=list(path="/usr/bin/TreeTagger", preset="en",lexicon="english-utf8.par"))
    res <- tagged.text@TT.res
    desc <- tagged.text@desc

    words <- res$token
    sylb_count <- syllable_sum(words) ## count syllables of word, from qdap package, not perfect 
    words <- words[!is.na(sylb_count)] 
    sylb_count <- sylb_count[!is.na(sylb_count)] 

    ################### compute the text metrics #####################

    num_of_sents[i] <- max(desc$sentences,1)
    num_of_words[i] <- max(length(words),1)
    num_of_chars[i] <- desc$chars.no.space
    num_of_digits[i] <- desc$digits
    num_of_sylb[i] <- sum(sylb_count)  ## count total syllables
    num_unique_words[i] <- length(unique(words))
    
    ################# word list lookups #########################
    
    df_emotions <- emotion_lexicon[words,]
    num_emotion_words[i,] <- colSums(df_emotions,na.rm=TRUE)
    if(any(num_emotion_words[i,-6:-7] != 0)){
      num_emotion_words[i,-6:-7] <- sqrt(num_emotion_words[i,-6:-7]) #regularise
      num_emotion_words[i,-6:-7] <- num_emotion_words[i,-6:-7] / sum(num_emotion_words[i,-6:-7] ) #normalise the vector
    }
    ################# compute syntactic features ##################
    num_punctuation[i] <- desc$punct
    pct_adjectives[i] <- length(which(res$wclass=="adjective")) / num_of_words[i]
    pct_adverbs[i] <- length(which(res$wclass=="adverb")) / num_of_words[i]
    pct_nouns[i] <- length(which(res$wclass=="noun")) / num_of_words[i]
    pct_verbs[i] <- length(which(res$wclass=="verb")) / num_of_words[i]
    pct_wh_words[i] <- nrow(res[grep("Wh",res$desc),]) / num_of_words[i]
    pct_compar_words[i] <-  nrow(res[grep("comparative|superlative",res$desc,ignore.case = TRUE),]) / num_of_words[i]
    
    s1 <- length(which(sylb_count==1))
    s2 <- length(which(sylb_count==2))
    num_of_poly_sylb <- num_of_words[i] - s1 - s2
    
    ## for Dale Chall readability
    df_words <- data.frame(table(words))
    num_diff_words <- sum(df_words$Freq[!(df_words$words %in% DC_word_list)]) ## look up the amount of words in the Dale Chall word list
    pct_difficult_words[i] <- num_diff_words / num_of_words[i]
    offset <- ifelse( pct_difficult_words[i] > 0.05, 3.6365, 0)
    
    ## check word lists
    num_stop_words[i] <- sum(df_words$Freq[(df_words$words %in% stop_words_list)])
    #ends up being equal to zero
    # num_bad_words[i] <- sum(df_words$Freq[(df_words$words %in% bad_word_list)])
    
    ################# compute word length based features #############
    
    l_c <- table(nchar(words)) #letters count
    all_letters <- sum(strtoi(names(l_c))*l_c)
    
    l1[i] <- unname(l_c["1"])
    l2[i] <- unname(l_c["2"])
    l5 <- unname(l_c["5"])
    l6 <- unname(l_c["6"])
    
    l1[i] <- ifelse(!is.na(l1[i]),l1[i],0)
    l2[i] <- ifelse(!is.na(l2[i]),l2[i],0)
    l5 <- ifelse(!is.na(l5),l5,0)
    l6 <- ifelse(!is.na(l6),l6,0)
    
    l2_plus[i] <- num_of_words[i] - l1[i] - l2[i]
    l2_l9[i] <- sum(l_c["2"],l_c["3"],l_c["4"],l_c["5"],l_c["6"],l_c["7"],l_c["8"],l_c["9"],na.rm = TRUE)
    l10_plus[i] <- num_of_words[i] - l1[i] - l2_l9[i]
    
    ################   compute entropy    ##############
    
    word_freq <- df_words$Freq / num_of_words[i]
    words_entropy[i] <- -sum(word_freq * log2(word_freq))
    char_freq <- table(strsplit(tolower(text),""))
    char_freq <- char_freq / sum(char_freq)
    chars_entropy[i] <- -sum(char_freq * log2(char_freq))
    length_freq <- l_c / num_of_words[i]
    length_entropy[i] <- -sum(length_freq * log2(length_freq))
    
    ################# compute readability #############

    # FOG <- 0.4*((num_of_words/num_of_sent) + 100*(num_of_poly_sylb/num_of_words))
    # FK  <- 0.39*(num_of_words/num_of_sent) + 11.8*(num_of_sylb/num_of_words) -15.59
    # ARI <- 4.71*(all_letters/num_of_words) + 0.5*(num_of_words/num_of_sent) - 21.43
    # CLI <- 0.0588*(all_letters/num_of_words*100) - 0.296*(num_of_sent/num_of_words*100) - 15.8
    # FRE  <- 206.835 - 1.015*(num_of_words/num_of_sent) - 84.6*(num_of_sylb/num_of_words) 
    # SMOG <- 1.0430 * sqrt(num_of_poly_sylb* 30 / num_of_sent) + 3.1291

    readability_results <- readability.num(txt.features = list(
      sentences = num_of_sents[i],
      words = num_of_words[i], 
      letters = c(all = all_letters, l5 = l5, l6 = l6),
      syllables = c(all = num_of_sylb[i], s1 = s1, s2 = s2),
      FOG.hard.words = num_of_poly_sylb),
      index = c("FOG","Flesch.Kincaid","ARI","Coleman.Liau","Flesch","SMOG"))

    FOG[i] <- readability_results@FOG$FOG
    FK[i] <- readability_results@Flesch.Kincaid$grade 
    ARI[i] <- readability_results@ARI$grade 
    CLI[i] <- readability_results@Coleman.Liau$grade 
    FRE[i] <- readability_results@Flesch$RE 
    SMOG[i] <- readability_results@SMOG$grade
    DC[i] <- 0.1579 * pct_difficult_words[i] * 100 + 0.0496 * num_of_words[i] / num_of_sents[i] + offset #the koRpus implementation returned incorrect values
    
    ################ check for errors in the text ############
    
    ## server needs to be running in order for this to work!
    ## java -cp languagetool-server.jar org.languagetool.server.HTTPServer --port 8081
    filtered_text <- gsub(pattern = "logitech\\s|bluetooth\\s|\\slogitech|\\sbluetooth|kindle\\s|\\skindle", x = texts[i], replacement = "", ignore.case=TRUE)
    resp <- getForm(url,"language" = "en-US", "text" = filtered_text)
    text_errors[i] <- length(fromJSON(resp, simplifyVector = FALSE)$matches)
    
    if(i%%10 == 0){
      cat(sprintf("Readability features of review %d prepared\n", i))
    }
  } 
  
  positive <- num_emotion_words$positive
  negative <- num_emotion_words$negative
  
  polarity <- (positive-negative)/(positive+negative)
  polarity[is.na(polarity)] <- 0
  subjectivity_2 <- (negative + positive) / num_of_words
  senti_diffs_per_ref <- (positive-negative) / num_of_words
  num_emotion_words$positive <- num_emotion_words$positive / num_of_words
  num_emotion_words$negative <- num_emotion_words$negative / num_of_words
  
  #num_emotion_words <- num_emotion_words / num_of_words
  num_excl_marks <- stri_count(texts, fixed="!") / num_of_chars
  num_ques_marks <- stri_count(texts, fixed="?") / num_of_chars
  num_commas <- stri_count(texts, fixed=",") / num_of_chars
  pct_uppercase <- stri_count(texts, regex="[A-Z]") / stri_count(texts, regex="[A-Za-z]") 
  pct_uppercase[is.na(pct_uppercase)] <- 0
  
  pct_caps_lock_words <- stri_count(texts, regex="\\b[A-Z]+\\b")
  alt_num_of_words <- stri_count(texts,regex = "\\b[A-Za-z]+\\b") ## this is done due to treetagger giving much less words than through regex
  alt_num_of_words[which(alt_num_of_words == 0)] <- 1
  pct_caps_lock_words <- pct_caps_lock_words / alt_num_of_words
  
  text_errors <- text_errors / alt_num_of_words
  num_of_digits <- num_of_digits / num_of_chars
  num_unique_words <- num_unique_words / num_of_words
  avg_sentence_length <- num_of_words / num_of_sents
  num_stop_words <- num_stop_words / num_of_words
  #num_bad_words <- num_bad_words / num_of_words
  pct_open_class_words <- pct_adverbs+pct_adjectives+pct_nouns+pct_verbs
  
  num_of_sylb <- num_of_sylb / num_of_words
  num_punctuation <- num_punctuation / num_of_chars
  l1 <- l1 / num_of_words
  l2 <- l2 / num_of_words
  l2_plus <- l2_plus / num_of_words
  l2_l9 <- l2_l9 / num_of_words
  l10_plus <- l10_plus / num_of_words

  #num_of_chars <- log(num_of_chars)
  #num_of_words <- log(num_of_words)
  #num_of_sents <- log(num_of_sents)
  
  df_readability = data.frame(num_of_chars,num_of_words,num_of_sents,num_of_sylb,
                              num_of_digits,num_unique_words,num_stop_words,pct_caps_lock_words,pct_uppercase,
                              words_entropy,chars_entropy,length_entropy,
                              FOG,FK,ARI,CLI,FRE,SMOG,DC,
                              text_errors,avg_sentence_length,num_excl_marks,num_ques_marks,num_punctuation,num_commas,
                              l1,l2,l2_plus,l2_l9,l10_plus,
                              pct_adverbs,pct_adjectives,pct_nouns,pct_verbs,pct_wh_words,pct_compar_words,pct_open_class_words )
  
  df_readability <- data.frame(df_readability,num_emotion_words,polarity,subjectivity_2,senti_diffs_per_ref)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  return(df_readability)
}

## Read and parse the textblob file returned by python.
parse_textblob_results = function(results){
  review_id <-c()      ## unique review id 
  subjectivity <- c()  ## define review subjectivity
  sentiment<-c()       ## define review sentiment  
  
  sss <- strsplit(results, ",")
  for (i in 1:length(results)){
    review_id[i] <- as.numeric(substr(sss[[i]][1],3,7))
    subjectivity[i] <- as.numeric(sss[[i]][2])
    sentiment[i] <- as.numeric(gsub('.{2}$', '', sss[[i]][3]))
  }  
  
  df_subj <- data.frame(subjectivity,sentiment, row.names = review_id)
  
  df_subj <- df_subj[order(review_id),]                             ## reorder data frame by unique review id
  
  cat("All Subjectivity features prepared\n")
  return (df_subj)
}
  
compute_reviewer_feats = function(df_reviewer,review_useful,review_votes){
  #rank <- log(df_reviewer$rwr_rank)
  rank <- df_reviewer$rwr_rank
  
  hof <- ifelse(!is.na(df_reviewer$top1),1,0) ## is a Hall of Famer
  top10 <- ifelse(!is.na(df_reviewer$top10) ,1,0)
  top50 <- ifelse(!is.na(df_reviewer$top50) | (rank > 10 & rank <= 50),1,0) ## this is due to inconsistencies between the real rank and the one displayed
  top500 <- ifelse(!is.na(df_reviewer$top500) | (rank > 50 & rank <= 500),1,0)
  top1000 <- ifelse(!is.na(df_reviewer$top1000) | (rank > 500 & rank <= 1000),1,0)
  vine <- ifelse(!is.na(df_reviewer$vine),1,0) 
  
  is_top <- hof | top10 | top50 | top500 | top1000 | vine
  
  #remove bias by removing the votes of the review itself
  help_vote <- df_reviewer$help_vote - review_useful
  vote <- df_reviewer$vote - review_votes
  ## computing the adjusted ratio
  hf_ratio <- ifelse(vote == 0,0,help_vote/vote)## ifelse to avoid division by 0
  
  #help_vote <- log(help_vote+1)
  #vote <- log(vote+1)
  
  #### help_vote: total helpful votes for the reviewer  
  #### vote:      total votes for the reviewer
  #### rank:      rank of reviewer
  #### top reviewer: Hall Of Fame,TOP10,TOP50,TOP500,TOP1000,VINE
  #### is_top : any of the above
  
  df_reviewer_feats <- data.frame(help_vote,vote,hf_ratio,rank,is_top,hof,vine)
  
  return (df_reviewer_feats)
}

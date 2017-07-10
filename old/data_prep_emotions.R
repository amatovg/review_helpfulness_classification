#to not have to run the entire pipeline

## original review data
df = read.csv(file="../data/data_pm.csv")
df_short <- df[!is.na(df$TOTAL_REVIEW_USEFUL),]    ## remove rows that don't have null helpful vote
df_readability <- read.csv("../data/readblty.csv")


texts <- as.character(df_short$REVIEW_FULL_TEXT)  
num_of_words <- rep(1,length(texts))

#http://saifmohammad.com/WebPages/lexicons.html
emotion_lexicon <- read.csv("../list/nrc_emoticon_lexicon_processed.csv",row.names = 1)
#https://gist.github.com/jamiew/1112488
bad_word_list <- read.csv("../list/bad_words_list.txt",sep = ":",header = FALSE)[1]

num_emotion_words <- data.frame(matrix(0,ncol = length(emotion_lexicon), nrow = length(texts)))
colnames(num_emotion_words) <- colnames(emotion_lexicon)

for (i in 1:length(texts)){
  text <- texts[i]
  
  ## to avoid issues with external functions not being able to handle non-ASCII characters
  text <- iconv(text, "latin1", "ASCII", sub="e")
  
  ##################### check review sanity ###################
  
  ##check whether the text contains any string
  if(!grepl(pattern = "[A-Za-z]",x = text)){
    next() ##skip the entire rest of the loop
  }
  
  #################### process the text ########################
  
  tf <- tempfile()
  write(tolower(text),tf)
  tagged.text <- treetag(tf, treetagger="manual", lang="en", TT.options=list(path="/usr/bin/TreeTagger", preset="en"))
  res <- tagged.text@TT.res
  desc <- tagged.text@desc
  
  words <- res$token
  sylb_count <- syllable_sum(words) ## count syllables of word, from qdap package, not perfect 
  words <- words[!is.na(sylb_count)] 
  
  ################### compute the text metrics #####################
  
  num_of_sents <- max(desc$sentences,1)
  num_of_words[i] <- max(length(words),1)
  
  ################# word list lookups #########################
  
  df_emotions <- emotion_lexicon[words,]
  num_emotion_words[i,] <- colSums(df_emotions,na.rm=TRUE)
  if(i%%10 == 0){
    cat(sprintf("Emotion features of review %d prepared\n", i))
  }

} 
#normalisation
norm_num_emotion_words <- num_emotion_words / num_of_words

positive <- num_emotion_words$positive
negative <- num_emotion_words$negative

polarity <- (positive-negative)/(positive+negative)
polarity[is.na(polarity)] <- 0
subjectivity_2 <- (negative + positive) / num_of_words
senti_diffs_per_ref <- (positive-negative) / num_of_words

df_readability2 <- data.frame(df_readability,norm_num_emotion_words,polarity,subjectivity_2,senti_diffs_per_ref)
write.csv(df_readability2,"/data/readblty2.csv", row.names = FALSE)     ## write data to file

###################################################################

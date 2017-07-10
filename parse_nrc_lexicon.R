#parse original NRC word list
df <- read.csv("list/nrc_emoticon_lexicon.txt",sep = "\t")
words <- unique(df$word)
emotions <- unique(df$emotion)

emotion_lexicon <- data.frame(matrix(FALSE,ncol = 11, nrow = length(words)))
colnames(emotion_lexicon) <- c("words",as.character(emotions))
emotion_lexicon$words <- as.character(words)
for(emotion in emotions){
  emotion_lexicon[,emotion] <- as.logical(df$score[which(df$emotion == emotion)])
}

write.csv(emotion_lexicon,"list/nrc_emoticon_lexicon_processed.csv", row.names = FALSE)

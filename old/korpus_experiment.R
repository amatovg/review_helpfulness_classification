library(koRpus)
library(qdap)

df = read.csv(file="../data/data_pm.csv")
df_short <- df[!is.na(df$TOTAL_REVIEW_USEFUL),]    ## remove rows that don't have null helpful vote
#KoRpus experiment
texts <- as.character(df_short$REVIEW_FULL_TEXT)

##only useful for unique words most likely
for(idx in 1:500){
  text <- tolower(texts[idx])
  text <- gsub("\'ve"," have",gsub("\'ll"," will",gsub("\'d"," would",gsub("\'m"," am",gsub("won\'t","will not",gsub("\'s"," is",gsub("n\'t"," not",gsub("\'re"," are",text))))))))
  apostrophes <- lengths(regmatches(txx, gregexpr("\'", text)))
  if(apostrophes != 0)
    print(idx)
}
# text <- tolower(texts[idx])
# lengths(regmatches(txx, gregexpr("\'", text)))
# text <- gsub("\'ve"," have",gsub("\'ll"," will",gsub("\'d"," would",gsub("\'m"," am",gsub("won\'t","will not",gsub("\'s"," is",gsub("n\'t"," not",gsub("\'re"," are",gsub("\'til"," until",text)))))))))
# lengths(regmatches(txx, gregexpr("\'", text)))
# # gregexpr("\'", text)
# 
# char_count <- character_count(words,missing=0,apostrophe.remove=TRUE,digit.remove=TRUE,count.space=FALSE)   ## count characters of word
# num_of_char <- sum(char_count) ## count total characters

for(idx in 2000:2100){
#idx <- 777
text <- texts[idx]


#################### process the text ########################
tf <- tempfile()
write(tolower(text),tf)
tagged.text <- treetag(tf, treetagger="manual", lang="en", TT.options=list(path="/usr/bin/TreeTagger", preset="en"))
res <- tagged.text@TT.res
desc <- tagged.text@desc
print(res[grep("Wh",res$desc, ignore.case = TRUE ),])
print(res[grep("comparative|superlative",res$desc,ignore.case = TRUE),])
}
words <- res$token
sylb_count <- syllable_sum(words) ## count syllables of word, from qdap package, not perfect 
words <- words[!is.na(sylb_count)]
sylb_count <- sylb_count[!is.na(sylb_count)]

################### compute the features #####################

num_of_sent <- desc$sentences
num_of_words <- length(words)
num_of_chars <- desc$chars.no.space

s1 <- length(which(sylb_count==1))
s2 <- length(which(sylb_count==2))
num_of_sylb <- sum(sylb_count)  ## count total syllables
#num_of_poly_sylb <- length(sylb_count[!is.na(sylb_count) & sylb_count >= 3]) ## count total words with >2 syllables 
num_of_poly_sylb <- num_of_words - s1 - s2

print(c(num_of_sent,num_of_words,num_of_char,num_of_sylb,num_of_poly_sylb))
###### compute readability

# FOG <- 0.4*((num_of_words/num_of_sent) + 100*(num_of_poly_sylb/num_of_words))
# FK  <- 0.39*(num_of_words/num_of_sent) + 11.8*(num_of_sylb/num_of_words) -15.59
# ARI <- 4.71*(all_letters/num_of_words) + 0.5*(num_of_words/num_of_sent) - 21.43
# CLI <- 0.0588*(all_letters/num_of_words*100) - 0.296*(num_of_sent/num_of_words*100) - 15.8
# FRE  <- 206.835 - 1.015*(num_of_words/num_of_sent) - 84.6*(num_of_sylb/num_of_words) 
# SMOG <- 1.0430 * sqrt(num_of_poly_sylb* 30 / num_of_sent) + 3.1291
# indices <- c(FOG,FK,ARI,CLI,FRE,SMOG)
# print(indices)

letters_count <- table(nchar(words))
all_letters <- sum(strtoi(names(letters_count))*letters_count)
l5 <- unname(letters_count[5])
l6 <- unname(letters_count[6])

readability_results <- readability.num(txt.features = list(
  sentences = num_of_sent,
  words = num_of_words, 
  letters = c(all = all_letters, l5 = l5, l6 = l6),
  syllables = c(all = num_of_sylb, s1 = s1, s2 = s2),
  FOG.hard.words = num_of_poly_sylb,
  Dale.Chall.NOL = num_diff_words),
  index = c("FOG","Flesch.Kincaid","ARI","Coleman.Liau","Flesch","SMOG","Dale.Chall"))

FOG <- readability_results@FOG$FOG
FK <- readability_results@Flesch.Kincaid$grade 
ARI <- readability_results@ARI$grade 
CLI <- readability_results@Coleman.Liau$grade 
FRE <- readability_results@Flesch$RE 
SMOG <- readability_results@SMOG$grade

df_words <- data.frame(table(words))
num_diff_words <- sum(df_words$Freq[!(df_words$words %in% DC_word_list)])
pct_diff <- num_diff_words / num_of_words
offset <- ifelse(pct_diff > 0.05, 3.6365, 0)
DC <- 0.1579 * pct_diff * 100 + 0.0496 * num_of_words / num_of_sent + offset #the koRpus implementation returned incorrect values

indices <- c(FOG,FK,ARI,CLI,FRE,SMOG,DC)
print(indices)


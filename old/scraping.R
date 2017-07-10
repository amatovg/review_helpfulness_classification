
#save webpage as character vector
web_prt <- readLines("http://www.amazon.com/dp/B00BJVJV9A")
web_rwr <- readLines("http://www.amazon.com/gp/pdp/profile/AVX6K4ZEK4BK4/ref=cm_cr_pr_pdp?ie=UTF8")
#find the positions of the price

## Individual Review
## Reviewer Characteristics
## Reviewer History
## Review Readability
## Review Subjectivity


price_reg <- regexpr('price">\\$',websource)  ## \\$ for dollar sign "$" since $ itself is metacharacater
rank_reg <- regexpr('Reviewer ranking',web_rwr) ## 


avg_rating_reg


#find which elements in the webpage character vector contain price
wh <- which(reg >= 0)
substr(websource[wh[1]],reg[reg >= 0][1]+8,reg[reg >= 0][1]+4+8)

wh_rank <- which(rank_reg >= 0)
substr(web_rwr[wh_rank[1]],rank_reg[rank_reg >= 0][1]+18,rank_reg[rank_reg >= 0][1]+19+15)

#save webpage as character vector
websource <- readLines("http://www.amazon.com/gp/pdp/profile/ABL3NQVB0CP3C/ref=cm_cr_pr_pdp?ie=UTF8")

#find the positions of the price
price_reg <- regexpr('price">\\$',websource)

#find which elements in the webpage character vector contain price
wh <- which(reg >= 0)

substr(websource[wh[1]],reg[reg >= 0][1]+8,reg[reg >= 0][1]+4+8)

install.packages("rvest")


library(rvest)
## read html of reviewer page
web_rwr <- read_html("http://www.amazon.com/gp/pdp/profile/AVX6K4ZEK4BK4/ref=cm_cr_pr_pdp?ie=UTF8")

## scrape reviewer rank
rank <- html_nodes(web_rwr,".a-spacing-micro+ .a-spacing-small .a-color-secondary")
rank <- html_text(rank) 

## scrape real name
name <- html_nodes(web_rwr,".a-spacing-micro+ .a-spacing-small .a-color-secondary")

## scrape hobbies

## scrape birthday

## scrape location

## scrape homepage

## scrape interests

## scrape snippet


## original review data
df = read.csv(file="../data/Review_Example.csv")
##only keep the columns where total_review_useful is not NA
df_short <- df[!is.na(df[,23]),]
##drop review 3901 (WHY?)
df_short_1 <- df_short[-3901,]
write.csv(df_short_1,"../data/data_pm.csv")


rwr_url <- as.character(df_short$REVIEWER_URL)

crawler<- function(url){
                      url <- read_html(url)
                     rank <- html_nodes(url,".a-spacing-micro+ .a-spacing-small .a-color-secondary")
                     rank <- html_text(rank) 
                     rank <- gsub("[^0-9]", "", unlist(rank)), "")
                       }

rwr_rank <- lapply(rwr_url,crawler)

rwr_rank<-c()
for(i in 1:50){
       reviewer<- read_html(rwr_url[i])
          rank <- html_nodes(reviewer,".a-spacing-micro+ .a-spacing-small .a-color-secondary")
         if(length(rank)==0){ 
          rank <- html_nodes(reviewer,".a-color-base .a-text-bold")
          rwr_rank[i] <- as.character(html_text(rank))
         }else{
            rwr_rank[i] <- as.character(html_text(rank))
              }
       print (i)
}
        rwr_rank[i] <- gregexpr("[0-9]", rwr_rank[i]) 
       matches <- regmatches(rwr_rank[i], gregexpr("[[:digit:]]+", rwr_rank[i]))
            print (i)
}
#####################################################################################
#####################################################################################
#####################################################################################
#################################################
df = read.csv(file="../data/Review_Example.csv")
df_short <- df[!is.na(df[,23]),]
rwr_url <- as.character(df_short$REVIEWER_URL)

rank <-c()
rank1 <- c()
rank2 <- c()
for(i in 1:length(rwr_url)){
             rwr <- readLines(rwr_url[i])
            reg1 <- regexpr('Reviewer ranking:',rwr) ## 
             wh1 <- which(reg1 >= 0)
           rank1 <- substr(rwr[wh1[1]],reg1[reg1 >= 0][1]+18,reg1[reg1 >= 0][1]+19+15)
           
            reg2 <- regexpr('bold">#',rwr) ## 
             wh2 <- which(reg2 >= 0)
           rank2 <- substr(rwr[wh2[1]],reg2[reg2 >= 0][1]+6,reg2[reg2 >= 0][1]+6+15)
              
          rank[i] <- ifelse(!is.na(rank1),rank1,rank2)
           print (i)
}
  
##extract and save the reviewer ranks
rwr_rank <- read.csv("../data/Reviewer_rank.csv")
m <- gregexpr('[0-9]+',rwr_rank[,2])
ran <- regmatches(rwr_rank[,2],m)

rwr_rank <- c()
for (i in 1:length(ran)){
rwr_rank[i] <- as.numeric(paste(unlist(ran[i]), collapse = ''))
}

rwr_rank <- as.data.frame(rwr_rank)
write.table(rwr_rank,"../data/reviewer.csv",row.names=FALSE)

######################################################################################
    hov <- c()
    
for(i in 1:length(rwr_url)){
   rwr <- readLines(rwr_url[i])
   reg <- regexpr('a-size-small a-color-secondary">\\(',rwr) ## 
   wh  <- which(reg >= 0)
hov[i] <- substr(rwr[wh[1]],reg[reg >= 0][1]+32,reg[reg >= 0][1]+32+20)
  print (i)
}
## use parser to obtain the (helpful) votes of the reviewers
write.csv(hov,"/data/Reviewer_hlplvt.csv")

hov <- read.csv ("../data/Reviewer_hlplvt.csv")
hov <- gsub(",",'',hov[,2])
m <- gregexpr('[0-9]+',hov)
ran <- regmatches(hov,m)

hlpl <- c()
vote <- c()
for(i in 1:length(ran)){
  hlpl[i] <- as.numeric(unlist(ran[i])[1])
  vote[i] <- as.numeric(unlist(ran[i])[2])
}
## add the (helpful) votes to reviewer.csv
          df <- read.csv("../data/reviewer.csv")
df$help_vote <- hlpl
     df$vote <- vote

write.csv(df,"../data/reviewer.csv")

###########################################################################
##data path###data/data_pm.csv#######
###########################################################################
df_short <- read.csv(file="../data/data_pm.csv")
rwr_url <- as.character(df_short$REVIEWER_URL)

t1    <-c()
t10   <-c()
t50   <-c()
t500  <-c()
t1000 <-c()

for(i in 1:length(rwr_url)){
  rwr <- readLines(rwr_url[i])
  reg_t1    <- regexpr('#1 HALL OF FAME',rwr) ## 
  reg_t10   <- regexpr('TOP 10 REVIEWER',rwr) ## 
  reg_t50   <- regexpr('TOP 50 REVIEWER',rwr) ##
  reg_t500  <- regexpr('TOP 500 REVIEWER',rwr) ##
  reg_t1000 <- regexpr('TOP 1000 REVIEWER',rwr) ##
 
  t1[i]     <- substr(rwr[which(reg_t1 >= 0)[1]],reg_t1[reg_t1 >= 0][1],reg_t1[reg_t1 >= 0][1]+15)
  t10[i]    <- substr(rwr[which(reg_t10 >= 0)[1]],reg_t10[reg_t10 >= 0][1],reg_t10[reg_t10 >= 0][1]+15)
  t50[i]    <- substr(rwr[which(reg_t50 >= 0)[1]],reg_t50[reg_t50 >= 0][1],reg_t50[reg_t50 >= 0][1]+15)
  t500[i]   <- substr(rwr[which(reg_t500 >= 0)[1]],reg_t500[reg_t500 >= 0][1],reg_t500[reg_t500 >= 0][1]+16)
  t1000[i]  <- substr(rwr[which(reg_t1000 >= 0)[1]],reg_t1000[reg_t1000 >= 0][1],reg_t1000[reg_t1000 >= 0][1]+17)
  print (i)
}

top <- data.frame(t1,t10,t50,t500,t1000)
write.csv(top,"../data/top.csv")

top_rwr <- read.csv("../data/top.csv",row.names=NULL)
vine <- read.csv("../data/vine.csv")
     df <- read.csv("../data/reviewer.csv")

df$top1 <- top_rwr$t1
df$top10 <- top_rwr$t10
df$top50 <- top_rwr$t50
df$top500 <- top_rwr$t500
df$top1000 <- top_rwr$t1000
df$vine <- vine[,2]


write.csv(df,"../data/reviewer.csv")


#######################################################################################################################
df_short <- read.csv(file="../data/data_pm.csv")
rwr_url <- as.character(df_short$REVIEWER_URL)

reviews <-c()
for(i in 1:length(rwr_url)){
       rwr  <- readLines(rwr_url[i])
   reg_rws  <- regexpr('Reviews \\(',rwr) ## 
reviews[i]  <- substr(rwr[which(reg_rws >= 0)[1]],reg_rws[reg_rws >= 0][1],reg_rws[reg_rws >= 0][1]+15)
  print (i)
}

write.csv(reviews,"../data/num_reviews.csv")

#############################################################################################################
df_short <- read.csv(file="../data/data_pm.csv")
web_url <- as.character(df_short$SITE_PRODUCT_URL)

avg_star <-c()
for(i in 1758:3000){
  web  <- readLines(web_url[i])
  reg_avgstar  <- regexpr('noUnderline" title="',web) ## 
  avg_star[i]  <- substr(web[which(reg_avgstar >= 0)[1]],reg_avgstar[reg_avgstar >= 0][1]+20,reg_avgstar[reg_avgstar >= 0][1]+22)
  print (i)
}

write.csv(avg_star,"../data/avg_star1758_3000.csv")

#######################################################################################################################
library(rvest)
library(plyr)

avg_star <-c()
for(i in 1758:3000){
  web  <- html(web_url[i])
  reg_avgstar  <- regexpr('noUnderline" title="',web) ## 
  avg_star[i]  <- substr(web[which(reg_avgstar >= 0)[1]],reg_avgstar[reg_avgstar >= 0][1]+20,reg_avgstar[reg_avgstar >= 0][1]+22)
  print (i)
}

write.csv(avg_star,"../data/avg_star1758_3000.csv")

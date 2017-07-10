## working scraping script on HTTPS
library(httr)
library(stringr)
library(rvest)

url1 <- "https://www.amazon.com/gp/pdp/profile/A15RJE9L6VSNYR/"
url2 <- "https://www.amazon.com/gp/pdp/profile/AVX6K4ZEK4BK4/"
url3 <- "https://www.amazon.com/gp/pdp/profile/A14ZQ17DIPJ6UB/"
url4 <- "https://www.amazon.com/gp/pdp/profile/A1IU7S4HCK1XK0/"

urls <- c(url1,url2,url3,url4)

base_url = "https://www.amazon.com"

urls <- 'https://www.amazon.com/review/top-reviewers' %>%
  GET %>%
  content(as="parsed") %>%
  html_nodes(".a-bordered .img [href]") %>%
  html_attr("href") %>%
  paste(base_url,.,sep="")
urls
# page_text <- content(GET(url),as="text")
# loc <- regexpr('Helpful votes',page_text)
# hf_votes_string <- substring(test,loc+15,loc+200)
# offset1 <- regexpr('a-size-small\\">',hf_votes_string)
# offset2 <- regexpr('</span>',hf_votes_string)
# loc3 = gregexpr('</span>',hf_votes_string)[[1]][2] ## second </span> is right after the number
# hf_votes <- strtoi(substring(hf_votes_string,offset1+14,offset2-1))
# print(hf_votes)
# 
# loc2 = regexpr('Reviewer ranking',page_text)
# ranking_string = substring(test,loc2+20,loc2+200)
# offset3 <- regexpr('a-size-base\\">',ranking_string)
# offset4 <- regexpr('</span>',ranking_string)
# reviewer_ranking <- strtoi(gsub(',','',substring(ranking_string,offset3+14,offset4-1)))
# print(reviewer_ranking)

for (url in urls) {
  page_parsed <- content(GET(url),as="parsed")
  
  hf_votes2 <- html_nodes(page_parsed,".a-expander-content .a-row .a-size-small") %>%
    html_text()%>%
    gsub(',','',.)%>%
    strtoi()
  print(hf_votes2[1])
  
  reviewer_ranking2 <- html_nodes(page_parsed,".a-expander-content .a-row .a-row .a-row .a-size-base") %>%
    html_text() %>%
    substring(2)%>%
    gsub(',','',.)%>%
    strtoi()
  print(reviewer_ranking2[1])
  
}

##original URL https://www.amazon.com/gp/pdp/profile/A1YKQ1K68UXKIO/ref=cm_cr_getr_d_pdp?ie=UTF8
links <- c()
url = 'https://www.amazon.com/glimpse/timeline/A1YKQ1K68UXKIO?isWidgetOnly=true'
test_parsed <- content(GET(url),as="parsed")
tmp3 <- html_node(test_parsed,".glimpse-main-pagination-trigger")
new_token <- html_attr(tmp3,"data-pagination-token")
tmp4 <- html_nodes(test_parsed,".glimpse-card-main .a-link-normal")
new_links = html_attr(tmp4,"href")
links = c(links, new_links[grepl('review',new_links)])

new_url = paste("https://www.amazon.com/glimpse/stories/next/ref=glimp_time_pag?token=",new_token,"&context=GlimpseTimeline&id=&preview=false",sep="")
test_parsed <- content(GET(new_url),as="parsed")
tmp3 <- html_node(test_parsed,".glimpse-main-pagination-trigger")
new_token <- html_attr(tmp3,"data-pagination-token")
tmp4 <- html_nodes(test_parsed,".glimpse-card-main .a-link-normal")
new_links = html_attr(tmp4,"href")
links = c(links, new_links[grepl('review',new_links)])

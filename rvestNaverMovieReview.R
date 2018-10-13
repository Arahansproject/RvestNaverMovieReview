install.packages("rvest")
install.packages("stringi")
install.packages("XML")
install.packages("Rcpp")
install.packages("yaml")
install.packages("xml2")
library(rvest)
library(stringi)
library(XML)
library(Rcpp)
library(yaml)
library(xml2)
library(rJava)
library(memoise)
library(KoNLP)
library(wordcloud)
library(dplyr)
library(stringr)
library(RColorBrewer)

url_base <- "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=136315&target=after&page="
head(url_base)
all.reviews <- c()
for(page in 1:20){
  url <- paste(
    url_base,
    page,
    sep='',
    Encoding = "euc-kr"
  )
  htxt <- read_html(url)
  table <- html_nodes(htxt,'.list_netizen')  # .list_netizen의 .은 class가 list_netizen인
  content <- html_nodes(table,'.title')
  reviews <- html_text(content)
  cat(head(reviews))
  if(length(reviews)==0){
    break;
  } # 리뷰없이 평점만 준 것은 제외
  all.reviews <- c(all.reviews,reviews)
  cat("검색한 페이지:", page)
}
# getwd()
# setwd('C:\\Users\\Administrator\\rlang_weekend2\\rvest')
t1 <- write.table(all.reviews,'review.txt')
t2 <- table(t1)
t3 <- head(sort(t2,decreasing = T),30)
t3

tgt1 <- readLines("review.txt")
tgt1
useSejongDic()
KoNLP::buildDictionary(
  ext_dic = c('sejong','woorimalsam')
)

tgt1 <- sapply(tgt1,
               extractNoun,
               USE.NAMES = F,
               autoSpacing=T)

tgt2 <- unlist(tgt1)
tgt2
tgt3 <- stringr::str_replace_all(tgt2,'[^[:alpha:]]','')
tgt3

# gsub1 <- function(){
#   gsb <- c(
#     ' ', '[~!@#$%&*()_+=?<>]',"\\[",
#     '[ㄱ-ㅎ]','(ㅜ|ㅠ)',"\\d+"
#   )
#   i <- 0
#   for(i in 1:length(gsb)){
#     tgt3 <- gsub(gsb[i],"",tgt2)
#   }
#   return(tgt3)
# }
# 
# gsub2 <- function(){
#   tgt4 <-  gsub("인피니티","",tgt3)
#   tgt4 <-  gsub("어벤저스","",tgt3)
# }
# 
# tgt3 <- gsub1()
# tgt4 <- gsub2()

tgt4 <- Filter(function(x){nchar(x)>=2},tgt3)
tgt4
tgt4 <- unlist(tgt4)
tgt4
tgt5 <- table(tgt4)
tgt5
tgt6 <- head(sort(tgt5,decreasing = T),30)
tgt6

pal <- brewer.pal(8,"Dark2")
set.seed(1234) # 모양이 일치 안하면 숫자를 바꿔가며 모양 찾기
wordcloud(
  names(tgt6),
  freq = tgt6,
  scale = c(2.5,0.1), # 단어크기 0.1 ~ 2.5
  rot.per =0.25, # 회전비율
  min.freq = 2, # 최저 빈도수 2회이상
  max.words = 200,
  random.order = F, # 고빈도 단어 중앙배치
  random.color = T,
  colors = pal
)

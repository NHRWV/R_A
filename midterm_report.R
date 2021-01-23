library(tm)
library(ggmap)
library(ggplot2)
library(twitteR)
library(wordcloud)
library(lubridate)
library(data.table)
library(rtweet)
library(KoNLP)
library(rJava)
library(dplyr)
library(tm)
library(tidytext)
library(qgraph)
library(networkD3)



CONSUMER_SECRET <- "******"
CONSUMER_KEY <- "******"
ACCESS_SECRET <- "******"
ACCESS_TOKEN <- "******"

setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)

searchTerm <- enc2utf8("조국")
trendingTweets <- searchTwitter(searchTerm, n=1000, resultType = "recent")

load("tweet_data1_select.Rda")



useNIADic()


tweet_data1_select$created_at <- as.Date(tweet_data1_select$created_at)
#View(tweet_data1_select)

sample_j<-tweet_data1_select %>% group_by(created_at) %>% sample_frac(0.01, replace = T)
count(tweet_data1_select, created_at)

vec<-c()
for(i in 1:length(sample_j$text)){
  vec<-append(vec, paste(sample_j$text[i]))
}#벡터에 트윗텍스트 넣기 



  
data <- sapply(vec, extractNoun, USE.NAMES = F) #트윗 중에서 명사만 추출
data_unlist <- unlist(data) 

regular_ex <- function(aaa){
  aaa <-gsub("[0-9]", "", aaa)
  aaa <-gsub("[a-z]", "", aaa)
  aaa <-gsub("[A-Z]", "", aaa)
  aaa <-gsub("#", "", aaa)
  aaa <-gsub("&;", "", aaa)
  aaa <-gsub("들이", "", aaa)
  aaa <-gsub("하라", "", aaa)
  aaa <-gsub("진짜", "", aaa)
  aaa <-gsub("미터", "", aaa)
  aaa <-gsub("리얼", "", aaa)
  aaa <-gsub("관련", "", aaa)
  aaa <-gsub("누구", "", aaa)
  aaa <-gsub("때문", "", aaa)
  aaa <-gsub("하면", "", aaa)
  aaa <-gsub("해서", "", aaa)
  aaa <-gsub("하기", "", aaa)
  
  aaa <-gsub("↗", "", aaa)
  aaa <-gsub("'", "", aaa)
  aaa <-gsub('"', "", aaa)
  aaa <-gsub("~", "", aaa)
  aaa <-gsub("!", "", aaa)
  aaa <-gsub("?", "", aaa)
  aaa <-gsub("-", "", aaa)
  
  return(aaa)
}

data_unlist <- regular_ex(data_unlist)

wordcount <- table(data_unlist)
data_j <- as.data.frame(wordcount, stringsAsFactors = F)

data_j <- rename(data_j, word = data_unlist, freq = Freq)

data_j <- filter(data_j, nchar(data_j$word) >= 2)




data_j <- data_j %>% arrange(desc(freq))
top50 <- data_j %>% top_n(50)





for(i in 1:length(data)){
  for(j in 1: length(data[[i]])){
    if(nchar(data[[i]][j]) < 2){
      a[[i]][j] = "AA"
    }
  }
}
for(i in 1:length(data)){
  data[[i]] <- regular_ex(data[[i]])
}



doc <- Corpus(VectorSource(vec))
nounwords <- function(doc){
  doc <- as.character(doc)
  return(extractNoun(doc))
}



tdm <- TermDocumentMatrix(doc, control = list(tokenize = nounwords, removePunctuation=TRUE,
                                              removeNumbers=TRUE, wordLengths=c(4, 10), 
                                              stemDocument=T, 
                                              stripWhitespace=T,
                                              weighting = 
                                                function(x) weightTfIdf(x, normalize = TRUE)))

tdm_mat <- as.matrix(tdm)
inspect(tdm_mat)
TF <- rowSums(tdm_mat)
TDF <- as.matrix(sort(TF, decreasing = T)) #tfidf
wordorder <- rownames(TDF)
TDF_ORI <- tdm_mat[wordorder[1:100],]
TDF_ORI <- as.matrix(rowSums(TDF_ORI)) 
TDF_MAT <- TDF_ORI %*% t(TDF_ORI) # 단어간의빈도..co_occurence
rownames(TDF_MAT) <- colnames(TDF_MAT)



findFreqTerms(tdm,lowfreq = 40) #최소 40번 이상 출현
findAssocs(tdm,"사퇴",0.3)
findAssocs(tdm,"윤석렬",0.3)
findAssocs(tdm,"정경심",0.3)
findAssocs(tdm,"검찰",0.3)


qgraph(TDF_MAT, labels = rownames(TDF_MAT),
       diag = FALSE, layout = 'spring', threshold = 3,
       vsize=log(diag(TDF_MAT)) * 1.5)

node_df <- data.frame(node = rownames(TDF_MAT), value = as.numeric(diag(TDF_MAT))) %>% 
  mutate(idx = row_number()-1)
link_df <- as.data.frame(as.table(TDF_MAT)) %>%
  filter(Freq > 3000) %>%
  rename(source = 'Var1', target = 'Var2') %>%
  left_join(node_df %>% rename(source_idx=idx) %>% select(-value), 
            by = c('source' = 'node')) %>% 
  left_join(node_df %>% rename(target_idx=idx) %>% select(-value), 
            by = c('target' = 'node'))

forceNetwork(Links = as.data.frame(link_df), Nodes = as.data.frame(node_df),
             Source = 'source_idx', Target = 'target_idx',
             NodeID = 'node', Group = 'node')


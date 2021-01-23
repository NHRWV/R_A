library(twitteR)
library(lubridate)
library(dplyr)
library(syuzhet)
getwd()
setwd("C:/Users/Namlister/Desktop/Assignments/R_A")
CONSUMER_SECRET <- "******"
CONSUMER_KEY <- "******"
ACCESS_SECRET <- "******"
ACCESS_TOKEN <- "******"


setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)

searchTerm1 <- "#GalaxyNote10"
searchTerm2 <- "#iPhone11"

# 갤럭시와 아이폰의 최근 일주일 데이터 수집.
tweetsG = searchTwitter(searchTerm1, since = "2019-09-27", n=11000,lang = "en") #갤럭시.
tweetsI = searchTwitter(searchTerm2, since = "2019-09-27", n=12000,lang = "en") #아이폰.

head(tweetsG)
head(tweetsI)
#View(tweetsG)

tweetsG.df = twListToDF(tweetsG) #데이터프레임화.
tweetsI.df = twListToDF(tweetsI)

View(tweetsI.df)

#생성된 날짜에서 연월일만 추출해서 새로운 칼럼 생성한 후 따로 데이터프레임화.
G_ymd <- transmute(tweetsG.df, G_ymd = date(tweetsG.df$created))

I_ymd <- transmute(tweetsI.df, I_ymd = date(tweetsI.df$created))

# 갤럭시와 아이폰 빈도 비교.
table(G_ymd$G_ymd)
table(I_ymd$I_ymd)



G_Sentiments <- get_nrc_sentiment(tweetsG.df$text)
I_Sentiments <- get_nrc_sentiment(tweetsI.df$text)
head(G_Sentiments)

tweetsG <- cbind(tweetsG.df, G_Sentiments)
tweetsI <- cbind(tweetsI.df, I_Sentiments)


sentimentTotalsG <- data.frame(colSums(tweetsG[,c(17:26)]))
sentimentTotalsI <- data.frame(colSums(tweetsI[,c(17:26)]))

names(sentimentTotalsG) <- "count"
names(sentimentTotalsI) <- "count"
sentimentTotalsG <- cbind("sentiment" = rownames(sentimentTotalsG), sentimentTotalsG)
sentimentTotalsI <- cbind("sentiment" = rownames(sentimentTotalsI), sentimentTotalsI)
rownames(sentimentTotalsG) <- NULL
rownames(sentimentTotalsI) <- NULL
sentimentTotalsG
sentimentTotalsI

ggplot(data = sentimentTotalsG, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45)) +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("GalaxyNote10")

ggplot(data = sentimentTotalsI, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45)) +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("iPhone11")



head(sort(table(tweetsG.df$screenName), decreasing = T))
head(sort(table(tweetsI.df$screenName), decreasing = T))

get_follower_list <- function(userName){
  
  twitterUser <- getUser(userName)
  twitterUserFollowerIDs<-twitterUser$getFollowers(retryOnRateLimit=1)
  
  
  return (sapply(twitterUserFollowerIDs,screenName))
}

append_to_df<-function(dt, elems)
{ 
  return(rbindlist(list(dt,  elems),use.names = TRUE))
}



coreUserName_G <- "whitestonedome"
coreUserName_I <- "sshanthan8"

whitestonedome <- getUser(coreUserName_G)
sshanthan8 <- getUser(coreUserName_I)

# Extract Followers for the core user
whitestonedome_follower_IDs <- whitestonedome$getFollowers(retryOnRateLimit=10, n = 100)
sshanthan8_follower_IDs <- sshanthan8$getFollowers(retryOnRateLimit=10, n = 100)

whitestonedome_followers_df = rbindlist(lapply(
  whitestonedome_follower_IDs,as.data.frame
))
sshanthan8_followers_df = rbindlist(lapply(
  sshanthan8_follower_IDs,as.data.frame
))


G_filtered <- subset(whitestonedome_followers_df, 
                      followersCount < 100 &
                        followersCount > 50 &
                        #statusesCount > 10000 & #to reduce number of followers
                        # statusesCount > 100 & 
                        # statusesCount < 5000 & #too many tweets from bots?
                        protected==FALSE)
G_filtered_follower <- G_filtered$screenName

I_filtered <- subset(sshanthan8_followers_df, 
                     followersCount < 100 &
                       followersCount > 50 &
                       #statusesCount > 10000 & #to reduce number of followers
                       # statusesCount > 100 & 
                       # statusesCount < 5000 & #too many tweets from bots?
                       protected==FALSE)
I_filtered_follower <- I_filtered$screenName

edge_df_G <- data.frame(from=G_filtered_follower,
                    to=rep(coreUserName_G, 
                           length(G_filtered_follower)), 
                    stringsAsFactors=FALSE)
edge_df_I <- data.frame(from=I_filtered_follower,
                        to=rep(coreUserName_I, 
                               length(I_filtered_follower)), 
                        stringsAsFactors=FALSE)


counter = 1
for(follower in G_filtered_follower){
  
  followerScreenNameList_G <- get_follower_list(follower)
  print(paste("Processing completed for:",
              follower,
              "(",counter,"/",
              length(G_filtered_follower),")"
  ))
  
  edge_df_G <- append_to_df(edge_df_G,list(from=followerScreenNameList_G,
                                       to=rep(follower, 
                                              length(followerScreenNameList_G))))
  counter <- counter + 1
}
save(edge_df_G, file = "edge_df_G.Rda")
load("edge_df_G.Rda")

counter_I = 1
for(follower in I_filtered_follower){
  
  followerScreenNameList_I <- get_follower_list(follower)
  print(paste("Processing completed for:",
              follower,
              "(",counter,"/",
              length(I_filtered_follower),")"
  ))
  
  edge_df_I <- append_to_df(edge_df_I,list(from=followerScreenNameList_I,
                                           to=rep(follower, 
                                                  length(followerScreenNameList_I))))
  counter_I <- counter_I + 1
}
save(edge_df_I, file = "edge_df_I.Rda")
load("edge_df_I.Rda")


net_G <- graph_from_data_frame(edge_df_G, directed=T)
net_I <- graph_from_data_frame(edge_df_I, directed=T)

table(edge_df_G$to)
table(edge_df_I$to)

edge_df_G[to=="whitestonedome"]$from
edge_df_G[from=="whitestonedome"]$to

edge_df_I[to=="sshanthan8"]$from
edge_df_I[from=="sshanthan8"]$to
# simplify network
net_G <- simplify(net_G, remove.multiple = F, remove.loops = T)
net_I <- simplify(net_I, remove.multiple = F, remove.loops = T)
# temp -> temp is deleted later using remove.loops option

# adjust the size of nodes based on in and out degrees
deg_G <- degree(net_G, mode="all")
V(net_G)$size <- deg_G*0.05 + 1
V(net_G)[name == coreUserName_G]$size <- 15
V(net_G)[size >= 15]$name
V(net_G)[name == coreUserName_G]$size

deg_I <- degree(net_I, mode="all")
V(net_I)$size <- deg_I*0.05 + 1
V(net_I)[name == coreUserName_I]$size <- 15
V(net_I)[size >= 15]$name
V(net_I)[name == coreUserName_I]$size

# node coloring
pal3 <- brewer.pal(10, "Set3")

# overall follower graph
plot(net_G, edge.arrow.size=0.1,
     #vertex.label = ifelse(V(net)$size >= 15, V(net)$name, NA),
     vertex.label = ifelse(V(net_G)$size >= 5, V(net_G)$name, NA),
     vertex.color = pal3)
plot(net_I, edge.arrow.size=0.1,
     #vertex.label = ifelse(V(net)$size >= 15, V(net)$name, NA),
     vertex.label = ifelse(V(net_I)$size >= 5, V(net_I)$name, NA),
     vertex.color = pal3)
  



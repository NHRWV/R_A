############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and build a follower graph
# Note         :   Your results may be different than the ones discussed 
#                  in the chapter due to dyanmic nature of Twitter
############################################################################

library(twitteR)
library(data.table)
library(igraph)
library(RColorBrewer)

# set the credentials (just in case)
CONSUMER_SECRET <- "E8u5PZvaNvCNXIi34yEE8Efs8AEz2TxJbvyjpjWi2OpE3HENDQ"
CONSUMER_KEY <- "hvHT3aR6Reugq4OMSEUG0sums"
ACCESS_SECRET <- "BbKb4BFRfzyVppA380bh5FHxEjgV2s0PefdNy7Avthmsy"
ACCESS_TOKEN <- "701807611465740288-g0Dp8IPrCv8VTEJfKRStR1dGHONeoxN"

############################################################################
#       Utility Functions
############################################################################

# get follower names
get_follower_list <- function(userName){
  # get user data
  twitterUser <- getUser(userName)
  twitterUserFollowerIDs<-twitterUser$getFollowers(retryOnRateLimit=1)
  
  # extract the list of followers
  return (sapply(twitterUserFollowerIDs,screenName))
}

# append rows to dataframe
append_to_df<-function(dt, elems)
{ 
  return(rbindlist(list(dt,  elems),use.names = TRUE))
}

############################################################################
#             Follower Graph Analysis
############################################################################
# connect to twitter app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)
#Access token and secret should be provided due to API change in July 2018.

# Begin with a certain username
#coreUserName <- "jack"  #more than 4M followers... too big for an exercise
coreUserName <- "Ajou_University"

twitterUser <- getUser(coreUserName)

# Extract Followers for the core user
twitterUser_follower_IDs <- twitterUser$getFollowers(retryOnRateLimit=10)
str(twitterUser_follower_IDs[1])
twitterUser_followers_df = rbindlist(lapply(
  twitterUser_follower_IDs,as.data.frame
))
View(twitterUser_followers_df)
str(twitterUser_followers_df)
str(twitterUser_followers_df[1])

# filter dummy accounts (and reduce the number of followers for performance)
filtered_df <- subset(twitterUser_followers_df, 
                      followersCount < 100 &
                      followersCount > 50 &
                        #statusesCount > 10000 & #to reduce number of followers
                        # statusesCount > 100 & 
                        # statusesCount < 5000 & #too many tweets from bots?
                        protected==FALSE) 
#statusesCount: number of tweets
#follwersCount: number of follwers (who follows this user)
#favoritesCount
#friendsCount: number of followees (whom this user follows)
#name: profile name that you can change
#protected: public or not
#verified: authentic (for celeb or organization)
#screenName: twitter ID (handle)
#location:
#language:
#id: integers (system-purpose key values?)
#listedCount: number of lists
#followRequestSent: ?
#profileImageUrl: profile image

filtered_follower_IDs <- filtered_df$screenName
length(filtered_follower_IDs)

# prepare edge data frame (edges to coreUserName)
edge_df<-data.frame(from=filtered_follower_IDs,
                   to=rep(coreUserName, 
                          length(filtered_follower_IDs)), 
                   stringsAsFactors=FALSE)
head(edge_df)
tail(edge_df)
# edge_df <- append_to_df(edge_df,list(from=filtered_follower_IDs,
#                                      to=rep(coreUserName, 
#                                             length(filtered_follower_IDs))))
# above lines were used to add edges to coreUserName later

# Iterate and extract list of followers of followers
counter = 1
for(follower in filtered_follower_IDs){
  # fetch follower list for current user
  followerScreenNameList <- get_follower_list(follower)
  Sys.sleep(61)  #twitter API limit is 15 for 15 mins
  print(paste("Processing completed for:",
              follower,
              "(",counter,"/",
              length(filtered_follower_IDs),")"
  ))
  # append to edge list
  edge_df <- append_to_df(edge_df,list(from=followerScreenNameList,
                                       to=rep(follower, 
                                              length(followerScreenNameList))))
  counter <- counter + 1
}
save(edge_df, file = "edge_df.Rda")
load("edge_df.Rda")

# prepare network object


#net <- graph.data.frame(edge_df, directed=T)  #same with graph_from_data_frame()
net <- graph_from_data_frame(edge_df, directed=T)
table(edge_df$to)
edge_df[to=="Ajou_University"]$from
edge_df[from=="Ajou_University"]$to

# simplify network
net <- simplify(net, remove.multiple = F, remove.loops = T)
# temp -> temp is deleted later using remove.loops option

# adjust the size of nodes based on in and out degrees
deg <- degree(net, mode="all")
V(net)$size <- deg*0.05 + 1
V(net)[name == coreUserName]$size <- 15
V(net)[size >= 15]$name
V(net)[name == coreUserName]$size

# node coloring
pal3 <- brewer.pal(10, "Set3")

# overall follower graph
plot(net, edge.arrow.size=0.1,
     #vertex.label = ifelse(V(net)$size >= 15, V(net)$name, NA),
     vertex.label = ifelse(V(net)$size >= 5, V(net)$name, NA),
     vertex.color = pal3)

############################################################################
#             Friends Among Follwors (optional)
############################################################################


# Plot to highlight Followers with large number of followers
deg <- degree(net, mode="out")
V(net)$size <- deg*0.05+2
V(net)[size==max(V(net)$size)]  #the most ties

# Highlight the coreUser
V(net)[coreUserName]$size <- 15

# identify friend vertices (the vertices coreUserName is also following)
friendVertices <- ends(net, es=E(net)[from(coreUserName)])[,2]   #ends finds vertices at the ends of edges
ends(net, es=E(net)[from(coreUserName)])[,2]

# Generate edge color variable: (normal: grey80, friend: red)
ecol <- rep("grey80", ecount(net))
ecol[which (V(net)$name %in% friendVertices)] <- 'red'

# Generate edge width variable: (normal: 2, friend: 4)
ew <- rep(2, ecount(net))
ew[which (V(net)$name %in% friendVertices)] <- 4

# add core_user for vertex coloring
friendVertices <- append(friendVertices,coreUserName)

# Generate node color variable: (normal: grey80, friend & coreUser: gold)
vcol <- rep("grey80", vcount(net))
vcol[which (V(net)$name %in% friendVertices)] <- "gold"

# vertex label size
V(net)$label.cex <- 1.2

plot(net, 
     vertex.color=vcol, 
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0, 
     vertex.label = ifelse(V(net)$name %in% friendVertices, V(net)$name, NA), 
     vertex.label.color="black",
     vertex.label.font=2,
     edge.curved=0.1
)

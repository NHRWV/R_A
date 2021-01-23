############################################################################
# Chapter      :   2 
# Description  :   Connect to Twitter app and extract sample tweets
############################################################################

# load the package
#install.packages("ROAuth")
library(twitteR)

# set the credentials
CONSUMER_SECRET <- "E8u5PZvaNvCNXIi34yEE8Efs8AEz2TxJbvyjpjWi2OpE3HENDQ"
CONSUMER_KEY <- "hvHT3aR6Reugq4OMSEUG0sums"
ACCESS_SECRET <- "BbKb4BFRfzyVppA380bh5FHxEjgV2s0PefdNy7Avthmsy"
ACCESS_TOKEN <- "701807611465740288-g0Dp8IPrCv8VTEJfKRStR1dGHONeoxN"

# connect to twitter app
setup_twitter_oauth(consumer_key = CONSUMER_KEY,
                    consumer_secret = CONSUMER_SECRET,
                    access_token = ACCESS_TOKEN,
                    access_secret = ACCESS_SECRET)
#Access token and secret should be provided due to API change in July 2018.

# set twitter user
twitterUser <- getUser("jack")

# extract a few sample tweets from this user's timeline
tweets <- userTimeline(twitterUser, n = 10)
tweets <- userTimeline(twitterUser, n = 10, includeRts = T)
tweets <- userTimeline(twitterUser, n = 3200, includeRts = T)
#The Search API is not complete index of all Tweets, but instead an index of recent Tweets. At the moment that index includes between 6-9 days of Tweets.
#This method can only return up to 3,200 of a user's most recent Tweets. Native retweets of other statuses by the user is included in this total, regardless of whether include_rts is set to false when requesting this resource.

# display attributes and function of tweet object
tweets[[1]]$getClass()


# display main body(text) of tweet
tweets[[1]]$text

# display favorite count
tweets[[1]]$favoriteCount

# check current rate limit (Check /statuses/user_timeline)
getCurRateLimitInfo()

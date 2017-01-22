library(twitteR)
library(ggplot2)
library(dplyr)

# dostęp do API
source('twitter_api_access.r')
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)



clinton_tweets = searchTwitter("Hillary Clinton+@HillaryClinton", n=200, lang="en")
trump_tweets = searchTwitter("Donald Trump+@realDonaldTrump", n=200, lang="en")

trump_tweets_df = do.call("rbind", lapply(trump_tweets, as.data.frame))
trump_tweets_df = subset(trump_tweets_df, select = c(text))

clinton_tweets_df = do.call("rbind", lapply(clinton_tweets, as.data.frame))
clinton_tweets_df = subset(clinton_tweets_df, select = c(text))

library(twitteR)
library(ggplot2)
library(dplyr)

# dostęp do API
consumer_key <- "A3X5BcazhcNn55B2UdIQSvVSr"
consumer_secret <- "B0FwSEl8LkiKpemsePUzdvshwssT4LBA9apCzzCtpcDGY7XZ9L"
access_token <- "66435928-8G1xIXbsrOd06lrCJ7NdBvZN4bAxPM4MHatUlraPo"
access_secret <- "dUnRBUUab2aZIu80DnM4OcyFBR357ZnbWUt6xAJuHCgWW"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)



clinton_tweets = searchTwitter("Hillary Clinton+@HillaryClinton", n=200, lang="en")
trump_tweets = searchTwitter("Donald Trump+@realDonaldTrump", n=200, lang="en")

trump_tweets_df = do.call("rbind", lapply(trump_tweets, as.data.frame))
trump_tweets_df = subset(trump_tweets_df, select = c(text))

clinton_tweets_df = do.call("rbind", lapply(clinton_tweets, as.data.frame))
clinton_tweets_df = subset(clinton_tweets_df, select = c(text))

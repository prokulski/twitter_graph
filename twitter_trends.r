library(twitteR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/Lemur/SkyDrive/dane/RStudio_projects/twitter")

# dostęp do API
source('twitter_api_access.r')
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)



## top 5 trendów - pobranie wszysckich tweetów
woeid <- availableTrendLocations() %>% filter(country=="Poland")
rownames(woeid) <- woeid$name
# trendy, ale tylko hashtagi
trends <- getTrends(woeid["Poland",3]) %>% filter(substr(name, 1, 1) == "#")

trend_tweets <- data.frame()
for(i in 1:5) {
   trend <- trends[i,1] # najpopularniejszy temat
   print(trend)
   trend.tweets <- searchTwitter(trend, n=1000, resultType="recent")
   trend.tweets.df <- twListToDF(trend.tweets) %>% select(screenName, text, created, retweetCount, favoriteCount, isRetweet, id)
   trend.tweets.df$trend <- trend
   trend_tweets <- rbind(trend_tweets, trend.tweets.df)
}

rm(woeid, trend, trend.tweets, trend.tweets.df)

trend_tweets <- unique(trend_tweets)
trend_tweets <- trend_tweets %>%
   filter(substr(trend_tweets$text, 1, 3) != "RT ") %>%     # bez RT
   filter(screenName != "trendinaliaPL") %>%                # bez tego bota
   mutate(value=retweetCount+favoriteCount)

trend_tweets <- trend_tweets %>%
   filter(created >= Sys.time() - dhours(6))            # ostatnie 6h
   
# najpopulatniejszew twity per tag:
for(i in 1:5) {
   trend_h <- trends[i,1] # najpopularniejszy temat
   
   t <- trend_tweets %>%
      filter(trend==trend_h) %>%
      arrange(desc(value)) %>%
      top_n(1, value) %>%
      select(screenName, id, text, retweetCount, favoriteCount)

   twit <- paste0("Top-tweet (RTs=", t[1,4], ", FAVs=", t[1,5], ", TOT=", t[1,4]+t[1,5], ") w ostatnich 6h z top-trendu ", trend_h, "\nhttps://twitter.com/", t[1,1], "/status/", t[1,2])
   # tweet(twit)
   cat(twit)
   cat(paste0("\nZnaków: ", nchar(twit)))
   cat("\n\n")
   cat(paste("Kto:", t[1,1], "\n"))
   cat(paste("Co: ", t[1,3], "\n"))
   cat(paste0("RTs=", t[1,4], ", FAVs=", t[1,5], ", TOT=", t[1,4]+t[1,5], "\n"))
   cat("\n==============\n\n")
}


# kto pisze najwięcej w trendach?
p1 <- trend_tweets %>%
   group_by(trend, screenName) %>%
   summarise(n=n()) %>%
   top_n(2, n) %>%
   ungroup() %>%
   arrange(trend, desc(n)) %>%
   mutate(r=row_number()) %>%
   mutate(screenName = factor(screenName, levels=rev(screenName))) %>%
   ggplot() +
      geom_bar(aes(screenName, n, fill=trend), stat="identity") +
      coord_flip() +
      theme_minimal()
   

# który trend najpopularniejszy?
p2 <- trend_tweets %>%
   group_by(trend) %>%
   summarise(n=n()) %>%
   ungroup() %>%
   arrange(desc(n)) %>%
   mutate(trend=factor(trend, levels = trend)) %>%
   ggplot() +
   geom_bar(aes(trend, n, fill=trend), stat="identity") +
   theme_minimal()

p3 <- trend_tweets %>%
   mutate(hour=hour(created)) %>%
   group_by(trend, hour) %>%
   summarise(n=n()) %>%
   ungroup() %>%
   ggplot(aes(hour, n, fill=trend)) +
      geom_bar(stat="identity", position="dodge") +
      theme_minimal()

grid.arrange(p1, p2, p3, layout_matrix=rbind(c(1, 2), c(3, 3)))

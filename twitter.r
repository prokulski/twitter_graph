library(twitteR)
library(ggplot2)
library(dplyr)

# dostęp do API
consumer_key <- "A3X5BcazhcNn55B2UdIQSvVSr"
consumer_secret <- "B0FwSEl8LkiKpemsePUzdvshwssT4LBA9apCzzCtpcDGY7XZ9L"
access_token <- "66435928-8G1xIXbsrOd06lrCJ7NdBvZN4bAxPM4MHatUlraPo"
access_secret <- "dUnRBUUab2aZIu80DnM4OcyFBR357ZnbWUt6xAJuHCgWW"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# pogranie mojego timeline
timeLine <- homeTimeline(n = 800, retryOnRateLimit = 1000)
timeline.df <- twListToDF(timeLine) %>%
   select(screenName, text, created, retweetCount, isRetweet, favoriteCount, longitude, latitude, statusSource)
# usuń htmle z statusSource
timeline.df$statusSource <- gsub("(<[^>]*>)", "", timeline.df$statusSource)
timeline.df$text <- gsub("”", "\"", timeline.df$text)
timeline.df$text <- gsub("„", "\"", timeline.df$text)
timeline.df$text <- gsub("…", "...", timeline.df$text)


# kto pisze najwięcej swoich tweetów (bez RT)
timeline.df %>% filter(isRetweet=="FALSE") %>%
   mutate(cnt=1) %>%
   group_by(screenName, cnt) %>%
   summarise(n=sum(cnt)) %>%
   ggplot(aes(screenName, n)) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle=90))




# pobierz obserwujących
getFollowers <- function(screenname)
{
   me <- getUser(screenname)

   # pobieranie listy followersów
   me.followers <- me$getFollowers()
   me.followers.df <- twListToDF(me.followers) %>%
      select(screenName, name, created, statusesCount, friendsCount, followersCount, profileImageUrl, lang, location, description)

   return(me.followers.df)
}


getFriends <- function(screenname)
{
   me <- getUser(screenname)
   
   # pobieranie listy friendsów
   me.friends <- me$getFriends()
   me.friends.df <- twListToDF(me.friends) %>%
      select(screenName, name, created, statusesCount, friendsCount, followersCount, profileImageUrl, lang, location, description)
      
   return(me.friends.df)
}



# moi obserwujący
me.followers.df <- getFollowers("lemur78")



# Wyszukiwanie hasła
s <- searchTwitter("New York", n=3200)
s.df <- twListToDF(s) %>%
   select(screenName, text, created, retweetCount, isRetweet, favoriteCount, longitude, latitude, statusSource)
# usuń htmle z statusSource
s.df$statusSource <- gsub("(<[^>]*>)", "", s.df$statusSource)

s.df %>%
   mutate(day=format(created, "%Y-%m-%d"), cnt=1) %>%
   group_by(day, cnt) %>%
   summarise(n=sum(cnt)) %>%
   ggplot(aes(day, n)) + geom_point() + geom_smooth()


s.df.geo <- s.df %>% filter(!is.na(longitude))
s.df.geo$longitude <- as.numeric(s.df.geo$longitude)
s.df.geo$latitude <- as.numeric(s.df.geo$latitude)
my.long <- meadian(s.df.geo$longitude)
my.lat <- median(s.df.geo$latitude)

map <- get_map(location = c(lon=-74, lat=40.8),
               maptype = "roadmap", source = "google", language = "pl-PL",
               zoom=11)

plot <- ggmap(map) + geom_point(data=s.df.geo, aes(x=longitude, y=latitude, color=statusSource), size=3)
plot

# twitty o najpopularniejszym haśle w Polsce:
# dostępne lokalizacje dla Trendów w Polsce
woeid <- availableTrendLocations() %>% filter(country=="Poland")
rownames(woeid) <- woeid$name
(woeid)
# trendy
trends <- getTrends(woeid["Poland",3])
trend <- trends[1,1] # najpopularniejszy temat
(trend)
# pobieramy twity
trend.tweets <- searchTwitter(trend, n=3200)
trend.tweets.df <- twListToDF(trend.tweets) %>%
   select(screenName, text, created, retweetCount, isRetweet, favoriteCount, longitude, latitude, statusSource)
# usuń htmle z statusSource
trend.tweets.df$statusSource <- gsub("(<[^>]*>)", "", trend.tweets.df$statusSource)

# twity w trendach dzisiejsze czy z kilku dni?
if(as.Date(min(trend.tweets.df$created)) == as.Date(Sys.time())) {
   # dzisiejsze - wykres godzinowy
   p <- trend.tweets.df %>%
      mutate(time=format(created, "%H:%M"), cnt=1) %>%
      group_by(time, cnt, isRetweet) %>%
      summarise(n=sum(cnt)) %>%
      ggplot(aes(time, n, fill=isRetweet)) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle=90)) +
      labs(title=paste0("Liczba tweetów dla hasła \"", trend, "\""), x="Godzina", y="Liczba tweetów")
} else {
   # kilkudniowe - wykres dzienny
   p <- trend.tweets.df %>%
      mutate(day=format(created, "%d.%m"), cnt=1) %>%
      group_by(day, cnt, isRetweet) %>%
      summarise(n=sum(cnt)) %>%
      ggplot(aes(day, n, fill=isRetweet)) +
         geom_bar(stat="identity") +
         theme(axis.text.x = element_text(angle=90)) +
         labs(title=paste0("Liczba tweetów dla hasła \"", trend, "\""), x="Dzień", y="Liczba tweetów")
}

p


# najpopularniejszy RT
trend.tweets.df %>% filter(retweetCount == max(retweetCount)) %>% select(text) %>% unique


# TMD dla timeline
library(tm)
stopwords_pl <- read.delim("polish_stopwords.txt", encoding="UTF-8", header=FALSE, quote="")
stopwords_pl <- paste(stopwords_pl$V1)

tweets <- timeline.df$text

corpus <- Corpus(VectorSource(tweets))
corpus <- tm_map(corpus, removeWords, timeline.df$screenName) # usunięcie nicków
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords_pl)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

tdm <- TermDocumentMatrix(corpus)
tweets.tdm <- as.matrix(tdm)
tweets.tdm.df <- as.data.frame(rowSums(tweets.tdm))
View(tweets.tdm.df)


tweets.tdm.df$word <- rownames(tweets.tdm.df)
names(tweets.tdm.df) <- c("word_count", "word")
tweets.tdm.df <- tweets.tdm.df[which(grepl("^http*", tweets.tdm.df$word)==FALSE),]

# odrzucany 99,5% słów i rysujemy najpopularniejsze
tweets.tdm.df %>%
   filter(word_count>quantile(tweets.tdm.df$word_count, 0.95)[1]) %>%
   ggplot(aes(x=word, y=word_count)) +
   geom_bar(stat="identity")





# obserwowani przez obserwowanych przeze mnie
twuser <- "lemur78"

me.friends.df <- getFriends(twuser)

me.friends.df <- me.friends.df %>% filter(friendsCount != 0) %>%
   # tylko ci co mają więcej niż 100 friendsów - żeby było szybciej ;)
   filter(friendsCount <= 100) 

friends.names <- me.friends.df$screenName

l <- data.frame(user=as.character(), friend=as.character())

#obserwowani przeze mnie
l <- rbind(l, data.frame(user=twuser, friend=friends.names, value=1))

for(i in 1:length(friends.names))
{
   print(paste("user:", friends.names[i]))
   f <- getFriends(friends.names[i])$screenName
   l <- rbind(l, data.frame(user=friends.names[i], friend=f))
   print(paste(" - following", length(f), "users"))
}

l$value <- 1

# top 10
l %>% group_by(friend) %>% count() %>% arrange(desc(n)) %>% head(10)
# kto obserwuje lidera?
l %>% filter(friend=="TadeuszSznuk") %>% select(user) %>% head(10)

# data frame to matrix
m <- acast(l, user~friend~value, sum)
mat <- matrix(m, nrow=nrow(m))
rownames(mat) <- rownames(m)
colnames(mat) <- colnames(m)

m.dist <- dist(mat %*% t(mat))

m.scale <- cmdscale(m.dist)

plot(m.scale)
text(m.scale, labels = rownames(m.scale))


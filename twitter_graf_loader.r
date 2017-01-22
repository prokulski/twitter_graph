library(twitteR)
library(dplyr)

# dostęp do API
# dost?p do API
source('twitter_api_access.r')
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

twuser <- "lemur78"
maxfriends <- 100
filename <- paste0("twitter/twitter_friends_", maxfriends, ".Rdata")

getFriends <- function(screenname)
{
   me <- getUser(screenname)

   # max 1000 followersów (ze względu na ograniczenia API)
   me.friends <- me$getFriends(n=maxfriends)
   me.friends.df <- twListToDF(me.friends) %>%
      select(screenName)
   
   return(me.friends.df)
}


# init
if(!file.exists(filename)) {
   friends.names <- getFriends(twuser)$screenName
   l <- data.frame(user=twuser, friend=friends.names)
   save(l, friends.names, file=filename)
} else {
   load(filename)
}


# pierwszy poziom (ja obserwuję kogoś -> kogo obserwuje ten ktoś?)
for(i in 1:length(friends.names)) {
   if(!(friends.names[i] %in% l[,1])) {
      cat(paste("user:", friends.names[i]))
      f <- getFriends(friends.names[i])$screenName
      l <- rbind(l, data.frame(user=friends.names[i], friend=f))
      cat(paste(" - following", length(f), "users", "\n"))
   }
   save(l, friends.names, file=filename)
}

l$user <- as.character(l$user)
l$friend <- as.character(l$friend)
l <- unique(l)
save(l, friends.names, file=filename)

# drugi poziom
lista <- l[which(!(l[,2] %in% l[,1])),2]
for(i in 1:length(lista)) {
   if(!(lista[i] %in% l[,1])) {
      cat(paste("user:", lista[i]))
      f <- getFriends(lista[i])$screenName
      l <- rbind(l, data.frame(user=lista[i], friend=f))
      cat(paste(" - following", length(f), "users", "\n"))
   }
   save(l, friends.names, file=filename)
}

l$user <- as.character(l$user)
l$friend <- as.character(l$friend)
l <- unique(l)
save(l, friends.names, file=filename)

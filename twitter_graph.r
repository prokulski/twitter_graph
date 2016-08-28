library(twitteR)
library(ggplot2)
library(dplyr)
library(reshape2)

# dost?p do API
source('twitter_api_access.r')
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)



# pobierz obserwuj?cych
getFollowers <- function(screenname)
{
   me <- getUser(screenname)

   # pobieranie listy followers?w
   me.followers <- me$getFollowers()
   me.followers.df <- twListToDF(me.followers) %>%
      select(screenName, name, created, statusesCount, friendsCount, followersCount, profileImageUrl, lang, location, description)

   return(me.followers.df)
}

# pobierz obserwowanych
getFriends <- function(screenname)
{
   me <- getUser(screenname)

   # pobieranie listy friends?w
   me.friends <- me$getFriends()
   me.friends.df <- twListToDF(me.friends) %>%
      select(screenName, name, created, statusesCount, friendsCount, followersCount, profileImageUrl, lang, location, description)

   return(me.friends.df)
}


# obserwowani przez obserwowanych przeze mnie
twuser <- "lemur78"

me.friends.df <- getFriends(twuser)

me.friends.df2 <- me.friends.df %>% filter(friendsCount != 0) #%>%
   # tylko ci co maj? wi?cej ni? 100 friends?w - ?eby by?o szybciej ;)
   filter(friendsCount > 10)

friends.names <- me.friends.df2$screenName

l <- data.frame(user=as.character(), friend=as.character(), value=as.integer())
l <- rbind(l, data.frame(user=twuser, friend=friends.names, value=1))


for(i in 1:length(friends.names))
{
   print(paste(i, "user:", friends.names[i]))
   f <- getFriends(friends.names[i])$screenName
   l <- rbind(l, data.frame(user=friends.names[i], friend=f, value=1))
   print(paste(" - following", length(f), "users"))
   write.csv2(l, "saved_l.csv")
}

l$user <- as.character(l$user)
l$friend <- as.character(l$friend)


# top 10
l %>% group_by(friend) %>% summarise(n=sum(value)) %>% arrange(desc(n)) %>% head(10)
# kto obserwuje lidera?
leader <- as.character((l %>% group_by(friend) %>% summarise(n=sum(value)) %>% arrange(desc(n)) %>% head(1))$friend)
l %>% filter(friend==leader) %>% select(user)

# data frame to matrix
m <- acast(l, user~friend~value, sum)
mat <- matrix(m, nrow=nrow(m))
rownames(mat) <- rownames(m)
colnames(mat) <- colnames(m)

save(mat, l, m, twuser, me.friends.df, file="saved_data.Rdata")

# sieć wprost
m.dist <- dist(mat %*% t(mat))
m.scale <- cmdscale(m.dist)
plot(m.scale)
text(m.scale, labels = rownames(m.scale))

#sieć z macierzą korelacji
# mat.cor <- cor(mat)

# poniżej to samo tylko ręcznie - dla 100 najbardziej obserwowanych
corr.matrix.dimension <- 200

# losowa próbka:
# rownums <- sample(1:ncol(mat), corr.matrix.dimension, replace = FALSE)

sumy <- colSums(mat)
rownums <- head(order(-sumy), corr.matrix.dimension)

mat.cor <- matrix(NA, nrow = corr.matrix.dimension, ncol = corr.matrix.dimension)

for(i in 1:corr.matrix.dimension)
	for(j in 1:corr.matrix.dimension)
		mat.cor[i,j] <- cor(mat[,rownums[i]], mat[,rownums[j]])

mat.cor.names <- colnames(mat)
colnames(mat.cor) <- mat.cor.names[rownums]
rownames(mat.cor) <- mat.cor.names[rownums]


library(corrplot)
corrplot(mat.cor, type="lower", diag=FALSE, order="AOE")

library(corrgram)
corrgram(mat.cor, order = TRUE, lower.panel = panel.conf, upper.panel = panel.shade)



mat.cor.dist <- dist(mat.cor %*% mat.cor)
mat.cor.scale <- cmdscale(mat.cor.dist)
#plot(mat.cor.scale)
#text(mat.cor.scale, labels = rownames(mat.cor.scale))

mat.cor.scale.df <- as.data.frame(mat.cor.scale)
mat.cor.scale.df$name <- rownames(mat.cor.scale.df)
mat.cor.scale.df %>%
	# filter(V1>=750, V2>=0) %>%
	ggplot(aes(x=V1, y=V2, label=name)) + geom_point() + geom_text(size=2)

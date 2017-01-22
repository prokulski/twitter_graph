library(ggplot2)
library(dplyr)
library(igraph)

twuser <- "lemur78"
maxfriends <- 100
load(paste0("twitter/twitter_friends_", maxfriends, ".Rdata"))

# graf użytkowników - kto kogo obserwuje
g <- graph_from_data_frame(l, directed = TRUE)

sgc <- spinglass.community(g)
V(g)$membership <- sgc$membership

# kolory wierzchołów - zależne od grupy
vertex_cols <- rainbow(max(sgc$membership))
V(g)$color <- vertex_cols[V(g)$membership]
V(g)$label.color = "gray50"
V(g)$size <- 1
V(g)[name==twuser]$size <- 20


E(g)$color = "gray80"
E(g)$width = .5
E(g)$arrow.width = .25
E(g)$arrow.size = .1


lay <- layout.fruchterman.reingold(g)

plot(g,
     layout=lay,
     vertex.label.cex=0.7,
     vertex.label=NA,
     edge.width=NA)

setwd("C:/Users/willi/Desktop/DSI/twitter")
consumerKey <- "7cyVsbF7S9O4rO6gXsRlpKpq3"
consumerSecret <- "Fhx0tuv4SvhMTU3wyNcrF0kaeszjKwiVv6xltbe5oETQXK23Kd"
access_token <- "937596313209671682-VnWT7JfOvjcQ7MbdpmCrnyU1RI4qDjN"
access_token_secret <- "F17ddxLG5gpE9mKytXVa9PWPgWijSJTPI1EQ0voAWEd8i"
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
#3785705
pkgs <-c('twitteR','ROAuth','httr','plyr','stringr','ggplot2','plotly', "maps", "streamR", "tm", "dplyr")
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#load("C:/Users/willi/Desktop/twitter/twitter-token.Rdata")
save(my_oauth, file="C:/Users/willi/Desktop/DSI/twitter/twitter-token.Rdata")
fileName <- paste0("Data/project-geo_", format(Sys.time(),"%d%M%y%H%M"),".json")
#Run filename before capturing tweets
filterStream(file.name=fileName, locations=c(-125, 25, -66, 50),
             track=c("Christmas","xmas","x-mas","Cristmas"),
             timeout=60, oauth=my_oauth)
#Get new file data
fileData <- list.files("Data/", full.names = TRUE)

anything <- lapply(fileData, function(x){
   parseTweets(x) 
})
tweets <- do.call(rbind,anything)
tweets <- filter(tweets, geo_enabled == TRUE)
tweets$lat <- ifelse(is.na(tweets$lat), tweets$place_lat, tweets$lat)
tweets$lon <- ifelse(is.na(tweets$lon), tweets$place_lon, tweets$lon)
#states <- map.where("state", tweets$lon, tweets$lat)
#head(sort(table(states), decreasing=TRUE))
## First create a data frame with the map data 
map.data <- map_data("state")

# And we use ggplot2 to draw the map:
# 1) map base
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "grey90", 
                            color = "grey50", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  # 2) limits for x and y axis
  scale_x_continuous(limits=c(-125,-66)) + scale_y_continuous(limits=c(25,50)) +
  # 3) adding the dot for each tweet
  geom_point(data = tweets, 
             aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "darkred") +
  # 4) removing unnecessary graph elements
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) 


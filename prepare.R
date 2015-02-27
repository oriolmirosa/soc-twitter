library(twitteR)
library(httr)
library(plyr)

setwd("~/Documents/Research/R/Twitter")

sink("soctwitter2.log", append=TRUE, split=TRUE)

load("secrets.rda")

setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, access_token=access_token, access_secret=access_secret)

follows <- list() # list of all the sociologists that each sociologist follows

# Get who each sociologist follows, done in the downloads script
for (sociologist in 1:length(sociologists)) {
  follows[[sociologist]] <- sociologists[[sociologist]]$getFriendIDs() # need to use try()
}

# Clean follows from followers who are not sociologists
for (follow in length(follows):1) {
  if (sum(laply(sociologist, function(x) grepl(follows[[follow]], x$id))) == 0) follows[[follow]] <- NULL
}

socnames <- laply(sociologists, function(x) x$id)

socmatrix <- matrix(data = 0, nrow = length(sociologists), ncol = length(sociologists))

colnames(socmatrix) <- socnames
rownames(socmatrix) <- socnames

# Now we fill it with the data from the friends. The relationship is: row follows column

for (row in nrow(socmatrix)) {
  for (col in ncol(socmatrix)) {
    if (the sociologist in the column is in the list of follows of sociologist in the row) socmatrix[row,col] <- 1
  }
}




socdf <- data.frame(matrix(ncol = length(sociologists), nrow = 0))

names(socdf) <- soccols

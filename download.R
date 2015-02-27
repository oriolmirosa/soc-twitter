library(twitteR)
library(httr)
library(plyr)

# setwd("~/Documents/Research/R/Twitter")

sink("soctwitter.log", append=FALSE, split=TRUE)

load("secrets.rda")

#setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret, access_token=access_token, access_secret=access_secret)

#user <- getUser("familyunequal")
user <- getUser("kjhealy")

sociologists <- list(user)
otherfriends <- vector(mode="character", length=0)
follows <- list()

limits <- getCurRateLimitInfo()
limits$resetCT <- format(limits$reset, tz="America/Chicago", usetz=TRUE)

cl <- as.integer(limits$remaining[limits$resource=="/application/rate_limit_status"])
id1 <- as.integer(limits$remaining[limits$resource=="/friends/ids"])
id2 <- as.integer(limits$remaining[limits$resource=="/users/show/:id"])

checkLimits <- function(cl, id1, id2) {
  if (cl <= 2 | id1 <= 1 | id2 <= 1) {
    limits <- getCurRateLimitInfo()
    limits$resetCT <- format(limits$reset, tz="America/Chicago", usetz=TRUE)
    maxtime <- max(limits$resetCT[limits$resource=="/application/rate_limit_status"], limits$resetCT[limits$resource=="/friends/ids"], limits$resetCT[limits$resource=="/users/show/:id"])
    cat(paste0("\n\nWaiting for reset at: ", maxtime, "\n"))
    while (Sys.time() < maxtime) Sys.sleep(5)
    cl <- 180
    id1 <- 15
    id2 <- 180
  }
  return(list(cl = cl, id1 = id1, id2 = id2))
}

i <- length(sociologists)
while (i <= length(sociologists)) {
  lims <- checkLimits(cl, id1, id2)
  cl <- lims$cl
  id1 <- lims$id1
  id2 <- lims$id2
  friends <- try(sociologists[[i]]$getFriendIDs(), silent=TRUE)
  id1 <- id1 - 1
  id2 <- id2 - 1
  if (class(friends) == "try-error") {
    cat(paste0("i: ", i, " - Error!"))
    next
  }
  follows[[i]] <- friends
  allfriends <- length(friends)
  cat(paste0("\nJust downloaded ", sociologists[[i]]$screenName,"'s ", allfriends, " friends!"))
  for (friend in length(friends):1) {
    if (sum(laply(sociologists, function(x) grepl(friends[friend], x$id))) > 0) friends <- friends[-friend]
  }
  nosoc <- length(friends)
  for (friend in length(friends):1) {
    if (sum(laply(otherfriends, function(x) grepl(friends[friend], x))) > 0) friends <- friends[-friend]
  }
  insoc <- allfriends - nosoc
  toanalyze <- length(friends)
  inother <- nosoc - toanalyze
  cat("\n", paste0(insoc, " (", formatC(insoc/allfriends*100, digits = 1, format='f'), "%) are sociologists we already got."))
  cat("\n", paste0(inother, " (", formatC(inother/allfriends*100, digits = 1, format='f'), "%) are non-sociologist friends we already checked."))
  cat("\n", paste0(toanalyze, " (", formatC(toanalyze/allfriends*100, digits = 1, format='f'), "%) are new friends that we need to analyze.\n"))
  new <- 0
  for (friend in 1:length(friends)) {
    lims <- checkLimits(cl, id1, id2)
    cl <- lims$cl
    id1 <- lims$id1
    id2 <- lims$id2
    user <- try(getUser(friends[friend]), silent=TRUE)
    id2 <- id2 - 1
    if (class(user) == "try-error") next
    if (grepl("sociolog", user$description, ignore.case=TRUE) == TRUE) {
      sociologists[[length(sociologists)+1]] <- user
      cat(paste0("\nFriends of #", i, ": ", sociologists[[i]]$screenName,". Added #", length(sociologists), ": ", user$screenName))
      new <- new + 1
    } else {
      otherfriends[length(otherfriends)+1] <- user$id
    }
  }
  cat(paste0("\n\n", new, " new sociologists added (", formatC(new/toanalyze*100, digits = 1, format='f'), "% of the ", toanalyze, " friends analyzed for this user)"))
  cat(paste0("\n", toanalyze - new, " non-sociologist friends (", formatC((toanalyze - new)/toanalyze*100, digits = 1, format='f'), "% of the ", toanalyze, " friends analyzed for this user)\n"))
  i = i + 1
}


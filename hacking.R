library("reshape2")
library("plyr")
processDraftData <- function()
{
  
  draftData <- read.csv("./Draft.csv")
  
  draftData <- melt(draftData, id.vars = "ROUND")
  
  draftData <- draftData[order(draftData$ROUND), ]
  
  draftData$pickNum <- lapply(1:(16*8), snakeFunction)
  
  draftData <- as.data.frame(lapply(draftData, unlist))
  
  draftData <- draftData[order(draftData$pickNum),]
  
  rename(draftData, c("variable" = "Drafter", "value" = "Player", "pickNum" = "Pick #", "ROUND" = "Round"))
}

# Creates the sequence: 1,2,3,4,5,6,7,8,16,15,14,13,12,11,10,9,17,18,19,20....
snakeFunction <- function(i, rowLen=8)
{
  groupIndicator <- ceiling(i / rowLen)
  # Increasing group
  if (groupIndicator %% 2 == 1)
  {
    i
  }
  # Decreasing group
  else
  {
    rowLen*groupIndicator - ((i - 1) %% rowLen)
  }
}


# Writes off the data to csv
#write.csv(processDraftData(), "./reorderedDraft.csv", row.names = FALSE)

# Read in draft data
draftData <- read.csv("./reorderedDraft.csv")

# read in ESPN Data
espn <- read.csv("./ESPN_Rankings.csv")

# Join espn rankings data with draft data 
joinedData <- join(draftData, espn, by="Player")

# Some players will not have an ESPN ranking, give these players a worst ranking + 1
joinedData$Rank[is.na(joinedData$Rank)] <- 301

joinedData$RankDiff <-  joinedData$Pick.. - joinedData$Rank

avgDiffs <- aggregate(RankDiff ~ Drafter, joinedData, FUN=mean)

avgDiffs <- within(avgDiffs, Drafter <- factor(Drafter, levels=avgDiffs[order(avgDiffs$RankDiff), "Drafter"]))

ggplot(avgDiffs, aes(x=Drafter, y=RankDiff)) + geom_bar(stat = "identity") + ylab("Difference Between Pick # and Ranking")


posDiffs <- aggregate(RankDiff ~ Pos, joinedData, FUN=mean)

posDiffs <- within(posDiffs, Pos <- factor(Pos, levels=posDiffs[order(posDiffs$RankDiff), "Pos"]))

# Bar Plot
ggplot(posDiffs, aes(x=Pos, y=RankDiff)) + geom_bar(stat = "identity") + ylab("Difference Between Pick # and Ranking")

# Scatter colored by drafter 
ggplot(joinedData, aes(x=Pick..,y=Rank)) + geom_point(aes(color=Drafter)) + geom_abline() + xlab("Pick #")

# Scatter colored by position 
ggplot(joinedData, aes(x=Pick..,y=Rank)) + geom_point(aes(color=Pos)) + geom_abline() + xlab("Pick #")

# Line plot for performance per round 
ggplot(joinedData, aes(x=Round, y=RankDiff)) + geom_line() + facet_wrap(~Drafter, ncol=2) + geom_hline(yintercept = 0, linetype=2) + geom_point(aes(color=Pos)) + ylab("Difference Between Pick # and Ranking")
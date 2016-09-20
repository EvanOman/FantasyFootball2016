library("reshape2")
library("plyr")

# Writes off the data to csv
write.csv(processDraftData(), "./reorderedDraft.csv", row.names = FALSE)

# Read in draft data
draftData <- read.csv("./reorderedDraft.csv")

# read in ESPN Data
espn <- read.csv("./ESPN_Rankings.csv")

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
snakeFunction <- function(i)
{
  groupIndicator <- ceiling(i / 8)
  # Increasing group
  if (groupIndicator %% 2 == 1)
  {
    i
  }
  # Decreasing group
  else
  {
    8*groupIndicator - ((i - 1) %% 8)
  }
}






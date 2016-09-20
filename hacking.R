library("reshape2")
library("plyr")

rm(list=ls())

draftData <- read.csv("./Draft.csv")

draftData <- melt(draftData, id.vars = "ROUND")

draftData <- draftData[order(draftData$ROUND), ]

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

snakeIndices <- lapply(1:(16*8), snakeFunction)

draftData$pickNum <- snakeIndices

draftData <- as.data.frame(lapply(draftData, unlist))

draftData <- draftData[order(draftData$pickNum),]

draftData <- rename(draftData, c("variable" = "Drafter", "value" = "Player", "pickNum" = "Pick #", "Round = ROUND"))

# write.csv2(df5, "./reorderedDraft.csv")
library(reshape2)

df <- read.csv("./Draft.csv")

df2 <- melt(df, id.vars = "ROUND")

df3 <- df2[order(df2$ROUND), ]

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
        (8*groupIndicator) - (((i - 1) %% 8) + 1) + 1
    }
}

snakeIndices <- lapply(1:(16*8), snakeFunction)

df3$pickNum <- snakeIndices

df4 <- as.data.frame(lapply(df3, unlist))

df5 <- df4[order(df4$pickNum),]
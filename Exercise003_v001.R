## Assess a random 5-card hand based on its poker value
## Assume you are passed a 13x4 matrix with 5 1 (cards dealt) and 47 0


findMaxSuit <- function(inFile,subRows=1:13) {
    max(sum(inFile[subRows,1]),sum(inFile[subRows,2]),sum(inFile[subRows,3]),sum(inFile[subRows,4]))
}


assessFinalHand <- function(cardData) {
    
    colTotals <- colSums(cardData) ## Number to flush
    rowTotals <- rowSums(cardData) ## Number pairs
    rowCover <- ifelse(rowTotals==0,0,1) ## Number covered
    
    sfMax <- rep(0,13)
    strGapMax <- rep(0,13)
    
    for ( intCtr in 1:9 ) {
        sfMax[intCtr] <- findMaxSuit(cardData,intCtr:(intCtr+4))
        strGapMax[intCtr] <- sum(rowCover[intCtr:(intCtr+4)])
    }
    
    for ( intCtr in 10:13 ) {
        sfMax[intCtr] <- findMaxSuit(cardData,c(1,intCtr:13))
        strGapMax[intCtr] <- sum(rowCover[c(1,intCtr:13)])
    }
    
    numPair <- sum(rowTotals==2)
    numHighPair <- sum(rowTotals[1:4]==2)
    
    if (sfMax[1]==5) { handType <- "01.  Royal" 
    } else if (max(sfMax)==5) { handType <- "02.  Straight Flush" 
    } else if (max(rowTotals)==4) { handType = "03.  Quad" 
    } else if (max(rowTotals==3)) { handType <- ifelse(numPair==1,"04.  Full House","07.  Trips")
    } else if (max(colTotals)==5) { handType = "05.  Flush"
    } else if (max(strGapMax)==5) { handType = "06.  Straight"
    } else if (numPair==2) { handType <- "08.  Two Pair"
    } else if (numHighPair==1) { handType <- "09.  High Pair"
    } else { handType <- "10.  Nothing" 
    }
}


simulateDraws <- function(nHands=10000,cumFile) {


    ## Run nHands number of random draws and add the result to cumFile
    for ( intCtr in 1:nHands ) {
        cardData <- matrix(0, nrow=13,ncol=4)

        randPicks <- sample(1:52,5,replace=FALSE)
        
        ## Draw some cards
        for ( temp in 1:5 ) {
            cardData[(randPicks[temp]%%13)+1,(randPicks[temp]+12)%/%13] <- 1
        }
    
        ## This step consumes ~70% of the total processing time
        myHandType <- assessFinalHand(cardData)
        
        ## This step consumes ~20% of the total procesing time
        cumFile$numSeen[cumFile$Hand==myHandType] <- cumFile$numSeen[cumFile$Hand==myHandType] + 1
    }

    ## Get the updated cumFile
    return(cumFile)    
}


myStart <- proc.time()


## Assess number of types of hand (suit independent)
myHands <- function() {
    
    ## Calculate 5-0-0-0 (Royal, Straight Flush, Flush)
    num5000 <- choose(13,5) ## 4 combinations
    
    ## Calculate 4-1-0-0 (Straight, High Pair)
    num4100 <- choose(13,4)*choose(13,1) ## 12 combinations
    
    ## Calculate 3-2-0-0 (Straight, Two Pair, High Pair)
    num3200 <- choose(13,3)*choose(13,2) ## 12 combinations
    
    ## Calculate 3-1-1-0 (Straight, Trips, Two Pair, High Pair)
    num3110 <- choose(13,3)*choose(13,1)*choose(13,1) ## 12 combinations
    
    ## calculate 2-2-1-0 (Full House, Straight, Trips, Two Pair, High Pair)
    num2210 <- choose(13,2)*choose(13,2)*choose(13,1) ## 12 combinations
    
    ## calculate 2-1-1-1 (Quad, Full House, Straight, Trips, Two Pair, High Pair)
    num2111 <- choose(13,2)*choose(13,1)*choose(13,1)*choose(13,1) ## 4 combinations
    
    print(paste0("5-0-0-0 combinations: ",num5000))
    print(paste0("4-1-0-0 combinations: ",num4100))
    print(paste0("3-2-0-0 combinations: ",num3200))
    print(paste0("3-1-1-0 combinations: ",num3110))
    print(paste0("2-2-1-0 combinations: ",num2210))
    print(paste0("2-1-1-1 combinations: ",num2111))
    print(paste0("Total combinations: ",sum(num5000,num4100,num3200,num3110,num2210,num2111)))
}

myHands()

## Read in the file of hand values
handValues <- read.csv("finalHandValues.csv", stringsAsFactors = FALSE)
handValues$numSeen <- 0


## Select a number of hands and then run the trial
numPerTrial <- 20000
handValues <- simulateDraws(nHands=numPerTrial, cumFile=handValues)


print("Through code (finished)")
print(proc.time() - myStart)

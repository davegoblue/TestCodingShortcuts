## Assess a random 5-card hand based on its poker value
## Assume you are passed a 13x4 matrix with 5 1 (cards dealt) and 47 0


findMaxSuit <- function(inFile,subRows=1:13) {
    max(sum(inFile[subRows,1]),sum(inFile[subRows,2]),sum(inFile[subRows,3]),sum(inFile[subRows,4]))
}


assessFinalHand <- function(cardData) {
    
    colTotals <- colSums(cardData) ## Number to flush
    rowTotals <- rowSums(cardData) ## Number pairs
    rowCover <- ifelse(rowTotals==0,0,1) ## Number covered
    
    ## sfMax <- rep(0,13) ## Can make this redundant
    strGapMax <- rep(0,13)
    
    for ( intCtr in 1:9 ) {
        ## sfMax[intCtr] <- findMaxSuit(cardData,intCtr:(intCtr+4))
        strGapMax[intCtr] <- sum(rowCover[intCtr:(intCtr+4)])
    }
    
    for ( intCtr in 10 ) {
        ## sfMax[intCtr] <- findMaxSuit(cardData,c(1,intCtr:13))
        strGapMax[intCtr] <- sum(rowCover[c(1,intCtr:13)])
    }
    
    numPair <- sum(rowTotals==2)
    numHighPair <- sum(rowTotals[1:4]==2)
    
    if (strGapMax[1]==5 & max(colTotals)==5) { handType <- "01.  Royal" 
    } else if (max(strGapMax)==5 & max(colTotals)==5) { handType <- "02.  Straight Flush" 
    } else if (max(rowTotals)==4) { handType = "03.  Quad" 
    } else if (max(rowTotals==3)) { handType <- ifelse(numPair==1,"04.  Full House","07.  Trips")
    } else if (max(colTotals)==5) { handType = "05.  Flush"
    } else if (max(strGapMax)==5) { handType = "06.  Straight"
    } else if (numPair==2) { handType <- "08.  Two Pair"
    } else if (numHighPair==1) { handType <- "09.  High Pair"
    } else { handType <- "10.  Nothing" 
    }
}


assessHands <- function(keyMatrix,cumFile) {

    for ( intCtr in 1:nrow(keyMatrix) ) {
        
        ## Calculate cardData from each row of keyMatrix
        cardData <- matrix(data=keyMatrix[intCtr,1:52], nrow=13,ncol=4)

        ## This step consumes ~70% of the total processing time
        myHandType <- assessFinalHand(cardData)
        
        ## This step consumes ~20% of the total procesing time
        cumFile$numSeen[cumFile$Hand==myHandType] <- cumFile$numSeen[cumFile$Hand==myHandType] + 1
    }

    ## Return the updated cumFile
    return(cumFile)    
}

genHandTypes <- function(numSuits=c(5,0,0,0)) {
    
    modSuits <- as.integer(numSuits[order(-as.integer(numSuits))])
    
    if(sum(modSuits) != 5 | min(modSuits) < 0 | length(modSuits) > 4) {
        print(numSuits)
        print(modSuits)
        stop(paste0("This is wrong - see above for numSuits and modSuits: "))
    }
    
    
    ## Just generate the first column
    cardVector1 <- matrix(data=0,nrow=choose(13,modSuits[1]),ncol=13)
    cardChosen1 <- t(combn(1:13,modSuits[1]))
    
    for (intCtr in 1:choose(13,modSuits[1])) {
        cardVector1[intCtr,c(cardChosen1[intCtr,])] <- 1
    }
    
    ## Just generate the second column
    cardVector2 <- matrix(data=0,nrow=choose(13,modSuits[2]),ncol=13)
    cardChosen2 <- t(combn(1:13,modSuits[2]))
    
    for (intCtr in 1:choose(13,modSuits[2])) {
        cardVector2[intCtr,c(cardChosen2[intCtr,])] <- 1
    }
    
    ## Just generate the third column
    cardVector3 <- matrix(data=0,nrow=choose(13,modSuits[3]),ncol=13)
    cardChosen3 <- t(combn(1:13,modSuits[3]))
    
    for (intCtr in 1:choose(13,modSuits[3])) {
        cardVector3[intCtr,c(cardChosen3[intCtr,])] <- 1
    }
    
    ## Just generate the fourth column
    cardVector4 <- matrix(data=0,nrow=choose(13,modSuits[4]),ncol=13)
    cardChosen4 <- t(combn(1:13,modSuits[4]))
    
    for (intCtr in 1:choose(13,modSuits[4])) {
        cardVector4[intCtr,c(cardChosen4[intCtr,])] <- 1
    }
    
    nMax <- choose(13,modSuits[1])*choose(13,modSuits[2])*
            choose(13,modSuits[3])*choose(13,modSuits[4])
    
    lenVector4 <- choose(13,modSuits[4])
    lenVector3 <- choose(13,modSuits[3])
    lenVector2 <- choose(13,modSuits[2])
    ratio1_1 <- nMax / choose(13,modSuits[1])
    
    ## Strategy should be to fully permute this - initialize vectors to make that happen:
    useCardVector1 <- matrix(data=0,nrow=nMax,ncol=13)
    useCardVector2 <- matrix(data=0,nrow=nMax,ncol=13)
    useCardVector3 <- matrix(data=0,nrow=nMax,ncol=13)
    useCardVector4 <- matrix(data=0,nrow=nMax,ncol=13)
    
    for (intCtr in 1:nMax) {
    
        ## Take cardvector4 and repeat it times as-is (straight remainder division)
        modUse4 <- 1 + (intCtr-1) %% lenVector4
        
        ## Take cardVector3 and repeat each row in blocks of size lenVector4
        ## So, if lenVector4 is 13, you need row1 repeated 13 times and etc. and repeat until done
        ## But then modUse3 can only be as big as its own length
        modUse3 <- 1 + (intCtr-1) %/% lenVector4
        modUse3 <- 1 + (modUse3-1) %% lenVector3
        
        ## Take cardVector2 and repeat each row in blocks of size lenVector4*lenVector3
        ## But then modUse2 can only be as big as its own length
        modUse2 <- 1 + (intCtr-1) %/% (lenVector4*lenVector3)
        modUse2 <- 1 + (modUse2-1) %% lenVector2
        
        ## Take cardVector1 and repeat it in blocks of size ratio1_1
        modUse1 <- 1 + (intCtr-1) %/% ratio1_1
        
        useCardVector1[intCtr,] <- cardVector1[modUse1 , ]
        useCardVector2[intCtr,] <- cardVector2[modUse2 , ]
        useCardVector3[intCtr,] <- cardVector3[modUse3 , ]
        useCardVector4[intCtr,] <- cardVector4[modUse4 , ]
    }

    cardVector <- cbind(useCardVector1 , useCardVector2 , useCardVector3 , useCardVector4)
    
    return(cardVector)
}


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


myStart <- proc.time()

## Read in the file of hand values
handValues <- read.csv("finalHandValues.csv", stringsAsFactors = FALSE)
handValues$numSeen <- 0

## Print statistics for the various types of hands
myHands()

## Pick a hand type and run it through the hand type calculator
cardVector <- genHandTypes(c(2,1,1,1))
if (ncol(cardVector) != 52) {
    stop("cardVector must have exactly 52 columns before adding EV as column 53")
}
cardVector<-cbind(cardVector,rep(0,nrow(cardVector)))
handValues <- assessHands(keyMatrix=cardVector,cumFile=handValues)


print("Through code (finished)")
print(proc.time() - myStart)

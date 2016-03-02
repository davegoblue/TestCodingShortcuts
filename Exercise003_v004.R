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
    
    return(handType)
    
}


assessHands <- function(keyMatrix) {

    ## Create a character vector of myHandType for return
    ## Through experimentation, declaring length in advance runs order of magnitude
    ## faster than alternative of growing by way of a <- c(a,thisTime)
    myHandType <- character(length=nrow(keyMatrix))
    
    for ( intCtr in 1:nrow(keyMatrix) ) {
        
        ## Calculate myHandType from each row of keyMatrix
        cardData <- matrix(data=keyMatrix[intCtr,1:52], nrow=13,ncol=4)
        myHandType[intCtr] <- assessFinalHand(cardData)
        
    }

    ## Return the updated myHandType
    return(myHandType)    
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


## The combinatorics calculator, based on the lists as provdided
comboCalc <- function(comboList) {
    
    totCombo <- 0
    
    for (listCtr in comboList) {
        
        possCombo <- 1
        for (intCtr in 1:length(listCtr)) {
            possCombo <- possCombo * choose(13,listCtr[intCtr])
        }
        
        print(paste0(paste(listCtr,collapse="-") , " combinations: " , possCombo ) )
        totCombo <- totCombo + possCombo
    }
    
    return(totCombo)
    
}


## The core processing function that calls everything else
genMatrices <- function(handDistrib) {
    
    ## Run through the hand type creator
    cardVector <- genHandTypes(numSuits=handDistrib)
    if (ncol(cardVector) != 52) {
        stop("cardVector must have exactly 52 columns")
    }
    
    ## Determine the hand type for each row of cardVector
    ## Throw it to the main envrionment with <<- (I know, I know . . . )
    myHandTypes <<- assessHands(keyMatrix=cardVector)
    
    print(paste0("Finished creating cardVector for: ", paste(handDistrib , collapse = "-") ) )
    print(proc.time() - myStart)
    
    return(cardVector)
}


## The main code is below -- declarations and then calls to the various functions
## Grab the starting time, read in the handvalues file, and declare the desired matrices
myStart <- proc.time()
handValues <- read.csv("finalHandValues.csv", stringsAsFactors = FALSE)
totCardVector <- NULL
runHandDistribs <- list( c(5,0,0,0) , 
                         c(2,1,1,1) ,                         
                         c(2,2,1,0) , 
                         c(4,1,0,0) , 
                         c(3,2,0,0) , 
                         c(3,1,1,0) 

                        )

## Permute the inputs and report on the total amount of data to be generated
totCombo <- comboCalc(comboList = runHandDistribs)
print(paste0("Total combinations: " , totCombo) )


## Run the algorithm for each entry in runHandDistribs


for (listCtr in runHandDistribs) {
    
    cardVector <- genMatrices(handDistrib=listCtr)    
    assocInt <- as.integer(paste(listCtr,collapse=""))
    
    ## Update cardVector column 53 with the results (time hog -- try to optimize elsewhere)
    ## Update cardVector column 54 with the corresponding handType
    ## Store the whole thing in totCardVector
    cardVector <- cbind(cardVector , rep(0,nrow(cardVector)) , rep(assocInt,nrow(cardVector)))
    for (intCtr in 1:nrow(cardVector)) {
        cardVector[intCtr,53] <- handValues[handValues$Hand==myHandTypes[intCtr],]$Value
    }
    totCardVector <- rbind(totCardVector,cardVector)
    
    ## Find the associated frequencies and merge in to handValues
    myFreq <- as.data.frame(table(myHandTypes),stringsAsFactors = FALSE)
    names(myFreq) <- c("Hand",paste0("num",assocInt))
    handValues <- merge(x=handValues,y=myFreq,by="Hand",all.x=TRUE)
    
}


print("Through code (finished)")
print(proc.time() - myStart)
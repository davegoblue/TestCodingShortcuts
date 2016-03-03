## Code to generate and evaluate all combinations of 5 cards drawn from 52

## There are 6 possible suit distributions for 5 cards from 52:
## 5-0-0-0 (all flavors of flush), 4-1-0-0 (straight, pair)
## 3-2-0-0 (straight, two pair, pair), 3-1-1-0 (straight, trips, two pair, pair)
## 2-2-1-0 (all but quad/flushes), 2-1-1-1 (all but flushes)
## Reduction of redundancy produces total matrix volume 13% of 52c5
## This is not a full reduction of redundancy -- allows both 3-1-1-0 and 2-2-1-0 and all 2-1-1-1
## Investigate alternate approach which is to go by amount of pairing?

## Each possible hand for each of the suit distributions is generated and evaluated
## The results are aggregated across suit distributions and stored in totCardVector


## Function to assess the value of any given 5-card hand
assessFinalHand <- function(cardData) {
    
    ## These chew ~25% of the time
    colTotals <- colSums(cardData) ## Number to flush
    rowTotals <- rowSums(cardData) ## Number pairs
    rowCover <- ifelse(rowTotals==0,0,1) ## Number covered

    ## These for loops chew ~50% of the time
    strGapMax <- rep(0,10)
    for ( intCtr in 1:9 ) {
        strGapMax[intCtr] <- sum(rowCover[intCtr:(intCtr+4)])
    }
    for ( intCtr in 10 ) {
        strGapMax[intCtr] <- sum(rowCover[c(1,intCtr:13)])
    }
    
    ## These take essentially no time
    numPair <- sum(rowTotals==2)
    numHighPair <- sum(rowTotals[1:4]==2)
    
    ## These if evaluations chew ~15% of the time
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


## Function calls assessFinalHand and stores results in character vector myHandType
assessHands <- function(keyMatrix) {

    ## Create a character vector of myHandType for return
    ## Through experimentation, declaring length in advance runs order of magnitude
    ## faster than alternative of growing by way of a <- c(a,thisTime)
    myHandType <- rep("",nrow(keyMatrix))
    
    for ( intCtr in 1:nrow(keyMatrix) ) {
        
        ## Calculate myHandType from each row of keyMatrix (keep only columns 1-52, the cards)
        cardData <- matrix(data=keyMatrix[intCtr,1:52], nrow=13,ncol=4)
        myHandType[intCtr] <- assessFinalHand(cardData)
    }

    ## Return the updated myHandType
    return(myHandType)    
}


## Function to generate hand types - works fine, hard to read though
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


## Function for creating the requested combinatorics in handDistrib
genMatrices <- function(handDistrib) {
    
    ## Run through the hand type creator
    cardVector <- genHandTypes(numSuits=handDistrib)
    
    if (ncol(cardVector) != 52) {
        stop("cardVector must have exactly 52 columns")
    }
    
    return(cardVector)
}


## The main code is below -- declarations and then calls to the various functions
## Grab the starting time, read in the handvalues file, and declare the desired matrices
myStart <- proc.time()
handValues <- read.csv("finalHandValues.csv", stringsAsFactors = FALSE)
totCardVector <- NULL
runHandDistribs <- list( c(5,0,0,0) , 
                         c(4,1,0,0) , 
                         c(3,2,0,0) , 
                         c(3,1,1,0) ,
                         c(2,2,1,0) , 
                         c(2,1,1,1)                         
                        )


## Calculate and report on the total amount of data to be generated
totCombo <- comboCalc(comboList = runHandDistribs)
print(paste0("Total combinations: " , totCombo) )


## Run the algorithm for each entry in runHandDistribs
for (listCtr in runHandDistribs) {

    print(paste0("Starting with: ", paste(listCtr , collapse = "-") ) )
    print(proc.time() - myStart)
    
    ## Generate the requested cardVector (all valid combinatorics for listCtr)
    ## Takes ~2.5 seconds for the ~170k hands of 2-1-1-1
    cardVector <- genMatrices(handDistrib=listCtr)    
    print(paste0("Finished creating cardVector for: ", paste(listCtr , collapse = "-") ) )
    print(proc.time() - myStart)
    
    ## Determine the hand type for each row of cardVector
    ## Takes ~10 seconds for the ~170k hands of 2-1-1-1
    myHandTypes <- assessHands(keyMatrix=cardVector)
    print(paste0("Finished creating myHandTypes for: ", paste(listCtr , collapse = "-") ) )
    print(proc.time() - myStart)
    
    ## Update cardVector column 53 with the results and column 54 with handDistrib (as integer)
    ## Store the whole thing in totCardVector
    ## Used to Take ~13 seconds for the ~170k hands of 2-1-1-1 if using for-loop
    ## Takes ~0.1 seconds for the ~170k hands of 2-1-1-1 using match!  So, match it is
    assocInt <- as.integer(paste(listCtr,collapse=""))
    cardVector <- cbind(cardVector , rep(0,nrow(cardVector)) , rep(assocInt,nrow(cardVector)))
    cardVector[,53] <- handValues$Value[match(myHandTypes,handValues$Hand)]

    totCardVector <- rbind(totCardVector,cardVector)
    
    ## Find the associated frequencies and merge in to handValues
    ## Takes ~0.4 seconds for the ~170k hands of 2-1-1-1
    myFreq <- as.data.frame(table(myHandTypes),stringsAsFactors = FALSE)
    names(myFreq) <- c("Hand",paste0("num",assocInt))
    handValues <- merge(x=handValues,y=myFreq,by="Hand",all.x=TRUE)
    
    print(paste0("Finished everything for: ", paste(listCtr , collapse = "-") ) )
    print(proc.time() - myStart)
    
}


print("Through code (finished)")
print(proc.time() - myStart)
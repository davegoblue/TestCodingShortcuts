## Simulation for the Risk of Ruin (infinite trials)
## Inputs - probability of outcomes, random add
## v002 is an attempt to optimize for memory considerations in calcOutcomes()


getBaseOutcomes <- function(myFileName="BaseOutcomes.csv", forceEQ=FALSE) {
    
    if (file.exists(myFileName)) {
        baseOutcomes <- read.csv(myFileName,stringsAsFactors = FALSE)
        if (ncol(baseOutcomes) != 2) { stop("Error in CSV file, should have exactly 2 columns") }
        colnames(baseOutcomes) <- c("probs","outcomes")
    } else {
        baseOutcomes <- data.frame(probs=c(0.01,0.02,0.05,0.18,0.24,0.50),outcomes=c(10,5,2,1,0,-1))
    }
    
    baseOutcomes <- baseOutcomes[baseOutcomes$probs != 0,] ## Can have zeroes as inputs -- ignore those
    
    if ( forceEQ ) {
        pDelta <- sum(baseOutcomes$probs) - 1
        if ( abs(pDelta) < 0.0000001 & 
             abs(pDelta) / baseOutcomes[nrow(baseOutcomes),]$probs < 0.1
        ) 
        {
            print(paste0("Modifying probablities ",paste0(baseOutcomes[nrow(baseOutcomes),],collapse=" ")))
            baseOutcomes[nrow(baseOutcomes),]$probs <- baseOutcomes[nrow(baseOutcomes),]$probs - pDelta
            print(paste0("New probablities ",paste0(baseOutcomes[nrow(baseOutcomes),],collapse=" ")))
        }
    }
    
    if (sum(baseOutcomes$probs)!=1 | min(baseOutcomes$probs) < 0 | 
        sum(is.na(baseOutcomes$probs)) > 0 | sum(is.na(baseOutcomes$outcomes)) > 0) { 
        stop("Please resolve the issue with inputs for probs and outcomes, aborting") 
    }
    
    ## Store the original value read in as outcomes
    baseOutcomes$oldOutcomes <- baseOutcomes$outcomes
    
    baseMean <- sum(baseOutcomes$probs*baseOutcomes$outcomes)
    baseVar <- sum(baseOutcomes$probs*(baseOutcomes$outcomes-baseMean)^2)
    
    print(paste0("Probabilities sum to 1.  Outcomes has mean ",format(baseMean,digits=3),
                 " and variance ",format(baseVar,digits=3)))
    
    return(baseOutcomes)
}


modBaseOutcomes <- function(baseOutcomes=baseOutcomes, nAddOnePer) {

    ## Place in the random additions component
    baseOutcomes$oldProbs <- baseOutcomes$probs

    ## Create a new row that changes the -1 to 0
    if (nAddOnePer > 0 & baseOutcomes[nrow(baseOutcomes),"probs"] > 1/nAddOnePer) {
        baseOutcomes <- rbind(baseOutcomes, baseOutcomes[nrow(baseOutcomes),])
        baseOutcomes[nrow(baseOutcomes)-1,]$outcomes <- baseOutcomes[nrow(baseOutcomes)-1,]$outcomes + 1
        baseOutcomes[nrow(baseOutcomes)-1,]$probs <- 1/nAddOnePer
        baseOutcomes[nrow(baseOutcomes)-1,]$oldProbs <- 0

        ## Fix the bottom row accordingly
        baseOutcomes[nrow(baseOutcomes),]$probs <- baseOutcomes[nrow(baseOutcomes),]$probs - 1/nAddOnePer
        
    } else {
        print(paste0("No modifications due to nAddOnePer=",nAddOnePer,
                     " and last row prob=",baseOutcomes[nrow(baseOutcomes),"probs"]
                     )
              )
    }

    ## Report on changes in metrics
    oldMean <- sum(baseOutcomes$oldProbs * baseOutcomes$outcomes)
    newMean <- sum(baseOutcomes$probs * baseOutcomes$outcomes)
    oldVar <- sum(baseOutcomes$oldProbs * baseOutcomes$outcomes^2) - oldMean^2
    newVar <- sum(baseOutcomes$probs * baseOutcomes$outcomes^2) - newMean^2

    print(paste0("Means have shifted from ", round(oldMean,5), " to ", round(newMean,5)))
    print(paste0("Variance has shifted from ", round(oldVar,2), " to ", round(newVar,2)))
    
    return(baseOutcomes)
    
}


testSeqP <- function(pVec, nMax, nStart=0, nEnd=1) {
    
    mtxTest <- matrix(data=rep(-9, 2*(nBuckets+1)), nrow=(nBuckets+1))
    
    prob0 <- pVec[pVec$outcomes == 0, ]$probs
    probNeg <- pVec[pVec$outcomes == -1, ]$probs
    posProbs <- pVec[pVec$outcomes > 0, ]
    
    for (intCtr in 0:nMax) {
        testP <- nStart + (intCtr/nMax)*(nEnd - nStart)
        mtxTest[intCtr+1, 1] <- testP
        myRuin <- probNeg
        
        for (intCtr2 in 1:nrow(posProbs)) {
            myRuin <- myRuin + posProbs[intCtr2,"probs"] * (testP ^ (posProbs[intCtr2,"outcomes"] + 1) )
        }
        
        mtxTest[intCtr+1, 2] <- myRuin / (1 - prob0) ## Null is like a no trial
    }
    
    return(mtxTest)
}


calcOutcomes <- function(pdfFrame, cb=0, roughMaxN=4e+07) {
    
    myCDF <- numeric(nrow(pdfFrame)+1)
    myCDF[1] <- 0
    
    for ( intCtr in 1:nrow(pdfFrame) ) {
        myCDF[intCtr+1] <- myCDF[intCtr] + pdfFrame$probs[intCtr]
    }
    
    print(myCDF)
    
    ## nTrials and nPerTrial will be found in global environment
    ## Determine whether to split the processing
    nSplit <- ceiling(nTrials*nPerTrial/roughMaxN)
    print(paste0("Splitting in to ", nSplit," pieces each of roughly ", 
                 round(roughMaxN/1000000,1)," million trials"
                 )
          )
    nPerSub <- ceiling(nPerTrial / nSplit) ## Allow a very slight round up
    print(paste0("Each sub-split will contain ",nPerSub," trials"))
    
    ## Initialize two vectors for storing data
    vecLast <- rep(0, nTrials) ## Will hold the current last value by trial
    vecMin <- rep(0, nTrials) ## Will hold the current minimum value by trial
    
    for (splitCtr in 1:nSplit) {
        mtxTemp <- matrix(pdfFrame$outcomes[findInterval(matrix(data=runif(nTrials * nPerSub, 0, 1),
                                                                nrow=nPerSub, ncol=nTrials
                                                                ), myCDF, rightmost.closed=TRUE
                                                         )
                                           ], nrow=nPerSub, ncol=nTrials
                          )
    
        mtxTemp <- apply(mtxTemp, 2, FUN=cumsum)
    
        if (cb > 0) {
            for (intCtr in 1:nrow(mtxTemp)) {
                ## Very slightly imperfect (potentially) around the edges of nSplit - ignored
                mtxTemp[intCtr, ] <- mtxTemp[intCtr, ] + floor(cb*intCtr)
            }
        }

        ## Add whatever was last to each row of the matrix from this run
        mtxTemp <- mtxTemp + matrix(data=rep(vecLast, nPerSub), nrow=nPerSub, ncol=nTrials, byrow=TRUE) 
        
        ## Store the last row (after adjustment above) from this run back to vecLast
        vecLast <- mtxTemp[nrow(mtxTemp), ]
        
        ## Find the minimum from this run, keep if worse than previous minima
        vecMin <- pmin(apply(mtxTemp, 2, FUN=min), vecMin, 0)
        
        ## Report back on the progress
        print(paste0("Finished sub-split #", splitCtr))
    }
    
    return(vecMin)
}


## Step 1: Read in the outcomes file
baseOutcomes <- getBaseOutcomes(myFileName="Play001Outcomes.csv",forceEQ=TRUE)

## Step 2: Modify the base outcomes file
nAddOnePer <- 68
baseOutcomes <- modBaseOutcomes(baseOutcomes=baseOutcomes, nAddOnePer=nAddOnePer)

## Step 3: Aggregate and keep only probs and outcomes
useProbs <- aggregate(probs ~ outcomes, data=baseOutcomes, FUN=sum)
if (sum(useProbs$outcomes == -1) != 1 | 
    sum(useProbs$outcomes < 0) != 1 | 
    sum(useProbs$outcomes == 0) != 1
    ) {
        stop("Error: Algorithm will not work given these inputs")    
    }

## Step 4: solve for testP where testP is raised to each of the outcomes with prob
## Step 4a: First iteration (defaults to between 0 and 1)
nBuckets <- 2000
mtxResults <- testSeqP(pVec=useProbs, nMax=nBuckets)

## Step 4b: Find locations of any column1 >= column2 (that is the crossover point)
posDelta <- which(mtxResults[,1] >= mtxResults[,2])

## Step 4c: Find the appropriate starting point for a second run through the data
if (length(posDelta)==0 | (length(posDelta)==1 & posDelta[1]==(nBuckets+1) ) ) {
    ## Failed to find any, search starting with the second last element of mtxResults
    newStart <- mtxResults[nBuckets, 1] ## Note that there are nBuckets+1 elements; this is second-last
    newEnd <- mtxResults[nBuckets+1, 1]
} else {
    ## Found at least one, take the earliest instance, and start from it minus 1
    newStart <- mtxResults[posDelta[1]-1, 1]
    newEnd <- mtxResults[posDelta[1]+1, 1]
}


## Step 4d: Second iteration (defaults to 1000 trials, give it the new end points)
mtxResults <- testSeqP(pVec=useProbs, nMax=nBuckets, nStart=newStart, nEnd=newEnd)


## Step 4e: Print the best value found
myBest <- which.min(abs(mtxResults[-(nBuckets+1),1]-mtxResults[-(nBuckets+1),2]))
mtxResults[myBest,]
rr1 <- mtxResults[myBest, 1]
plot(mtxResults[,1], mtxResults[,2]-mtxResults[,1], col="blue", xlab="pRuin Attempt",
     ylab="Error (simulated minus attempt)", 
     main=paste0("Risk of Ruin (best p=", round(rr1,7), ")")
     )
abline(h=0, v=mtxResults[myBest,1], lty=2, lwd=1.5, col="dark green")


## Step 5: Simulate with actual random draws (1000 trials of 10000 hands)
nTrials <- 500 ## Each trial is a column
nPerTrial <- 1000000 ## Each row will be a cumulative outcome

## Step 5a: Run it straight up with the new probabilities
vecMinNew <- calcOutcomes(pdfFrame=baseOutcomes[ ,c("probs","outcomes")])
vecMinNew <- vecMinNew[order(vecMinNew)]

## Step 5b: Run it with the original probabilities and outcomes
pdfOrig <- data.frame(probs=baseOutcomes$oldProbs, outcomes=baseOutcomes$outcomes)
pdfOrig <- pdfOrig[pdfOrig$probs > 0, ]
vecMinOrig <- calcOutcomes(pdfFrame=pdfOrig, cb=1/nAddOnePer)
vecMinOrig <- vecMinOrig[order(vecMinOrig)]

## Step 5c: Calculate minima for modified vector
xMax <- 100 * (ceiling(max(-vecMinNew, -vecMinOrig)/100) + 0)

par(mfrow=c(1,2))

## Left-hand plot
hist(vecMinOrig, col=rgb(0.5, 0, 0, 0.25), 
     main="Lowest value achieved by trial", xlab="Lowest value"
     )
hist(vecMinNew, col=rgb(0, 0, 0.5, 0.25), add=TRUE)
legend("topleft", legend=c("Direct CB", "Modify Probs", "Overlap"), 
       col=c(rgb(0.5, 0, 0, 0.25), rgb(0, 0, 0.5, 0.25), rgb(0.5, 0, 0.5, 0.5)), pch=20
       )

## Right-hand plot
plot(x=-vecMinNew, y=(1:length(vecMinNew))/length(vecMinNew), xlim=c(0, xMax), 
     xlab="Units", ylab="Risk of Ruin", main="Risk of Ruin Curves", 
     col="blue", cex=0.5
     )
points(x=-vecMinOrig, y=(1:length(vecMinOrig))/length(vecMinOrig), col="purple", cex=0.5)
points(x=0:xMax, y=rr1^(1:(xMax+1)), col="dark green", cex=0.25)
abline(h=c(1,0), lty=2)
legend("topright", legend=c("Sim (Mod Prob)", "Sim (CB)", "Theory"), 
       col=c("blue", "purple", "dark green"), lwd=c(2, 2, 2))

par(mfrow=c(1,1))
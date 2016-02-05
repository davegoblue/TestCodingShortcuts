## This script is to implement Exercise001 from https://github.com/davegoblue/TestCodingShortcuts
## Step A:  Create a baseOuctomes vector
## Step B:  Fill a series of outcomes based on random numbers between 0-1
## Step C:  Calculate the cumulative outcome
## Step D:  Calculate where a specified condition occured
## Step E:  Sort by Condition(Y/N) then Cumulative at Condition then Cumulative Final then _N_

## Step A: Create a baseOutcomes vector
## Per the exercise, assume that there is an outcomes table (hard code for now)

baseOutcomes <- data.frame(probs=c(0.01,0.02,0.05,0.18,0.24,0.50),outcomes=c(10,5,2,1,0,-1))

if (sum(baseOutcomes$probs)!=1) { stop("Probabilities need to add to exactly 1, aborting") }

baseMean <- sum(baseOutcomes$probs*baseOutcomes$outcomes)
baseVar <- sum(baseOutcomes$probs*(baseOutcomes$outcomes-baseMean)^2)

print(paste0("Probabilities sum to 1.  Outcomes has mean ",baseMean," and variance ",baseVar))

## Make the CDF vector
myCDF <- numeric(nrow(baseOutcomes)+1)
myCDF[1] <- 0

for ( intCtr in 1:nrow(baseOutcomes) ) {
    myCDF[intCtr+1] <- myCDF[intCtr] + baseOutcomes$probs[intCtr]
}


## Step B:  Fill a series of outcomes based on random numbers between 0-1
nTrials <- 5
nPerTrial <- 10
mtxRands <- matrix(data=runif(nTrials*nPerTrial,0,1),nrow=nPerTrial,ncol=nTrials)
mtxOutcomes <- matrix(baseOutcomes$outcomes[findInterval(mtxRands,myCDF,rightmost.closed=TRUE)],
                      nrow=nPerTrial,ncol=nTrials)
print(paste0("Ouctomes across ",nTrials*nPerTrial," draws have mean: ",
             mean(mtxOutcomes)," and var: ",sd(mtxOutcomes)^2)
      )


## Step C: Calculate the cumulative total for each column in mtxOutcomes
## cumsum() works properly on the columns of a data frame, but coerces a matrix to a single vector (no good)
## So, create a data frame for cumulative sums - each row is still a trial, each column is still an experiment 

dfCumOutcomes <- cumsum(as.data.frame(mtxOutcomes))

maxPerTrial <- as.numeric(apply(dfCumOutcomes,2,FUN=max))
minPerTrial <- as.numeric(apply(dfCumOutcomes,2,FUN=min))
lastPerTrial <- as.numeric(dfCumOutcomes[nrow(dfCumOutcomes),])

dfSummary <- data.frame(myTrial = NA, myMax = maxPerTrial, myMin = minPerTrial, myLast = lastPerTrial,
                              myCond = FALSE, myN_Cond = NA, myVal_Cond = NA)


## Step D:  Calculate where a specified condition first occurred
## This is awful, find a faster way then treating each column as an individual!
## It is also awful that I am hardcoding the condition for now

dfCondOutcomes <- dfCumOutcomes <= -2

for ( intCtr in 1:nTrials ) {
    dfSummary$myTrial[intCtr] = intCtr
    dfSummary$myCond[intCtr] <- sum(dfCondOutcomes[,intCtr]) > 0
    myBool <- dfCondOutcomes[,intCtr] & !duplicated(dfCondOutcomes[,intCtr]) ## & works, && does not
    
    if (sum(myBool) > 1) {
        stop("Error, 2 or more non-duplicated TRUE may not occur, aborting")
    } else if (sum(myBool) == 1) {
        keyBool <- sum(1, cumsum(myBool) == 0)
        dfSummary$myN_Cond[intCtr] <- keyBool
        dfSummary$myVal_Cond[intCtr] <- dfCumOutcomes[keyBool,intCtr]
    }
}


## Step E:  Sort by Condition(Y then N) then Cumulative at Condition () then Cumulative Final then N
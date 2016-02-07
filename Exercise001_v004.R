## This script is to implement Exercise001 from https://github.com/davegoblue/TestCodingShortcuts
## Step A:  Create a baseOuctomes vector
## Step B:  Fill a series of outcomes based on random numbers between 0-1
## Step C:  Calculate the cumulative outcome
## Step D:  Calculate where a specified condition occured
## Step E:  Sort by Condition(Y/N) then _N_ at Condition then Cumulative Final

## Step A: Create a baseOutcomes vector
## Per the exercise, assume that there is an outcomes table (hard code for now)

if (file.exists("BaseOutcomes.csv")) {
    baseOutcomes <- read.csv("BaseOutcomes.csv")
    if (ncol(baseOutcomes) != 2) { stop("Error in CSV file, should have exactly 2 columns") }
    colnames(baseOutcomes) <- c("probs","outcomes")
} else {
    baseOutcomes <- data.frame(probs=c(0.01,0.02,0.05,0.18,0.24,0.50),outcomes=c(10,5,2,1,0,-1))
}

if (sum(baseOutcomes$probs)!=1 | min(baseOutcomes$probs) <= 0 | 
    sum(is.na(baseOutcomes$probs)) > 0 | sum(is.na(baseOutcomes$outcomes)) > 0) { 
        stop("Please resolve the issue with inputs for probs and outcomes, aborting") 
}

baseMean <- sum(baseOutcomes$probs*baseOutcomes$outcomes)
baseVar <- sum(baseOutcomes$probs*(baseOutcomes$outcomes-baseMean)^2)

print(paste0("Probabilities sum to 1.  Outcomes has mean ",format(baseMean,digits=3),
             " and variance ",format(baseVar,digits=3)))

## Make the CDF vector
myCDF <- numeric(nrow(baseOutcomes)+1)
myCDF[1] <- 0

for ( intCtr in 1:nrow(baseOutcomes) ) {
    myCDF[intCtr+1] <- myCDF[intCtr] + baseOutcomes$probs[intCtr]
}


## Step B:  Fill a series of outcomes based on random numbers between 0-1
nTrials <- 1000
nPerTrial <- 4000
mtxRands <- matrix(data=runif(nTrials*nPerTrial,0,1),nrow=nPerTrial,ncol=nTrials)

mtxOutcomes <- matrix(baseOutcomes$outcomes[findInterval(mtxRands,myCDF,rightmost.closed=TRUE)],
                      nrow=nPerTrial,ncol=nTrials)

print(paste0("Ouctomes across ",nTrials*nPerTrial," draws have mean: ",
             format(mean(mtxOutcomes),digits=3)," and variance: ",format(sd(mtxOutcomes)^2,digits=3))
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
## Can I find a way to do this more efficiently than once each per column?
## How can I do the condition without hard-coding?  Want to pass a string like "<= -2" and have it used
## Start by assuming the condition will be of type <= and use eval(expresson())
## Goal is also to be able tpo generalize so that a true condition can be passed -- >, >=, ==, <=, <, etc.


myHurdle <- -600
myCond <- expression(dfCumOutcomes <= myHurdle)
dfCondOutcomes <- eval(myCond)


for ( intCtr in 1:nTrials ) {
    dfSummary$myTrial[intCtr] = intCtr
    dfSummary$myCond[intCtr] <- sum(dfCondOutcomes[,intCtr]) > 0
    myBool <- dfCondOutcomes[,intCtr] & !duplicated(dfCondOutcomes[,intCtr]) ## & works, && does not
    
    if (sum(myBool) > 1) {
        stop("Error, 2 or more non-duplicated TRUE may not occur, aborting")
    } else if (sum(myBool) == 1) {
        keyBool <- sum(1, cumsum(myBool) == 0)  ## myBool is F F . . . F T F . . . F F F; cumsum(myBool)
                                                ## will get all the F prior to that first T
        dfSummary$myN_Cond[intCtr] <- keyBool   ## The single T is where myCond first happened
        dfSummary$myVal_Cond[intCtr] <- dfCumOutcomes[keyBool,intCtr]
    }
}


## Step E:  Sort by Condition(Y then N) then _N_ at Condition then Cumulative Final
dfResults <- dfSummary[order(-dfSummary$myCond, dfSummary$myN_Cond, -dfSummary$myLast),]
print(summary(dfResults))

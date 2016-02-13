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

## Null the baseOutcomes$outcomes where outcomes >= X
myHurdle <- ">=119"
myCond <- parse(text=paste0("baseOutcomes$outcomes",myHurdle))
baseOutcomes$outcomes[eval(myCond)] <- 0

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
nTrials <- 8000
nPerTrial <- 4000
mtxRands <- matrix(data=runif(nTrials*nPerTrial,0,1),nrow=nPerTrial,ncol=nTrials)

mtxOutcomes <- matrix(baseOutcomes$outcomes[findInterval(mtxRands,myCDF,rightmost.closed=TRUE)],
                      nrow=nPerTrial,ncol=nTrials)

print(paste0("Ouctomes across ",nTrials*nPerTrial," draws have mean: ",
             format(mean(mtxOutcomes),digits=3)," and variance: ",format(sd(mtxOutcomes)^2,digits=3))
      )


## Step C: Calculate the cumulative total for each column in mtxOutcomes
## cumsum() works properly on columns of a data frame, but coerces a matrix to a single vector (no good)
## So, create a data frame for cumulative sums - each row is a trial, each column is an experiment 

dfCumOutcomes <- cumsum(as.data.frame(mtxOutcomes))

maxPerTrial <- as.numeric(apply(dfCumOutcomes,2,FUN=max))
minPerTrial <- as.numeric(apply(dfCumOutcomes,2,FUN=min))
lastPerTrial <- as.numeric(dfCumOutcomes[nrow(dfCumOutcomes),])

dfSummary <- data.frame(myTrial = NA, myMax = maxPerTrial, myMin = minPerTrial, myLast = lastPerTrial,
                              myCond = FALSE, myN_Cond = NA, myVal_Cond = NA)


## Step D:  Calculate where a specified condition first occurred
## Can I find a way to do this more efficiently than once each per column?
## While not considered elegant, parse() followed by eval() seems to do the job


myHurdle <- "<=-120"
myCond <- parse(text=paste0("dfCumOutcomes",myHurdle))
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

## Would be good to have the x and y units auto-calculated
minX <- min(dfSummary$myMin)                  ## Find most negative element
maxX <- max(0, dfSummary$myLast)              ## Find most positive element (use 0 if all are negative)
powX <- log10(max(1, abs(minX), abs(maxX)))   ## Find rough "power" of data

unitX <- 10^(round(powX,0)-1)                 ## If thousands, use hundreds; if hundreds, use tens; etc.
minX <- unitX*(floor(minX/unitX)-1)           ## Round to similar units as unitX
maxX <- unitX*(ceiling(maxX/unitX)+1)         ## Round to similar units as unitX

hist(dfSummary$myMin,
     col=rgb(1,0,0,.25),
     main=paste0("Results: ",nTrials," Trials (",nPerTrial," draws per trial)"), 
     xlab="Units", ylab="N Trials",
     breaks=seq(minX,maxX,by=unitX),
     xlim=c(minX, maxX)
     )

hist(dfSummary$myLast,col=rgb(0,0,1,.25),
     breaks=seq(minX,maxX,by=unitX),
     xlim=c(minX,maxX),
     add=TRUE
     )

legend("topright",col=c(rgb(1,0,0,.25),rgb(0,0,1,.25),rgb(0.5,0,0.5,0.5)),
       legend=c("Minimum","Final","Overlap"),pch=20,pt.cex=2)
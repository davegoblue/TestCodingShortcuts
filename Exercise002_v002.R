## This script is to implement Exercise002 from https://github.com/davegoblue/TestCodingShortcuts
## The core of this exercise is to make the code simpler and/or more efficient; 

## Step A: Create a baseOutcomes vector (keep this step as-is, but make it a function)

getBaseOutcomes <- function(myFileName="BaseOutcomes.csv", myHurdle=NULL) {
    
    if (file.exists(myFileName)) {
        baseOutcomes <- read.csv(myFileName)
        if (ncol(baseOutcomes) != 2) { stop("Error in CSV file, should have exactly 2 columns") }
        colnames(baseOutcomes) <- c("probs","outcomes")
    } else {
        baseOutcomes <- data.frame(probs=c(0.01,0.02,0.05,0.18,0.24,0.50),outcomes=c(10,5,2,1,0,-1))
    }
    
    if (sum(baseOutcomes$probs)!=1 | min(baseOutcomes$probs) <= 0 | 
        sum(is.na(baseOutcomes$probs)) > 0 | sum(is.na(baseOutcomes$outcomes)) > 0) { 
        stop("Please resolve the issue with inputs for probs and outcomes, aborting") 
    }
    
    ## Store the original value read in as outcomes
    baseOutcomes$oldOutcomes <- baseOutcomes$outcomes
    
    ## Null the baseOutcomes$outcomes where outcomes >= X
    if (!is.null(myHurdle)) {
        myCond <- parse(text=paste0("baseOutcomes$outcomes",myHurdle))
        baseOutcomes$outcomes[eval(myCond)] <- 0
        print(paste0("Converted all cases where ",myCond," to baseOutcomes$outcomes = 0"))
    }
    
    baseMean <- sum(baseOutcomes$probs*baseOutcomes$outcomes)
    baseVar <- sum(baseOutcomes$probs*(baseOutcomes$outcomes-baseMean)^2)
    
    print(paste0("Probabilities sum to 1.  Outcomes has mean ",format(baseMean,digits=3),
                 " and variance ",format(baseVar,digits=3)))

    return(baseOutcomes)
}

myStart <- proc.time()
baseOutcomes <- getBaseOutcomes(myFileName="BaseOutcomes.csv",myHurdle=">=399")


## Make the CDF vector
myCDF <- numeric(nrow(baseOutcomes)+1)
myCDF[1] <- 0

for ( intCtr in 1:nrow(baseOutcomes) ) {
    myCDF[intCtr+1] <- myCDF[intCtr] + baseOutcomes$probs[intCtr]
}


## Step B:  Fill a series of outcomes based on random numbers between 0-1
nTrials <- 12000
nPerTrial <- 5000

## Could I instead generate all the randoms, map them to results, and then randomize those?
## Old version took 4.84 user / 5.54 elapsed

mtxCumOutcomes <- matrix(baseOutcomes$outcomes[findInterval(matrix(data=runif(nTrials*nPerTrial,0,1),
                                                                   nrow=nPerTrial,
                                                                   ncol=nTrials
                                                                   ),
                                                            myCDF,rightmost.closed=TRUE
                                                            )
                                               ],
                         nrow=nPerTrial,
                         ncol=nTrials
                         )

print(paste0("Ouctomes across ",nTrials*nPerTrial," draws have mean: ",
             format(mean(mtxCumOutcomes),digits=3)," and variance: ",
             format(sd(mtxCumOutcomes)^2,digits=3)
             )
      )


print("Through section B")
print(proc.time() - myStart)


## Step C: Calculate the cumulative total for each column in mtxOutcomes
## cumsum() works properly on columns of a data frame, but requires apply to work on a matrix 

mtxCumOutcomes <- apply(mtxCumOutcomes,2,FUN=cumsum)  ## About 2.5 seconds for 12,000 x 5,000

print("Through section C - cumsum")
print(proc.time() - myStart)

maxPerTrial <- apply(mtxCumOutcomes,2,FUN=max)  ## About 1.0 seconds for 12,000 x 5,000
minPerTrial <- apply(mtxCumOutcomes,2,FUN=min)  ## About 1.0 seconds for 12,000 x 5,000

print("Through section C - max/min")
print(proc.time() - myStart)

lastPerTrial <- as.numeric(mtxCumOutcomes[nrow(mtxCumOutcomes),])

dfSummary <- data.frame(myTrial = 1:nTrials, myMax = maxPerTrial, myMin = minPerTrial, myLast = lastPerTrial,
                              myCond = FALSE, myN_Cond = NA, myVal_Cond = NA)

print("Through section C")
print(proc.time() - myStart)


## Step D:  Calculate where a specified condition first occurred

myHurdle <- "<=-280"
dfSummary$myCond <- eval(parse(text=paste0("dfSummary$myMin",myHurdle)))

print("Through section D myCond")
print(proc.time() - myStart)

foo <- function(x) { 
    which(eval(parse(text=paste0("x",myHurdle))))[1]
}

dfSummary$myN_Cond <- apply(mtxCumOutcomes,2,FUN=foo)  ## About 2.5 seconds for 12,000 x 5,000

print("Through section D myN_Cond")
print(proc.time() - myStart)

for ( intCtr in 1:nTrials ) {
    dfSummary$myVal_Cond[intCtr] <- mtxCumOutcomes[dfSummary$myN_Cond[intCtr],dfSummary$myTrial[intCtr]]
}

print("Through section D")
print(proc.time() - myStart)


## Step E:  Sort by Condition(Y then N) then _N_ at Condition then Cumulative Final; plot it
graphSummary <- function(graphData) {
    
    graphData <- graphData[order(-graphData$myCond, graphData$myN_Cond, -graphData$myLast),]
    print(summary(graphData))

    ## Would be good to have the x and y units auto-calculated
    minX <- min(graphData$myMin)                  ## Find most negative element
    maxX <- max(0, graphData$myLast)              ## Find most positive element (use 0 if all are negative)
    powX <- log10(max(1, abs(minX), abs(maxX)))   ## Find rough "power" of data

    unitX <- 10^(round(powX,0)-1)                 ## If thousands, use hundreds; if hundreds, use tens; etc.
    minX <- unitX*(floor(minX/unitX)-1)           ## Round to similar units as unitX
    maxX <- unitX*(ceiling(maxX/unitX)+1)         ## Round to similar units as unitX

    hist(graphData$myMin,
         col=rgb(1,0,0,.25),
         main=paste0("Results: ",nTrials," Trials (",nPerTrial," draws per trial)"), 
         xlab="Units", ylab="N Trials",
         breaks=seq(minX,maxX,by=unitX),
         xlim=c(minX, maxX)
         )

    hist(graphData$myLast,col=rgb(0,0,1,.25),
         breaks=seq(minX,maxX,by=unitX),
         xlim=c(minX,maxX),
         add=TRUE
         )

    legend("topright",col=c(rgb(1,0,0,.25),rgb(0,0,1,.25),rgb(0.5,0,0.5,.5)),
           legend=c("Minimum","Final","Overlap"),pch=20,pt.cex=2
           )

}

graphSummary(graphData=dfSummary)

print("Through section E (finished)")
print(proc.time() - myStart)
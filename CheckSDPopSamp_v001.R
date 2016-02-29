## Check Population vs. Sample Standard Deviation

## First run this with the rnorm(mean=0, sd=1) distribution
myMax <- 50
myDraws <- 2000

sdSam <- rep(0,myDraws)
sdPop <- rep(0,myDraws)
varSam <- rep(0,myDraws)
varPop <- rep(0,myDraws)

mySDSam <- rep(0,myMax)
mySDPop <- rep(0,myMax)
myVarSam <- rep(0,myMax)
myVarPop <- rep(0,myMax)

for (intCtr in 1:myMax) {
    
    for (intCtr2 in 1:myDraws) {
        a <- rnorm(intCtr,mean=0,sd=1)
        sdSam[intCtr2] <- sd(a)
        sdPop[intCtr2] <- sd(c(a,mean(a)))
        varSam[intCtr2] <- var(a)
        varPop[intCtr2] <- var(c(a,mean(a)))
    }
    
    mySDSam[intCtr] <- mean(sdSam)
    mySDPop[intCtr] <- mean(sdPop)
    myVarSam[intCtr] <- mean(varSam)
    myVarPop[intCtr] <- mean(varPop)    
}

plot(x=1:intCtr,y=myVarSam,type="l",col="blue",ylim=c(0,1.2),
     xlab="Sample Size",ylab="Variance",main="Reduces bias in variance"
     )
lines(x=1:intCtr,y=myVarPop,col="red")
abline(h=1,col="dark green")
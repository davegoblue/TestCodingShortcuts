## This file will be to read in the large matrix and then run lookup against it
## Script Exercise003_v006.R saved file "myPermFinal.rds"
## This takes a long time to read in -- not clear saving the file save any time!
## At least make it a single read per session by checking before reading

myStart <- proc.time()

readMyRDS <- function(myInFile, checkFile="myPermFinal", forceRead=FALSE) {
    if (!exists(checkFile) | forceRead==TRUE) {
        print(paste0("Reading in ",myInFile))
        myOutFile <- readRDS(file=myInFile)
        return(myOutFile)
    } else {
        print(paste0(checkFile," already exists; not reloading"))
        myOutFile <- get(checkFile)
        return(myOutFile)
    }
}

myPermFinal <- readMyRDS("myPermFinal.rds")

print("Through reading in the .rds file")
print(proc.time() - myStart)

## Create random hands and look them up
for (intCtr in 1:100) {
    myHand <- paste0(rep("0",52),collapse="")
    myRand <- sample(52,5,replace=FALSE)
    for (myNum in myRand) {
        substr(myHand,myNum,myNum) <- "1"
    }

    myPermFinal[match(myHand,myPermFinal$cards),-7]
}

print("Finished the random lookups")
print(proc.time() - myStart)
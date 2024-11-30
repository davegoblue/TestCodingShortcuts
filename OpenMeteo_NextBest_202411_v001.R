
runPartialImportanceRF <- function(dfTrain, 
                                   yVar, 
                                   dfTest,
                                   impDB=dfImp,
                                   nImp=+Inf,
                                   otherX=c(),
                                   isContVar=TRUE, 
                                   useLabel=keyLabel, 
                                   useSub=stringr::str_to_sentence(keyLabel), 
                                   rndTo=NULL,
                                   rndBucketsAuto=50,
                                   nSig=NULL,
                                   refXY=FALSE,
                                   makePlots=FALSE, 
                                   returnElem=c("rfImp", "rfAcc")
                                   ) {
    
    # FUNCTION ARGUMENTS
    # dfTrain: training data
    # yVar: y variable in dfTrain
    # dfTest: test data
    # impDB: tibble containing variable importance by dependent variable
    # nImp: use the top nImp variables by variable importance
    # otherX: include these additional x variables
    # isContVar: boolean, is this a continuous variable (regression)? FALSE means classification
    # useLabel: label for description
    # useSub: label for plot
    # rndTo: controls the rounding parameter for plots, passed to runFullRF 
    #        (NULL means no rounding)
    #        -1L means make an estimate based on underlying data
    # rndBucketsAuto: integer, if rndTo is -1L, about how many buckets are desired for predictions?
    # nSig: number of significant digits for automatically calculated rounding parameter
    #       (NULL means calculate exactly)    
    # refXY: controls the reference line parameter for plots, passed to runFullRF
    # makePlots: boolean, should plots be created?
    # returnElem: character vector of list elements to be returned
    
    runFullRF(dfTrain=dfTrain, 
              yVar=yVar, 
              xVars=unique(c(impDB %>% filter(n<=nImp, src==yVar) %>% pull(metric), otherX)), 
              dfTest=dfTest, 
              isContVar = isContVar, 
              useLabel=useLabel, 
              useSub=useSub, 
              rndTo=rndTo,
              rndBucketsAuto=rndBucketsAuto,
              nSig=nSig,
              refXY=refXY,
              makePlots=makePlots,
              returnData=TRUE
              )[returnElem]
    
}

autoPartialImportance <- function(dfTrain, 
                                  dfTest, 
                                  yVar, 
                                  isContVar,
                                  impDB=dfImp,
                                  impNums=c(1:10, 16, 25, nrow(filter(dfImp, src==yVar)))
                                  ) {
    
    # FUNCTION ARGUMENTS:
    # dfTrain: training data
    # dfTest: test (holdout) data
    # yVar: dependent variable
    # isContVar: boolean, is this a contnuous variable (R-2) or categorical variable (accuracy)?
    # impDB: tibble containing sorted variable importances by predictor
    # impNums: vector of number of variables to run (each element in vector run)
    
    # Accuracy on holdout data
    tblRPI <- tibble::tibble(nImp=impNums, 
                             rfAcc=sapply(impNums, 
                                          FUN=function(x) {y <- runPartialImportanceRF(dfTrain=dfTrain, 
                                                                                       yVar=yVar, 
                                                                                       dfTest=dfTest, 
                                                                                       isContVar=isContVar, 
                                                                                       impDB=impDB, 
                                                                                       nImp=x, 
                                                                                       makePlots=FALSE
                                          )[["rfAcc"]]
                                          if(isTRUE(isContVar)) y <- y["r2"]
                                          y
                                          }
                             )
    )
    print(tblRPI)
    
    # Plot of holdout accuracy/r-squared vs. number of variables
    # if(isTRUE(isContVar)) tblRPI <- tblRPI %>% mutate(rfAcc=r2)
    if(isTRUE(isContVar)) prtDesc <- "R-squared" else prtDesc <- "Accuracy"
    p1 <- tblRPI %>%
        select(nImp, rfAcc) %>%
        bind_rows(tibble::tibble(nImp=0, rfAcc=0)) %>%
        ggplot(aes(x=nImp, y=rfAcc)) + 
        geom_line() + 
        geom_point() + 
        labs(title=paste0(prtDesc, " on holdout data vs. number of predictors"), 
             subtitle=paste0("Predicting ", yVar),
             y=paste0(prtDesc, " on holdout data"), 
             x="# Predictors (selected in order of variable importance in full model)"
        ) + 
        lims(y=c(0, 1)) + 
        geom_hline(data=~filter(., rfAcc==max(rfAcc)), aes(yintercept=rfAcc), lty=2)
    print(p1)
    
    return(tblRPI)
    
}


runNextBestPredictor <- function(varsRun, 
                                 xFix, 
                                 yVar, 
                                 isContVar,
                                 dfTrain,
                                 dfTest=dfTrain, 
                                 useLabel="predictions based on training data applied to holdout dataset",
                                 useSub=stringr::str_to_sentence(keyLabel_v3), 
                                 makePlots=FALSE
                                 ) {
    
    # FUNCTION ARGUMENTS:
    # varsRun: variables to be run as potential next-best predictors
    # xFix: variables that are already included in every test of next-best
    # yVar: dependent variable of interest
    # isContVar: boolean, is yvar continuous?
    # dfTrain: training data
    # dfTest: test data
    # useLabel: descriptive label
    # useSub: subtitle description
    # makePlots: boolean, should plots be created for each predictor run?
    
    vecAcc <- sapply(varsRun, FUN=function(x) {
        y <- runFullRF(dfTrain=dfTrain, 
                       yVar=yVar, 
                       xVars=c(xFix, x),
                       dfTest=dfTest, 
                       useLabel=useLabel, 
                       useSub=useSub,
                       isContVar=isContVar,
                       makePlots=makePlots,
                       returnData=TRUE
                       )[["rfAcc"]]
        if(isTRUE(isContVar)) y[["r2"]] else y
    }
    )
    
    vecAcc %>% 
        as.data.frame() %>% 
        purrr::set_names("rfAcc") %>% 
        rownames_to_column("pred") %>% 
        tibble::tibble() %>%
        arrange(desc(rfAcc)) %>%
        print(n=40)
    
    vecAcc
    
}


getNextBestVar <- function(x, returnTbl=FALSE, n=if(isTRUE(returnTbl)) +Inf else 1) {
    
    # FUNCTION ARGUMENTS:
    # x: named vector of accuracy or r-squared
    # returnTbl: boolean, if TRUE convert to tibble and return, if FALSE return vector of top-n predictors 
    # n: number of predictrs to return (+Inf will return the full tibble or vector)
    
    tbl <- vecToTibble(x, colNameName="pred") %>%
        arrange(-value) %>%
        slice_head(n=n)
    if(isTRUE(returnTbl)) return(tbl)
    else return(tbl %>% pull(pred))
    
}



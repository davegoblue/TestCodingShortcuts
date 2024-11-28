# File for functions used in simpleOneVar for OpenMeteoEDA_xxxxxx_vxxx.Rmd

# Simple predictive model for categorical variable
simpleOneVarPredict <- function(df, 
                                tgt, 
                                prd, 
                                dfTest=NULL,
                                nPrint=30, 
                                showPlot=TRUE, 
                                returnData=TRUE
                                ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame or tibble with key elements (training data set)
    # tgt: target variable
    # prd: predictor variable
    # dfTest: test dataset for applying predictions
    # nPrint: maximum number of lines of confusion matrix to print
    #         0 means do not print any summary statistics
    # showPlot: boolean, should overlap plot be created and shown?
    
    # Counts of predictor to target variable
    dfPred <- df %>%
        group_by(across(all_of(c(prd, tgt)))) %>%
        summarize(n=n(), .groups="drop") %>%
        arrange(across(all_of(prd)), desc(n)) %>%
        group_by(across(all_of(prd))) %>%
        mutate(correct=row_number()==1, predicted=first(get(tgt))) %>%
        ungroup()
    
    # Confusion matrix and accuracy
    dfConf <- dfPred %>%
        group_by(across(all_of(c(tgt, "correct")))) %>%
        summarize(n=sum(n), .groups="drop") %>%
        pivot_wider(id_cols=tgt, names_from=correct, values_from=n, values_fill=0) %>%
        mutate(n=`TRUE`+`FALSE`, 
               pctCorrect=`TRUE`/n, 
               pctNaive=1/(nrow(.)), 
               lift=pctCorrect/pctNaive-1
        )
    
    # Overall confusion matrix
    dfConfAll <- dfConf %>%
        summarize(nMax=max(n), across(c(`FALSE`, `TRUE`, "n"), sum)) %>%
        mutate(pctCorrect=`TRUE`/n, 
               pctNaive=nMax/n, 
               lift=pctCorrect/pctNaive-1, 
               nBucket=length(unique(dfPred[[prd]]))
        )
    
    # Print confusion matrices
    if(nPrint > 0) {
        cat("\nAccuracy by target subgroup (training data):\n")
        dfConf %>% print(n=nPrint)
        cat("\nOverall Accuracy (training data):\n")
        dfConfAll %>% print(n=nPrint)
    }
    
    # Plot of overlaps
    if(isTRUE(showPlot)) {
        p1 <- dfPred %>%
            group_by(across(c(all_of(tgt), "predicted", "correct"))) %>%
            summarize(n=sum(n), .groups="drop") %>%
            ggplot(aes(x=get(tgt), y=predicted)) + 
            labs(x="Actual", 
                 y="Predicted", 
                 title=paste0("Training data - Actual vs. predicted ", tgt), 
                 subtitle=paste0("(using ", prd, ")")
            ) + 
            geom_text(aes(label=n)) + 
            geom_tile(aes(fill=correct), alpha=0.25)
        print(p1)
    }
    
    # Create metrics for test dataset if requested
    if(!is.null(dfTest)) {
        # Get maximum category from training data
        mostPredicted <- count(dfPred, predicted, wt=n) %>% slice(1) %>% pull(predicted)
        # Get mapping of metric to prediction
        dfPredict <- dfPred %>% 
            group_by(across(all_of(c(prd, "predicted")))) %>% 
            summarize(n=sum(n), .groups="drop")
        # Create predictions for test data
        dfPredTest <- dfTest %>%
            select(all_of(c(prd, tgt))) %>%
            left_join(select(dfPredict, -n)) %>%
            replace_na(list(predicted=mostPredicted)) %>%
            group_by(across(all_of(c(prd, tgt, "predicted")))) %>%
            summarize(n=n(), .groups="drop") %>%
            mutate(correct=(get(tgt)==predicted))
        # Create confusion statistics for test data
        dfConfTest <- dfPredTest %>%
            group_by(across(all_of(c(tgt, "correct")))) %>%
            summarize(n=sum(n), .groups="drop") %>%
            pivot_wider(id_cols=tgt, names_from=correct, values_from=n, values_fill=0) %>%
            mutate(n=`TRUE`+`FALSE`, 
                   pctCorrect=`TRUE`/n, 
                   pctNaive=1/(nrow(.)), 
                   lift=pctCorrect/pctNaive-1
            )
        # Overall confusion matrix for test data
        dfConfAllTest <- dfConfTest %>%
            summarize(nMax=max(n), across(c(`FALSE`, `TRUE`, "n"), sum)) %>%
            mutate(pctCorrect=`TRUE`/n, 
                   pctNaive=nMax/n, 
                   lift=pctCorrect/pctNaive-1, 
                   nBucket=length(unique(dfConfTest[[prd]]))
            )
        # Print confusion matrices
        if(nPrint > 0) {
            cat("\nAccuracy by target subgroup (testing data):\n")
            dfConfTest %>% print(n=nPrint)
            cat("\nOverall Accuracy (testing data):\n")
            dfConfAllTest %>% print(n=nPrint)
        }
    } else {
        dfPredTest <- NULL
        dfConfTest <- NULL
        dfConfAllTest <- NULL
        
    }
    
    # Return data if requested
    if(isTRUE(returnData)) list(dfPred=dfPred, 
                                dfConf=dfConf, 
                                dfConfAll=dfConfAll, 
                                dfPredTest=dfPredTest, 
                                dfConfTest=dfConfTest, 
                                dfConfAllTest=dfConfAllTest
    )
    
}


# Fit a single predictor to a single categorical variable
simpleOneVarFit <- function(df, 
                            tgt, 
                            prd, 
                            rankType="last", 
                            naMethod=TRUE
                            ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame or tibble with key elements (training data set)
    # tgt: target variable
    # prd: predictor variable
    # rankType: method for breaking ties of same n, passed to base::rank as ties.method=
    # naMethod: method for handling NA in ranks, passed to base::rank as na.last=
    
    # Counts of predictor to target variable, and associated predictions
    df %>%
        group_by(across(all_of(c(prd, tgt)))) %>%
        summarize(n=n(), .groups="drop") %>%
        arrange(across(all_of(prd)), desc(n), across(all_of(tgt))) %>%
        group_by(across(all_of(prd))) %>%
        mutate(rankN=n()+1-rank(n, ties.method=rankType, na.last=naMethod)) %>%
        arrange(across(all_of(prd)), rankN) %>%
        ungroup()
    
}


# Create categorical predictions mapper
simpleOneVarMapper <- function(df, tgt, prd) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame or tibble from SimpleOneVarFit()
    # tgt: target variable
    # prd: predictor variable
    
    # Get the most common actual results
    dfCommon <- df %>% count(across(all_of(tgt)), wt=n, sort=TRUE)
    
    # Get the predictions
    dfPredictor <- df %>%
        group_by(across(all_of(prd))) %>%
        filter(row_number()==1) %>%
        select(all_of(c(prd, tgt))) %>%
        ungroup()
    
    list(dfPredictor=dfPredictor, dfCommon=dfCommon)
    
}


# Map the categorical predictions to unseen data
simpleOneVarApplyMapper <- function(df, 
                                    tgt,
                                    prd, 
                                    mapper, 
                                    mapperDF="dfPredictor", 
                                    mapperDefault="dfCommon",
                                    prdName="predicted"
                                    ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame containing prd for predicting tgt
    # tgt: target variable in df
    # prd: predictor variable in df
    # mapper: mapping list from sinpleOneVarMapper()
    # mapperDF: element that can be used to merge mappings
    # mapperDefault: element that can be used for NA resulting from merging mapperDF
    # prdName: name for the prediction variable
    
    # Extract the mapper and default value
    vecRename <- c(prdName) %>% purrr::set_names(tgt)
    dfMap <- mapper[[mapperDF]] %>% select(all_of(c(prd, tgt))) %>% colRenamer(vecRename=vecRename)
    chrDefault <- mapper[[mapperDefault]] %>% slice(1) %>% pull(tgt)
    
    # Merge mappings to df
    df %>%
        left_join(dfMap, by=prd) %>%
        replace_na(list("predicted"=chrDefault))
    
}


# Create confusion matrix data for categorical predictions
simpleOneVarConfusionData <- function(df, 
                                      tgtOrig,
                                      tgtPred, 
                                      otherVars=c(),
                                      weightBy="n"
                                      ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame from simpleOneVarApplyMapper()
    # tgtOrig: original target variable name in df
    # tgtPred: predicted target variable name in df
    # otherVars: other variables to be kept (will be grouping variables)
    # weightBy: weighting variable for counts in df (NULL means count each row of df as 1)
    
    # Confusion matrix data creation
    df %>%
        group_by(across(all_of(c(tgtOrig, tgtPred, otherVars)))) %>%
        summarize(n=if(!is.null(weightBy)) sum(get(weightBy)) else n(), .groups="drop") %>%
        mutate(correct=get(tgtOrig)==get(tgtPred))
    
}


# Print and plot confusion matrix for categorical predictions
simpleOneVarConfusionReport <- function(df, 
                                        tgtOrig,
                                        tgtPred, 
                                        otherVars=c(), 
                                        printConf=TRUE,
                                        printConfOrig=printConf, 
                                        printConfPred=printConf,
                                        printConfOverall=printConf, 
                                        plotConf=TRUE, 
                                        plotDesc="",
                                        nBucket=NA, 
                                        predictorVarName="", 
                                        returnData=FALSE
                                        ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame from simpleOneVarConfusionData()
    # tgtOrig: original target variable name in df
    # tgtPred: predicted target variable name in df
    # otherVars: other variables to be kept (will be grouping variables) - NOT IMPLEMENTED
    # printConf: boolean, should confusion matrix data be printed? Applies to all three
    # printConfOrig: boolean, should confusion data be printed based on original target variable?
    # printConfPred: boolean, should confusion data be printed based on predicted target variable?
    # printConfOverall: boolean, should overall confusion data be printed?
    # plotConf: boolean, should confusion overlap data be plotted?
    # plotDesc: descriptive label to be included in front of plot title
    # nBucket: number of buckets used for prediction (pass from previous data)
    # predictorVarName: variable name to be included in chart description
    # returnData: boolean, should the confusion matrices be returned?
    
    # Confusion data based on original target variable
    if(isTRUE(printConfOrig) | isTRUE(returnData)) {
        dfConfOrig <- df %>%
            group_by(across(all_of(c(tgtOrig)))) %>%
            summarize(right=sum(n*correct), wrong=sum(n)-right, n=sum(n), .groups="drop") %>%
            mutate(pctRight=right/n, pctNaive=n/(sum(n)), lift=pctRight/pctNaive-1)
    }
    
    # Confusion data based on predicted target variable
    if(isTRUE(printConfPred) | isTRUE(returnData)) {
        dfConfPred <- df %>%
            group_by(across(all_of(c(tgtPred)))) %>%
            summarize(right=sum(n*correct), wrong=sum(n)-right, n=sum(n), .groups="drop") %>%
            mutate(pctRight=right/n)
    }
    
    # Overall confusion data
    if(isTRUE(printConfOverall) | isTRUE(returnData)) {
        maxNaive <- df %>%
            group_by(across(all_of(tgtOrig))) %>%
            summarize(n=sum(n), .groups="drop") %>%
            arrange(desc(n)) %>%
            slice(1) %>%
            pull(n)
        dfConfOverall <- df %>%
            summarize(right=sum(n*correct), wrong=sum(n)-right, n=sum(n), .groups="drop") %>%
            mutate(maxN=maxNaive, pctRight=right/n, pctNaive=maxN/n, lift=pctRight/pctNaive-1, nBucket=nBucket)
    }
    
    # Confusion report based on original target variable
    if(isTRUE(printConfOrig)) {
        cat("\nConfusion data based on original target variable:", tgtOrig, "\n")
        dfConfOrig %>%
            print(n=50)
    }
    
    # Confusion report based on predicted target variable
    if(isTRUE(printConfPred)) {
        cat("\nConfusion data based on predicted target variable:", tgtPred, "\n")
        dfConfPred %>%
            print(n=50)
    }
    
    # Overall confusion matrix
    if(isTRUE(printConfOverall)) {
        cat("\nOverall confusion matrix\n")
        dfConfOverall %>%
            print(n=50)
    }
    
    # Plot of overlaps
    if(isTRUE(plotConf)) {
        p1 <- df %>%
            group_by(across(all_of(c(tgtOrig, tgtPred, "correct")))) %>%
            summarize(n=sum(n), .groups="drop") %>%
            ggplot(aes(x=get(tgtOrig), y=get(tgtPred))) + 
            labs(x="Actual", 
                 y="Predicted", 
                 title=paste0(plotDesc, "Actual vs. predicted ", tgtOrig), 
                 subtitle=paste0("(using ", predictorVarName, ")")
            ) + 
            geom_text(aes(label=n)) + 
            geom_tile(aes(fill=correct), alpha=0.25)
        print(p1)
    }
    
    # Return data if requested
    if(isTRUE(returnData)) list(dfConfOrig=dfConfOrig, dfConfPred=dfConfPred, dfConfOverall=dfConfOverall)
    
}


# Process for chaining predictor, applier, and confusion matrix for categorical variables
simpleOneVarChain <- function(df,
                              tgt,
                              prd,
                              mapper=NULL, 
                              rankType="last", 
                              naMethod=TRUE, 
                              printReport=TRUE, 
                              plotDesc="",
                              returnData=TRUE, 
                              includeConfData=FALSE
                              ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame or tibble with key elements (training or testing data set)
    # tgt: target variable
    # prd: predictor variable
    # mapper: mapping file to be applied for predictions (NULL means create from simpleOneVarApply())
    # rankType: method for breaking ties of same n, passed to base::rank as ties.method=
    # naMethod: method for handling NA in ranks, passed to base::rank as na.last=    
    # printReport: boolean, should the confusion report data and plot be printed?
    # plotDesc: descriptive label to be included in front of plot title
    # returnData: boolean, should data elements be returned?
    # includeConfData: boolean, should confusion data be returned?
    
    # Create the summary of predictor-target-n
    dfFit <- simpleOneVarFit(df, tgt=tgt, prd=prd, rankType=rankType, naMethod=naMethod)     
    
    # Create the mapper if it does not already exist
    if(is.null(mapper)) mapper <- simpleOneVarMapper(dfFit, tgt=tgt, prd=prd)
    
    # Apply mapper to data
    dfApplied <- simpleOneVarApplyMapper(dfFit, tgt=tgt, prd=prd, mapper=mapper)
    
    # Create confusion data
    dfConfusion <- simpleOneVarConfusionData(dfApplied, tgtOrig=tgt, tgtPred="predicted")
    
    # Create confusion report if requested
    if(isTRUE(printReport) | isTRUE(includeConfData)) {
        dfConfReport <- simpleOneVarConfusionReport(df=dfConfusion, 
                                                    tgtOrig=tgt, 
                                                    tgtPred="predicted", 
                                                    nBucket=length(unique(dfApplied[[prd]])), 
                                                    predictorVarName=prd, 
                                                    printConf=printReport, 
                                                    plotConf=printReport,
                                                    plotDesc=plotDesc,
                                                    returnData=includeConfData
        )
    }
    
    # Return data if requested
    if(isTRUE(returnData)) {
        ret <- list(dfFit=dfFit, mapper=mapper, dfApplied=dfApplied, dfConfusion=dfConfusion)
        if(isTRUE(includeConfData)) ret<-c(ret, list(dfConfData=dfConfReport))
        ret
    }
    
}


# Adds a train-test component for single variable predictions
simpleOneVarTrainTest <- function(dfTrain,
                                  dfTest,
                                  tgt,
                                  prd,
                                  rankType="last", 
                                  naMethod=TRUE, 
                                  printReport=FALSE, 
                                  includeConfData=TRUE, 
                                  returnData=TRUE
                                  ) {
    
    # FUNCTION ARGUMENTS:
    # dfTrain: data frame or tibble with key elements (training data set)
    # dfTest: data frame or tibble with key elements (testing data set)
    # tgt: target variable
    # prd: predictor variable
    # rankType: method for breaking ties of same n, passed to base::rank as ties.method=
    # naMethod: method for handling NA in ranks, passed to base::rank as na.last=    
    # printReport: boolean, should the confusion report data and plot be printed?
    # includeConfData: boolean, should confusion data be returned?
    # returnData: boolean, should data elements be returned?
    
    # Fit the training data
    tmpTrain <- simpleOneVarChain(df=dfTrain, 
                                  tgt=tgt, 
                                  prd=prd,
                                  rankType=rankType,
                                  naMethod=naMethod,
                                  printReport=printReport,
                                  plotDesc="Training data: ",
                                  returnData=TRUE,
                                  includeConfData=includeConfData
    )
    
    # Fit the testing data
    tmpTest <- simpleOneVarChain(df=dfTest, 
                                 tgt=tgt, 
                                 prd=prd,
                                 mapper=tmpTrain$mapper,
                                 rankType=rankType,
                                 naMethod=naMethod,
                                 printReport=printReport,
                                 plotDesc="Testing data: ",
                                 returnData=TRUE,
                                 includeConfData=includeConfData
    )
    
    # Return data if requested
    if(isTRUE(returnData)) list(tmpTrain=tmpTrain, tmpTest=tmpTest)
    
}



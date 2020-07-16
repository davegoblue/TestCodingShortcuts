
# Convert cloud height to buckets and factor
mapCloudHeight <- function(x) {
    
    factor(case_when(x==-100 ~ "None", 
                     x <= 1000 ~ "Surface", 
                     x <= 3000 ~ "Low", 
                     x <= 6000 ~ "Medium", 
                     x <= 12000 ~ "High", 
                     TRUE ~ "Error"
    ), 
    levels=c("Surface", "Low", "Medium", "High", "None")
    )
    
}


# The createTestTrain function is updated to purely split an input dataframe
createTestTrain <- function(df, 
                            testSize=0.3, 
                            sortTrain=FALSE,
                            noNA=TRUE,
                            seed=NULL
                            ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble for analysis
    # testSize: the proportion of the data to be used as test
    # sortTrain: boolean, whether to sort training indices to maintain the original order of training data
    # noNA: boolean, whether to include only complete cases
    # seed: the random seed to be used (NULL means no seed)
    
    # Filter out NA if requested
    if (noNA) {
        df <- df %>%
            filter(complete.cases(df))
    }
    
    # Get the desired number of train objects
    nTrain <- round((1-testSize) * nrow(df))
    
    # Set the random seed if it has been passed
    if (!is.null(seed)) { set.seed(seed) }
    
    # Get the indices for the training data
    idxTrain <- sample(1:nrow(df), size=nTrain, replace=FALSE)
    
    # Sort if requested
    if (sortTrain) { idxTrain <- sort(idxTrain) }
    
    # Return a list containing the train and test data
    list(trainData=df[idxTrain, ], 
         testData=df[-idxTrain, ]
    )
    
}


# Evaluate model predictions
evalPredictions <- function(lst, 
                            plotCaption, 
                            keyVar="locale", 
                            locOrder=NULL
                            ) {
    
    # FUNCTION ARGUMENTS:
    # lst: the list containing outputs of the modeling
    # plotCaption: description of predictors used
    # keyVar: the variable that represents truth when assessing predictions
    # locOrder: desired sort for x/y axes for p2 (NULL means ordered by accuracy, TRUE means order by keyVar)
    
    # Create summary of accuracy
    all2016Accuracy <- lst$testData %>%
        mutate(locale=factor(get(keyVar), levels=levels(predicted))) %>%
        count(locale, predicted, correct) %>%
        group_by(locale) %>%
        mutate(pct=n/sum(n)) %>%
        ungroup()
    
    # Calculate the number of levels
    nLevels <- length(levels(factor(all2016Accuracy$locale)))
    nullAcc <- 1 / nLevels
    
    # Create plot for overall accuracy
    p1 <- all2016Accuracy %>%
        filter(locale==predicted) %>%
        ggplot(aes(x=fct_reorder(locale, pct))) + 
        geom_point(aes(y=pct), size=2) + 
        geom_text(aes(y=pct+0.04, label=paste0(round(100*pct), "%"))) +
        geom_hline(aes(yintercept=nullAcc), lty=2) +
        coord_flip() + 
        ylim(0, 1) + 
        labs(x="", 
             y="Correctly Predicted", 
             title="Accuracy of Locale Predictions", 
             subtitle="(positive detection rate by locale)", 
             caption=paste0(plotCaption, 
                            " as predictors\n(", 
                            round(100*nullAcc), 
                            "% is baseline null accuracy)"
             )
        )
    print(p1)
    
    # Order locales sensibly if locOrder has been passed as TRUE or NULL
    if (isTRUE(locOrder)) {
        locOrder <- all2016Accuracy %>%
            pull(locale) %>%
            unique() %>%
            sort(decreasing=TRUE)
    }
    if (is.null(locOrder)) {
        locOrder <- all2016Accuracy %>%
            filter(correct) %>%
            arrange(pct) %>%
            pull(locale)
    }
    
    # Create plot for which locales are classified as each other
    p2 <- all2016Accuracy %>%
        mutate(locale=factor(locale, levels=locOrder), 
               predPretty=factor(str_replace(predicted, pattern=", ", replacement="\n"), 
                                 levels=str_replace(locOrder, pattern=", ", replacement="\n")
               )
        ) %>%
        ggplot(aes(y=locale, x=predPretty)) + 
        geom_tile(aes(fill=pct)) + 
        geom_text(aes(label=paste0(round(100*pct), "%"))) + 
        scale_fill_continuous("% Predicted As", low="white", high="green") + 
        scale_x_discrete(position="top") +
        theme(axis.text.x=element_text(angle=90)) + 
        labs(x="",
             y="Actual Locale", 
             title="Predicted Locale vs. Actual Locale", 
             caption=paste0(plotCaption, " as predictors")
        )
    print(p2)
    
    # Return the accuracy object
    all2016Accuracy
    
}


# Run a random forest for two locales
rfTwoLocales <- function(df,
                         loc1, 
                         loc2,
                         locVar="source",
                         otherVar="dtime",
                         vrbls=c("TempF", "DewF"),
                         pred="locale",
                         seed=NULL,
                         ntree=100,
                         mtry=NULL, 
                         testSize=0.3, 
                         classify=TRUE
                         ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble
    # loc1: the first locale
    # loc2: the second locale
    # locVar: the name of the variable where loc1 and loc2 can be found
    # otherVar: other variables to be kept, but not used in modeling
    # vrbls: explanatory variables for modeling
    # pred: predictor variable for modeling
    # seed: the random seed (NULL means no seed)
    # ntree: the number of trees to grow in the random forest
    # mtry: the splitting parameter for the random forest (NULL means use all variables)
    # testSize: proportion of data to use as test dataset
    # classify: boolean, whether this is a classification (FALSE will run regression)
    
    # Filter df such that it includes only observations in loc1 or loc2
    # Select only the predictor and variables of interest
    dfModel <- df %>%
        filter(get(locVar) %in% c(loc1, loc2)) %>%
        select_at(vars(all_of(c(pred, otherVar, vrbls))))
    
    # Create the test-train split (will randomize using seed if provided)
    ttLists <- createTestTrain(dfModel, testSize=testSize, seed=seed)
    
    # Set the seed if requested
    if (!is.null(seed)) { set.seed(seed) }
    
    # Set mtry to be the length of the variables if not provided
    if (is.null(mtry)) { mtry <- length(vrbls) }
    
    # Find the y-variable (factor for classification, as-is for regression)
    yVar <- ttLists$trainData[, pred, drop=TRUE]
    if (classify) yVar <- factor(yVar)
    
    # Run the random forest on the training data
    rfModel <- randomForest::randomForest(x=ttLists$trainData[, vrbls], 
                                          y=yVar, 
                                          mtry=mtry, 
                                          ntree=ntree
    )
    
    # Create predictions on the test data
    testPred <- predict(rfModel, ttLists$testData)
    
    # Augment the test data with the predictions
    testData <- ttLists$testData %>%
        mutate(predicted=testPred, correct=(testPred==get(pred)))
    
    # Return the objects as a list
    list(rfModel=rfModel, 
         testData=testData,
         errorRate=rfModel$err.rate[nrow(rfModel$err.rate), ]
    )
    
}


# Helper function for map_dfr
helperAccuracyLocale <- function(x) {
    
    y <- x$errorRate
    tibble::tibble(locale1=names(y)[2], 
                   locale2=names(y)[3], 
                   accOverall=1-y[1], 
                   accLocale1=1-y[2], 
                   accLocale2=1-y[3]
    )
    
}


# Run random forest for multiple locales
rfMultiLocale <- function(tbl, 
                          vrbls,
                          locs=NULL, 
                          locVar="source", 
                          otherVar="dtime",
                          pred="locale", 
                          seed=NULL, 
                          ntree=100, 
                          mtry=NULL, 
                          testSize=0.3
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble
    # vrbls: explanatory variables for modeling
    # locs: the locations to use (NULL means all)
    # locVar: the name of the variable where locs can be found
    # otherVar: other variables to be kept, but not used in modeling
    # pred: predictor variable for modeling
    # seed: the random seed (NULL means no seed)
    # ntree: the number of trees to grow in the random forest
    # mtry: the splitting parameter for the random forest (NULL means use all variables)
    # testSize: the fractional portion of data that should be used as the test dataset
    
    # Create locs if it has not been passed
    if (is.null(locs)) {
        
        # Pull all values of locVar
        locs <- tbl %>% pull(locVar)
        
        # If locVar is a factor, then use the levels of the factor, otherwise use the unique values
        if ("factor" %in% class(locs)) { locs <- levels(locs) }
        else { locs <- locs %>% unique() %>% sort() }
        
        # Print the levels that it will run for
        cat("\nRunning for locations:\n")
        print(locs)
    }
    
    # Pass to rfTwoLocales
    rfOut <- rfTwoLocales(tbl, 
                          loc1=locs, 
                          loc2=c(), 
                          locVar=locVar,
                          otherVar=otherVar, 
                          vrbls=vrbls, 
                          pred=pred, 
                          seed=seed, 
                          ntree=ntree, 
                          mtry=mtry, 
                          testSize=testSize
    )
    
    # Return the list object
    rfOut
    
}


# Function to predict model on to df, then plot the outcomes
helperPredictPlot <- function(model, 
                              df, 
                              vrbls=NULL, 
                              origVar="locale", 
                              limitObs=500, 
                              predOrder=NULL, 
                              locMapper=NULL
                              ) {
    
    # FUNCTION ARGUMENTS:
    # model: a trained model
    # df: a data frame or tibble for the model to be predicted on to
    # vrbls: the variables needed by model (records with NA will be deleted)
    # origVar: the original (truth) variable from df
    # limitObs: the minimum number of observations for a locale to be plotted
    # predOrder: ordering of factor variable "predicted" (NULL means leave as-is)
    # locMapper: mapping file for locations (will order locale to match predicted if not NULL)
    
    # Get variable names if NULL, assuming random forest
    if (is.null(vrbls)) {
        vrbls <- model$importance %>% rownames()
    }
    
    # Filter df to have only complete cases for vrbls
    dfPred <- df %>%
        filter_at(vars(all_of(vrbls)), all_vars(!is.na(.)))
    
    # Make predictions
    dfPred <- dfPred %>%
        mutate(predicted=predict(model, newdata=select_at(dfPred, vars(all_of(vrbls)))))
    
    # Summarize predictions
    dfAccuracy <- dfPred %>%
        group_by_at(vars(all_of(c(origVar, "predicted")))) %>%
        summarize(n=n()) %>%
        mutate(pct=n/sum(n)) %>%
        ungroup()
    
    # Filter dfAccuracy to only origVar with count of at least limitObs
    hasEnough <- dfAccuracy %>%
        group_by_at(vars(all_of(origVar))) %>%
        summarize(n=sum(n)) %>%
        filter(n > limitObs) %>%
        pull(origVar)
    
    # Create the proper order for the factor variable 'predicted' if requested
    if (!is.null(predOrder)) {
        dfAccuracy <- dfAccuracy %>%
            mutate(predicted=factor(predicted, levels=predOrder))
    }
    
    # Create the base locale mapping
    locales <- dfAccuracy %>%
        pull(origVar) %>%
        unique()
    
    # Modify the locale mapping if a mapper is provided
    if (!is.null(locMapper) & !is.null(predOrder)) {
        localeStrip <- str_replace(locales, pattern=" .\\d{4}.", replacement="")
        locales <- locales[order(match(locMapper[localeStrip], predOrder))]
    }
    
    # Create plot for which locales are classified as each other
    p1 <- dfAccuracy %>%
        filter_at(vars(all_of(origVar)), all_vars(. %in% hasEnough)) %>%
        mutate(locale=factor(get(origVar), levels=locales)) %>%
        ggplot(aes(y=locale, x=predicted)) +
        geom_tile(aes(fill=pct)) +
        geom_text(aes(label=paste0(round(100*pct), "%"))) +
        scale_fill_continuous("% Predicted As", low="white", high="green") +
        scale_x_discrete(position="top") +
        theme(axis.text.x=element_text(angle=90)) +
        labs(x="",
             y="Actual Locale",
             title="Predicted Locale vs. Actual Locale"
        )
    print(p1)
    
    # Return the predictions summary
    dfAccuracy
    
}


helperPlotVarImp <- function(model, titleAdd="", mapper=varMapper) {
    
    p1 <- model %>%
        caret::varImp() %>%
        rownames_to_column("predictor") %>%
        ggplot(aes(x=fct_reorder(paste0(predictor, "\n", varMapper[predictor]), Overall), y=Overall)) + 
        geom_col(fill="lightblue") + 
        labs(x="", y="", title=paste0("Variable Importance", titleAdd)) + 
        coord_flip()
    print(p1)
    
}


assessPredictionCertainty <- function(lst, 
                                      plotCaption, 
                                      keyVar="locale",
                                      testData=lst[["testData"]],
                                      listModel="rfModel", 
                                      pkVars=c("dtime"), 
                                      thresh=0.8, 
                                      showHists=FALSE, 
                                      showAcc=FALSE
                                      ) {
    
    # FUNCTION ARGUMENTS:
    # lst: the list containing outputs of the modeling
    # plotCaption: description of predictors used
    # keyVar: the variable that represents truth when assessing predictions
    # testData: the test dataset (default is the 'testData element of list lst)
    # listModel: the named element of lst containing the random forest model
    # pkVars: the variable(s) of listData that, with keyVar, function as a primary key (unique identifier)
    # thresh: threshhold for voting confidence
    # showHists: boolean, whether to create histograms for confidence
    # showAcc: boolean, for whether to show accuracy by percent of votes 
    #          (only makes sense when testData and listModel have the same levels for keyVar)
    
    # Get variable names for the random forest
    vrbls <- lst[[listModel]]$importance %>% rownames()
    
    # Filter testData to have only complete cases for vrbls
    dfPred <- testData %>%
        filter_at(vars(all_of(vrbls)), all_vars(!is.na(.))) %>%
        select_at(vars(all_of(c(keyVar, pkVars, vrbls))))
    
    # Create voting summary for every element in testData
    predSummary <- predict(lst[[listModel]], newdata=dfPred, type="prob") %>%
        as.data.frame() %>%
        tibble::as_tibble() %>%
        bind_cols(select_at(dfPred, vars(all_of(c(keyVar, pkVars))))) %>%
        mutate(finalPrediction=predict(lst[[listModel]], newdata=dfPred)) %>%
        pivot_longer(cols=-c(keyVar, pkVars, "finalPrediction"), 
                     names_to="prediction", 
                     values_to="pctVotes"
        )
    
    # Create summary of maximum prediction
    allPredictions <- predSummary %>%
        group_by_at(vars(all_of(c(keyVar, pkVars)))) %>%
        mutate(maxPct=max(pctVotes), topProb=(pctVotes==maxPct)) %>%
        ungroup()
    
    
    # Create and show histograms if requested
    if (showHists) {
        
        # Create plot for confidence level by final prediction
        p1 <- allPredictions %>%
            filter(finalPrediction==prediction) %>%
            ggplot(aes(x=pctVotes, y=..count../sum(..count..))) + 
            geom_histogram() + 
            labs(title="Percent of Votes Received When Truth Is:", 
                 y="Frequency of Result", 
                 x="Percent of Votes Received"
            ) +
            facet_wrap(~get(keyVar))
        print(p1)
        
        # Create plot for confidence level by final prediction
        p2 <- allPredictions %>%
            filter(finalPrediction==prediction) %>%
            ggplot(aes(x=pctVotes, y=..count../sum(..count..))) + 
            geom_histogram() + 
            labs(title="Percent of Votes Received When Final Prediction Is:", 
                 y="Frequency of Result", 
                 x="Percent of Votes Received"
            ) +
            facet_wrap(~finalPrediction)
        print(p2)
    }
    
    
    # Create plot of total votes by type
    p3 <- allPredictions %>%
        group_by_at(vars(all_of(c(keyVar, "prediction")))) %>%
        summarize(meanVotes=mean(pctVotes)) %>%
        ggplot(aes_string(x="prediction", y=keyVar)) + 
        geom_tile(aes(fill=meanVotes)) + 
        geom_text(aes(label=paste0(round(100*meanVotes), "%"))) + 
        scale_fill_continuous("% Predicted As", low="white", high="green", limits=c(0, 1)) + 
        scale_x_discrete(position="top") +
        # theme(axis.text.x=element_text(angle=90)) + 
        labs(x="",
             y="Actual Locale", 
             title="Predicted Locale vs. Actual Locale", 
             subtitle="Average votes received by prediction",
             caption=paste0(plotCaption, " as predictors")
        )
    print(p3)
    
    
    # Create plot of accuracy when threshhold is met
    if (showAcc) {
        
        # Percent accuracy when threshold is met
        p4 <- allPredictions %>%
            filter(finalPrediction==prediction) %>%
            mutate(correct=(get(keyVar)==finalPrediction), 
                   meetThresh=factor(ifelse(pctVotes >= thresh, "Threshold Met", "Threshold Not Met"), 
                                     levels=c("Threshold Not Met", "Threshold Met")
                   )
            ) %>%
            group_by_at(c(keyVar, "meetThresh")) %>%
            summarize(pctCorrect=mean(correct)) %>%
            ggplot(aes_string(x=keyVar)) + 
            geom_point(aes(y=pctCorrect)) + 
            geom_text(aes(y=pctCorrect+0.08, label=paste0(round(100*pctCorrect), "%"))) +
            coord_flip() +
            ylim(c(0, 1.08)) + 
            facet_wrap(~meetThresh) +
            labs(x="Truth", 
                 y="Percent Correctly Classified", 
                 title="Accuracy of Classifying", 
                 subtitle=paste0("Threshold Met means percent of votes is >= ", round(100*thresh), "%"), 
                 caption=paste0(plotCaption, " as predictors")
            )
        print(p4)
        
    }
    
    
    # Create plot of total votes by threshold
    p6 <- allPredictions %>%
        filter(finalPrediction==prediction) %>%
        mutate(modPredict=ifelse(pctVotes>=thresh, prediction, "Too Low")) %>%
        group_by_at(vars(all_of(c(keyVar, "modPredict")))) %>%
        summarize(n=n()) %>%
        mutate(pct=n/sum(n)) %>%
        ggplot(aes_string(x="modPredict", y=keyVar)) + 
        geom_tile(aes(fill=pct)) + 
        geom_text(aes(label=paste0(round(100*pct), "%"))) + 
        scale_fill_continuous("% Predicted As", low="white", high="lightblue", limits=c(0, 1)) + 
        scale_x_discrete(position="top") +
        # theme(axis.text.x=element_text(angle=90)) + 
        labs(x="",
             y="Actual Locale", 
             title="Predicted Locale vs. Actual Locale", 
             subtitle=paste0("Predictions with <", round(100*thresh), "% of votes classified as 'Too Low'"),
             caption=paste0(plotCaption, " as predictors")
        )
    print(p6)
    
    # Return the accuracy object
    allPredictions
    
}



# Repeat the process until all variables have been added
# 1. Run forests with the pre-existing variables plus each potential new variable
# 2. Assess the accuracy on OOB and the archetypes
# 3. Add the best new predictor to the pre-existing variables list

# STEP 1: Helper function to run forests for pre-existing variables plus one new variable
helperRFCombinations <- function(possVars, 
                                 df,
                                 prevVars=c(), 
                                 predVarName="city",
                                 ntree=25,
                                 seed=NULL, 
                                 maxmtry=4
                                 ) {
    
    # FUNCTION ARGUMENTS:
    # possVars: possible variables to be considered
    # df: the data frame or tibble containing the data
    # prevVars: the previous variables run
    # predVarName: the name of the variable being predicted
    # ntree: the number of trees to run in the forest
    # seed: the seed to use (NULL means none, can be passed through to next function)
    # maxmtry: the maximum value for mtry
    
    
    # Define the variables to be checked
    possVars <- possVars[!(possVars %in% prevVars)]
    cat("\nWill run for:", paste(possVars, collapse=" "))
    cat("\nFixed variable(s) will be:", paste(prevVars, collapse=" "), "\n")
    
    # Create a container for the accuracy data
    accContainer <- vector("list", length=length(possVars))
    
    # Run loop to get all results
    n <- 1
    for (vrbl in possVars) {
        
        # Create the variables to pass
        vrbls <- c(prevVars, vrbl)
        
        # Run random forest for 2015-2017 data
        accContainer[[n]] <- rfMultiLocale(df, 
                                           vrbls=vrbls,
                                           locs=NULL, 
                                           locVar=predVarName,
                                           pred=predVarName,
                                           ntree=ntree, 
                                           seed=seed, 
                                           mtry=min(maxmtry, length(vrbls))
        )
        
        # Increment n
        n <- n + 1
        
    }
    
    # Return the accuracy container
    accContainer
    
}


# Helper function to extract data from rfContainer
helperExtractAccuracy <- function(x, elem="errorRate") {
    tibble::tibble(locale=names(x[[elem]]), 
                   accuracy=1-x[[elem]]
    )
}


# STEP 2: Assess accuracy on OOB and archetype locales
helperVariableAccuracy <- function(lst, 
                                   possVars,
                                   elem="errorRate", 
                                   prevVars=c()
                                   ) {
    
    # FUNCTION ARGUMENTS:
    # lst: the list containing the random forest runs
    # possVars: the full set of possible variables being considered
    # elem: the element of the list containing the error rates
    # prevVars: the subset of possible variables that have already been included
    
    # Filter possVars to exclude variables already considered
    possVars <- possVars[!(possVars %in% prevVars)]
    
    # Get the accuracy data
    tmp <- map_dfr(lst, .f=helperExtractAccuracy, .id="vrblNum") %>%
        mutate(vrblName=possVars[as.integer(vrblNum)], 
               locale=factor(locale, levels=c("OOB", unique(locale)[unique(locale) != "OOB"]))
        )
    
    # Get the variable order
    varOrder <- tmp %>%
        filter(locale=="OOB") %>%
        arrange(accuracy) %>%
        pull(vrblName)
    
    # Plot the variable impacts on accuracy
    p1 <- ggplot(tmp, aes(x=factor(vrblName, levels=varOrder))) + 
        geom_point(aes(y=accuracy)) + 
        geom_text(aes(y=accuracy + 0.1, label=paste0(round(100*accuracy), "%"))) + 
        ylim(c(0, 1.1)) +
        facet_wrap(~locale) + 
        labs(title="Accuracy by Next Variable Added", 
             y="Overall Accuracy", 
             x="", 
             subtitle=paste0("Variables previously added:", paste(prevVars, collapse=" "))
        ) +
        coord_flip()
    print(p1)
    
    tmp
    
}


# Get the accuracy data from a test dataset
getAccuracy <- function(x, keyVar="locale") { 
    
    x %>% 
        mutate(locale=get(keyVar)) %>%
        count(locale, predicted, correct) %>% 
        group_by(locale) %>% 
        mutate(pct=n/sum(n)) %>% 
        ungroup() %>% 
        filter(locale==predicted) 

}


# Function to plot the accuracy by locale of two test datasets
deltaAccuracy <- function(prevObject, currObject, listItem="testData") {
    
    # FUNCTION ARGUMENTS:
    # prevObject: the previous object, either the test data frame, or a list
    # currObject: the current object, either the test data frame, or a list
    # listItem: the item to extract from the list if passed
    
    # Extract the data from the list if the object passed is a list
    if ("list" %in% class(prevObject)) { prevObject <- prevObject[[listItem]] }
    if ("list" %in% class(currObject)) { currObject <- currObject[[listItem]] }
    
    # Get the change in prediction accuracy
    p1 <- list(prev=prevObject, curr=currObject) %>% 
        map_dfr(.f=getAccuracy, .id="source") %>% 
        group_by(locale) %>% 
        mutate(maxPct=max(pct)) %>% 
        ungroup() %>% 
        ggplot(aes(x=fct_reorder(locale, maxPct), y=pct)) + 
        geom_point(aes(color=source)) + 
        geom_text(aes(y=pct+0.05*(2*(pct==maxPct)-1), label=paste0(round(100*pct), "%"))) +
        coord_flip() + 
        ylim(c(0, 1)) + 
        labs(x="", 
             y="Correctly Predicted", 
             title="Accuracy of Locale Predictions", 
             subtitle="(positive detection rate by locale)"
        ) + 
        scale_color_manual("Model", 
                           values=c("prev"="red", "curr"="darkgreen"), 
                           labels=c("prev"="previous", "curr"="current")
        )
    print(p1)
    
}


localeByArchetype <- function(rfList, 
                              fullData, 
                              archeCities, 
                              targetYear=c(2016), 
                              keyVar="locale",
                              sortDescMatch=FALSE
                              ) {
    
    # FUNCTION ARGUMENTS:
    # rfList: the list containing the random forest model and test data
    # fullData: the full data prior to filtering by archetype
    # archeCities: the list of cities/years included in the random forest modeling
    # targetYear: the years that fullData should be filtered for
    # keyVar: the key variable of interest in fullData
    # sortDescMatch: boolean, whether to sort by descending highest match percentage
    
    # Get the test dataset from the rf model
    archeTestData <- rfList[["testData"]] %>%
        select(-predicted, -correct)
    
    # Get the data that are not the archetypes, but are still in targetYear
    archeNotData <- fullData %>%
        filter(!(source %in% archeCities), year %in% targetYear) %>%
        mutate(hr=lubridate::hour(dtime)) %>%
        select_at(vars(all_of(names(archeTestData)))) %>%
        filter_all(all_vars(!is.na(.)))
    
    # Combine the data and make prediction using random forest
    fullTestData <- archeTestData %>%
        bind_rows(archeNotData) %>%
        mutate(actual=get(keyVar), 
               predicted=predict(rfList$rfModel, newdata=.)
        )
    
    # Plot the predictions by archetype
    p1 <- fullTestData %>%
        count(actual, predicted) %>%
        group_by(actual) %>%
        mutate(pct=n/sum(n), predicted=str_replace(predicted, ",.*", ""), maxPct=max(pct)) %>%
        ungroup() %>%
        ggplot(aes(x=predicted, y=if(sortDescMatch) fct_reorder(actual, maxPct) else actual)) + 
        geom_tile(aes(fill=pct)) +
        geom_text(aes(label=paste0(round(100*pct), "%"))) + 
        labs(x="", 
             y="", 
             title="Predicted Archetype by Locale", 
             subtitle=if(sortDescMatch) "Sorted by maximum match percentage" else "",
             caption=""
        ) + 
        theme(axis.text.x=element_text(angle=90)) + 
        scale_x_discrete(position="top") + 
        scale_fill_continuous(low="white", high="green")
    print(p1)
    
}


# Function for plotting error evolution by number of iterations
errorEvolution <- function(mdl, 
                           modelName="rfModel", 
                           errData="err.rate",
                           useCategory=NULL, 
                           oobEvery=50, 
                           subT=""
                           ) {
    
    # FUNCTION ARGUMENTS:
    # mdl: the model, either as a list with a sub-element, or directly
    # modelName: if mdl is a list, the name of the element containing the model
    # errData: the location of the error data in the model
    # useCategory: the categories for error plotting (NULL means all)
    # oobEvery: show OOB error for every increment of this amount
    # subT: the subtitle for the plot
    
    # If the model has been passed as a list, extract the model element
    if ("list" %in% class(mdl)) { mdl <- mdl[[modelName]] }
    
    # Extract the error rate data
    errRate <- mdl[[errData]]
    
    # Convert to data.frame and then to tibble
    tblError <- as.data.frame(errRate) %>%
        setNames(colnames(errRate)) %>%
        tibble::as_tibble() %>%
        mutate(ntree=1:n()) %>%
        pivot_longer(-ntree, names_to="Category", values_to="Error")
    
    # Filter if useCateogry has been passed
    if (!is.null(useCategory)) {
        tblError <- tblError %>%
            filter(as.character(Category) %in% as.character(useCategory))
    }
    
    # Create a plot of the error evolution
    p1 <- tblError %>%
        ggplot(aes(x=ntree, y=Error)) + 
        geom_line(aes(color=Category)) + 
        labs(x="# Trees", 
             y="Error Rate", 
             title="Error Rate vs. Number of Trees", 
             subtitle=subT
        ) + 
        ylim(c(0, NA)) + 
        geom_text(data=~filter(., Category=="OOB", (ntree==1 | (ntree %% oobEvery)==0)), 
                  aes(x=ntree, y=1.1*max(Error), label=paste0("OOB\n", round(100*Error, 1), "%"))
        )
    print(p1)
    
    # Return the error object
    tblError
    
}


# Create mapping file of sources to archetypes
createLocMapper <- function(x, valVar="locType", nameVar="source") {
    
    # FUNCTION ARGUMENTS - creates named vector where 'nameVar' elements will be 1:1 to valVar elements
    # x: the tibble or data frame containing the data
    # valVar: the variable that will be mapped to
    # nameVar: the variable that will be mapped from
    
    # Create locMapper
    mapper <- x %>% pull(valVar)
    names(mapper) <- x %>% pull(nameVar)
    
    # Return the mapper
    mapper
    
}


# Apply the mapper to a tibble
applyLocMapper <- function(tbl, 
                           mapper, 
                           keepAsIs="Same", 
                           excludeAll="Exclude", 
                           yearsUse=2016, 
                           sourceVar="source", 
                           localeVar="locale"
) {
    
    # FUNCTION ARGUMENTS
    # tbl: the input tibble or data frame
    # mapper: the mapping file
    # keepAsIs: the mapping file item(s) that should not be mapped
    # excludeAll: the mapping file item(s) that should be discarded
    # yearsUse: the years to be used for the analysis
    # sourceVar: the source variable in tbl that mapper will be applied to
    # localeVar: the locale variable in tbl that should be used when mapper results is "Same"
    
    # Create the data file with locType
    locData <- tbl %>% 
        mutate(locType=ifelse(mapper[get(sourceVar)] %in% keepAsIs, get(localeVar), mapper[get(sourceVar)]), 
               hr=lubridate::hour(dtime)
        ) %>% 
        filter(year %in% yearsUse, !(locType %in% excludeAll))
    
    # Counts by locType
    locData %>%
        count(locType) %>%
        arrange(-n) %>%
        as.data.frame() %>%
        print()
    
    # Return the data file
    locData
    
}


# Create a file with the same number of observations per 'locVar'
createBalancedSample <- function(tbl, seed=NULL, locVar="locType", summaryVar="locale") {
    
    # FUNCTION ARGUMENTS:
    # tbl: the tibble or data frame
    # seed: the random seed to be used (NULL means no seed)
    # locVar: the variable that will have equal counts across the resulting sample
    # summaryVar: the variable for showing counts in the final small data
    
    # Set the seed unless it has been passed as NULL
    if (!is.null(seed)) {
        set.seed(seed)
    }
    
    # Find the smallest locale type
    nSmall <- tbl %>%
        group_by_at(vars(all_of(locVar))) %>%
        summarize(n=n()) %>%
        pull(n) %>%
        min()
    
    # Create the relevant data subset
    smallData <- tbl %>%
        group_by_at(vars(all_of(locVar))) %>%
        sample_n(size=nSmall, replace=FALSE) %>%
        ungroup()
    
    # Sumarize the data subset
    smallData %>% 
        group_by_at(vars(all_of(summaryVar))) %>%
        summarize(n=n()) %>%
        arrange(-n) %>%
        as.data.frame() %>%
        print()
    
    # Return the data
    smallData
    
}


# Function for running random forest regressions
rfRegression <- function(df, 
                         depVar, 
                         predVars, 
                         critFilter=vector("list", 0),
                         locVar="locale",
                         otherVar=c("source", "dtime"),
                         seed=NULL, 
                         ntree=100, 
                         mtry=NULL, 
                         testSize=0.3
                         ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble
    # depVar: the dependent variable for the modeling
    # predVars: explanatory variables for modeling
    # critFilter: named list, of format name=values, where filtering will be (get(name) %in% values)
    # locVar: locale variable (for consistency with rfTwoLocales)
    # otherVar: other variables to be kept, but not used in modeling
    # seed: the random seed (NULL means no seed)
    # ntree: the number of trees to grow in the random forest
    # mtry: the splitting parameter for the random forest (NULL means use all variables)
    # testSize: the fractional portion of data that should be used as the test dataset
    
    # Filter df appropriately
    # Filter for only non-NA data across all of depVar, predVars, otherVar, names(critFilter) are included
    subDF <- df %>%
        filter_at(vars(all_of(c(depVar, predVars, otherVar, names(critFilter)))), all_vars(!is.na(.)))
    
    # Filter such that only matches to critFilter are included
    for (xNum in seq_len(length(critFilter))) {
        subDF <- subDF %>%
            filter_at(vars(all_of(names(critFilter)[xNum])), ~. %in% critFilter[[xNum]])
    }
    
    # All locales should be used since the filtering is run above
    locs <- subDF %>% 
        pull(locVar) %>% 
        unique() %>% 
        sort()
    
    # Pass to rfTwoLocales
    rfOut <- rfTwoLocales(subDF, 
                          loc1=locs, 
                          loc2=c(), 
                          locVar=locVar,
                          otherVar=otherVar, 
                          vrbls=predVars, 
                          pred=depVar, 
                          seed=seed, 
                          ntree=ntree, 
                          mtry=mtry, 
                          testSize=testSize, 
                          classify=FALSE
    )
    
    # Return the list object with rsq and mse attached
    # Note that err.rate is returned by rfTwoLocales as NULL since it does not exist for regression
    rfOut %>%
        append(list(rsq=rfOut$rfModel$rsq, mse=rfOut$rfModel$mse))
    
}

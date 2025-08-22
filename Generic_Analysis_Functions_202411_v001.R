
# Run a simple random forest using ranger::ranger
runSimpleRF <- function(df, yVar, xVars=NULL, ...) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame containing observations
    # yVar: variable to be predicted (numeric for regression, categorical for classification)
    # xVars: predictor variables (NULL means everything in df except for yVar)
    # ...: other arguments passed to ranger::ranger
    
    # Create xVars if passed as NULL
    if(is.null(xVars)) xVars <- setdiff(names(df), yVar)
    
    # Simple random forest model
    ranger::ranger(as.formula(paste0(yVar, "~", paste0(xVars, collapse="+"))), 
                   data=df[, c(yVar, xVars)], 
                   ...
    )
    
}


# Plot variables importances from a rando forest ranger model
plotRFImportance <- function(rf, 
                             impName="variable.importance", 
                             divBy=1000, 
                             plotTitle=NULL, 
                             plotData=TRUE, 
                             returnData=!isTRUE(plotData)
                             ) {
    
    # FUNCTION ARGUMENTS:
    # rf: output list from random forest with an element for importance
    # impName: name of the element to extract from rf
    # divBy: divisor for the importance variable
    # plotTitle: title for plot (NULL means use default)
    # plotData: boolean, should the importance plot be created and printed?
    # returnData: boolean, should the processed data be returned?
    
    # Create title if not provided
    if(is.null(plotTitle)) plotTitle <- "Importance for simple random forest"
    
    # Create y-axis label
    yAxisLabel="Variable Importance"
    if(!isTRUE(all.equal(divBy, 1))) yAxisLabel <- paste0(yAxisLabel, " (", divBy, "s)")
    
    # Create variable importance
    df <- rf[[impName]] %>% 
        as.data.frame() %>% 
        purrr::set_names("imp") %>% 
        rownames_to_column("metric") %>% 
        tibble::as_tibble() 
    
    # Create and print plot if requested
    if(isTRUE(plotData)) {
        p1 <- df %>%
            ggplot(aes(x=fct_reorder(metric, imp), y=imp/divBy)) + 
            geom_col(fill="lightblue") + 
            labs(x=NULL, y=yAxisLabel, title=plotTitle) +
            coord_flip()
        print(p1)
    }
    
    # Return data if requested
    if(isTRUE(returnData)) return(df)
    
}


# Return predictions given a random forest ranger model and test dataset
predictRF <- function(rf, df, newCol="pred", predsOnly=FALSE) {
    
    # FUNCTION ARGUMENTS:
    # rf: a trained random forest model
    # df: data frame for adding predictions
    # newCol: name for new column to be added to df
    # predsOnly: boolean, should only the vector of predictions be returned?
    #            if FALSE, a column named newCol is added to df, with df returned
    
    # Performance on holdout data
    preds <- predict(rf, data=df)$predictions
    
    # Return just the predictions if requested otherwise add as final column to df
    if(isTRUE(predsOnly)) return(preds)
    else {
        df[newCol] <- preds
        return(df)
    }
    
}


# Report accuracy or R-squared for a file containing truth and predicted
# Update for continuous variables
reportAccuracy <- function(df, 
                           trueCol, 
                           predCol="pred", 
                           reportAcc=TRUE, 
                           rndReport=2, 
                           useLabel="requested data",
                           returnAcc=!isTRUE(reportAcc), 
                           reportR2=FALSE
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame containing actual and predictions
    # trueCol: column containing true value
    # predCol: column containing predicted value
    # reportAcc: boolean, should accuracy be reported (printed to output)?
    # rndReport: number of significant digits for reporting (will be converted to percentage first)
    # useLabel: label for data to be used in reporting
    # returnAcc: boolean, should the accuracy be returned 
    #            return value is not converted to percentage, not rounded
    # reportR2: boolean, should accuracy be calculated as R-squared?
    #           (default FALSE measures as categorical)
    
    # Continuous or categorical reporting
    if(isTRUE(reportR2)) {
        tc <- df %>% pull(get(trueCol))
        pc <- df %>% pull(get(predCol))
        mseNull <- mean((tc-mean(tc))**2)
        msePred <- mean((tc-pc)**2)
        r2 <- 1 - msePred/mseNull
        if(isTRUE(reportAcc)) 
            cat("\nR-squared of ", 
                useLabel, 
                " is: ", 
                round(100*r2, rndReport), 
                "% (RMSE ",
                round(sqrt(msePred), 2), 
                " vs. ", 
                round(sqrt(mseNull), 2),
                " null)\n", 
                sep=""
            )
        acc <- c("mseNull"=mseNull, "msePred"=msePred, "r2"=r2)
    } else {
        acc <- mean(df[trueCol]==df[predCol])
        if(isTRUE(reportAcc)) 
            cat("\nAccuracy of ", useLabel, " is: ", round(100*acc, rndReport), "%\n", sep="")    
    }
    
    # Return accuracy statistic if requested
    if(isTRUE(returnAcc)) return(acc)
    
}


# Create the confusion matrix for a file containing truth and predicted
plotConfusion <- function(df, 
                          trueCol, 
                          predCol="pred", 
                          useTitle=NULL,
                          useSub=NULL, 
                          plotCont=FALSE, 
                          rndTo=NULL,
                          rndBucketsAuto=100,
                          nSig=NULL,
                          refXY=FALSE
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame containing actual and predictions
    # trueCol: column containing true value
    # predCol: column containing predicted value
    # useTitle: title to be used for chart (NULL means create from trueCol)
    # useSub: subtitle to be used for chart (NULL means none)
    # plotCont: boolean, should plotting assume continuous variables?
    #           (default FALSE assumes confusion plot for categorical variables)
    # rndTo: every number in x should be rounded to the nearest rndTo
    #        NULL means no rounding (default)
    #        -1L means make an estimate based on data
    # rndBucketsAuto: integer, if rndTo is -1L, about how many buckets are desired for predictions?
    # nSig: number of significant digits for automatically calculated rounding parameter
    #       (NULL means calculate exactly)
    # refXY: boolean, should a reference line for y=x be included? (relevant only for continuous)
    
    # Create title if not supplied
    if(is.null(useTitle)) useTitle <- paste0("Predicting ", trueCol)
    
    # Function auto-round returns vector as-is when rndTo is NULL and auto-rounds when rndTo is -1L
    df <- df %>%
        mutate(across(all_of(c(trueCol, predCol)), 
                      .fns=function(x) autoRound(x, rndTo=rndTo, rndBucketsAuto=rndBucketsAuto, nSig=nSig)
        )
        )
    
    # Create base plot (applicable to categorical or continuous variables)
    # Use x as true and y as predicted, for more meaningful geom_smooth() if continuous
    # Flip coordinates if categorical
    p1 <- df %>%
        group_by(across(all_of(c(trueCol, predCol)))) %>%
        summarize(n=n(), .groups="drop") %>%
        ggplot(aes(y=get(predCol), x=get(trueCol))) + 
        labs(y="Predicted", x="Actual", title=useTitle, subtitle=useSub)
    
    # Update plot as appropriate
    if(isTRUE(plotCont)) {
        p1 <- p1 +
            geom_point(aes(size=n), alpha=0.5) + 
            scale_size_continuous("# Obs") +
            geom_smooth(aes(weight=n), method="lm")
        if(isTRUE(refXY)) p1 <- p1 + geom_abline(slope=1, intercept=0, lty=2, color="red")
    } else {
        p1 <- p1 + 
            geom_tile(aes(fill=n)) + 
            geom_text(aes(label=n), size=2.5) +
            coord_flip() +
            scale_fill_continuous("", low="white", high="green")
    }
    
    # Output plot
    print(p1)
    
}


# Run random forest process including modeling, predictions on test data, and reporting on model quality
runFullRF <- function(dfTrain, 
                      yVar, 
                      xVars, 
                      useExistingRF=NULL,
                      dfTest=dfTrain,
                      useLabel="test data",
                      useSub=NULL, 
                      isContVar=FALSE,
                      rndTo=NULL,
                      rndBucketsAuto=100,
                      nSig=NULL,
                      refXY=FALSE,
                      makePlots=TRUE,
                      plotImp=makePlots,
                      plotConf=makePlots,
                      returnData=FALSE, 
                      ...
                      ) {
    
    # FUNCTION ARGUMENTS:
    # dfTrain: training data
    # yVar: dependent variable
    # xVars: column(s) containing independent variables
    # useExistingRF: an existing RF model, meaning only steps 3-5 are run (default NULL means run all steps)
    # dfTest: test dataset for applying predictions
    # useLabel: label to be used for reporting accuracy
    # useSub: subtitle to be used for confusion chart (NULL means none)
    # isContVar: boolean, is the variable continuous? (default FALSE means categorical)
    # rndTo: every number in x should be rounded to the nearest rndTo
    #        NULL means no rounding (default)
    #        -1L means make an estimate based on data
    # rndBucketsAuto: integer, if rndTo is -1L, about how many buckets are desired for predictions?
    # nSig: number of significant digits for automatically calculated rounding parameter
    #       (NULL means calculate exactly)    
    # refXY: boolean, should a reference line for y=x be included? (relevant only for continuous)
    # makePlots: boolean, should plots be created for variable importance and confusion matrix?
    # plotImp: boolean, should variable importance be plotted? (default is makePlots)
    # plotConf: boolean, should confusion matrix be plotted? (default is makePlots)
    # returnData: boolean, should data be returned?
    # ...: additional parameters to pass to runSimpleRF(), which are then passed to ranger::ranger()
    
    # Create the RF and plot importances, unless an RF is passed
    if(is.null(useExistingRF)) {
        # 1. Run random forest using impurity for importance
        rf <- runSimpleRF(df=dfTrain, yVar=yVar, xVars=xVars, importance="impurity", ...)
        
        # 2. Create, and optionally plot, variable importance
        rfImp <- plotRFImportance(rf, plotData=plotImp, returnData=TRUE)
    }
    else {
        rf <- useExistingRF
        rfImp <- NA
    }
    
    # 3. Predict on test dataset
    tstPred <- predictRF(rf=rf, df=dfTest)
    
    # 4. Report on accuracy (updated for continuous or categorical)
    rfAcc <- reportAccuracy(tstPred, 
                            trueCol=yVar, 
                            rndReport=3, 
                            useLabel=useLabel, 
                            reportR2=isTRUE(isContVar),
                            returnAcc=TRUE
    )
    
    # 5. Plot confusion data (updated for continuous vs. categorical) if requested
    if(isTRUE(plotConf)) {
        plotConfusion(tstPred, 
                      trueCol=yVar, 
                      useSub=useSub, 
                      plotCont=isTRUE(isContVar), 
                      rndTo=rndTo, 
                      rndBucketsAuto=rndBucketsAuto,
                      nSig=nSig,
                      refXY=refXY
        )
    }
    
    #6. Return data if requested
    if(isTRUE(returnData)) return(list(rf=rf, rfImp=rfImp, tstPred=tstPred, rfAcc=rfAcc))
    
}


# Create a scatterplot with smooth for identifying Simpson's paradox
tmpSmoothPlot <- function(df, 
                          x, 
                          y, 
                          xRound=NULL, 
                          yRound=NULL,
                          xName=x, 
                          yName=y, 
                          printPlot=TRUE, 
                          returnPlot=!isTRUE(printPlot)
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame
    # x: x variable
    # y: y variable
    # {x,y}Round: rounding to apply to vector {x,y} using function autoRound()
    #             NULL means no rounding (default)
    #             -1L means make an estimate based on data (around 100 buckets created)
    #             a positive float or integer means round everything to the nearest multiple
    # xName: name to describe x variable
    # yName: name to describe y variable
    # printPlot: boolean, should plot be printed?
    # returnPlot: boolean, should plot object be returned?
    
    p1 <- df %>% 
        select(src, all_of(c(x, y))) %>%
        purrr::set_names(c("src", "x1", "y1")) %>%
        mutate(x1=autoRound(x1, rndTo=xRound), 
               y1=autoRound(y1, rndTo=yRound)
        ) %>%
        count(src, x1, y1) %>% 
        ggplot(aes(x=x1, y=y1)) + 
        geom_smooth(aes(weight=n, color=src), method="lm") + 
        geom_smooth(method="lm", lty=2, aes(weight=n), color="black") + 
        labs(title=paste0("Relationship between ", xName, " and ", yName), 
             subtitle="Dashed black line is overall relationship", 
             y=if(y!=yName) paste0(yName, "\n(", y, ")") else y,
             x=if(x!=xName) paste0(xName, "\n(", x, ")") else x
        ) + 
        scale_color_discrete(NULL)
    
    # Print plot if requested
    if(isTRUE(printPlot)) print(p1)
    
    # Return plot if requested
    if(isTRUE(returnPlot)) return(p1)
    
}


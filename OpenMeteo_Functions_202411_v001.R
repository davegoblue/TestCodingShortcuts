# File for functions used in OpenMeteoEDA_xxxxxx_vxxx.Rmd


# A. Mixed functions (partially specific to Open Meteo analysis)

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

# Update for automated rounding
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



# B. Weather-specific functions

# Create URL with specified parameters for downloading data from Open Meteo
openMeteoURLCreate <- function(mainURL="https://archive-api.open-meteo.com/v1/archive", 
                               lat=45, 
                               lon=-90, 
                               startDate=paste(year(Sys.Date())-1, "01", "01", sep="-"), 
                               endDate=paste(year(Sys.Date())-1, "12", "31", sep="-"), 
                               hourlyMetrics=NULL, 
                               dailyMetrics=NULL,
                               tz="GMT", 
                               ...
                               ) {
    
    # Create formatted string
    fString <- paste0(mainURL, 
                      "?latitude=", 
                      lat, 
                      "&longitude=", 
                      lon, 
                      "&start_date=", 
                      startDate, 
                      "&end_date=", 
                      endDate
    )
    if(!is.null(hourlyMetrics)) fString <- paste0(fString, "&hourly=", hourlyMetrics)
    if(!is.null(dailyMetrics)) fString <- paste0(fString, "&daily=", dailyMetrics)
    
    # Return the formatted string
    paste0(fString, "&timezone=", stringr::str_replace(tz, "/", "%2F"), ...)
    
}


# Helper function to simplify entry of parameters for Open Meteo download requests
helperOpenMeteoURL <- function(cityName=NULL,
                               lat=NULL,
                               lon=NULL,
                               hourlyMetrics=NULL,
                               hourlyIndices=NULL,
                               hourlyDesc=tblMetricsHourly,
                               dailyMetrics=NULL,
                               dailyIndices=NULL,
                               dailyDesc=tblMetricsDaily,
                               startDate=NULL, 
                               endDate=NULL, 
                               tz=NULL,
                               ...
                               ) {
    
    # Convert city to lat/lon if lat/lon are NULL
    if(is.null(lat) | is.null(lon)) {
        if(is.null(cityName)) stop("\nMust provide lat/lon or city name available in maps::us.cities\n")
        cityData <- maps::us.cities %>% tibble::as_tibble() %>% filter(name==cityName)
        if(nrow(cityData)!=1) stop("\nMust provide city name that maps uniquely to maps::us.cities$name\n")
        lat <- cityData$lat[1]
        lon <- cityData$long[1]
    }
    
    # Get hourly metrics by index if relevant
    if(is.null(hourlyMetrics) & !is.null(hourlyIndices)) {
        hourlyMetrics <- hourlyDesc %>% slice(hourlyIndices) %>% pull(metric)
        hourlyMetrics <- paste0(hourlyMetrics, collapse=",")
        cat("\nHourly metrics created from indices:", hourlyMetrics, "\n\n")
    }
    
    # Get daily metrics by index if relevant
    if(is.null(dailyMetrics) & !is.null(dailyIndices)) {
        dailyMetrics <- dailyDesc %>% slice(dailyIndices) %>% pull(metric)
        dailyMetrics <- paste0(dailyMetrics, collapse=",")
        cat("\nDaily metrics created from indices:", dailyMetrics, "\n\n")
    }
    
    # Use default values from OpenMeteoURLCreate() for startDate, endDate, and tz if passed as NULL
    if(is.null(startDate)) startDate <- eval(formals(openMeteoURLCreate)$startDate)
    if(is.null(endDate)) endDate <- eval(formals(openMeteoURLCreate)$endDate)
    if(is.null(tz)) tz <- eval(formals(openMeteoURLCreate)$tz)
    
    # Create and return URL
    openMeteoURLCreate(lat=lat,
                       lon=lon, 
                       startDate=startDate, 
                       endDate=endDate, 
                       hourlyMetrics=hourlyMetrics, 
                       dailyMetrics=dailyMetrics, 
                       tz=tz,
                       ...
    )
    
}


# Read JSON data returned from Open Meteo
readOpenMeteoJSON <- function(js, mapDaily=tblMetricsDaily, mapHourly=tblMetricsHourly) {
    
    # FUNCTION arguments: 
    # js: JSON list returned by download from Open-Meteo
    # mapDaily: mapping file for daily metrics
    # mapHourly: mapping file for hourly metrics
    
    # Get the object and names
    jsObj <- jsonlite::read_json(js, simplifyVector = TRUE)
    nms <- jsObj %>% names()
    cat("\nObjects in JSON include:", paste(nms, collapse=", "), "\n\n")
    
    # Set default objects as NULL
    tblDaily <- NULL
    tblHourly <- NULL
    tblUnitsDaily <- NULL
    tblUnitsHourly <- NULL
    
    # Get daily and hourly as tibble if relevant
    if("daily" %in% nms) tblDaily <- jsObj$daily %>% tibble::as_tibble() %>% omProcessDaily()
    if("hourly" %in% nms) tblHourly <- jsObj$hourly %>% tibble::as_tibble() %>% omProcessHourly()
    
    # Helper function for unit conversions
    helperMetricUnit <- function(x, mapper, desc=NULL) {
        if(is.null(desc)) 
            desc <- as.list(match.call())$x %>% 
                deparse() %>% 
                stringr::str_replace_all(pattern=".*\\$", replacement="")
        x %>% 
            tibble::as_tibble() %>% 
            pivot_longer(cols=everything()) %>% 
            left_join(mapper, by=c("name"="metric")) %>% 
            mutate(value=stringr::str_replace(value, "\u00b0", "deg ")) %>% 
            mutate(metricType=desc) %>% 
            select(metricType, everything())
    }
    
    # Get the unit descriptions
    if("daily_units" %in% nms) tblUnitsDaily <- helperMetricUnit(jsObj$daily_units, mapDaily)
    if("hourly_units" %in% nms) tblUnitsHourly <- helperMetricUnit(jsObj$hourly_units, mapHourly)
    if(is.null(tblUnitsDaily) & !is.null(tblUnitsHourly)) tblUnits <- tblUnitsHourly
    else if(!is.null(tblUnitsDaily) & is.null(tblUnitsHourly)) tblUnits <- tblUnitsDaily
    else if(!is.null(tblUnitsDaily) & !is.null(tblUnitsHourly)) 
        tblUnits <- bind_rows(tblUnitsHourly, tblUnitsDaily)
    else tblUnits <- NULL
    
    # Put everything else together
    tblDescription <- jsObj[setdiff(nms, c("hourly", "hourly_units", "daily", "daily_units"))] %>%
        tibble::as_tibble()
    
    # Return the list objects
    list(tblDaily=tblDaily, tblHourly=tblHourly, tblUnits=tblUnits, tblDescription=tblDescription)
    
}


# Return Open meteo metadata in prettified format
prettyOpenMeteoMeta <- function(df, extr="tblDescription") {
    if("list" %in% class(df)) df <- df[[extr]]
    for(name in names(df)) {
        cat("\n", name, ": ", df %>% pull(name), sep="")
    }
    cat("\n\n")
}


# Process Open Meteo daily data
omProcessDaily <- function(tbl, extr="tblDaily") {
    if("list" %in% class(tbl)) tbl <- tbl[[extr]]
    tbl %>% mutate(date=lubridate::ymd(time)) %>% select(date, everything())
}


# Process Open meteo hourly data
omProcessHourly <- function(tbl, extr="tblHourly") {
    if("list" %in% class(tbl)) tbl <- tbl[[extr]]
    tbl %>% 
        mutate(origTime=time, 
               time=lubridate::ymd_hm(time), 
               date=lubridate::date(time), 
               hour=lubridate::hour(time)
        ) %>% 
        select(time, date, hour, everything())
}


newCityPredict <- function(rf, 
                           dfTest, 
                           trueCol, 
                           isContVar=FALSE,
                           reportR2=isTRUE(isContVar), 
                           plotCont=isTRUE(isContVar), 
                           reportAcc=TRUE, 
                           rndReport=2, 
                           useLabel="requested data",
                           useTitle=NULL,
                           useSub=NULL, 
                           rndTo=NULL,
                           rndBucketsAuto=100,
                           nSig=NULL,
                           refXY=FALSE, 
                           returnData=TRUE
                           ) {
    
    # FUNCTION ARGUMENTS:
    # rf: The existing "ranger" model OR a list containing element "rf" that has the existing "ranger" model
    # dfTest: the new dataset for predictions
    # trueCol: column containing true value
    # isContVar: boolean, is the variable continuous? (default FALSE means categorical)
    # reportR2: boolean, should accuracy be calculated as R-squared?
    #           (FALSE measures as categorical)
    # plotCont: boolean, should plotting assume continuous variables?
    #           (FALSE assumes confusion plot for categorical variables)
    # reportAcc: boolean, should accuracy be reported (printed to output)?
    # rndReport: number of significant digits for reporting (will be converted to percentage first)
    # useLabel: label for data to be used in reporting
    # useTitle: title to be used for chart (NULL means create from trueCol)
    # useSub: subtitle to be used for chart (NULL means none)
    # rndTo: every number in x should be rounded to the nearest rndTo
    #        NULL means no rounding (default)
    #        -1L means make an estimate based on data
    # rndBucketsAuto: integer, if rndTo is -1L, about how many buckets are desired for predictions?
    # nSig: number of significant digits for automatically calculated rounding parameter
    #       (NULL means calculate exactly)
    # refXY: boolean, should a reference line for y=x be included? (relevant only for continuous)
    # returnData: boolean, should a list be returned containing tstPred and rfAcc?
    
    # Get the ranger data
    if(!("ranger" %in% class(rf))) {
        if(!("rf" %in% names(rf))) {
            stop("\nERROR: rf must be of class 'ranger' OR a list with element 'rf' that is of class 'ranger")
        }
        rf <- rf[["rf"]]
    }
    if(!("ranger" %in% class(rf)))
        stop("\nERROR: rf must be of class 'ranger' OR a list with element 'rf' that is of class 'ranger")
    
    # Predict on new dataset
    tstPred <- predictRF(rf=rf, df=dfTest)
    
    # Report on accuracy
    rfAcc <- reportAccuracy(tstPred, 
                            trueCol=trueCol, 
                            reportAcc=reportAcc,
                            rndReport=rndReport, 
                            useLabel=useLabel, 
                            reportR2=reportR2,
                            returnAcc=TRUE
    )
    
    # Plot confusion data
    plotConfusion(tstPred, 
                  trueCol=trueCol, 
                  useTitle=useTitle,
                  useSub=useSub, 
                  plotCont=plotCont, 
                  rndTo=rndTo,
                  rndBucketsAuto=rndBucketsAuto,
                  nSig=nSig,
                  refXY=refXY
    )
    
    # Return data if requested
    if(isTRUE(returnData)) return(list(tstPred=tstPred, rfAcc=rfAcc))
    
}


formatOpenMeteoJSON <- function(x, 
                                glimpseData=TRUE, 
                                addVars=FALSE, 
                                addExtract="tblHourly", 
                                showStats=addVars
                                ) {
    
    # FUNCTION ARGUMENTS:
    # x: Saved json file for passage to readOpenMeteoJSON
    # glimpseData: boolean, should a glimpse of the file and metadata be shown?
    # addVars: boolean, should variables be added for later processing?
    # addExtract: list elemented to be extracted (relevant only for addVars=TRUE)
    # showStats: boolean, should counts of key elements be shown (relevant only for addVars=TRUE)
    
    # Read file
    lst <- readOpenMeteoJSON(x)
    
    # Show a glimpse if requested
    if(isTRUE(glimpseData)) {
        print(lst)
        prettyOpenMeteoMeta(lst)
    }
    
    # If no variables to be added, return the file
    if(!isTRUE(addVars)) return(lst)
    
    # Add statistics
    df <- lst[[addExtract]] %>%
        mutate(year=year(date), 
               month=factor(month.abb[lubridate::month(date)], levels=month.abb), 
               hour=lubridate::hour(time), 
               fct_hour=factor(hour), 
               tod=ifelse(hour>=7 & hour<=18, "Day", "Night"), 
               doy=yday(date),
               season=case_when(month %in% c("Mar", "Apr", "May") ~ "Spring", 
                                month %in% c("Jun", "Jul", "Aug") ~ "Summer", 
                                month %in% c("Sep", "Oct", "Nov") ~ "Fall", 
                                month %in% c("Dec", "Jan", "Feb") ~ "Winter", 
                                TRUE~"typo"
               ), 
               todSeason=paste0(season, "-", tod), 
               tod=factor(tod, levels=c("Day", "Night")), 
               season=factor(season, levels=c("Spring", "Summer", "Fall", "Winter")), 
               todSeason=factor(todSeason, 
                                levels=paste0(rep(c("Spring", "Summer", "Fall", "Winter"), each=2), 
                                              "-", 
                                              c("Day", "Night")
                                )
               ),
               across(where(is.numeric), .fns=function(x) round(100*percent_rank(x)), .names="pct_{.col}")
        )
    
    # Show counts if requested
    if(isTRUE(showStats)) {
        # Glimpse file
        glimpse(df)
        # Counts of day-of-year/month
        p1 <- df %>% 
            count(doy, month) %>% 
            ggplot(aes(y=doy, x=month)) + 
            geom_boxplot(aes(weight=n), fill="lightblue") + 
            labs(title="Observations by day-of-year and month", x=NULL, y="Day of Year")
        print(p1)
        # Counts of year/month
        p2 <- df %>% 
            count(year, month) %>% 
            ggplot(aes(y=factor(year), x=month)) + 
            geom_tile(aes(fill=n)) + 
            geom_text(aes(label=n), size=3) + 
            scale_fill_continuous("# Records", low="white", high="green") + 
            labs(title="Records by year and month", x=NULL, y=NULL)
        print(p2)
        # Counts of todSeason-season-tod, hour-fct_hour-tod, and month-season
        df %>% count(todSeason, season, tod) %>% print()
        df %>% count(hour, fct_hour, tod) %>% print(n=30)
        df %>% count(month, season) %>% print()
    }
    
    # Return the file
    df
    
}


# Approximate formula for relative humidity
# Source https://www.omnicalculator.com/physics/relative-humidity
calcRH <- function(t, d, c1=17.63, c2=243) {
    100 * exp((c1*d)/(c2+d)) / exp((c1*t)/(c2+t))
}


# Approximate formula for vapor pressure deficit
# Source https://pulsegrow.com/blogs/learn/vpd
calcVPD <- function(t, d, c1=610.78, c2=17.2694, c3=237.3) {
    # SVP (saturation vapor pressure) = 610.78 * exp(T * 17.2694 / (T + 237.3))
    # VPD = (1 - RH/100) * SVP
    # Formula produces VPD in Pa, divide by 1000 to convert to kPa
    (1 - calcRH(t, d)/100) * c1 * exp(t * c2 / (t + c3)) / 1000
}


singleCityGLM <- function(dfTrain, 
                          dfTest=dfTrain, 
                          srcKey="NYC", 
                          plotPred=TRUE, 
                          printAcc=TRUE, 
                          returnGLM=TRUE
                          ) {
    
    # FUNCTION ARGUMENTS
    # dfTrain: training data
    # dfTest: test data
    # srcKey: cities that will be considered "positive" for GLM
    # plotPred: boolean, should boxplot of probability by actual city be plotted?
    # printAcc: boolean, should report of accuracy by test/train and actual city be reported?
    # returnGLM: boolean, should the GLM model be returned?
    
    tst <- dfTrain %>% 
        mutate(isKey=ifelse(src %in% srcKey, 1, 0)) %>%
        select(isKey, all_of(varsTrain)) %>% 
        select(-diffuse_radiation) %>% # perfectly correlated with other radiation variables
        glm(isKey ~ ., data=., family="binomial")
    
    if(isTRUE(plotPred)) {
        p1 <- dfTest %>% 
            mutate(pred=predict(tst, newdata=., type="response")) %>% 
            ggplot(aes(x=src, y=pred)) + 
            geom_boxplot(fill="lightblue", outlier.shape=NA) + 
            facet_wrap(~tt) + 
            labs(x=NULL, 
                 y=paste0("Predicted probability of ", paste(srcKey, collapse=", ")), 
                 title=paste0("Boxplot for predicted probability of ", paste(srcKey, collapse=", ")), 
                 subtitle="Outliers not plotted"
            )
        print(p1)
    }
    
    if(isTRUE(printAcc)) {
        
        # Accuracy on holdout using 0.5 as threshold
        dfTest %>% 
            mutate(pred=predict(tst, newdata=., type="response")) %>% 
            group_by(tt, src) %>%
            summarize(acc=mean(ifelse(src %in% srcKey, pred>0.5, pred<0.5)), .groups="drop") %>%
            pivot_wider(id_cols="src", names_from="tt", values_from="acc") %>%
            print()
        
    }
    
    if(isTRUE(returnGLM)) return(tst)
    
}


comboVarRF <- function(possXVars, 
                       yVar,
                       isContVar,
                       dfTrain=dfTrainCloud[,] %>% mutate(weathercode=factor(weathercode)), 
                       idxTrain=NULL,
                       dfTest=dfTestCloud %>% mutate(weathercode=factor(weathercode)), 
                       mtry=2, 
                       keyLabel="predictions based on pre-2022 training data applied to 2022 holdout dataset"
                       ) {
    
    # FUNCTION ARGUMENTS:
    # possXVars: vector of x-variables (each combination of 2 variables will be explored)
    # yVar: dependent variable
    # isContVar: boolean, is yVar continuous (regression) or discrete (classification)?
    # dfTrain: training dataset
    # idxTrain: indices from the training data to use (NULL means use all)
    # dfTest: test dataset
    # mtry: parameter passed to ranger::ranger() - mtry=2 means use both variables in every tree
    # keyLabel: descriptive text for reporting accuracy
    
    # Create matrix for storing data
    mtxLarge <- matrix(nrow=0, ncol=3)
    
    # Update training data for indices (if provided)
    if(!is.null(idxTrain)) dfTrain <- dfTrain[idxTrain, ]
    
    # Run select combinations
    for(idx1 in 1:(length(possXVars)-1)) {
        for(idx2 in (idx1+1):length(possXVars)) {
            r2Large <- runFullRF(dfTrain=dfTrain, 
                                 yVar=yVar, 
                                 xVars=possXVars[c(idx1, idx2)], 
                                 dfTest=dfTest,
                                 useLabel=keyLabel, 
                                 useSub=stringr::str_to_sentence(keyLabel), 
                                 isContVar=isContVar,
                                 mtry=mtry,
                                 makePlots=FALSE,
                                 returnData=TRUE
            )[["rfAcc"]]
            if(isTRUE(isContVar)) r2Large <- r2Large[["r2"]]
            mtxLarge <- rbind(mtxLarge, c(idx1, idx2, r2Large))
        }
    }
    
    # Create tibble of accuracy/R2 by combination
    dfLargeR2 <- as.data.frame(mtxLarge) %>% 
        purrr::set_names(c("idx1", "idx2", "r2")) %>% 
        tibble::as_tibble() %>% 
        mutate(var1=possXVars[idx1], var2=possXVars[idx2], rn=row_number()) 
    
    # Print top 20
    dfLargeR2 %>% 
        arrange(desc(r2)) %>% 
        select(var1, var2, r2) %>% 
        print(n=20)
    
    # Return the data
    dfLargeR2
    
}


mtxToDFComboVar <- function(mtxVars, 
                            vecVarNames,
                            reportData=TRUE,
                            returnData=TRUE
                            ) {
    
    # FUNCTION ARGUMENTS:
    # mtxVars: matrix containing variables and R2/accuracy data with columns idx1-idx2-r2/acc
    # vecVarNames: vector containing (in order) the name associated with each index
    # reportData: should a sorted sample of data be printed?
    #             FALSE: do not print
    #             integer or float: print round(reportData) items
    #             TRUE or anything that is not boolean, integer, or float: print 20 items
    # returnData: boolean, should data be returned?
    
    # Create tibble from existing file
    df <- as.data.frame(mtxVars) %>% 
        purrr::set_names(c("idx1", "idx2", "r2")) %>% 
        tibble::as_tibble() %>% 
        mutate(var1=vecVarNames[idx1], var2=vecVarNames[idx2], rn=row_number()) 
    
    # Print results of tibble if requested
    if(!isFALSE(reportData)) {
        if(class(reportData) %in% c("numeric", "integer")) nPrint=round(max(reportData, 1))
        else nPrint=20
        df %>% 
            arrange(desc(r2)) %>% 
            select(var1, var2, r2) %>% 
            print(n=nPrint)
    }
    
    # Return tibble if requested
    if(isTRUE(returnData)) return(df)
    
}


accComboVarRF <- function(df, 
                          yVarName,
                          exclVars=c(),
                          plotData=TRUE,
                          printData=!isTRUE(plotData), 
                          nPrint=20,
                          isR2=TRUE, 
                          plotTitle=TRUE, 
                          plotSub=TRUE, 
                          plotYLab=TRUE, 
                          plotXLab=NULL
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame (such as from mtxToDFComboVar()) containing idx1-idx2-r2-var1-var2-rn
    # yVarName: name to be used for dependent variable
    # exclVars: independent variables that are excluded from consideration
    # plotData: boolean, should the data (post-exclusions) be plotted?
    # printData: boolean, should the data (post-exclusions) be printed?
    # nPrint: number of rows to print (relevant only for isTRUE(printData))
    # isR2: boolean, are the data R-squared (FALSE means accuracy)?
    # plotTitle: title to be used for plot (TRUE means use default)
    # plotSub: subtitle to be used for plot (TRUE means use default)
    # plotYLab: y-axis-label to be used for plot (TRUE means use default)
    # plotXLab: x-axis-label to be used for plot (NULL is the default)
    
    # Update plot titles
    if(isTRUE(isR2)) dsc <- "R-squared" else dsc <- "Accuracy"
    if(isTRUE(plotTitle)) 
        plotTitle <- paste0(dsc, " in every 2-predictor model including self and one other")
    if(isTRUE(plotSub)) {
        plotSub <- paste0("Predicting ", yVarName)
        if(length(exclVars)>0) plotSub <- paste0(plotSub, 
                                                 " (excluding predictors ", 
                                                 paste0(exclVars, collapse=", "), 
                                                 ")"
        )
    }
    if(isTRUE(plotYLab)) 
        plotYLab <- paste0("Range of ", dsc, " (min-mean-max)")
    
    # Update df to exclude exclVars, if passed
    if(length(exclVars)>0) df <- df %>% filter(!(var1 %in% exclVars), !(var2 %in% exclVars))
    
    # Print data if requested
    if(isTRUE(printData)) df %>% arrange(desc(r2)) %>% select(var1, var2, r2) %>% print(n=nPrint)
    
    # Create plot if requested
    if(isTRUE(plotData)) {
        p1 <- df %>% 
            pivot_longer(cols=c(var1, var2)) %>% 
            group_by(value) %>% 
            summarize(across(r2, .fns=list("min"=min, "mu"=mean, "max"=max))) %>% 
            ggplot(aes(x=fct_reorder(value, r2_mu))) + 
            coord_flip() + 
            geom_point(aes(y=r2_mu)) + 
            geom_errorbar(aes(ymin=r2_min, ymax=r2_max)) + 
            lims(y=c(NA, 1)) + 
            geom_hline(yintercept=1, lty=2, color="red") +
            labs(title=plotTitle, subtitle=plotSub, y=plotYLab, x=plotXLab)
        print(p1)
    }
}


getVarsTrain <- function(df) {
    # Create set of relevant training variables
    df %>%
        select(starts_with("pct")) %>%
        names() %>%
        str_replace(pattern="pct_", replacement="")
}


genericKeyLabelOM <- function() 
    "predictions based on pre-2022 training data applied to 2022 holdout dataset"
# File for functions used in OpenMeteoEDA_xxxxxx_vxxx.Rmd

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
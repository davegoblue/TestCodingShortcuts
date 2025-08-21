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

# Helper function to download daily data
helperNewCityDailyDownload <- function(cityName, 
                                       tz, 
                                       abb,
                                       fName=paste0("testOM_daily_", abb, ".json"),
                                       idx=1:nrow(tblMetricsDaily), 
                                       startDate="1960-01-01", 
                                       endDate="2023-12-31"
                                       ) {
    
    testURLDaily <- helperOpenMeteoURL(cityName=cityName, 
                                       dailyIndices=idx,
                                       startDate=startDate, 
                                       endDate=endDate, 
                                       tz=tz
    )
    cat("\nDownload URL:\n")
    print(testURLDaily)
    
    # Download file
    if(!file.exists(fName)) {
        fileDownload(fileName=fName, url=testURLDaily)
    } else {
        cat("\nFile", fName, "already exists, skipping download\n")
    }
    
}


# Create the daily data frame from previously downloaded data
createDailyDF <- function(fName=NULL, abb=NULL, glimpseFOMJ=FALSE, glimpseList=FALSE, glimpseDF=TRUE) {
    
    # Create fName from abb if fName not passed
    if(is.null(fName)) {
        if(is.null(abb)) stop("\nMust provide abb or fName to function createDailyDF()\n")
        else fName <- paste0("testOM_daily_", abb, ".json")
    }
    
    # Read daily JSON file
    lstDaily <- formatOpenMeteoJSON(fName, glimpseData=glimpseFOMJ)
    
    # Sample records of tibble inside list
    if(isTRUE(glimpseList)) lstDaily$tblDaily %>% glimpse()
    
    # Convert variables to proper type
    dfDaily <- lstDaily$tblDaily %>%
        mutate(weathercode=factor(weathercode), 
               sunrise_chr=sunrise, 
               sunset_chr=sunset,
               sunrise=lubridate::ymd_hm(sunrise), 
               sunset=lubridate::ymd_hm(sunset), 
               fct_winddir=factor(winddirection_10m_dominant)
        )
    
    # Sample records of final tibble
    if(isTRUE(glimpseDF)) glimpse(dfDaily)
    
    # Return final tibble
    dfDaily
    
}


# Plot means for continuous variables
plotContVarMean <- function(df, 
                            tmpMapNames=c("precipitation_sum"="3. Precipitation (mm)", 
                                          "windspeed_10m_max"="4. Windspeed (kph)", 
                                          "temperature_2m_max"="1. High Temperature (C)",
                                          "temperature_2m_min"="2. Low Temperature (C)"
                            ), 
                            idxMapSum=c(1),
                            isMonthly=TRUE,
                            titleStarter=if(isTRUE(isMonthly)) "Monthly" else "Annual",
                            titleLoc="",
                            titleEnder=paste0("(", titleLoc, if(str_length(titleLoc)>0) " ", "1960-2023)"),
                            printPlot=TRUE, 
                            returnPlot=!isTRUE(printPlot)
                            ) {
    
    # FUNCTION ARGUMENTS:
    # df: tibble or data frame from createDailyDF()
    # tmpMapNames: variables to summarize in format c("variablename"="variable description")
    # idxMapSum: indices of the variables in tmpMapNames thatshould be summed rather than averaged
    # isMonthly: boolean with TRUE being monthly and FALSE being annual
    # titleStarter: opening phrase for title
    # titleLoc: location to be used in parentheses at end of title ("" means just show year range)
    # titleEnder: ending phrase for title
    # printPlot: boolean, should the plot be printed?
    # returnPlot: boolean, should the plot be returned?
    
    p1 <- df %>%
        mutate(month=factor(month(date), levels=1:12, labels=month.abb), year=year(date)) %>%
        select(all_of(c("year", if(isTRUE(isMonthly)) "month", names(tmpMapNames)))) %>%
        group_by(pick(if(isTRUE(isMonthly)) c("year", "month") else c("year"))) %>%
        summarize(across(-all_of(names(tmpMapNames)[idxMapSum]), .fns=mean), 
                  across(all_of(names(tmpMapNames)[idxMapSum]), .fns=sum), 
                  .groups="drop"
        ) 
    
    if(isTRUE(isMonthly)) {
        p1 <- p1 %>%
            group_by(month) %>%
            summarize(across(-c(year), .fns=mean)) %>%
            pivot_longer(-c(month))
    } else {
        p1 <- p1 %>%
            pivot_longer(-c(year))
    }
    
    p1 <- p1 %>%
        ggplot(aes(x=.data[[if(isTRUE(isMonthly)) "month" else "year"]], y=value)) + 
        geom_line(aes(group=1)) + 
        facet_wrap(~tmpMapNames[name], scales="free_y") + 
        labs(x=NULL, 
             y=NULL, 
             title=paste0(titleStarter, " averages for key metrics ", titleEnder)
        )
    
    if(isTRUE(printPlot)) print(p1)
    if(isTRUE(returnPlot)) p1
    
}


# Plot means for categorical variables
plotCatVarMean <- function(df, 
                           tmpMapNames=c("wc"="1. Weather Type", 
                                         "winddir"="2. Predominant Wind Direction"
                           ), 
                           idxMapSum=c(1),
                           isMonthly=TRUE,
                           titleStarter=if(isTRUE(isMonthly)) "Monthly" else "Annual",
                           titleLoc="",
                           titleEnder=paste0("(", titleLoc, if(str_length(titleLoc)>0) " ", "1960-2023)"),
                           printPlot=TRUE, 
                           returnPlot=!isTRUE(printPlot)
                           ) {
    
    tmpDFPlot <- df %>%
        mutate(month=factor(month(date), levels=1:12, labels=month.abb), 
               year=year(date), 
               winddir=case_when(winddirection_10m_dominant>360~"Invalid", 
                                 winddirection_10m_dominant>=315~"1. N", 
                                 winddirection_10m_dominant>=225~"2. W", 
                                 winddirection_10m_dominant>=135~"3. S", 
                                 winddirection_10m_dominant>=45~"4. E", 
                                 winddirection_10m_dominant>=0~"1. N", 
                                 TRUE~"Invalid"
               ), 
               wc=case_when(weathercode==0~"1. Clear", 
                            weathercode %in% c(1, 2, 3)~"2. Dry", 
                            weathercode %in% c(51, 53, 55)~"3. Drizzle", 
                            weathercode %in% c(61, 63, 65)~"4. Rain", 
                            weathercode %in% c(71, 73, 75)~"5. Snow", 
                            TRUE~"Error"
               )
        ) %>%
        select(all_of(c(if(isTRUE(isMonthly)) "month" else "year", names(tmpMapNames))))
    
    if(isTRUE(isMonthly)) {
        tmpDFPlot <- tmpDFPlot %>%
            pivot_longer(-c(month)) %>%
            count(month, name, value)
    } else {
        tmpDFPlot <- tmpDFPlot %>%
            pivot_longer(-c(year)) %>%
            count(year, name, value)
    }
    
    tmpPlotFN <- function(x) {
        p1 <- tmpDFPlot %>%
            filter(name==x) %>%
            ggplot(aes(x=.data[[if(isTRUE(isMonthly)) "month" else "year"]], y=n)) + 
            geom_line(aes(group=value, color=value), lwd=2) + 
            labs(x=NULL, 
                 y=NULL, 
                 title=paste0(titleStarter, " average for ", tmpMapNames[x], " ", titleEnder)
            ) + 
            scale_color_discrete(NULL) + 
            theme(legend.position = "bottom")
        return(p1)
    }
    
    # Update to be more flexible
    grid::grid.newpage()
    p1 <- gridExtra::arrangeGrob(tmpPlotFN("wc"), tmpPlotFN("winddir"), nrow=1)
    
    if(isTRUE(printPlot)) grid::grid.draw(p1)
    if(isTRUE(returnPlot)) p1
    
}


# Create box plots
omCreateBoxPlot <- function(df, 
                            keyVar, 
                            chgLag=NULL,
                            ymin=NA, 
                            ymax=NA, 
                            mapDesc=c("windspeed_10m_max"="Maximum wind speed (kph)", 
                                      "precipitation_hours"="Precipitation (hours)", 
                                      "precipitation_sum"="Precipitation (mm)", 
                                      "temperature_2m_max"="Maximum Temperature (C)", 
                                      "temperature_2m_min"="Minimum Temperature (C)"
                            ),
                            keyVarDesc=if(keyVar %in% names(mapDesc)) mapDesc[keyVar] else "Key Variable",
                            titleLoc="",
                            titleEnder=paste0("(", titleLoc, if(str_length(titleLoc)>0) " ", "1960-2023)")
                            ) {
    
    p1 <- df %>%
        mutate(month=factor(month(date), levels=1:12, labels=month.abb), year=year(date)) %>%
        select(all_of(c("month", keyVar)))
    
    if(!is.null(chgLag)) 
        p1 <- p1 %>%
            mutate(across(all_of(keyVar), .fns=function(x) x - lag(x, chgLag))) %>%
            filter(if_all(all_of(keyVar), .fns=function(x) !is.na(x)))
    
    p1 <- p1 %>%
        ggplot(aes(x=month, y=.data[[keyVar]])) + 
        geom_boxplot(fill="lightblue") + 
        labs(x=NULL, 
             y=keyVarDesc, 
             title=paste0(if(is.null(chgLag)) "" else "Change in ", keyVarDesc, " by month ", titleEnder)
        )
    if(!is.na(ymin) | !is.na(ymax)) p1 <- p1 + lims(y=c(ymin, ymax))
    print(p1)
    
}


# Create ACF and PACF for key variables
omACFPACF <- function(df, keyVar, bigLag=1000, smallLag=50, smallACF=FALSE, smallPACF=TRUE) {
    
    # Big lag for ACF
    acfTemp <- acf(df %>% pull(keyVar), lag.max=bigLag, main=paste0("ACF: ", keyVar))
    
    # Peaks for ACF
    cat("\nACF peaks\n")
    as.vector(acfTemp$acf) %>% findPeaks(width=21) %>% which() %>% print()
    
    # Troughs for ACF
    cat("\nACF troughs\n")
    as.vector(acfTemp$acf) %>% findPeaks(width=21, FUN=min) %>% which() %>% print()
    
    # Small lag for ACF (if requested)
    if(isTRUE(smallACF)) acfTemp <- acf(df %>% pull(keyVar), lag.max=smallLag, main=paste0("ACF: ", keyVar))
    
    # Big lag for PACF
    pacfTemp <- pacf(df %>% pull(keyVar), lag.max=bigLag, main=paste0("PACF: ", keyVar))
    
    # Small lag for PACF (if requested)
    if(isTRUE(smallPACF)) pacfTemp <- pacf(df %>% pull(keyVar), lag.max=smallLag, main=paste0("PACF: ", keyVar))
    
}


# Daily mean and standard deviation
omDailyMeanSD <- function(df, 
                          rollVars, 
                          rollK,                             
                          mapDesc=c("windspeed_10m_max"="windMax", 
                                    "precipitation_hours"="precipHours", 
                                    "precipitation_sum"="precipSum", 
                                    "temperature_2m_max"="tempMax", 
                                    "temperature_2m_min"="tempMin"
                          ), 
                          addSuffix=TRUE, 
                          makeSEM=FALSE, 
                          printPlot=TRUE, 
                          returnPlot=!isTRUE(printPlot)
                          ) {
    
    if(isTRUE(addSuffix)) for(x in names(mapDesc)) mapDesc[x] <- paste0(mapDesc[x], "_r", rollK)
    
    # Calculate means
    df_r21 <- df %>% mutate(doy=pmin(yday(date), 365))
    
    for(x in rollVars) {
        df_r21 <- df_r21 %>% 
            helperRollingAgg(origVar=x, newName=unname(mapDesc[x]), k=rollK)
    }
    
    df_r21 <- df_r21 %>% select(all_of(c("date", "doy", unname(mapDesc[rollVars]))))
    
    # Calculate standard deviations
    df_r21_sd <- df %>%
        mutate(doy=pmin(yday(date), 365)) %>% 
        group_by(doy) %>%
        mutate(across(.cols=all_of(rollVars), .fns=sd)) %>%
        ungroup()
    
    for(x in rollVars) { 
        df_r21_sd <- df_r21_sd %>% 
            helperRollingAgg(origVar=x, newName=unname(mapDesc[x]), k=rollK)
    }
    
    df_r21_sd <- df_r21_sd %>% select(all_of(c("date", "doy", unname(mapDesc[rollVars]))))
    
    
    # Create plot of means and standard deviations or standard errors of the mean
    if(isTRUE(makeSEM)) divBy <- sqrt(length(unique(year(df_r21$date)))-1)
    else divBy <- 1
    
    p1 <- df_r21 %>%
        bind_rows(df_r21_sd, .id="src") %>%
        na.omit() %>%
        mutate(musig=c("1"="Mean", "2"="SD")[src]) %>%
        group_by(doy, musig) %>%
        summarize(across(where(is.numeric), .fns=mean), .groups="drop") %>%
        pivot_longer(cols=-c(doy, musig)) %>%
        pivot_wider(id_cols=c(doy, name), names_from="musig") %>%
        ggplot(aes(x=doy)) + 
        geom_line(aes(y=Mean, group=name, color=name), lwd=2) + 
        geom_ribbon(aes(ymin=Mean-SD/divBy, ymax=Mean+SD/divBy, fill=name), alpha=0.5) +
        facet_wrap(~name, scales="free_y") +
        labs(x="Day of Year", 
             y=paste0("Rolling ", 
                      rollK, 
                      "-day mean +/- 1 ", 
                      if(isTRUE(makeSEM)) "SEM (approx)" else "sd"
             ), 
             title=paste0("Rolling ", 
                          rollK, 
                          "-day mean +/- 1 rolling ", 
                          rollK, 
                          "-day ", 
                          if(isTRUE(makeSEM)) "SEM (approx)" else "sd"
             )
        ) + 
        theme(legend.position="none")
    
    if(isTRUE(printPlot)) print(p1)
    if(isTRUE(returnPlot)) return(p1)
    
}


# Run all steps for creating OM daily data
omRunAllSteps <- function(dfDaily=NULL, 
                          dlData=FALSE, 
                          abbCity=NULL, 
                          tzCity=NULL, 
                          useNameCity="New city", 
                          dbNameCity=useNameCity, 
                          returnDF=FALSE,
                          runStats=TRUE, 
                          mainStatVars=c("temperature_2m_max", "windspeed_10m_max", "precipitation_sum"), 
                          xtraStatVars=c("temperature_2m_min", "precipitation_hours"),
                          minZeroVars=c("windspeed_10m_max", "precipitation_sum", "precipitation_hours")
                          ) {
    
    # 1. Download data (if requested)
    if(isTRUE(dlData)) helperNewCityDailyDownload(cityName=dbNameCity, tz=tzCity, abb=abbCity)
    
    # 2. Load and process data (if not passed)
    if(is.null(dfDaily)) dfDaily <- createDailyDF(abb=abbCity)
    
    # Stop processing if requested
    if(!isTRUE(runStats)) {
        if(isTRUE(returnDF)) {
            return(dfDaily)
        } else {
            return(NULL)
        }
    }
    
    # 3. Plot means for continuous variables
    plotContVarMean(dfDaily, isMonthly=TRUE, titleLoc=useNameCity)
    plotContVarMean(dfDaily, isMonthly=FALSE, titleLoc=useNameCity)
    
    # 4. Plot means for categorical variables
    plotCatVarMean(dfDaily, isMonthly=TRUE, titleLoc=useNameCity)
    plotCatVarMean(dfDaily, isMonthly=FALSE, titleLoc=useNameCity)
    
    # 5. Create boxplots for select variables
    purrr::walk(.x=c(mainStatVars, xtraStatVars), 
                .f=function(x) omCreateBoxPlot(dfDaily, 
                                               keyVar=x, 
                                               ymin=if(x %in% minZeroVars) 0 else NA, 
                                               titleLoc=useNameCity
                )
    )
    
    # 6. Analyze ACF/PACF for select variables
    purrr::walk(.x=mainStatVars, 
                .f=function(x) omACFPACF(dfDaily, keyVar=x, smallACF=TRUE)
    )
    
    # 7. Create boxplots for select variables (with lag-1, daily difference)
    purrr::walk(.x=mainStatVars, 
                .f=function(x) omCreateBoxPlot(dfDaily, keyVar=x, chgLag=1, titleLoc=useNameCity)
    )
    
    # 8. Create daily mean +/- 1 SD and/or SEM for select variables
    omDailyMeanSD(dfDaily, rollK=21, rollVars=mainStatVars)
    omDailyMeanSD(dfDaily, rollK=21, rollVars=mainStatVars, makeSEM=TRUE)
    
    # Return file if requested
    if(isTRUE(returnDF)) return(dfDaily)
    
}


# Create plots for continuous variables across cities
omMultiCityCont <- function(df, varName, varDesc, divBy) {
    
    p1 <- df %>%
        select(all_of(c("date", varName, "cityName"))) %>%
        bind_rows(mutate(., cityName="Overall")) %>%
        group_by(cityName, date) %>%
        summarize(across(where(is.numeric), .fns=mean), .groups="drop") %>%
        arrange(cityName, date) %>%
        mutate(doy=pmin(yday(date), 365)) %>%
        group_by(cityName) %>%
        helperRollingAgg(origVar=varName, newName="mu_r21", k=21) %>%
        group_by(cityName, doy) %>%
        mutate(sd=sd(get(varName))) %>%
        group_by(cityName) %>%
        helperRollingAgg(origVar="sd", newName="sd_r21", k=21) %>%
        ungroup() %>%
        select(cityName, doy, mu_r21, sd_r21) %>%
        pivot_longer(cols=-c(cityName, doy)) %>%
        na.omit() %>%
        group_by(cityName, doy, name) %>%
        summarize(across(where(is.numeric), .fns=mean), .groups="drop") %>%
        pivot_wider(id_cols=c(cityName, doy), names_from="name") %>%
        ggplot(aes(x=doy)) + 
        geom_line(data=~filter(., cityName!="Overall"), 
                  aes(y=mu_r21, group=cityName, color=cityName), 
                  lwd=2
        ) + 
        geom_ribbon(data=~filter(., cityName!="Overall"), 
                    aes(ymin=mu_r21-sd_r21/divBy, ymax=mu_r21+sd_r21/divBy, fill=cityName), 
                    alpha=0.5
        ) +
        labs(x="Day of Year", 
             y=paste0("Rolling ", 21, "-day mean +/- 1 ", if(isTRUE(TRUE)) "SEM (approx)" else "sd"), 
             title=paste0("Rolling ", 
                          21, 
                          "-day mean +/- 1 rolling ", 
                          21, 
                          "-day ", 
                          if(isTRUE(TRUE)) "SEM (approx)" else "sd", "\n", varDesc)
        ) + 
        scale_color_discrete(NULL) + 
        scale_fill_discrete(NULL) + 
        geom_line(data=~filter(., cityName=="Overall"), aes(y=mu_r21, group=cityName), lwd=1, lty=2)
    
    p1
    
}



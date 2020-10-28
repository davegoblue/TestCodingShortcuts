# Function to read and run the CDC all-cause deaths analysis
readRunCDCAllCause <- function(loc, 
                               startYear, 
                               curYear,
                               weekThru, 
                               startWeek, 
                               lst, 
                               epiMap,
                               agePopData,
                               cvDeathThru,
                               cdcPlotStartWeek=startWeek,
                               periodKeep=paste0(startYear, "-", curYear-1), 
                               dlData=FALSE, 
                               ovrWrite=FALSE, 
                               ovrWriteError=TRUE,
                               dir="./RInputFiles/Coronavirus/"
                               ) {
    
    # FUNCTION ARGUMENTS:
    # loc: the CDC .csv file name (without path)
    # startYear: the starting year in the CDC data
    # curYear: the current analyis year in the CDC data
    # weekThru: how many weeks of the current year are the data valid thru?
    # startWeek: the starting week to use for cumulative sum of difference in expected all-cause deaths
    # lst: a state clustering process output list
    # epiMap: a mapping file of ew-month-quarter that mas epiweek (ew) to an appropriate month and quarter
    # agePopData: data containing US population as age (fct) - pop (int)
    # cvDeathThru: the date to use for pulling the CV death data
    # cdcPlotStartWeek: start week for CDC plots (10 is March which avoids a 1-week February outlier)
    # periodKeep: the period of previous data in the CDC all-cause deaths file (should be kept regardless)
    # dir: the CDC .csv directory (will use paste0(dir, loc) as the file location)
    
    # STEP 0: Download CDC data if requested
    helperDownload <- function(url, fileLoc, ow=ovrWrite, owError=ovrWriteError) {
        # If the file exists, mention it and proceed as per the guard checks
        if (file.exists(fileLoc)) {
            cat("\nFile:", fileLoc, "already exists\n")
            if (!ow & owError) stop("\nExiting due to ow=FALSE and owError=TRUE\n")
            if (!ow & !owError) {
                cat("\nFile is NOT downloaded again\n")
                return(NULL)
            }
        }
        # Download the file and change to read-only
        cat("\nDownloading CDC data from", url, "to", fileLoc, "\n")
        download.file(url, destfile=fileLoc, method="curl")
        Sys.chmod(fileLoc, mode="0555", use_umask = FALSE)
    }
    
    if (dlData) {
        helperDownload(url="https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD", 
                       fileLoc=paste0(dir, loc)
        )
    }
    
    # STEP 1: Read and process the CDC data
    cdc <- readProcessCDC(loc, weekThru=weekThru, periodKeep=periodKeep, fDir=dir)
    
    # STEP 2: Create the key data required for using state-level clusters
    clusterList <- helperKeyStateClusterMetrics(lst)
    
    # STEP 3: Generate plots of the processed CDC data
    cdcBasicPlots(cdc, clustVec=clusterList$clData)
    
    # STEP 4: Full US excess deaths
    list_allUS <- cdcCohortAnalysis(cohortName="all US", 
                                    df=cdc,
                                    curYear=curYear, 
                                    startYear=startYear,
                                    startWeek=startWeek,
                                    plotTitle="All-cause US total deaths",
                                    predActualPlotsOnePage=TRUE
    )
    
    # STEP 5: Generate cluster-level aggregate plots
    clusterAgg <- cdcAggregateSummary(df=cdc, 
                                      critVar="state", 
                                      critSubsets=clusterList$stateCluster,
                                      startWeek=startWeek, 
                                      critListNames=paste0("cluster ", 1:length(clusterList$stateCluster)),
                                      factorCritList=FALSE,
                                      popData=clusterList$pop,
                                      cvDeathData=clusterList$deaths,
                                      idVarName="cluster"
    )
    
    # STEP 6: Generate state-level aggregate data, then plot
    stateAgg <- cdcAggregateSummary(df=cdc, 
                                    critVar="state", 
                                    critSubsets=names(clusterList$clData),
                                    startWeek=startWeek, 
                                    idVarName="state", 
                                    subListNames=names(clusterList$clData),
                                    showAllPlots=FALSE
    )
    helperKeyStateExcessPlots(df=stateAgg, 
                              epiMonth=epiMap,
                              cvDeaths=lst$consolidatedPlotData,
                              startWeek=cdcPlotStartWeek,
                              cvDeathDate=as.Date(cvDeathThru),
                              subT=paste0("CDC data through week ", weekThru, " of ", curYear)
    )
    
    # STEP 7: Generate age-level aggregate data, then plot
    ageAgg <- cdcAggregateSummary(df=cdc, 
                                  critVar="age", 
                                  critSubsets=levels(cdc$age),
                                  startWeek=startWeek, 
                                  idVarName="age", 
                                  subListNames=levels(cdc$age),
                                  showAllPlots=TRUE
    )
    helperKeyAgeExcessPlots(df=ageAgg, 
                            epiMonth=epiMap,
                            cvDeaths=lst$consolidatedPlotData,
                            popData=agePopData,
                            startWeek=cdcPlotStartWeek,
                            cvDeathDate=as.Date(cvDeathThru),
                            subT=paste0("CDC data through week ", weekThru, " of ", curYear)
    )
    
    # STEP 8: Return a list of key files
    list(cdc=cdc, 
         clusterList=clusterList, 
         allUSAgg=list_allUS$preds,
         clusterAgg=clusterAgg, 
         stateAgg=stateAgg, 
         ageAgg=ageAgg
    )
    
}



# Function to read and process raw CDC all-cause deaths data
readProcessCDC <- function(fName, 
                           weekThru,
                           periodKeep="2015-2019",
                           fDir="./RInputFiles/Coronavirus/",
                           col_types="ccciicdcccc", 
                           renameVars=c("Week Ending Date"="weekEnding", 
                                        "State Abbreviation"="state", 
                                        "Year"="year", 
                                        "Week"="week", 
                                        "Age Group"="age", 
                                        "Number of Deaths"="deaths", 
                                        "Time Period"="period", 
                                        "Type"="type"
                           ), 
                           maxSuppressAllowed=10
                           ) {
    
    # FUNCTION ARGUMENTS:
    # fName: name of the downloaded CDC data file
    # weekThru: any record where week is less than or equal to weekThru will be kept
    # periodKeep: any record where period is in periodKeep will be kept
    # fDir: directory name for the downloaded CDC data file
    # col_types: variable type by column in the CDC data (passed to readr::read_csv())
    # renameVars: named vector for variable renaming of type c("Existing Name"="New Name")
    # maxSuppressAllowed: maximum number of data suppressions (must be in current week/year) to avoid error
    
    # STEP 1: Read the CSV data
    cdcRaw <- readr::read_csv(paste0(fDir, fName), col_types=col_types)
    glimpse(cdcRaw)
    
    # STEP 2: Rename the variables for easier interpretation
    cdcRenamed <- cdcRaw
    names(cdcRenamed) <- ifelse(is.na(renameVars[names(cdcRenamed)]), 
                                names(cdcRenamed), 
                                renameVars[names(cdcRenamed)]
    )
    cdcRenamed <- cdcRenamed %>%
        mutate(weekEnding=lubridate::mdy(weekEnding))
    glimpse(cdcRenamed)
    
    # STEP 2a: Check control levels for key variables in renamed file
    cat("\nCheck Control Levels and Record Counts for Renamed Data:\n")
    helperCDCFields("age", df=cdcRenamed) %>% print()
    helperCDCFields(c("period", "year", "type"), df=cdcRenamed) %>% print()
    helperCDCFields(c("period", "Suppress"), df=cdcRenamed) %>% print()
    helperCDCFields(c("period", "Note"), df=cdcRenamed) %>% print()
    helperCDCFields(c("state", "Jurisdiction"), df=cdcRenamed) %>% 
        arrange(-deaths) %>% 
        as.data.frame() %>% 
        print()
    p1 <- helperCDCFields(c("year", "week"), df=cdcRenamed) %>% 
        ggplot(aes(x=week, y=deaths, group=factor(year), color=factor(year))) + 
        geom_line() + 
        labs(y="Deaths (multiple counting on several dimensions", x="Epidemiological Week") + 
        scale_color_discrete("Year") +
        ylim(c(0, NA))
    print(p1)
    
    # STEP 3: Convert to factored data
    cdcFactored <- cdcRenamed %>%
        mutate(age=factor(age, levels=c("Under 25 years", "25-44 years", "45-64 years", 
                                        "65-74 years", "75-84 years", "85 years and older"
        )
        ), 
        period=factor(period, levels=c("2015-2019", "2020")), 
        year=factor(year, levels=2015:2020)
        )
    glimpse(cdcFactored)
    
    # STEP 4: Filter the data to include only weighted deaths and only through the desired time period
    cdcFiltered <- cdcFactored %>%
        filter(type=="Predicted (weighted)", period %in% periodKeep | week <= weekThru)
    glimpse(cdcFiltered)
    
    # STEP 4a: Check that all suppressed data and NA deaths have been eliminated
    cat("\n\n *** Data suppression checks *** \n")
    suppressData <- cdcFiltered %>%
        filter(!is.na(Suppress) | is.na(deaths))
    print(suppressData)
    nProblem <- suppressData %>% 
        filter(year == periodKeep | week != weekThru) %>%
        nrow()
    if ((nrow(suppressData) <= maxSuppressAllowed) & (nProblem==0)) {
        cat("\nData suppression checks OK -", nrow(suppressData), "records in current week/year suppressed\n")
        cdcFiltered <- cdcFiltered %>%
            filter(!is.na(deaths))
    } else {
        cat("\n *** Data suppression checks failed - total of", nrow(suppressData), "suppressions")
        cat("\n *** Of these suppressions,", nProblem, "are NOT from weekThru of current year")
        stop("\nInvestigate data suppression issues and re-run with updated parameters or cleaned data\n")
    }
    
    # STEP 5: Delete the US record and convert YC to be part of NY
    cdcProcessed <- cdcFiltered %>%
        filter(!(state %in% c("US", "PR"))) %>%
        mutate(state=ifelse(state=="YC", "NY", state), 
               Jurisdiction=ifelse(state %in% c("NY", "YC"), "New York State (NY plus YC)", Jurisdiction)
        ) %>%
        group_by(Jurisdiction, weekEnding, state, year, week, age, period, type, Suppress) %>%
        arrange(!is.na(Note)) %>%
        summarize(n=n(), deaths=sum(deaths), Note=first(Note)) %>%
        ungroup()
    glimpse(cdcProcessed)
    
    # STEP 5a: Confirm that there are no duplicates in the final data file
    firstDup <- cdcProcessed %>%
        select(state, year, week, age) %>%
        anyDuplicated()
    cat("\nFirst duplicate is in row number (0 means no duplicates):", firstDup)
    
    # STEP 6: Return the processed data file
    cdcProcessed
    
}



helperCDCFields <- function(x, df=cdcRenamed) {
    
    df %>%
        group_by_at(x) %>%
        summarize(n=n(), n_deaths_na=sum(is.na(deaths)), deaths=sum(deaths, na.rm=TRUE))
    
}



# Convert the output of a state-level clustering list to population, membership, and deaths
helperKeyStateClusterMetrics <- function(lst) {
    
    # FUNCTION ARGUMENTS:
    # lst: the list containing the outputs of the state clustering routing
    
    # Get the population by cluster
    pop <- lst[["consolidatedPlotData"]] %>%
        filter(state=="cluster") %>%
        group_by(cluster) %>%
        summarize(pop=max(pop)) %>%
        ungroup() %>%
        mutate(cluster=as.character(cluster))
    
    # Get the cluster vector
    clData <- lst[["useClusters"]]
    
    # Create a list of states by cluster
    stateCluster <- lapply(1:length(unique(clData)), FUN=function(x) { names(clData[clData==x]) })
    
    # Create reported cvDeaths by cluster
    deaths <- lst[["consolidatedPlotData"]] %>%
        filter(name=="deaths", state=="cluster") %>%
        ungroup() %>%
        mutate(week=lubridate::epiweek(date), cluster=as.character(cluster)) %>%
        group_by(cluster, week) %>%
        summarize(cvDeaths=sum(value)) %>%
        ungroup()
    
    list(pop=pop, clData=clData, stateCluster=stateCluster, deaths=deaths)
    
}



# Basic plots of the CDC data, including by a state-level cluster (passed as argument)
cdcBasicPlots <- function(df, 
                          weekThru=NULL, 
                          curYear=NULL, 
                          clustVec=NULL
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: a processed CDC data file
    # weekThru: week of the current year that data are thru (NULL means infer from data)
    # curYear: current year (NULL means infer from data)
    # clustVec: clustering vector with names as state abbreviations (NULL means no plots by cluster)
    
    # Get the week and year if passed as NULL
    if (is.null(curYear)) { curYear <- df %>% pull(year) %>% as.character() %>% as.integer() %>% max() }
    if (is.null(weekThru)) { weekThru <- df %>% filter(year==curYear) %>% pull(week) %>% max() }
    
    # Plot of total deaths by year (50 states plus DC)
    p1 <- df %>%
        group_by(year) %>%
        summarize(deaths=sum(deaths)) %>%
        ggplot(aes(x=year, y=deaths/1000)) + 
        geom_col(fill="lightblue") + 
        geom_text(aes(y=deaths/2000, label=round(deaths/1000))) + 
        labs(title="CDC Deaths for 50 states plus DC", 
             x="", 
             y="Deaths (000s)", 
             subtitle=paste0("Through week ", weekThru, " of ", curYear)
        )
    print(p1)
    
    # Plot of total deaths by week by year
    p2 <- df %>%
        group_by(year, week) %>%
        summarize(deaths=sum(deaths)) %>%
        ungroup() %>%
        ggplot(aes(x=week, y=deaths, group=year, color=year)) + 
        geom_line() +
        ylim(c(0, NA)) + 
        labs(x="Week of Year", y="Deaths", title="US deaths per week by year")
    print(p2)
    
    # Plot of total deaths by year by age cohort
    p3 <- df %>%
        group_by(year, week, age) %>%
        summarize(deaths=sum(deaths)) %>%
        ungroup() %>%
        ggplot(aes(x=week, y=deaths, group=year, color=year)) + 
        geom_line() +
        ylim(c(0, NA)) + 
        facet_wrap(~age) +
        labs(x="Week of Year", y="Deaths", title="US deaths per week by year")
    print(p3)
    
    # Plots of total deaths by week by year by cluster
    if (!is.null(clustVec)) {
        p4 <- df %>%
            mutate(cluster=clustVec[state]) %>%
            group_by(year, week, cluster) %>%
            summarize(deaths=sum(deaths)) %>%
            ungroup() %>%
            ggplot(aes(x=week, y=deaths, group=year, color=year)) + 
            geom_line() +
            ylim(c(0, NA)) + 
            facet_wrap(~cluster) +
            labs(x="Week of Year", 
                 y="Deaths", 
                 title="US deaths per week by year", 
                 subtitle="Facetted by state cluster"
            )
        print(p4)
        
        p5 <- df %>%
            mutate(cluster=clustVec[state]) %>%
            filter(year==curYear) %>%
            group_by(year, age, week, cluster) %>%
            summarize(deaths=sum(deaths)) %>%
            ungroup() %>%
            ggplot(aes(x=week, y=deaths, fill=age)) + 
            geom_col(position="stack") +
            ylim(c(0, NA)) + 
            facet_wrap(~cluster) +
            labs(x="Week of Year", 
                 y="Deaths", 
                 title=paste0("US deaths per week (Year ", curYear, ")"), 
                 subtitle="Facetted by state cluster"
            )
        print(p5)
    }
    
}



# Integrated function with all steps
cdcCohortAnalysis <- function(cohortName, 
                              df=cdcProcessed, 
                              critFilter=vector("list", 0), 
                              plotTitle=NULL, 
                              curYear=2020, 
                              startYear=2015,
                              prevYears=paste0(startYear, "-", curYear-1), 
                              startWeek=9, 
                              showSubsetPlots=TRUE, 
                              showPredActualPlots=TRUE, 
                              predActualPlotsOnePage=FALSE
                              ) {
    
    # FUNCTION ARGUMENTS:
    # cohortName: the name of the cohort
    # df: the processed CDC death data
    # critFilter: the filtering criteria in the form of a named list "variable"=c("possibleValues")
    # plotTitle: title to be used for plot in subsetCDC()
    # curYear: the current year for the death data
    # startYear: the starting year for the death data
    # prevYears: the previous years for the death data (used for making predictions)
    # startWeek: the starting week for counting excess deaths
    # showSubsetPlots: boolean, whether to show the two plots in subsetCDC()
    # showPredActualPlots: boolean, whether to show the four plots in cdcPredictedvsActual()
    # predActualPlotsOnePage: boolean, whether to place the predicted vs. actual plots all on one page
    
    # Subset the data (will show both plots by default)
    allDeath_cohort <- subsetCDC(df=df, 
                                 critFilter=critFilter,
                                 showPlot=showSubsetPlots,
                                 plotTitle=plotTitle, 
                                 curYear=curYear,
                                 prevYears=prevYears
    )
    
    # Create the linear model for the subsetted data
    lm_cohort <- cdcRegression(df=allDeath_cohort, curYear=curYear, startYear=startYear)
    
    # Apply the predictions to the data
    allPred_cohort <- cdcPrediction(df=allDeath_cohort, lmReg=lm_cohort)
    
    # Plots for predicted vs. actual (will show all plots by default)
    cdcPredictedvsActual(df=allPred_cohort, 
                         cohortName=cohortName, 
                         curYear=curYear,
                         prevYears=prevYears,
                         startWeek=startWeek, 
                         showCurrentPredvsPrev=showPredActualPlots,
                         showActualsvsPred=showPredActualPlots,
                         showActualsvsPredRatio=showPredActualPlots,
                         showExcessDeaths=showPredActualPlots, 
                         predActualPlotsOnePage=predActualPlotsOnePage
    )
    
    # Return a list containing the lm model and the filtered data
    list(lmReg=lm_cohort, preds=allPred_cohort)
    
}



# Function to run the linear model to predict CDC deaths by week and year
cdcRegression <- function(df, 
                          curYear=2020, 
                          startYear=2015
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed CDC data
    # curYear: the current year
    # startYear: the year that is defined as the zero-point for the regression
    
    # Very basic linear model for deaths vs. week and year
    df %>%
        filter(year != curYear) %>%
        lm(deaths ~ weekfct + I(yearint-startYear) + 0, data=.)
    
}


# Function to subset CDC data
subsetCDC <- function(df, 
                      critFilter=vector("list", 0), 
                      showPlot=TRUE, 
                      plotTitle=NULL, 
                      curYear=2020, 
                      prevYears=paste0("2015-", curYear-1)
                      ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed CDC data
    # critFilter: a filtering list of the form variable=c(possibleValues)
    # showPlot: boolean, whether to show plots of deaths by week and year
    # plotTitle: the title to be used for the plot (NULL will use a default title)
    # curYear: the current year (will be excluded from the mean and standard deviation plot)
    
    # Filter such that only matches to critFilter are included
    for (xNum in seq_len(length(critFilter))) {
        df <- df %>%
            filter_at(vars(all_of(names(critFilter)[xNum])), ~. %in% critFilter[[xNum]])
    }
    
    # Create a default title if none provided
    if (is.null(plotTitle)) plotTitle <- "All-cause deaths for filtered cohort"
    
    # All deaths by week and year for the filtered cohort
    allDeath <- df %>%
        group_by(year, week) %>%
        summarize(deaths=sum(deaths)) %>%
        ungroup() %>%
        mutate(weekfct=factor(week), yearint=as.integer(as.character(year)))
    
    # Show plots if requested
    if (showPlot) {
        # Plot of all deaths for the filtered cohort by week and year
        p1 <- allDeath %>%
            ggplot(aes(x=week, y=deaths, color=year, group=year)) + 
            geom_line() + 
            ylim(c(0, NA)) + 
            labs(x="Week", 
                 y="Deaths", 
                 title=plotTitle
            )
        print(p1)
        
        # Plot of mean and standard deviation from previous years
        p2 <- allDeath %>%
            filter(year != curYear) %>%
            group_by(week) %>%
            summarize(mean=mean(deaths), sd=sd(deaths)) %>%
            ggplot(aes(x=week)) + 
            geom_line(aes(y=mean), color="red", size=2) + 
            geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd), fill="pink", alpha=0.5) +
            labs(x="Week", 
                 y="Deaths", 
                 title=paste0(prevYears, " ", plotTitle), 
                 subtitle=paste0("Line is ", 
                                 prevYears, 
                                 " annual mean deaths, ribbon is +/- 1 standard deviation"
                 )
            ) + 
            ylim(c(0, NA))
        print(p2)
        
    }
    
    # Return the processed data frame
    allDeath
    
}



# Function to make predictions based on the regression
cdcPrediction <- function(df, 
                          lmReg
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble containing the data
    # lmReg: the linear regression model
    
    # Predictions and deltas
    allPred <- df %>%
        mutate(pred=predict(lmReg, newdata=.), delta=(deaths-pred)) %>%
        group_by(year) %>%
        arrange(week) %>%
        mutate(cumDelta=cumsum(delta), cumPred=cumsum(pred)) %>%
        ungroup()
    
    # Return the predictions and deltas    
    allPred
    
}



# Function to make plots of the predictions
cdcPredictedvsActual <- function(df, 
                                 cohortName,
                                 curYear=2020, 
                                 prevYears=paste0("2015-", curYear-1), 
                                 startWeek=9, 
                                 showCurrentPredvsPrev=TRUE, 
                                 showActualsvsPred=TRUE,
                                 showActualsvsPredRatio=TRUE, 
                                 showExcessDeaths=TRUE, 
                                 predActualPlotsOnePage=FALSE
                                 ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing actual and predicted deaths
    # cohortName: cohort name to use for plot titles
    # curYear: the current year
    # prevYears: the previous years
    # startWeek: week to use for starting the counts of cumulative excess deaths
    # showCurrentPredvsPrev: boolean, whether to plot current year predictions vs. previous years
    # showActualsvsPred: boolean, whether to plot actuals vs. predictions by year
    # showActualsvsPredRatio: boolean, whether to plot actuals vs. predictions ratios by yrea
    # showExcessDeaths: boolean, whether to plot estimated excess deaths
    # predActualPlotsOnePage: boolean, whether to place the predicted vs. actual plots all on one page
    
    if (showCurrentPredvsPrev) {
        # Current year predictions vs. previous year actuals
        p1 <- df %>%
            ggplot(aes(x=week)) + 
            geom_line(data=~filter(., year != curYear), aes(y=deaths, color=year, group=year)) + 
            geom_line(data=~filter(., year == curYear), aes(y=pred), lty=2, size=1) +
            ylim(c(0, NA)) + 
            labs(x="Week", 
                 y="Deaths", 
                 title=paste0("All-cause deaths for ", cohortName, " cohort"), 
                 subtitle=paste0("Solid lines are actual ", 
                                 prevYears, 
                                 ", dashed line is predicted ", 
                                 curYear
                 )
            )
        if (!predActualPlotsOnePage) print(p1)
    }
    
    if (showActualsvsPred) {
        # Plot of predictions and actuals facetted by year - deaths
        p2 <- df %>%
            select(year, week, deaths, pred) %>%
            pivot_longer(c(deaths, pred)) %>%
            mutate(name=ifelse(name=="deaths", "Actual", "Predicted")) %>%
            ggplot(aes(x=week)) + 
            geom_line(aes(y=value, color=name, group=name)) + 
            ylim(c(0, NA)) + 
            facet_wrap(~year) + 
            labs(x="Week", 
                 y="Deaths", 
                 title=paste0("All-cause deaths for ", cohortName, " cohort"), 
                 subtitle=paste0("Predictions are based on a simple linear model for ", prevYears, " data")
            ) + 
            scale_color_discrete("Deaths")
        if (!predActualPlotsOnePage) print(p2)
    }
    
    if (showActualsvsPredRatio) {
        # Plot of predictions and actuals facetted by year - ratios
        p3 <- df %>%
            mutate(rat=deaths/pred) %>%
            ggplot(aes(x=week)) + 
            geom_line(aes(y=rat)) + 
            facet_wrap(~year) + 
            geom_hline(yintercept=1, lty=2) +
            labs(x="Week", 
                 y="Ratio (Actual vs. Predicted Deaths)", 
                 title=paste0("Actual vs. predicted deaths for ", cohortName, " cohort"), 
                 subtitle=paste0("Predictions are based on a simple linear model for ", prevYears, " data")
            )
        if (!predActualPlotsOnePage) print(p3)
    }
    
    if (showExcessDeaths) {
        # Plot of excess deaths after a starting week
        p4 <- df %>%
            select(-cumDelta, -cumPred) %>%
            filter(week >= startWeek) %>%
            group_by(year) %>%
            arrange(week) %>%
            mutate(cumDelta=cumsum(delta)) %>%
            ggplot(aes(x=week)) + 
            geom_line(aes(y=cumDelta)) + 
            facet_wrap(~year) + 
            geom_hline(yintercept=0, lty=2) +
            labs(x="Week", 
                 y=paste0("Excess deaths vs. prediction (cumulative beginning week ", startWeek, ")"), 
                 title=paste0("Cumulative estimated excess deaths starting week ", 
                              startWeek, 
                              " for ", 
                              cohortName, 
                              " cohort"
                 ), 
                 subtitle=paste0("Predictions are based on a simple linear model for ", prevYears, " data")
            )
        if (!predActualPlotsOnePage) print(p4)
    }
    
    # Create a one-page summary
    if (predActualPlotsOnePage) {
        if (!exists("p1", inherits=FALSE)) p1 <- ggplot()
        if (!exists("p2", inherits=FALSE)) p2 <- ggplot()
        if (!exists("p3", inherits=FALSE)) p3 <- ggplot()
        if (!exists("p4", inherits=FALSE)) p4 <- ggplot()
        gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)
    }
    
}



# Function to create data and plots for an aggregate (cluster in this case)
cdcAggregateSummary <- function(df, 
                                critVar, 
                                critSubsets, 
                                startWeek,
                                idVarName=critVar,
                                curYear=2020, 
                                startYear=2015,
                                subListNames=NULL,
                                critListNames=subListNames,
                                factorCritList=!is.null(critListNames),
                                popData=NULL,
                                cvDeathData=NULL,
                                showAllPlots=TRUE, 
                                showStep1Plots=showAllPlots,
                                showStep3Plots=showAllPlots
                                ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed CDC all-cause deaths data
    # critVar: the main variable supplied to critFilter
    # critSubsets: the subsets used for critVar
    # startWeek: the starting week for analysis (will assume no excess deaths prior to startWeek or curYear)
    # idVarName: name to be used for the identifying variable when excess deaths data are combined
    # curYear: current year of CDC deaths data
    # subListNames: names to be used in place of numbers for idVarName column of excessFull
    #               (NULL means use numbers)
    # critListNames: names to be used for describing the key cohorts (NULL means use numbers)
    # factorCritList: boolean, whether to factor the variable that results from critListNames
    # startYear: starting year of CDC deaths data
    # popData: population data file, unique and matching by idVarName (NULL means no population plots)
    # cvDeathData: coronavirus deaths data file, unique and matching by idVarName
    #              NULL means no plots of excess all-cause deaths vs reported coronavirus deaths
    # showAllPlots: boolean, whether to show all the plots
    # showStep1Plots: boolean, whether to show a 1-page plot summary for each element of critSubsets
    # showStep3Plots: boolean, whether to show a 1-page cross-element summary
    
    # STEP 0: Initialize list to store results for each combination of critSubsets    
    tmpList <- vector("list", length(critSubsets))
    
    # STEP 1: Run the process for each combination of critSubsets (produce plots if showStep1Plots is TRUE)
    for (ctr in seq_along(critSubsets)) {
        critUse <- list(critSubsets[[ctr]])
        names(critUse) <- critVar
        useName <- if(is.null(critListNames)) paste0("Iteration: ", ctr) else critListNames[ctr]
        tmpList[[ctr]] <- cdcCohortAnalysis(cohortName=useName, 
                                            df=df, 
                                            critFilter=critUse, 
                                            plotTitle="",
                                            curYear=curYear,
                                            startYear=startYear,
                                            startWeek=startWeek, 
                                            showSubsetPlots=FALSE, 
                                            showPredActualPlots=showStep1Plots,
                                            predActualPlotsOnePage=showStep1Plots
        )
    }
    
    # STEP 1a: Add names if requeated
    if (!is.null(subListNames)) names(tmpList) <- subListNames
    
    # STEP 2: Create the excess file
    excessFull <- map_dfr(.x=tmpList, .f=function(x) { x[["preds"]] }, .id=idVarName)
    
    # STEP 2a: Convert idVarName to factor if critListName is passed
    if (factorCritList) {
        excessFull <- excessFull %>%
            mutate(!!idVarName:=factor(get(idVarName), levels=critListNames))
    }
    
    # STEP 3: Produce a plot of excess deaths by week by idVarName
    if (showStep3Plots) {
        p1 <- excessFull %>%
            filter(year==curYear) %>%
            ggplot(aes(x=week, y=delta)) + 
            geom_line(aes_string(group=idVarName, color=idVarName)) + 
            labs(title=paste0("Actual minus predicted deaths by week (", curYear, ")"), 
                 x="Week", 
                 y="Actual minus predicted deaths"
            ) + 
            scale_color_discrete(idVarName)
        print(p1)
    }
    
    # STEP 4: Augment the data to include population (if popData is not NULL) and plot "per million"
    if (!is.null(popData)) {
        # Add population data to excessFull (only variables should be idVarName and pop)
        excessFull <- excessFull %>%
            left_join(select_at(popData, vars(all_of(c(idVarName, "pop")))), by=idVarName)
        # Plot of excess deaths per million by week by cluster
        p2 <- excessFull %>%
            filter(year==curYear) %>%
            ggplot(aes(x=week, y=1000000*delta/pop)) + 
            geom_line(aes(group=cluster, color=cluster)) + 
            labs(title=paste0("Actual minus predicted deaths by week (", curYear, ")"), 
                 subtitle="Per million people, per week",
                 x="Week", 
                 y="Actual minus predicted deaths (per million people)"
            ) + 
            scale_color_discrete(idVarName) + 
            geom_hline(aes(yintercept=0), lty=2)
        print(p2)
    }
    
    # STEP 5: Augment the data to show plot of excess all-cause deaths vs. reported coronavirus deaths
    if (!is.null(cvDeathData)) {
        # Add the reported coronavirus deaths total
        excessFull <- excessFull %>%
            left_join(select_at(cvDeathData, vars(all_of(c(idVarName, "week", "cvDeaths")))), 
                      by=c(idVarName, "week")
            ) %>%
            mutate(cvDeaths=ifelse(is.na(cvDeaths) | year != curYear, 0, cvDeaths))
        # Plot for total excess deaths and reported coronavirus deaths by idVarName by week
        p3 <- excessFull %>%
            filter(year==curYear) %>%
            select_at(vars(all_of(c(idVarName, "week", "cvDeaths", "delta")))) %>%
            pivot_longer(c(cvDeaths, delta), names_to="source", values_to="deaths") %>%
            ggplot(aes(x=week, y=deaths)) + 
            geom_line(aes(group=source, 
                          color=c("cvDeaths"="Reported COVID", "delta"="Excess All-Cause")[source])) + 
            facet_wrap(~get(idVarName)) + 
            scale_color_discrete("Deaths") + 
            labs(x="Week", 
                 y="Deaths", 
                 title=paste0("Deaths by ", idVarName, " by week"))
        print(p3)
    }
    
    # STEP n: Return the excess file
    excessFull
    
}



# Create plots for state-level excess deaths
helperKeyStateExcessPlots <- function(df, 
                                      epiMonth,
                                      cvDeaths,
                                      subT,
                                      startWeek,
                                      cvDeathDate,
                                      curYear=2020, 
                                      popData=select(usmap::statepop, state=abbr, pop=pop_2015)
                                      ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed state-level excess deaths data
    # epiMonth: a mapping file with columns ew-month-quarter that maps an epiweek to a month/quarter
    # cvDeaths: a file containing cv deaths that includes state-date-name-vpm
    # subT: the subtitle to be used for the plots 9describe the vintage of the CDC data)
    # startWeek: the starting week for the analysis (only data in year==curYear with week>=startWeek used)
    # cvDeathDate: the date to use for puling deaths from the cvDeaths file
    # curYear: the current year
    # popData: a file containing only state-pop, with population by state
    
    # STEP 0: Create a general plotting database with quarter and month
    plotData <- df %>%
        filter(year==curYear) %>%
        left_join(epiMonth, by=c("week"="ew")) %>%
        mutate(quarter=factor(paste0("Q", quarter, "-", curYear)), 
               postStart=ifelse(week>=startWeek, 1, 0)
        ) %>%
        group_by(state, quarter, month, postStart) %>%
        summarize(excess=sum(delta)) %>%
        ungroup() %>%
        left_join(popData, by="state") %>%
        mutate(excesspm=excess*1000000/pop)
    
    # STEP 1: Plot of excess deaths by quarter
    p1 <- plotData %>%
        group_by(state, quarter) %>%
        summarize(excess=sum(excess)) %>%
        ggplot(aes(x=fct_reorder(state, excess, .fun=sum), y=excess/1000, fill=fct_rev(quarter))) + 
        geom_col(position="stack") + 
        coord_flip() + 
        labs(x="State", 
             y="Excess Deaths (000s)", 
             title=paste0("All-cause excess deaths in ", curYear), 
             subtitle=subT
        ) +
        scale_fill_discrete("Quarter")
    print(p1)
    
    # STEP 2: Plot of excess deaths per million by state by quarter
    p2 <- plotData %>%
        group_by(state, quarter) %>%
        summarize(excesspm=sum(excesspm)) %>%
        ggplot(aes(x=fct_reorder(state, excesspm, .fun=sum), y=excesspm, fill=fct_rev(quarter))) + 
        geom_col(position="stack") + 
        coord_flip() + 
        labs(x="State", 
             y="Excess Deaths (per million people)", 
             title=paste0("All-cause excess deaths per million people in ", curYear), 
             subtitle=subT
        ) +
        scale_fill_discrete("Quarter")
    print(p2)
    
    # STEP 3: Plot of excess deaths by epi-month
    pes_001 <- plotData %>%
        filter(postStart==1) %>%
        ggplot(aes(x=fct_reorder(state, excesspm, .fun=sum))) + 
        geom_col(aes(y=excesspm, fill=fct_rev(month)), position="stack") + 
        coord_flip() + 
        labs(x="State", 
             y="Excess Deaths (per million people)", 
             title="All-cause excess deaths per million people in 2020", 
             subtitle=subT, 
             caption="Epi week converted to month based on most frequent month in epi week"
        ) +
        scale_fill_discrete("Epi Month")
    print(pes_001)
    
    # STEP 4: Layer on the reported coronavirus deaths data
    cumDeath <- cvDeaths %>% 
        filter(name=="deaths", state != "cluster") %>% 
        group_by(state) %>% 
        mutate(cumdpm=cumsum(vpm)) %>%
        filter(date %in% c(cvDeathDate, max(date)))
    pes_002 <- pes_001 + 
        geom_point(data=filter(cumDeath, date==cvDeathDate), aes(x=state, y=cumdpm), size=2) + 
        labs(title=paste0("Deaths per million people using data through ", format(cvDeathDate, "%b %d, %Y")), 
             subtitle="Bars based on excess CDC all-cause deaths, points are reported coronavirus deaths", 
             y="Deaths (per million people)"
        )
    print(pes_002)
    
    # STEP 5: Show ratio of estimated all-cause deaths to reported coronavirus deaths
    p5 <- plotData %>%
        filter(postStart==1) %>%
        group_by(state) %>%
        summarize(excesspm=sum(excesspm)) %>%
        inner_join(select(filter(cumDeath, date==cvDeathDate), state, cumdpm)) %>%
        mutate(rat=excesspm/cumdpm) %>%
        ggplot(aes(x=fct_reorder(state, rat), y=rat)) + 
        geom_col(fill="lightblue") + 
        geom_text(aes(y=rat/2, label=paste0(round(100*rat), "%"))) + 
        coord_flip() + 
        labs(x="State", 
             y="Ratio of estimated all-cause excess deaths to reported coronavirus deaths", 
             title="Ratio of estimated all-cause excess deaths to reported coronavirus deaths", 
             subtitle=paste0("All data through ", format(cvDeathDate, "%b %d, %Y"))
        ) + 
        ylim(c(0, NA)) + 
        geom_hline(aes(yintercept=1), lty=2)
    print(p5)
    
    # STEP n: Return the plotting data
    plotData
    
}



# Create plots for age-cohort excess deaths
helperKeyAgeExcessPlots <- function(df, 
                                    epiMonth,
                                    cvDeaths,
                                    popData,
                                    subT,
                                    startWeek,
                                    cvDeathDate,
                                    curYear=2020
                                    ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed state-level excess deaths data
    # epiMonth: a mapping file with columns ew-month-quarter that maps an epiweek to a month/quarter
    # cvDeaths: a file containing cv deaths that includes state-date-name-vpm
    # popData: a file containing only age-pop, with population by age cohort
    # subT: the subtitle to be used for the plots 9describe the vintage of the CDC data)
    # startWeek: the starting week for the analysis (only data in year==curYear with week>=startWeek used)
    # cvDeathDate: the date to use for puling deaths from the cvDeaths file
    # curYear: the current year
    
    # STEP 0: Create a general plotting database with quarter and month
    plotData <- df %>%
        filter(year==curYear) %>%
        left_join(epiMonth, by=c("week"="ew")) %>%
        mutate(quarter=factor(paste0("Q", quarter, "-", curYear)), 
               postStart=ifelse(week>=startWeek, 1, 0)
        ) %>%
        group_by(age, quarter, month, postStart) %>%
        summarize(excess=sum(delta)) %>%
        ungroup() %>%
        left_join(popData, by="age") %>%
        mutate(excesspm=excess*1000000/pop)
    
    # STEP 1: Plot of excess deaths by quarter
    p1 <- plotData %>%
        group_by(age, quarter) %>%
        summarize(excess=sum(excess)) %>%
        ggplot(aes(x=fct_reorder(age, excess, .fun=sum), y=excess/1000, fill=fct_rev(quarter))) + 
        geom_col(position="stack") + 
        coord_flip() + 
        labs(x="Age Group", 
             y="Excess Deaths (000s)", 
             title="All-cause excess deaths in 2020", 
             subtitle=subT
        ) +
        scale_fill_discrete("Quarter")
    print(p1)
    
    # STEP 2: Plot of excess deaths by epi-month
    pea_001 <- plotData %>%
        filter(postStart==1) %>%
        ggplot(aes(x=fct_reorder(age, excesspm, .fun=sum))) + 
        geom_col(aes(y=excesspm, fill=fct_rev(month)), position="stack") + 
        coord_flip() + 
        labs(x="Age Group", 
             y="Excess Deaths (per million people)", 
             title="All-cause excess deaths per million in 2020", 
             subtitle=subT, 
             caption="Epi week converted to month based on most frequent month in epi week"
        ) +
        geom_text(data=. %>% group_by(age) %>% summarize(excesspm=sum(excesspm)), 
                  aes(y=excesspm+100, label=round(excesspm), hjust=0)
        ) +
        scale_fill_discrete("Epi Month")
    print(pea_001)
    
    # STEP 3: Return the plotting data
    plotData
    
}

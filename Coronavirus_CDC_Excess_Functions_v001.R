# File to be sourced for working with CDC all-cause deaths data

# 0. cdcExcessParams - list of parameters for CDC excess all-cause deaths processing
# 1. readRunCDCAllCause() - main function
# 2. readProcessCDC() - read and process CDC all-cause deaths data
# 3. helperKeyStateClusterMetrics() - converts processed CDC coronavirus deaths to be unique by cluster-year-week
# 4. cdcBasicPlots() - EDA plots for processed CDC all-cause deaths data across several dimensions
# 5. subsetCDC() - sum and plot deaths for a subset of the CDC all-cause deaths data
# 6. cdcRegression() - linear model for deaths by week for baseline historical data in a CDC subset
# 7. cdcPrediction() - application of linear model to create expected deaths by week
# 8. cdcPredictedvsActual() - plots for predicted and actual deaths by cohort over time
# 9. cdcCohortAnalysis() - combines subsetCDC(), cdcRegression(), cdcPrediction(), and CDCPredictedvsActual()
# 10. cdcAggregateSummary() - runs cdcCohortAnalysis(), plots excess deaths, returns data frame
# 11. helperKeyStateExcessPlots() - excess deaths data plotting (state-level cohorts)
# 12. helperKeyAgeExcessPlots() - excess deaths data plotting (age-level cohorts)
# 13. excessDeathFacets() - facets for excess deaths by age-group and COVID vs. non-COVID
# 14. makeCumulativeDeath() - create plots for cumulative COVID and excess non-COVID deaths
# 15. deathTrends() - create trend data and calculate excess deaths
# 16. analyzeAllCause() - primary function for analyzing death by age-location-cause data
# 17. exploreDeathPlace() - analyze place-of-death data
# 18. plotDeathDetails() - analyze deaths by age and place
# 19. compareCDCDeaths() - compare deaths by state by all-cause data source
# 20. processAllCauseLocation() - process the all-cause deaths by age-location-cause data
# 21. plotAgeWeekDeath() - plot deaths vs. trend by age and week
# 22. getDiffTrend() - calculate differences in deaths from linear model trend
# 23. tempGetReg() - run regression on trend data and extract coefficients
# 24. compareTotals() - plot totals vs. sums of sub-totals
# 25. checkSubTotals() - create data frame for totals and sums of sub-totals
# 26. plotQCReadProcessCDC() - plots and quality checks on processed CDC all-cause deaths data
# 27. checkCDCSuppression() - check for excessive suppression in CDC all-cause deaths data

# Parameters for use in processing
cdcExcessParams <- list(remapVars=c('Jurisdiction'='fullState', 
                                    'Week Ending Date'='weekEnding', 
                                    'State Abbreviation'='state', 
                                    'Age Group'='age', 
                                    'Number of Deaths'='deaths', 
                                    'Time Period'='period', 
                                    'Year'='year', 
                                    'Week'='week'
                                    ),
                        colTypes="ccciicdcccc",
                        ageLevels=c("Under 25 years", 
                                    "25-44 years", 
                                    "45-64 years", 
                                    "65-74 years", 
                                    "75-84 years", 
                                    "85 years and older"
                                    ),
                        periodLevels=c("2015-2019", "2020", "2021"),
                        periodKeep=c("2015-2019", "2020"),
                        yearLevels=2015:2021
                        )



# Function to read and run the CDC all-cause deaths analysis
# Function to read and run the CDC all-cause deaths analysis
readRunCDCAllCause <- function(loc, 
                               weekThru, 
                               lst, 
                               startYear=cdcExcessParams$yearLevels[1], 
                               curYear=cdcExcessParams$yearLevels[length(cdcExcessParams$yearLevels)],
                               startWeek=1, 
                               cvDeathThru=NULL,
                               epiMap=readFromRDS("epiMonth"),
                               agePopData=readFromRDS("usPopBucket2020"),
                               cdcPlotStartWeek=startWeek,
                               periodKeep=cdcExcessParams$periodKeep, 
                               dir="./RInputFiles/Coronavirus/", 
                               url="https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD",
                               pdfStem="./RInputFiles/Coronavirus/Plots/CDC_",
                               pdfCluster=NULL,
                               pdfAge=NULL,
                               dlData=isFALSE(file.exists(paste0(dir, loc))), 
                               stateNoCheck=c()
                               ) {
    
    # FUNCTION ARGUMENTS:
    # loc: the CDC .csv file name (without path)
    # weekThru: how many weeks of the current year are the data valid thru?
    # lst: a state clustering process output list
    # startYear: the starting year in the CDC data
    # curYear: the current analyis year in the CDC data
    # startWeek: the starting week to use for cumulative sum of difference in expected all-cause deaths
    # cvDeathThru: the date to use for pulling the CV death data (NULL means infer from curYear and startWeek)    
    # epiMap: a mapping file of ew-month-quarter that mas epiweek (ew) to an appropriate month and quarter
    # agePopData: data containing US population as age (fct) - pop (int)
    # cdcPlotStartWeek: start week for CDC plots (10 is March which avoids a 1-week February outlier)
    # periodKeep: the period of previous data in the CDC all-cause deaths file (should be kept regardless)
    # dir: the CDC .csv directory (will use paste0(dir, loc) as the file location)
    # url: the url for downloading CDC data
    # pdfStem: the base location for PDF files, used to auto-create pdfCluster and pdfAge
    # pdfCluster: location to place the PDF for plots for clustering
    #             TRUE means create from pdfStem and NULL means do not create
    # pdfAge: location to place the PDF for plots for age
    #         TRUE means create from pdfStem and NULL means do not create
    # dlData: should CDC data be downloaded?  default is to not overwrite an existing file
    # stateNoCheck: vector of states that should not error out for failing suppression checks
    
    # STEP 0: Download CDC data if requested
    if (dlData) fileDownload(fileName=paste0(dir, loc), url=url)
    
    # Get cvDeathDate if it has been passed as NULL
    if (is.null(cvDeathThru)) {
        cvDeathThru <- tibble::tibble(date=seq.Date(as.Date(paste0(max(curYear), "-01-01")), 
                                                    as.Date(paste0(max(curYear), "-12-31")), 
                                                    by="days"
        ), 
        epiWeek=lubridate::epiweek(date)
        ) %>%
            filter(epiWeek==weekThru) %>%
            pull(date) %>%
            max() %>%
            as.character()
        cat("\nParameter cvDeathThru has been set as:", cvDeathThru, "\n")
    }
    
    # Create pdfCluster and pdfAge
    if (isTRUE(pdfCluster)) 
        pdfCluster <- paste0(pdfStem, "cluster_", max(curYear), "w", zeroPad2(weekThru), ".pdf")
    if (isTRUE(pdfAge)) 
        pdfAge <- paste0(pdfStem, "age_", max(curYear), "w", zeroPad2(weekThru), ".pdf")
    
    # STEP 1: Read and process the CDC data
    cdc <- readProcessCDC(loc, weekThru=weekThru, periodKeep=periodKeep, fDir=dir, stateNoCheck=stateNoCheck)
    
    # STEP 2: Create the key data required for using state-level clusters
    clusterList <- helperKeyStateClusterMetrics(lst)
    
    # STEP 3: Generate plots of the processed CDC data
    cdcBasicPlots(cdc, clustVec=clusterList$clData, stateExclude=stateNoCheck, p5Years=2019:curYear)
    
    # Exclude stateNoCheck states
    cat("\nPlots will be run after excluding stateNoCheck states\n")
    cdcUse <- cdc %>% filter(!(state %in% stateNoCheck))
    
    # STEP 4: Full US excess deaths
    list_allUS <- cdcCohortAnalysis(cohortName="all US", 
                                    df=cdcUse,
                                    curYear=2020:curYear, 
                                    startYear=startYear,
                                    startWeek=startWeek,
                                    plotTitle="All-cause US total deaths",
                                    predActualPlotsOnePage=TRUE
    )
    # cat("\n\n\n*** THROUGH STEP 4 ***\n\n\n")
    
    # STEP 5: Generate cluster-level aggregate plots
    clNames <- clusterList$useClusters %>% unique()
    clValues <- lapply(clNames, FUN=function(x) names(clusterList$useClusters)[clusterList$useClusters==x])
    clusterAgg <- cdcAggregateSummary(df=cdcUse, 
                                      curYear=2020:curYear,
                                      critVar="state", 
                                      critSubsets=clValues,
                                      startWeek=startWeek, 
                                      subListNames=clNames,
                                      critListNames=paste0("cluster ", clNames),
                                      factorCritList=FALSE,
                                      popData=clusterList$deaths %>% 
                                          group_by(cluster) %>% 
                                          summarize(pop=max(pop), .groups="drop"),
                                      cvDeathData=clusterList$deaths %>% 
                                          select(cluster, year, week, cvDeaths=new_deaths),
                                      idVarName="cluster", 
                                      pdfFile=pdfCluster, 
                                      showAllPlots=TRUE
    )
    # cat("\n\n\n*** THROUGH STEP 5 ***\n\n\n")
    # print(clusterAgg)
    # print(cdcUse)
    # print(clusterList$useClusters)
    
    # STEP 6: Generate state-level aggregate data, then plot
    stateAgg <- cdcAggregateSummary(df=cdcUse, 
                                    curYear=2020:curYear,
                                    critVar="state", 
                                    critSubsets=setdiff(names(clusterList$useClusters), stateNoCheck),
                                    startWeek=startWeek, 
                                    idVarName="state", 
                                    subListNames=setdiff(names(clusterList$useClusters), stateNoCheck),
                                    showAllPlots=FALSE
    )
    # cat("\n\n\n*** THROUGH STEP 6a ***\n\n\n")
    # print(stateAgg)
    helperKeyStateExcessPlots(df=stateAgg, 
                              epiMonth=epiMap,
                              cvDeaths=lst$plotDataList$dfFull %>% 
                                  select(state, date, vpm=dpm) %>% 
                                  mutate(name="deaths") %>% 
                                  filter(!is.na(vpm)),
                              startWeek=cdcPlotStartWeek,
                              cvDeathDate=as.Date(cvDeathThru),
                              subT=paste0("CDC data through week ", 
                                          weekThru, 
                                          " of ", 
                                          max(curYear)
                              ), 
                              curYear=2020:curYear, 
                              popData=readFromRDS("statePop2019") %>% 
                                  select(state=stateAbb, pop=pop_2019)
    )
    # cat("\n\n\n*** THROUGH STEP 6b ***\n\n\n")
    
    # STEP 7: Generate age-level aggregate data, then plot
    ageAgg <- cdcAggregateSummary(df=cdcUse, 
                                  curYear=2020:curYear,
                                  critVar="age", 
                                  critSubsets=levels(cdc$age),
                                  startWeek=startWeek, 
                                  idVarName="age", 
                                  subListNames=levels(cdc$age),
                                  showAllPlots=TRUE, 
                                  pdfFile=pdfAge
    )
    helperKeyAgeExcessPlots(df=ageAgg, 
                            epiMonth=epiMap,
                            popData=agePopData,
                            startWeek=cdcPlotStartWeek,
                            subT=paste0("CDC data through week ", 
                                        weekThru, 
                                        " of ", 
                                        max(curYear)
                            ), 
                            curYear=2020:curYear
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
                           periodKeep=cdcExcessParams$periodKeep,
                           fDir="./RInputFiles/Coronavirus/",
                           col_types=cdcExcessParams$colTypes, 
                           renameVars=cdcExcessParams$remapVars,
                           maxSuppressAllowed=20, 
                           stateNoCheck=c()
) {
    
    # FUNCTION ARGUMENTS:
    # fName: name of the downloaded CDC data file
    # weekThru: any record where week is less than or equal to weekThru will be kept
    # periodKeep: any record where period is in periodKeep will be kept
    # fDir: directory name for the downloaded CDC data file
    # col_types: variable type by column in the CDC data (passed to readr::read_csv())
    # renameVars: named vector for variable renaming of type c("Existing Name"="New Name")
    # maxSuppressAllowed: maximum number of data suppressions (must be in current week/year) to avoid error
    # stateNoCheck: vector of states that do NOT have suppression errors thrown
    
    # STEP 1: Read the CSV data
    cdcRaw <- fileRead(paste0(fDir, fName), col_types=col_types)
    # glimpse(cdcRaw)
    
    # STEP 2: Rename the variables for easier interpretation
    cdcRenamed <- cdcRaw %>%
        colRenamer(vecRename=renameVars) %>%
        colMutater(selfList=list("weekEnding"=lubridate::mdy))
    # glimpse(cdcRenamed)
    
    # STEP 3: Convert to factored data
    cdcFactored <- cdcRenamed %>%
        colMutater(selfList=list("age"=factor), levels=cdcExcessParams$ageLevels) %>%
        colMutater(selfList=list("period"=factor), levels=cdcExcessParams$periodLevels) %>%
        colMutater(selfList=list("year"=factor), levels=cdcExcessParams$yearLevels)
    # glimpse(cdcFactored)
    
    # STEP 4: Filter the data to include only weighted deaths and only through the desired time period
    cdcFiltered <- cdcFactored %>%
        rowFilter(lstFilter=list("Type"="Predicted (weighted)")) %>%
        filter(period %in% all_of(periodKeep) | week <= weekThru)
    # glimpse(cdcFiltered)
    
    # STEP 4a: Check that all suppressed data and NA deaths have been eliminated
    cat("\n\n *** Data suppression checks *** \n")
    checkCDCSuppression(cdcFiltered, stateNoCheck=stateNoCheck, errTotAllowed=maxSuppressAllowed)
    cat("\n\nData suppression checks passed\n\n")
    
    # STEP 5: Remove any NA death fields, delete the US record, convert YC to be part of NY
    cdcProcessed <- cdcFiltered %>%
        rowFilter(lstExclude=list("state"=c("US", "PR"), "deaths"=c(NA))) %>%
        mutate(state=ifelse(state=="YC", "NY", state), 
               fullState=ifelse(state %in% c("NY", "YC"), "New York State (NY plus YC)", fullState)
        ) %>%
        group_by(fullState, weekEnding, state, year, week, age, period, Type, Suppress) %>%
        arrange(!is.na(Note)) %>%
        summarize(n=n(), deaths=sum(deaths), Note=first(Note), .groups="drop") %>%
        ungroup() %>%
        checkUniqueRows(uniqueBy=c("state", "year", "week", "age"))
    glimpse(cdcProcessed)
    
    # STEP 5a: Check control levels for key variables in processed file
    cat("\nCheck Control Levels and Record Counts for Processed Data:\n")
    plotQCReadProcessCDC(cdcProcessed)
    
    # STEP 6: Return the processed data file
    cdcProcessed
    
}



# Convert the output of a state-level clustering list to population, membership, and deaths
helperKeyStateClusterMetrics <- function(lst) {
    
    # FUNCTION ARGUMENTS:
    # lst: the list containing the outputs of the state clustering routing
    
    # Extract the relevant elements from lst
    plotClusters <- lst[["plotDataList"]][["plotClusters"]]
    dfFull <- lst[["plotDataList"]][["dfFull"]] %>% 
        select(state, date, pop, tot_deaths, new_deaths)
    
    # Create the aggregated data
    df <- joinFrames(plotClusters, dfFull, keyJoin="state")
    
    # Create reported cvDeaths by cluster
    dfWeekly <- df %>%
        mutate(year=lubridate::epiyear(date), 
               week=lubridate::epiweek(date), 
               cluster=as.character(cluster)
               ) %>%
        group_by(state, cluster, year, week) %>%
        summarize(pop=max(pop), 
                  tot_deaths=specNA(max)(tot_deaths), 
                  new_deaths=specNA(sum)(new_deaths), 
                  .groups="drop"
                  ) %>%
        group_by(cluster, year, week) %>%
        summarize(pop=sum(pop), 
                  tot_deaths=specNA(sum)(tot_deaths), 
                  new_deaths=specNA(sum)(new_deaths), 
                  .groups="drop"
                  ) %>%
        checkUniqueRows(uniqueBy=c("cluster", "year", "week"))
    
    list(deaths=dfWeekly, useClusters=lst[["useClusters"]])
    
}



# Basic plots of the CDC data, including by a state-level cluster (passed as argument)
cdcBasicPlots <- function(df, 
                          weekThru=NULL, 
                          curYear=NULL, 
                          p5Years=curYear,
                          clustVec=NULL, 
                          stateExclude=c()
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: a processed CDC data file
    # weekThru: week of the current year that data are thru (NULL means infer from data)
    # curYear: current year (NULL means infer from data)
    # p5Years: years for plotting deaths by week/cohort (defaults to only current year)
    # clustVec: clustering vector with names as state abbreviations (NULL means no plots by cluster)
    # stateExclude: vector of states to exclude from the analysis
    
    # Get the week and year if passed as NULL
    if (is.null(curYear)) { curYear <- df %>% pull(year) %>% as.character() %>% as.integer() %>% max() }
    if (is.null(weekThru)) { weekThru <- df %>% filter(year==curYear) %>% pull(week) %>% max() }
    
    # Filter to exclude stateExclude
    df <- df %>%
        filter(!(state %in% stateExclude))
    
    # Update subtitle appropriately
    subT <- NULL
    if (length(stateExclude) > 0) subT <- paste0("Excludes ", paste0(stateExclude, collapse=", "))
    
    # Plot of total deaths by year (50 states plus DC)
    p1 <- df %>%
        group_by(year) %>%
        summarize(deaths=sum(deaths), .groups="drop") %>%
        ggplot(aes(x=year, y=deaths/1000)) + 
        geom_col(fill="lightblue") + 
        geom_text(aes(y=deaths/2000, label=round(deaths/1000))) + 
        labs(title=paste0("CDC Deaths for 50 states plus DC through week ", weekThru, " of ", curYear), 
             x="", 
             y="Deaths (000s)"
             )
    if (!is.null(subT)) p1 <- p1 + labs(subtitle=subT)
    print(p1)
    
    # Plot of total deaths by week by year
    p2 <- df %>%
        group_by(year, week) %>%
        summarize(deaths=sum(deaths), .groups="drop") %>%
        ggplot(aes(x=week, y=deaths, group=year, color=year)) + 
        geom_line() +
        ylim(c(0, NA)) + 
        labs(x="Week of Year", y="Deaths", title="US deaths per week by year")
    if (!is.null(subT)) p2 <- p2 + labs(subtitle=subT)
    print(p2)
    
    # Plot of total deaths by year by age cohort
    p3 <- df %>%
        group_by(year, week, age) %>%
        summarize(deaths=sum(deaths), .groups="drop") %>%
        ggplot(aes(x=week, y=deaths, group=year, color=year)) + 
        geom_line() +
        ylim(c(0, NA)) + 
        facet_wrap(~age) +
        labs(x="Week of Year", y="Deaths", title="US deaths per week by year")
    if (!is.null(subT)) p3 <- p3 + labs(subtitle=subT)
    print(p3)
    
    # Plots of total deaths by week by year by cluster
    if (!is.null(clustVec)) {
        p4 <- df %>%
            mutate(cluster=clustVec[state]) %>%
            group_by(year, week, cluster) %>%
            summarize(deaths=sum(deaths), .groups="drop") %>%
            ggplot(aes(x=week, y=deaths, group=year, color=year)) + 
            geom_line() +
            ylim(c(0, NA)) + 
            facet_wrap(~cluster, scales="free_y") +
            labs(x="Week of Year", 
                 y="Deaths", 
                 title="US deaths per week by year"
                 )
        if (is.null(subT)) p4 <- p4 + labs(subtitle="Facetted by state cluster")
        else p4 <- p4 + labs(subtitle=paste0("Facetted by state cluster\n", subT))
        print(p4)
        
        p5 <- df %>%
            mutate(cluster=clustVec[state]) %>%
            filter(year %in% p5Years) %>%
            group_by(year, age, week, cluster) %>%
            summarize(deaths=sum(deaths), .groups="drop") %>%
            mutate(weekUse=week+53*(as.integer(as.character(year))-curYear)) %>%
            ggplot(aes(x=weekUse, y=deaths, fill=age)) + 
            geom_col(position="stack") +
            ylim(c(0, NA)) + 
            facet_wrap(~cluster, scales="free_y") +
            labs(x="Week of Year", 
                 y="Deaths", 
                 title=paste0("US deaths per week (Week 1 is start of year ", curYear, ")")
                 )
        if (is.null(subT)) p5 <- p5 + labs(subtitle="Facetted by state cluster")
        else p5 <- p5 + labs(subtitle=paste0("Facetted by state cluster\n", subT))
        print(p5)
    }
    
}



# Function to subset CDC data
subsetCDC <- function(df, 
                      critFilter=vector("list", 0), 
                      showPlot=TRUE, 
                      plotTitle=NULL, 
                      curYear=2020:2021, 
                      prevYears=paste0("2015-", min(curYear)-1)
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
        summarize(deaths=sum(deaths), .groups="drop") %>%
        mutate(weekfct=factor(week), yearint=as.integer(as.character(year)))
    
    # Show plots if requested
    if (showPlot) {
        # Plot of all deaths for the filtered cohort by week and year
        p1 <- allDeath %>%
            ggplot(aes(x=week, y=deaths, color=year, group=year)) + 
            geom_line() + 
            ylim(c(0, NA)) + 
            labs(x="Week", y="Deaths", title=plotTitle)
        print(p1)
        
        # Plot of mean and standard deviation from previous years
        p2 <- allDeath %>%
            filter(!(year %in% curYear)) %>%
            group_by(week) %>%
            summarize(mean=mean(deaths), sd=sd(deaths), .groups="drop") %>%
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



# Function to run the linear model to predict CDC deaths by week and year
cdcRegression <- function(df, 
                          curYear=2020:2021, 
                          startYear=2015
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed CDC data
    # curYear: the current year(s)
    # startYear: the year that is defined as the zero-point for the regression
    
    # Very basic linear model for deaths vs. week and year
    df %>%
        filter(!(year %in% curYear)) %>%
        lm(deaths ~ weekfct + I(yearint-startYear) + 0, data=.)
    
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
        mutate(oldweekfct=weekfct, weekfct=factor(ifelse(weekfct==53, 52, weekfct))) %>%
        mutate(pred=predict(lmReg, newdata=.), delta=(deaths-pred)) %>%
        group_by(year) %>%
        arrange(week) %>%
        mutate(cumDelta=cumsum(delta), cumPred=cumsum(pred), weekfct=oldweekfct) %>%
        select(-oldweekfct) %>%
        ungroup()
    
    # Return the predictions and deltas    
    allPred
    
}



# Function to make plots of the predictions
cdcPredictedvsActual <- function(df, 
                                 cohortName,
                                 curYear=2020:2021, 
                                 prevYears=paste0("2015-", min(curYear)-1), 
                                 startWeek=1, 
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
            geom_line(data=~filter(., !(year %in% curYear)), aes(y=deaths, color=year, group=year)) + 
            geom_line(data=~filter(., year %in% curYear), aes(y=pred, group=year, color=year), lty=2, size=1) +
            ylim(c(0, NA)) + 
            labs(x="Week", 
                 y="Deaths", 
                 title=paste0("All-cause deaths for ", cohortName, " cohort"), 
                 subtitle=paste0("Solid lines are actual ", 
                                 prevYears, 
                                 ", dashed line is predicted ", 
                                 if(length(curYear)>1) range(curYear) %>% paste0(collapse="-") else curYear
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
                 subtitle=paste0("Predictions based on simple linear model for ", prevYears, " data")
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
                 subtitle=paste0("Predictions based on simple linear model for ", prevYears, " data")
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
                 y=paste0("Deaths vs. prediction (cumulative from week ", startWeek, ")"), 
                 title=paste0("Cumulative estimated excess deaths starting week ", 
                              startWeek, 
                              "\nfor ", 
                              cohortName, 
                              " cohort"
                              ), 
                 subtitle=paste0("Predictions based on simple linear model for ", prevYears, " data")
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



# Integrated function with all steps
cdcCohortAnalysis <- function(cohortName, 
                              df=cdcProcessed, 
                              critFilter=vector("list", 0), 
                              plotTitle=NULL, 
                              curYear=2020:2021, 
                              startYear=2015,
                              prevYears=paste0(startYear, "-", min(curYear)-1), 
                              startWeek=1, 
                              showSubsetPlots=TRUE, 
                              showPredActualPlots=TRUE, 
                              predActualPlotsOnePage=FALSE
                              ) {
    
    # FUNCTION ARGUMENTS:
    # cohortName: the name of the cohort
    # df: the processed CDC death data
    # critFilter: the filtering criteria in the form of a named list "variable"=c("possibleValues")
    # plotTitle: title to be used for plot in subsetCDC()
    # curYear: the current year(s) for the death data
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



# Function to create data and plots for an aggregate (cluster in this case)
cdcAggregateSummary <- function(df, 
                                critVar, 
                                critSubsets, 
                                startWeek,
                                idVarName=critVar,
                                curYear=2020:2021, 
                                startYear=2015,
                                subListNames=NULL,
                                critListNames=subListNames,
                                factorCritList=!is.null(critListNames),
                                popData=NULL,
                                cvDeathData=NULL,
                                showAllPlots=TRUE, 
                                showStep1Plots=showAllPlots,
                                showStep3Plots=showAllPlots, 
                                pdfFile=NULL
                                ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed CDC all-cause deaths data
    # critVar: the main variable supplied to critFilter
    # critSubsets: the subsets used for critVar
    # startWeek: the starting week for analysis (will assume no excess deaths prior to startWeek or curYear)
    # idVarName: name to be used for the identifying variable when excess deaths data are combined
    # curYear: current year(s) of CDC deaths data
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
    # pdfFile: a path for the detailed step 1 plots to be stored as PDF (NULL means store in the log)
    
    # STEP 0: Initialize list to store results for each combination of critSubsets    
    tmpList <- vector("list", length(critSubsets))
    
    # STEP 1: Run the process for each combination of critSubsets (produce plots if showStep1Plots is TRUE)
    # Redirect detailed plots to PDF if requested
    if (!is.null(pdfFile)) {
        cat("\nDetailed", idVarName, "summary PDF file is available at:", pdfFile, "\n")
        pdf(pdfFile, width=11)
    }
    # Run the detailed sumaries for each level
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
    # Redirect all other output to the main log
    if (!is.null(pdfFile)) {
        cat("\nReturning plot outputs to the main log file\n")
        dev.off()
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
            filter(year %in% curYear) %>%
            ggplot(aes(x=week, y=delta)) + 
            geom_line(aes_string(group=idVarName, color=idVarName)) + 
            labs(title=paste0("Actual minus predicted deaths by week (", 
                              if(length(curYear)>1) range(curYear) %>% paste0(collapse="-") else curYear, 
                              ")"
                              ), 
                 x="Week", 
                 y="Actual minus predicted deaths"
                 ) + 
            facet_wrap(~year) +
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
            filter(year %in% curYear) %>%
            ggplot(aes(x=week, y=1000000*delta/pop)) + 
            geom_line(aes(group=cluster, color=cluster)) + 
            labs(title=paste0("Actual minus predicted deaths by week (", 
                              if(length(curYear)>1) range(curYear) %>% paste0(collapse="-") else curYear, 
                              ")"
                              ), 
                 subtitle="Per million people, per week",
                 x="Week", 
                 y="Actual minus predicted deaths (per million people)"
                 ) + 
            scale_color_discrete(idVarName) + 
            facet_wrap(~year) +
            geom_hline(aes(yintercept=0), lty=2)
        print(p2)
    }
    
    # STEP 5: Augment the data to show plot of excess all-cause deaths vs. reported coronavirus deaths
    if (!is.null(cvDeathData)) {
        # Add the reported coronavirus deaths total
        excessFull <- excessFull %>%
            left_join(select_at(cvDeathData, vars(all_of(c(idVarName, "year", "week", "cvDeaths")))), 
                      by=c(idVarName, "yearint"="year", "week")
                      ) %>%
            mutate(cvDeaths=ifelse(is.na(cvDeaths) | !(year %in% curYear), 0, cvDeaths))
        # Plot for total excess deaths and reported coronavirus deaths by idVarName by week
        p3 <- excessFull %>%
            filter(year %in% curYear) %>%
            mutate(useWeek=week+53*(yearint-min(curYear))) %>%
            select_at(vars(all_of(c(idVarName, "year", "week", "useWeek", "cvDeaths", "delta")))) %>%
            pivot_longer(c(cvDeaths, delta), names_to="source", values_to="deaths") %>%
            ggplot(aes(x=useWeek, y=deaths)) + 
            geom_line(aes(group=source, 
                          color=c("cvDeaths"="Reported COVID", "delta"="Excess All-Cause")[source]
                          )
                      ) + 
            facet_wrap(~get(idVarName)) + 
            scale_color_discrete("Deaths") + 
            labs(x="Week", 
                 y="Deaths", 
                 title=paste0("Deaths by ", idVarName, " by week"), 
                 subtitle=paste0("Week 1 is the first epi week of ", min(curYear))
                 )
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
                                      curYear=2020:2021, 
                                      popData=select(usmap::statepop, state=abbr, pop=pop_2015)
                                      ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed state-level excess deaths data
    # epiMonth: a mapping file with columns ew-month-quarter that maps an epiweek to a month/quarter
    # cvDeaths: a file containing cv deaths that includes state-date-name-vpm
    # subT: the subtitle to be used for the plots 9describe the vintage of the CDC data)
    # startWeek: the starting week for the analysis (data from min(curYear) is deleted if week < startWeek)
    # cvDeathDate: the date to use for puling deaths from the cvDeaths file
    # curYear: the current year(s)
    # popData: a file containing only state-pop, with population by state
    
    # STEP 0: Create a general plotting database with quarter and month
    plotData <- df %>%
        filter(year %in% curYear) %>%
        left_join(epiMonth, by=c("week"="ew")) %>%
        mutate(quarter=factor(paste0(year, "-", "Q", quarter)), 
               postStart=ifelse(week>=startWeek | yearint>min(curYear), 1, 0)
               ) %>%
        group_by(state, yearint, quarter, month, postStart) %>%
        summarize(excess=sum(delta), .groups="drop") %>%
        left_join(popData, by="state") %>%
        mutate(excesspm=excess*1000000/pop)
    
    # STEP 1: Plot of excess deaths by quarter
    p1 <- plotData %>%
        group_by(state, quarter) %>%
        summarize(excess=sum(excess), .groups="drop") %>%
        ggplot(aes(x=fct_reorder(state, excess, .fun=sum), y=excess/1000, fill=fct_rev(quarter))) + 
        geom_col(position="stack") + 
        coord_flip() + 
        labs(x="State", 
             y="Excess Deaths (000s)", 
             title=paste0("All-cause excess deaths in ", 
                          if(length(curYear)==1) curYear else paste0(curYear, collapse="-")
                          ), 
             subtitle=subT
             ) +
        scale_fill_discrete("Quarter")
    print(p1)
    
    # STEP 2: Plot of excess deaths per million by state by quarter
    p2 <- plotData %>%
        group_by(state, quarter) %>%
        summarize(excesspm=sum(excesspm), .groups="drop") %>%
        ggplot(aes(x=fct_reorder(state, excesspm, .fun=sum), y=excesspm, fill=fct_rev(quarter))) + 
        geom_col(position="stack") + 
        coord_flip() + 
        labs(x="State", 
             y="Excess Deaths (per million people)", 
             title=paste0("All-cause excess deaths per million people in ", 
                          if(length(curYear)==1) curYear else paste0(curYear, collapse="-")
                          ), 
             subtitle=subT
             ) +
        scale_fill_discrete("Quarter")
    print(p2)
    
    # STEP 3: Plot of excess deaths by epi-month
    pes_001 <- plotData %>%
        filter(postStart==1) %>%
        ggplot(aes(x=fct_reorder(state, excesspm, .fun=sum))) + 
        geom_col(aes(y=excesspm, 
                     fill=fct_rev(paste0(yearint, "-", match(month, month.abb) %>% zeroPad2()))
                     ), 
                 position="stack"
                 ) + 
        coord_flip() + 
        labs(x="State", 
             y="Excess Deaths (per million people)", 
             title=paste0("All-cause excess deaths per million people in ", 
                          if(length(curYear)==1) curYear else paste0(curYear, collapse="-")
                          ), 
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
        geom_point(data=filter(cumDeath, date==cvDeathDate), 
                   aes(x=fct_reorder(state, cumdpm), y=cumdpm), size=2
                   ) + 
        labs(title=paste0("Deaths per million people using data through ", 
                          format(cvDeathDate, "%b %d, %Y")
                          ), 
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
                                    popData,
                                    subT,
                                    startWeek,
                                    curYear=2020:2021
                                    ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the processed state-level excess deaths data
    # epiMonth: a mapping file with columns ew-month-quarter that maps an epiweek to a month/quarter
    # popData: a file containing only age-pop, with population by age cohort
    # subT: the subtitle to be used for the plots 9describe the vintage of the CDC data)
    # startWeek: the starting week for the analysis (only data in year==curYear with week>=startWeek used)
    # curYear: the current year
    
    # STEP 0: Create a general plotting database with quarter and month
    plotData <- df %>%
        filter(year %in% curYear) %>%
        left_join(epiMonth, by=c("week"="ew")) %>%
        mutate(quarter=factor(paste0(year, "-", "Q", quarter)), 
               postStart=ifelse(week>=startWeek | yearint>min(curYear), 1, 0)
               ) %>%
        group_by(age, yearint, quarter, month, postStart) %>%
        summarize(excess=sum(delta), .groups="drop") %>%
        left_join(popData, by="age") %>%
        mutate(excesspm=excess*1000000/pop)
    
    # STEP 1: Plot of excess deaths by quarter
    p1 <- plotData %>%
        group_by(age, quarter) %>%
        summarize(excess=sum(excess), .groups="drop") %>%
        ggplot(aes(x=fct_reorder(age, excess, .fun=sum), y=excess/1000, fill=fct_rev(quarter))) + 
        geom_col(position="stack") + 
        coord_flip() + 
        labs(x="Age Group", 
             y="Excess Deaths (000s)", 
             title=paste0("All-cause excess deaths in ", 
                          if(length(curYear)==1) curYear else paste0(curYear, collapse="-")
                          ), 
             subtitle=subT
             ) +
        scale_fill_discrete("Quarter")
    print(p1)
    
    # STEP 2: Plot of excess deaths by epi-month
    pea_001 <- plotData %>%
        filter(postStart==1) %>%
        ggplot(aes(x=fct_reorder(age, excesspm, .fun=sum))) + 
        geom_col(aes(y=excesspm, 
                     fill=fct_rev(paste0(yearint, "-", match(month, month.abb) %>% zeroPad2()))
                     ), 
                 position="stack"
                 ) + 
        coord_flip() + 
        labs(x="Age Group", 
             y="Excess Deaths (per million people)", 
             title=paste0("All-cause excess deaths in ", 
                          if(length(curYear)==1) curYear else paste0(curYear, collapse="-")
                          ), 
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



# Function to create facets for excess all-cause deaths
excessDeathFacets <- function(lstCDC, 
                              lstAll, 
                              dateThru, 
                              trendThru="2019-12-31", 
                              plotYLim=c(-200, 1000), 
                              aggParam=NULL
                              ) {
    
    # FUNCTION ARGUMENTS:
    # lstCDC: CDC excess deaths list
    # lstAll: all-cause excess deaths list
    # dateThru: date to look at cumulative excess deaths through
    # trendThru: last date for calculating baseline trends
    # plotYLim: y-limits for the facetted plots
    # aggParam: list of parameters to drive aggregation (NULL means use defaults)
    
    # Create main parameters for death trends
    mainParam <- list("cdc"=lstCDC, 
                      "cdcSub"=lstAll, 
                      "dateThru"=dateThru, 
                      "trendThru"=trendThru, 
                      "createPlot"=FALSE, 
                      "plotLegendPosition"="bottom", 
                      "plotYLim"=plotYLim, 
                      "printPlot"=FALSE
    )
    
    # Create aggregation parameters if not passed
    if (is.null(aggParam)) {
        aggParam <- list("p1"=list("cdcFilter"=list("age"=c("75-84 years", "85 years and older")), 
                                   "cdcSubFilter"=list("Age"=c("75-84 years", "85 years and over"), 
                                                       "State"="United States"
                                   ), 
                                   "plotSubtitle"="Age 75+"
        ), 
        "p2"=list("cdcFilter"=list("age"=c("65-74 years")), 
                  "cdcSubFilter"=list("Age"=c("65-74 years"), "State"="United States"), 
                  "plotSubtitle"="Age 65-74"
        ),
        "p3"=list("cdcFilter"=list("age"=c("45-64 years", "25-44 years", "Under 25 years")), 
                  "cdcSubFilter"=list("Age"=c("50-64 years", "40-49 years", "30-39 years", 
                                              "18-29 years", "0-17 years"
                  ), 
                  "State"="United States"
                  ), 
                  "plotSubtitle"="Age 0-64"
        ), 
        "p4"=list("cdcFilter"=list(), 
                  "cdcSubFilter"=list("Age"=c("All Ages"), "State"="United States"), 
                  "plotSubtitle"="All Ages"
        )
        )
        
    }
    
    # Create grobs based on parameters
    aggGrobs <- lapply(aggParam,
                       FUN=function(x) deathTrends(mainParam$cdc, 
                                                   lstCDCSub=mainParam$cdcSub,
                                                   lstCDCFilter=x[["cdcFilter"]], 
                                                   lstCDCSubFilter=x[["cdcSubFilter"]],
                                                   dateThru=mainParam$dateThru,
                                                   trendThru=mainParam$trendThru, 
                                                   createPlot=mainParam$createPlot
                       ) %>%
                           makeCumulativeDeath(plotSubtitle=x[["plotSubtitle"]], 
                                               plotLegendPosition=mainParam$plotLegendPosition, 
                                               plotYLim=mainParam$plotYLim, 
                                               printPlot=mainParam$printPlot
                           )
    )
    
    # Plot the grobs
    gridExtra::grid.arrange(aggGrobs$p4, aggGrobs$p1, aggGrobs$p2, aggGrobs$p3, nrow=2)
    
}



# Function to create cumulative death plots for COVID and excess non-COVID
makeCumulativeDeath <- function(lst, 
                                divBy=1000, 
                                yLab=NULL, 
                                plotTitle=NULL,
                                plotSubtitle=NULL,
                                plotYLim=NULL,
                                plotLegendPosition=NULL,
                                plotLabels=NULL,
                                returnData=FALSE, 
                                printPlot=TRUE, 
                                returnPlot=!isTRUE(printPlot)
                                ) {
    
    # FUNCTION ARGUMENTS
    # lst: a processed list file from deathTrends()
    # divBy: divide death values by (1000 means plots will be in thousands)
    # yLab: y-axis label (NULL means use divBy to create)
    # plotTitle: title (NULL means use divBy and earliest date in inner_join to create)
    # plotSubtitle: subtitle (NULL means none)
    # plotYLim: vector c(ymin, ymax) to force y-limits on the plot (NULL means use defaults)
    # plotLegendPosition: character string for theme(legend.position=) to override the default legend position
    # plotLabels: mapping file of elements to labels for the plot legend (NULL means key off plotLegendPosition)
    # returnData: boolean, should the data frame be returned?
    # printPlot: should the plot be printed
    # returnPlot: should the plot be returned
    
    # Create plotLabels if passed as NULL
    if(is.null(plotLabels)) {
        plotLabels <- c("nonCovid"="non-COVID\nvs. trend of\n2015-2019", "covid"="COVID")
        if(!is.null(plotLegendPosition)) 
            if(plotLegendPosition=="bottom") 
                plotLabels["nonCovid"] <- stringr::str_replace_all(plotLabels["nonCovid"], "\n", " ")
    }
    
    # Create the data frame
    df <- lst[["dfAllCause"]] %>%
        select(date, allCause=deaths, pred) %>%
        inner_join(select(lst[["dfSubset"]], date, nonCovidDeaths, covidDeaths), by="date") %>%
        mutate(deltaAllCause=allCause-covidDeaths-nonCovidDeaths, 
               deltaPred=nonCovidDeaths-pred
        ) %>%
        pivot_longer(-c(date)) %>%
        arrange(date) %>%
        group_by(name) %>%
        mutate(cumValue=cumsum(value))
    
    # Create the labels if needed
    # Unit conversion text
    if(divBy==1) xtraText <- ""
    else if(isTRUE(all.equal(log10(divBy), round(log10(divBy))))) 
        xtraText <- paste0(" (", stringr::str_sub(as.character(divBy), start=2), ")")
    else xtraText <- paste0(" (", as.character(round(divBy)), "s)")
    # Earliest date
    earlyDate <- format(min(df$date), "%B %Y")
    # Replacement of NULL values
    if (is.null(yLab)) yLab <- paste0("Cumulative Deaths", xtraText)
    if (is.null(plotTitle)) plotTitle <- paste0("Cumulative change in deaths", xtraText, " since ", earlyDate)
    
    # Create the plot
    p1 <- df %>%
        ggplot(aes(x=date, y=cumValue/divBy)) + 
        geom_text(data=~filter(., name=="deltaPred", date==max(date)), 
                  aes(color="nonCovid", label=round(cumValue/divBy)), 
                  hjust=0
        ) + 
        geom_text(data=~filter(., name=="covidDeaths", date==max(date)), 
                  aes(color="covid", label=round(cumValue/divBy)), 
                  hjust=0
        ) + 
        geom_line(data=~filter(., name=="deltaPred"), aes(color="nonCovid")) +
        geom_line(data=~filter(., name=="covidDeaths"), aes(color="covid")) + 
        labs(x=NULL, y=yLab, title=plotTitle, subtitle=plotSubtitle) + 
        scale_color_manual("Type:", values=c("nonCovid"="black", "covid"="red"), labels=plotLabels) + 
        geom_hline(yintercept=0, lty=2)
    if(!is.null(plotYLim)) p1 <- p1 + lims(y=plotYLim)
    if(!is.null(plotLegendPosition)) p1 <- p1 + theme(legend.position=plotLegendPosition)
    if(isTRUE(printPlot)) print(p1)
    
    # Return the plot or data if requested (plot takes precedence)
    if(isTRUE(returnPlot)) return(p1)
    if(isTRUE(returnData)) return(df)
    
}



# Create trend data for excess deaths
deathTrends <- function(lstCDC, 
                        lstCDCSub,
                        dateThru, 
                        trendThru, 
                        lstCDCFilter=list(),
                        lstCDCSubFilter=list("State"="United States", "Age"="All Ages"),
                        createPlot=TRUE,
                        mapColor=c("Actual\nall-cause"="red", 
                                   "Actual\nnon-COVID"="black", 
                                   "Trend from\n2015-2019"="red"
                        ),
                        mapLineType=c("Actual\nall-cause"="solid", 
                                      "Actual\nnon-COVID"="solid", 
                                      "Trend from\n2015-2019"="dotted"
                        ),
                        mapSize=c("Actual\nall-cause"=1.5, 
                                  "Actual\nnon-COVID"=0.5, 
                                  "Trend from\n2015-2019"=1
                        )
                        ) {
    
    # FUNCTION ARGUMENTS:
    # lstCDC: processed list of CDC all-cause deaths
    # lstCDCSub: processed list of CDC age-place-cause deaths
    # dateThru: latest date for the analysis
    # trendThru: latest date for the trend regression
    # lstCDCFilter: named list with filtering criteria for lstCDC, passed to rowFilter()
    # lstCDCSubFilter: named list with filtering criteria for lstCDCSub, passed to rowFilter()
    # createPlot: boolean, should the plot be created (if FALSE, only the data is created ad returned)
    # mapColor: mapping vector for plot color elements
    # mapLineType: mapping vector for plot linetype elements
    # mapSize: mapping vector for plot size elements
    
    # Extrapolate all-cause deaths by month
    dfAllCause <- lstCDC[["cdc"]] %>%
        rowFilter(lstFilter=lstCDCFilter) %>%
        group_by(weekEnding) %>%
        summarize(deathsPerDay=sum(deaths)/7)
    dfAllCause <- map_dfr(.x=0:6, .f=function(x) mutate(dfAllCause, day=weekEnding-lubridate::days(x))) %>%
        mutate(ym=customYYYYMM(day)) %>%
        group_by(ym) %>%
        summarize(deaths=sum(deathsPerDay)) %>%
        mutate(date=as.Date(paste0(ym, "-01"))) %>%
        filter(date <= dateThru) %>% 
        mutate(year=lubridate::year(date), 
               month=factor(month.abb[lubridate::month(date)], levels=month.abb)
        )
    
    # Create the linear trend of all-cause deaths
    lmAllCause <- lm(deaths ~ year + month, data=filter(dfAllCause, date <= trendThru))
    
    # Create the integrated all-cause deaths with projections
    dfAllCause <- dfAllCause %>%
        mutate(pred=predict(lmAllCause, newdata=.))
    
    # Extract the subset data
    dfSubset <- lstCDCSub[["allCauseList"]][["deathLoc_conv"]] %>% 
        rowFilter(lstFilter=list("Group"="By Month", "deathPlace"="Total - All Places of Death")) %>%
        rowFilter(lstFilter=lstCDCSubFilter) %>%
        group_by(Year, Month) %>%
        summarize(across(c(totalDeaths, covidDeaths), .fns=sum, na.rm=TRUE), .groups="drop") %>%
        mutate(date=as.Date(paste0(Year, "-", Month, "-1")), 
               nonCovidDeaths=totalDeaths-covidDeaths
        )
    
    if(isTRUE(createPlot)) {
        
        # Create and print the plot
        p1 <- dfAllCause %>% 
            ggplot(aes(x=date)) + 
            geom_line(aes(y=deaths/1000, 
                          color="Actual\nall-cause", 
                          linetype="Actual\nall-cause", 
                          size="Actual\nall-cause"
            )
            ) + 
            geom_line(data=filter(dfSubset, date <= dateThru), 
                      aes(y=nonCovidDeaths/1000, 
                          color="Actual\nnon-COVID", 
                          linetype="Actual\nnon-COVID", 
                          size="Actual\nnon-COVID"
                      )
            ) + 
            geom_line(aes(y=pred/1000, 
                          color="Trend from\n2015-2019", 
                          linetype="Trend from\n2015-2019", 
                          size="Trend from\n2015-2019"
            )
            ) + 
            lims(y=c(0, NA)) + 
            labs(x=NULL, 
                 y="Monthly deaths (000)", 
                 title="US monthly deaths (000) by type and vs. 2015-2019 trend"
            ) +
            scale_color_manual(name="Source:", values=mapColor) + 
            scale_linetype_manual(name="Source:", values=mapLineType) + 
            scale_size_manual(name="Source:", values=mapSize)
        print(p1)
        
    }
    
    # Return key data components
    list(dfAllCause=dfAllCause, dfSubset=dfSubset, lmAllCause=lmAllCause)
    
}



# Primary function for analyzing death by age-location-cause data
analyzeAllCause <- function(loc, 
                            cdcDailyList,
                            compareThruDate,
                            plotTitleTime=paste0("2020 through ", compareThruDate),
                            endMonth=NULL,
                            dlData=!file.exists(loc)
                            ) {
    
    # FUNCTION ARGUMENTS:
    # loc: the location where detailed death data is stored (or should be downloaded to)
    #      will have ./RInputFiles/Coronavirus pre-pended to it
    # cdcDailyList: list file containing processed CDC Daily files
    # compareThruDate: date for comparisons of deaths between files
    # plotTitleTime: text to be included in plot title to describe timing
    # endMonth: ending month to be used for COVID vs non-COVID plots (NULL means infer from compareThruDate)
    # dlData: boolean, should the data be downloaded?
    
    # STEP 1: find ending month if passed as NULL
    if(is.null(endMonth)) {
        endMonth <- lubridate::floor_date(as.Date(compareThruDate)+lubridate::days(1), unit="month")
        endMonth <- endMonth - lubridate::days(1)
    }
    
    # STEP 2: Load and process all-cause deaths data
    allCauseList <- processAllCauseLocation(loc)
    
    # STEP 3: Compare all-cause deaths between file
    deathDelta <- compareCDCDeaths(lstLoc=allCauseList, 
                                   lstState=cdcDailyList, 
                                   thruDate=compareThruDate, 
                                   returnData=TRUE
    )
    
    # STEP 4: Plots by age cohort
    deathAge <- plotDeathDetails(allCauseList, 
                                 keyVar="Age",
                                 timeLabel=plotTitleTime,
                                 dfFilter=list("deathPlace"="Total - All Places of Death"), 
                                 returnData=TRUE
    )
    
    # STEP 5: Plots by place of death cohort
    deathPlace <- plotDeathDetails(allCauseList, 
                                   keyVar="deathPlace",
                                   timeLabel=plotTitleTime,
                                   dfFilter=list("Age"="All Ages"), 
                                   p1Exclude=list("deathPlace"="Total - All Places of Death"),
                                   legendLabel="Death\nPlace",
                                   legendPosition="bottom",
                                   returnData=TRUE
    )
    
    # STEP 6: Additional plots for COVID vs non-COVID
    exploreDeathPlace(allCauseList, 
                      timeLabel=plotTitleTime,
                      endMonth=endMonth,
                      returnData=FALSE
    )
    
    # STEP 7: Return list
    list(allCauseList=allCauseList, 
         deathDelta=deathDelta, 
         deathAge=deathAge, 
         deathPlace=deathPlace, 
         compareThruDate=compareThruDate, 
         endMonth=endMonth
    )
    
}



# Analyze place-of-death data
exploreDeathPlace <- function(lst, 
                              timeLabel,
                              endMonth,
                              returnData=FALSE
                              ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a processed list file with sub-list "deathLoc_conv"
    # timeLabel: the label for the plot timing (e.g., "2020-October 2021")
    # endMonth: the last day of the final month to include in the monthly plots, formatted as "YYYY-MM-DD"
    # returnData: boolean, should df be returned?
    
    # Locations of death by age
    df <- lst[["deathLoc_conv"]] %>%
        mutate(nonCovidDeaths=zeroNA(totalDeaths)-zeroNA(covidDeaths)) %>%
        select(Group, startDate, endDate, State, deathPlace, Age, where(is.numeric), -Month, -Year) %>%
        pivot_longer(where(is.numeric))
    
    # Basic plotting data
    p1 <- df %>%
        filter(name %in% c("covidDeaths", "nonCovidDeaths"), 
               State=="United States", 
               Group=="By Total"
        ) %>%
        ggplot(aes(x=Age, y=value/1000)) + 
        coord_flip() + 
        scale_fill_discrete("") +
        theme(legend.position="bottom") +
        labs(x=NULL, 
             y="Deaths (000)", 
             title=paste0("United States deaths (", timeLabel, ")")
        )
    
    # Overall deaths by age and type
    p1a <- p1 + 
        geom_col(data=~filter(., deathPlace=="Total - All Places of Death", Age !="All Ages"), 
                 aes(fill=name), 
                 position="stack"
        )
    
    # Proportion deaths by age and type
    p1b <- p1 + 
        geom_col(data=~filter(., deathPlace=="Total - All Places of Death"), 
                 aes(fill=fct_rev(name)), 
                 position="fill"
        ) + 
        labs(y="Proportion of deaths")
    
    gridExtra::grid.arrange(p1a, p1b, nrow=1)
    
    # Overall deaths by age and type and location
    p1c <- p1 + 
        geom_col(data=~filter(., deathPlace!="Total - All Places of Death", Age != "All Ages"), 
                 aes(fill=name), 
                 position="stack"
        ) + 
        facet_wrap(~deathPlace)
    
    # Proportion of deaths by age and type and location
    p1d <- p1 + 
        geom_col(data=~filter(., Age !="All Ages"), 
                 aes(fill=fct_rev(name)), 
                 position="fill"
        ) + 
        facet_wrap(~deathPlace) + 
        labs(y="Proportion of deaths") + 
        geom_hline(yintercept=0.25, lty=2)
    
    gridExtra::grid.arrange(p1c, p1d, nrow=1)
    
    
    # Basic plotting data by month (not pivoted)
    p2 <- df %>%
        filter(name %in% c("covidDeaths", "nonCovidDeaths"), 
               State=="United States", 
               Group=="By Month", 
               endDate <= endMonth
        ) %>%
        ggplot(aes(x=fct_reorder(deathPlace, value, max), y=value/1000)) + 
        scale_color_discrete("") +
        theme(legend.position="bottom") +
        labs(x=NULL, 
             y="Deaths (000)", 
             title=paste0("United States deaths (", timeLabel, ")")
        )
    
    # Basic plotting data by month (percent deaths from coVID)
    p3 <- df %>%
        filter(name %in% c("covidDeaths", "totalDeaths"), 
               State=="United States", 
               Group=="By Month", 
               endDate <= endMonth
        ) %>%
        pivot_wider(names_from="name", values_from="value") %>% 
        mutate(pctCov=covidDeaths/totalDeaths)
    
    # Overall deaths by month and place
    p2a <- p2 +
        geom_line(data=~filter(., deathPlace!="Total - All Places of Death", Age =="All Ages"), 
                  aes(x=endDate, group=name, color=name)
        ) + 
        facet_wrap(~deathPlace[deathPlace!="Total - All Places of Death"], ncol=1)
    
    # Proportion of deaths by month and place
    p3a <- p3 %>%
        filter(deathPlace!="Total - All Places of Death", Age =="All Ages") %>%
        ggplot(aes(x=endDate, y=pctCov)) + 
        labs(x=NULL, 
             y="Proportion of deaths from COVID",
             title=paste0("United States deaths (", timeLabel, ")")
        ) +
        geom_col(fill="lightblue") + 
        geom_text(aes(label=paste0(round(100*pctCov), "%")), 
                  vjust=0, 
                  size=3
        ) + 
        facet_wrap(~deathPlace, ncol=1) + 
        lims(y=c(0, 1))
    
    gridExtra::grid.arrange(p2a, p3a, nrow=1)
    
    # Overall deaths by month and age
    p2b <- p2 + 
        geom_line(data=~filter(., deathPlace=="Total - All Places of Death", Age !="All Ages"), 
                  aes(x=endDate, group=name, color=name)
        ) + 
        facet_wrap(~Age[Age!="All Ages"], ncol=1)
    
    # Proportion of deaths by month and age
    p3b <- p3 %>%
        filter(deathPlace=="Total - All Places of Death", Age !="All Ages") %>%
        ggplot(aes(x=endDate, y=pctCov)) +
        labs(x=NULL, 
             y="Proportion of deaths from COVID",
             title=paste0("United States deaths (", timeLabel, ")")
        ) +
        geom_col(fill="lightblue") + 
        geom_text(aes(label=paste0(round(100*pctCov), "%")), 
                  vjust=0, 
                  size=3
        ) + 
        facet_wrap(~Age, ncol=1) + 
        lims(y=c(0, 1))
    
    gridExtra::grid.arrange(p2b, p3b, nrow=1)
    
    # Return the processed data frame, if requested
    if(isTRUE(returnData)) return(df)
    
}



# Analyze deaths by age and place
plotDeathDetails <- function(lst, 
                             keyVar,
                             timeLabel,
                             dfFilter=list(), 
                             p1Include=list("State"="United States", "Group"="By Total"),
                             p1Exclude=list("Age"="All Ages"),
                             p2Include=list("State"="United States", "Group"="By Month"),
                             p2Exclude=p1Exclude,
                             p3Include=p2Include,
                             p3Exclude=p2Exclude,
                             p2PlotVars=c("totalDeaths", "covidDeaths", "fluDeaths"),
                             p3PlotVars=p2PlotVars,
                             legendLabel=keyVar,
                             legendPosition=NULL,
                             returnData=FALSE
                             ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a processed list file with sub-list "deathLoc_conv"
    # keyVar: the key variable being explored (e.g., "Age" or "deathPlace")
    # timeLabel: the label for the plot timing (e.g., "2020-October 2021")
    # dfFilter: a list of format list("variable"=c("allowed values")) for filtering data to produce df
    # p1Include: a list of format list("variable"=c("allowed values")) for filtering data to produce plot 1
    # p1Exclude: a list of format list("variable"=c("disallowed values")) for filtering data to produce plot 1
    # p2Include: a list of format list("variable"=c("allowed values")) for filtering data to produce plot 2
    # p2Exclude: a list of format list("variable"=c("disallowed values")) for filtering data to produce plot 2
    # p3Include: a list of format list("variable"=c("allowed values")) for filtering data to produce plot 3
    # p3Exclude: a list of format list("variable"=c("disallowed values")) for filtering data to produce plot 3
    # p2PlotVars: variables to include in the second plot
    # p3PlotVars: variables to include in the third plot
    # legendLabel: label to be used for the legend
    # legendPosition: the position for the legend (NULL means leave defaults)
    # returnData: boolean, should the data be returned?
    
    # Create the data for processing
    df <- lst[["deathLoc_conv"]] %>%
        rowFilter(lstFilter=dfFilter)
    
    # Create the first plot
    p1 <- df %>%
        rowFilter(lstFilter=p1Include, lstExclude=p1Exclude) %>%
        select(all_of(keyVar), where(is.numeric), -Year, -Month) %>%
        pivot_longer(!all_of(keyVar)) %>%
        ggplot() + 
        geom_col(aes(x=name, y=value, fill=fct_rev(get(keyVar))), position="fill") + 
        labs(x=NULL, 
             y="Proportion of Deaths", 
             title=paste0("Proportion of deaths (", timeLabel, ")")
        ) + 
        scale_fill_discrete(legendLabel)
    if(!is.null(legendPosition)) p1 <- p1 + theme(legend.position=legendPosition)
    print(p1)
    
    # Create the second plot
    p2 <- df %>%
        rowFilter(lstFilter=p2Include, lstExclude=p2Exclude) %>%
        mutate(ym=lubridate::ym(paste0(Year, "-", zeroPad2(Month)))) %>%
        select(all_of(keyVar), ym, all_of(p2PlotVars)) %>%
        pivot_longer(!c(all_of(keyVar), ym)) %>%
        ggplot() + 
        geom_col(aes(x=ym, y=value, fill=fct_rev(get(keyVar))), position="fill") + 
        facet_wrap(~name) +
        labs(x=NULL, 
             y="Proportion of Deaths", 
             title=paste0("Proportion of deaths (", timeLabel, ")")
        ) + 
        scale_fill_discrete(legendLabel)
    if(!is.null(legendPosition)) p2 <- p2 + theme(legend.position=legendPosition)
    print(p2)
    
    # Create the third plot
    p3 <- df %>%
        rowFilter(lstFilter=p2Include, lstExclude=p2Exclude) %>%
        mutate(ym=lubridate::ym(paste0(Year, "-", zeroPad2(Month)))) %>%
        select(all_of(keyVar), ym, all_of(p3PlotVars)) %>%
        pivot_longer(!c(all_of(keyVar), ym)) %>%
        ggplot() + 
        geom_line(aes(x=ym, y=value, color=fct_rev(get(keyVar)), group=get(keyVar))) + 
        facet_wrap(~name, scales="free_y") +
        labs(x=NULL, 
             y="Deaths", 
             title=paste0("Deaths (", timeLabel, ")")
        ) + 
        scale_color_discrete(legendLabel)
    if(!is.null(legendPosition)) p3 <- p3 + theme(legend.position=legendPosition)
    print(p3)
    
    # Return the data file if requested
    if(isTRUE(returnData)) return(df)
    
}



# Compare deaths by state by all-cause data source
compareCDCDeaths <- function(lstLoc,
                             lstState,
                             thruDate,
                             keyDeathPlaces=c("Total - All Places of Death"),
                             keyAges=c("All Ages"), 
                             returnData=FALSE
                             ) {
    
    # FUNCTION ARGUMENTS
    # lstLoc: list containing the processed death-location data, with sub-list "deathLoc_conv"
    # lstState: list containing processed CDC COVID death data by state, with sub-list "dfPerCapita"
    # thruDate: character, formatted as YYYY-MM-DD
    # keyDeathPlaces: places of death to include from lstLoc
    # keyAges: ages to include from lstLoc
    # returnData: boolean, should data be returned?
    
    # Create summary by state and year-month
    death_sum <- lstLoc[["deathLoc_conv"]] %>%
        filter(!is.na(Year), 
               !is.na(Month), 
               deathPlace %in% all_of(keyDeathPlaces), 
               Age %in% all_of(keyAges)
        ) %>%
        mutate(ym=lubridate::ym(paste0(Year, "-", zeroPad2(Month)))) %>%
        select(State, abb, ym, where(is.numeric), -Year, -Month) %>%
        pivot_longer(-c(State, abb, ym)) %>%
        arrange(State, abb, name, ym) %>%
        group_by(State, abb, name) %>%
        mutate(cumValue=cumsum(ifelse(is.na(value), 0, value))) %>%
        ungroup() %>%
        mutate(date=lubridate::ceiling_date(ym, unit="month")-lubridate::days(1))
    
    # Create summary from state-level file
    death_daily <- lstState[["dfPerCapita"]] %>%
        select(date, abb=state, tot_deaths) %>%
        mutate(Year=lubridate::year(date), Month=lubridate::month(date)) %>%
        group_by(Year, Month) %>%
        filter(date==max(date)) %>%
        ungroup()
    
    # Create a plot for evolution of United States
    p1 <- death_sum %>%
        filter(abb=="US", name=="covidDeaths", ym <= thruDate) %>%
        ggplot(aes(x=date)) + 
        geom_line(aes(y=cumValue/1000, color="blue"), size=2) + 
        geom_point(data=summarize(group_by(filter(death_daily, date <= thruDate), date), 
                                  tot_deaths=sum(tot_deaths, na.rm=TRUE)
        ), 
        aes(y=tot_deaths/1000, color="green"), 
        size=3
        ) +
        labs(x="End of month", 
             y="Cumulative COVID Deaths (000)", 
             title="Cumulative COVID Deaths (000) in US by source"
        ) + 
        scale_color_manual("Source", 
                           labels=c("Summed\nCDC Location", "Summed\nCDC Daily"), 
                           values=c("green", "blue")
        )
    print(p1)
    
    # Comparison of totals by state
    plot_cum <- death_sum %>%
        filter(abb %in% c(state.abb, "DC"), 
               name=="covidDeaths", 
               date == thruDate
        ) %>%
        select(abb, cumValue) %>%
        inner_join(select(filter(death_daily, date == thruDate), abb, tot_deaths), by=c("abb")) %>%
        mutate(pctdiff=abs(tot_deaths-cumValue)/(tot_deaths+cumValue))
    plot_cum %>%
        arrange(-pctdiff) %>%
        print()
    plot_cum %>%
        summarize(across(where(is.numeric), sum)) %>%
        print()
    p2 <- plot_cum %>%
        ggplot(aes(x=fct_reorder(abb, cumValue))) + 
        geom_col(aes(y=cumValue/1000), fill="lightblue") + 
        geom_point(aes(y=tot_deaths/1000), size=3) +
        coord_flip() +
        labs(x=NULL, 
             y="Cumulative Deaths (000)", 
             title=paste0("Cumulative COVID Deaths (000) in US as of ", thruDate), 
             subtitle="Filled bars are summed CDC location, points are from CDC daily"
        )
    print(p2)
    
    if(isTRUE(returnData)) return(plot_cum)
    
}



# Process all-cause deaths by age-location-cause data
processAllCauseLocation <- function(loc, 
                                    url="https://data.cdc.gov/api/views/4va6-ph5s/rows.csv?accessType=DOWNLOAD",
                                    col_types="cccciiccccddddddc",
                                    vecRename=c("Data as of"="asofDate", 
                                                "Start Date"="startDate", 
                                                "End Date"="endDate", 
                                                "HHS Region"="HHSRegion", 
                                                "Place of Death"="deathPlace", 
                                                "Age group"="Age", 
                                                "COVID-19 Deaths"="covidDeaths", 
                                                "Total Deaths"="totalDeaths", 
                                                "Pneumonia Deaths"="pneumoDeaths", 
                                                "Pneumonia and COVID-19 Deaths"="pneumoCovidDeaths", 
                                                "Influenza Deaths"="fluDeaths", 
                                                "Pneumonia, Influenza, or COVID-19 Deaths"="pnemoFluCovidDeaths"
                                    ),
                                    selfList=list("asofDate"=lubridate::mdy, 
                                                  "startDate"=lubridate::mdy, 
                                                  "endDate"=lubridate::mdy
                                    ),
                                    dir="./RInputFiles/Coronavirus/",
                                    dlData=isFALSE(file.exists(paste0(dir, loc))), 
                                    allCheckVars=NULL, 
                                    subMap=c("State"="United States", 
                                             "Age"="All Ages", 
                                             "deathPlace"="Total - All Places of Death"
                                    ), 
                                    createPlot5=TRUE
                                    ) {
    
    # FUNCTION ARGUMENTS:
    # loc: the location of the CDC all-cause death by location file
    # url: the location of the all-cause death by location data
    # col_types: the column types for the data in loc
    # vecRename: vector for renaming columns in raw data
    # selfList: list for colMutater() for the data from loc
    # dir: the directory for the downloaded data in loc
    # dlData: boolean, should data be downloaded?
    # allCheckVars: variable list to be checked
    # subMap: subsets to be checked for summation to the whole
    # createPlot5: boolean, should comparisons of totals and sum of subtotals be created
    
    # Step 0: Download the data if requested
    if (dlData) fileDownload(fileName=paste0(dir, loc), url=url)
    
    # Step 1: Read the CSV data
    deathLoc_raw <- fileRead(paste0(dir, loc), col_types=col_types)
    
    # Step 2: Rename variables for easier interpretation, convert the dates, add the state abbreviation
    deathLoc_conv <- deathLoc_raw %>%
        colRenamer(vecRename=vecRename) %>%
        colMutater(selfList=selfList) %>%
        mutate(abb=c(state.abb, "DC", "US")[match(State, c(state.name, "District of Columbia", "United States"))])
    
    # Step 3: Plots for combinations included
    p1 <- deathLoc_conv %>%
        count(asofDate, startDate, endDate) %>%
        ggplot(aes(y=startDate, x=endDate)) + 
        geom_point(aes(size=n)) + 
        facet_wrap(~asofDate) + 
        labs(x="Ending Date", y="Starting Date", title="Combinations of Start and End Date")
    p2 <- deathLoc_conv %>%
        count(Group, deathPlace, Age) %>%
        ggplot(aes(x=Group, y=deathPlace)) + 
        geom_tile(aes(fill=n)) + 
        facet_wrap(~Age) + 
        labs(x="Group", y="Place of Death", title="Combinations of Age, Place of Death, and Group")
    gridExtra::grid.arrange(p1, p2, ncol=1)
    
    # Step 4: Deaths by state
    dfTemp <- deathLoc_conv %>%
        filter(Group=="By Total", deathPlace=="Total - All Places of Death", Age=="All Ages") %>%
        group_by(State, abb) %>%
        summarize(across(where(is.numeric), sum, na.rm=TRUE))
    cat("\nStates without abbreviations\n")
    print(dfTemp %>% filter(is.na(abb)))
    
    # Step 4a: Plots
    deathBase <- dfTemp %>%
        select(State, covidDeaths, totalDeaths) %>%
        mutate(noncovid=covidDeaths/totalDeaths) %>%
        filter(!(State %in% c("United States", "Puerto Rico"))) %>%
        pivot_longer(-c(State)) %>%
        ggplot(aes(x=fct_reorder(State, value, max), y=value/1000)) + 
        coord_flip() + 
        theme(legend.position="bottom")
    p3 <- deathBase + 
        geom_col(data=~filter(., name=="totalDeaths"), aes(fill="All")) +
        geom_col(data=~filter(., name=="covidDeaths"), aes(fill="COVID")) + 
        scale_fill_manual("Type", breaks=c("COVID", "All"), labels=c("COVID", "All"), values=c("red", "black")) + 
        labs(title="Deaths 2020-present by state", x=NULL, y="Deaths (000s)")
    p4 <- deathBase + 
        geom_col(data=~filter(., name=="noncovid"), aes(y=value), position="identity") + 
        labs(x=NULL, y=NULL, title="Proportion of deaths from COVID")
    gridExtra::grid.arrange(p3, p4, nrow=1)
    
    # Step 5: Check for alignment by variable combinations (run only if createPlot5 is TRUE)
    if(isTRUE(createPlot5)) {
        # Create allCheckVars if not passed
        if (is.null(allCheckVars)) {
            allCheckVars <- deathLoc_conv %>% 
                select(-where(is.numeric)) %>% 
                names %>% 
                setdiff(c("Footnote", "abb", "HHSRegion"))
        }
        # Run for the key subsets
        lapply(names(subMap), 
               FUN=function(x) deathLoc_conv %>% 
                   select(-Year, -Month) %>%
                   checkSubTotals(checkByVars=allCheckVars %>% setdiff(x), 
                                  subVar=x, 
                                  subVarTotal=unname(subMap[x])
                   ) %>%
                   checkNumbers(byVars=allCheckVars, keyVar=x)
        )
    }
    
    # Return a list of the key datasets
    list(deathLoc_raw=deathLoc_raw, deathLoc_conv=deathLoc_conv)
    
}



# Plots for deaths vs. trend by age and week
plotAgeWeekDeath <- function(lst, 
                             keyStates=NULL, 
                             addLM=TRUE,
                             lmYears=2015:2019, 
                             diffTrend=FALSE, 
                             printPlots=TRUE, 
                             returnData=FALSE, 
                             returnPlots=!isTRUE(printPlots)
                             ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a processed list file that includes the CDC deaths data
    # keyStates: states to be included
    # addLM: boolean, should a line for the linear model be added?
    # lmYears: what years should the linear model be fitted against?
    # diffTrend: boolean, should a difference from trend by year be calculated?
    # printPlots: boolean, should the plots be printed?
    # returnData: boolean, should the data be returned?
    # returnPlots: boolean, should the plot objects be returned?
    
    # Create the plot title (use 50 states plus DC if keyStates passed as NULL)
    plotTitle <- paste0("Total deaths by age cohort and week (", 
                        if (is.null(keyStates)) "50 states plus DC" else paste0(keyStates, collapse=", "),
                        ")"
    )
    
    # Set the keyStates field to 50 states plus DC if not provided
    if (is.null(keyStates)) keyStates <- c(state.abb, "DC")
    
    # Extract the CDC data
    df <- lst[["cdc"]]
    
    # Create plot data
    p1Data <- df %>% 
        filter(state %in% all_of(keyStates)) %>% 
        group_by(weekEnding, year, age) %>% 
        summarize(deaths=sum(deaths), .groups="drop")
    
    # Create the plot
    p1 <- p1Data %>% 
        ggplot(aes(x=weekEnding, y=deaths)) + 
        geom_line(aes(color=year)) + 
        facet_wrap(~age, scales="free_y") + 
        lims(y=c(0, NA)) + 
        labs(title=plotTitle, 
             x="Week", 
             y="All-cause deaths"
        ) + 
        # subtitle="Dashed line is linear model without seasonailty using 2015-2019 data", 
        scale_color_discrete("Year")
    
    # Add the linear model if requested
    if (isTRUE(addLM)) {
        p1 <- p1 + 
            geom_abline(data=~tempGetReg(., regYears=lmYears), 
                        aes(slope=slope, intercept=intercept), 
                        lty=2
            ) + 
            labs(subtitle=paste0("Dashed line is linear model without seasonailty using ", 
                                 paste0(lmYears, collapse="-"), 
                                 " data"
            )
            )
    }
    
    # Display the plot if requested
    if (printPlots) print(p1)
    
    # Create a dataset for difference from expected deaths by year
    if (isTRUE(diffTrend)) {
        p2 <- getDiffTrend(p1Data, 
                           regYears=lmYears, 
                           plotTitle=paste0("Deaths vs trend for: ", 
                                            if (setequal(keyStates, c(state.abb, "DC"))) 
                                                "50 states plus DC" 
                                            else 
                                                paste0(keyStates, collapse=", ")
                           ), 
                           returnData=returnData
        )
        p2Plot <- if(isTRUE(returnData)) p2$objPlot else p2
        if (printPlots) print(p2Plot)
    } else {
        p2 <- NULL
    }
    
    # Return objects if requested
    if (isTRUE(returnData) | isTRUE(returnPlots)) {
        return(list(p1=if(isTRUE(returnPlots)) p1 else NULL, 
                    p2=if(isTRUE(returnPlots)) p2Plot else NULL, 
                    p1Data=if(isTRUE(returnData)) p1Data else NULL, 
                    p2Full=if(isTRUE(returnData)) p2$fullDF else NULL, 
                    p2Sum=if(isTRUE(returnData)) p2$sumDF else NULL
        )
        )
    }
    
}



# Calculate deaths vs. trend based on linear model
getDiffTrend <- function(df, 
                         regYears, 
                         plotTitle, 
                         returnData=FALSE
                         ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame or tibble with filtered data
    # regYears: years to be used in regression
    # plotTitle: title for the plot
    # returnData: boolean, should the data be returned?
    
    # Run the linear mode and make the predictions
    tempLM <- lm(deaths ~ age + weekEnding:age + 0, data=subset(df, year %in% all_of(regYears)))
    tempDF <- df %>%
        mutate(pred=predict(tempLM, newdata=df))
    
    # Summarize to year-age
    sumDF <- tempDF %>%
        group_by(year, age) %>%
        summarize(across(where(is.numeric), sum), .groups="drop")
    
    # Plot the excess by year and age group
    p2 <- sumDF %>%
        ggplot(aes(x=year, y=deaths-pred)) + 
        geom_col(aes(fill=year)) + 
        geom_text(data=~filter(., !(year %in% all_of(regYears))), 
                  aes(y=0.5*(deaths-pred), label=round(deaths-pred))
        ) +
        facet_wrap(~age, scales="free_y") + 
        labs(x="Year", 
             y="Actual deaths vs. predicted", 
             title=plotTitle, 
             subtitle=paste0("Trend line is linear model without seasonailty using ", 
                             paste0(regYears, collapse="-"), 
                             " data"
             )
        )
    
    # Return the requested data
    if(returnData) list(objPlot=p2, fullDF=tempDF, sumDF=sumDF)
    else p2
    
}



# Run regression on trend data and extract coefficients
tempGetReg <- function(df, regYears=2015:2019) {
    
    lm(deaths ~ age + weekEnding:age + 0, data=subset(df, year %in% all_of(regYears))) %>% 
        broom::tidy() %>% 
        mutate(age=factor(str_remove(str_remove(term, pattern=":.*"), pattern="age")), 
               type=ifelse(str_detect(term, pattern=":"), "slope", "intercept")
        ) %>% 
        select(age, name=type, value=estimate) %>% 
        pivot_wider(age)
    
}



# Plot totals vs. sum of subtotals
checkNumbers <- function(lst, byVars, lstNames=NULL, absTol=100, pctTol=0.05, keyVar="key variable") {
    
    # FUNCTION ARGUMENTS:
    # lst: a list with two items that will be checked for similarity
    # byVars: by variables that should be identical across the list items
    # lstNames: names to use for the list (NULL means use names provided in lst)
    # absTol: absolute value of differences to flag
    # pctTol: percent tolerance for differences to flag
    # keyVar: name for the key variable in plot title
    
    # Check that lst is a list of length 2
    if (!("list" %in% class(lst)) | !(length(lst)==2)) stop("\nMust pass a list with two items\n")
    
    # Add names if passed in lstNames, otherwise use names(lst)
    if (!is.null(lstNames)) names(lst) <- lstNames 
    else lstNames <- names(lst)
    
    # Check for identical files using only byVars
    if (!isTRUE(identical(lst[[1]][, byVars], lst[[2]][, byVars]))) 
        stop("\nSub-lists differ by byVars, not comparing\n") 
    else cat("\nSub-lists are identical by:", paste0(byVars, collapse=", "), "\n")
    
    # Check the numeric values
    dfDelta <- lapply(lst, FUN=function(x) pivot_longer(x, cols=-all_of(byVars)) %>% 
                          mutate(value=ifelse(is.na(value), 0, value)) %>%
                          select(all_of(byVars), name, value)
    ) %>%
        purrr::reduce(.f=inner_join, by=c(all_of(byVars), "name")) %>%
        mutate(delta=value.x-value.y, pct=ifelse(delta==0, 0, delta/(value.x+value.y))) %>%
        purrr::set_names(c(all_of(byVars), "name", all_of(lstNames), "delta", "pct"))
    
    # Plot the differences using name as facet
    p1 <- dfDelta %>%
        ggplot(aes(x=delta, y=pct)) + 
        geom_point() + 
        facet_wrap(~name, scales="free") + 
        labs(title=paste0("Differences between totals and subtotals on variable: ", keyVar), 
             x="Difference between total and subtotal", 
             y="Percentage difference"
        )
    print(p1)
    
    # Flag significant outliers
    dfDelta %>%
        filter(abs(delta) >= absTol, abs(pct) >= pctTol) %>%
        arrange(-abs(delta)) %>%
        print()
    
}



# Function to check that totals match sum of sub-totals
checkSubTotals <- function(df, checkByVars, subVar, subVarTotal, sumVars=NULL, sumFunc=specNA(sum), ...) {
    
    # FUNCTION ARGUMENTS:
    # df: data.frame or tibble
    # checkByVars: variables that the frame will be checked by
    # subVar: variable that is being checked
    # subVarTotal: label for the value that is the total of subVar
    # sumVars: variables to be summed (NULL means all numeric)
    # sumFunc: function to be applied when summing all variables
    # ...: any other arguments to pass to summarize(across(all_of(checkByVars), .fns=sumFunc, ...))
    
    # If sumVars is NULL, find the sum variables
    if (is.null(sumVars)) sumVars <- df %>% head(1) %>% select_if(is.numeric) %>% names()
    
    # Keep only te desired variables in df
    df <- df %>%
        select(all_of(c(checkByVars, subVar, sumVars))) %>%
        arrange(across(all_of(checkByVars)))
    
    # Split the data frame by subtotal and total
    dfTot <- df %>%
        filter(get(subVar) == subVarTotal)
    dfSub <- df %>%
        filter(get(subVar) != subVarTotal) %>%
        group_by(across(all_of(checkByVars))) %>%
        summarize(across(all_of(sumVars), .fns=sumFunc, ...), .groups="drop") %>%
        mutate(fakeCol=subVarTotal) %>%
        colRenamer(vecRename=c("fakeCol"=subVar)) %>%
        select(names(dfTot))
    
    # Comparison of totals
    list(dfSub=dfSub, dfTot=dfTot)
    
}



# Plots and quality checks for processed CDC data
plotQCReadProcessCDC <- function(df, 
                                 ckCombos=list(c("age"), c("period", "year", "Type"), 
                                               c("period", "Suppress"), c("period", "Note")
                                 )
                                 ) {
    
    # Create dataset for analysis
    df <- df %>% 
        mutate(n=1, n_deaths_na=ifelse(is.na(deaths), 1, 0))
    
    # Check control totals by specified combinaions
    purrr::walk(ckCombos, .f=function(x) {
        cat("\n\nChecking variable combination:", x, "\n")
        checkControl(df, groupBy=x, useVars=c("n", "n_deaths_na", "deaths"), fn=specNA(sum))
    }
    )
    
    # Plot deaths by state
    p1 <- checkControl(df, 
                       groupBy=c("state"), 
                       useVars=c("deaths"), 
                       fn=specNA(sum), 
                       printControls=FALSE, 
                       pivotData=FALSE
    ) %>%
        ggplot(aes(x=fct_reorder(state, deaths), y=deaths)) + 
        geom_col(fill="lightblue") + 
        geom_text(aes(y=deaths, label=paste0(round(deaths/1000), "k")), hjust=0, size=3) + 
        coord_flip() +
        labs(y="Total deaths", x=NULL, title="Total deaths by state in all years in processed file")
    print(p1)
    
    # Plot deaths by week/year
    p2 <- checkControl(df, 
                       groupBy=c("year", "week"), 
                       useVars=c("deaths"), 
                       fn=specNA(sum), 
                       printControls=FALSE, 
                       pivotData=FALSE
    ) %>%
        ggplot(aes(x=week, y=deaths)) + 
        geom_line(aes(group=year, color=year)) + 
        labs(title="Deaths by year and epidemiological week", x="Epi week", y="US deaths") + 
        scale_color_discrete("Year") + 
        lims(y=c(0, NA))
    print(p2)
    
}



# Check for excessive suppression in CDC all-cause deaths data
checkCDCSuppression <- function(df, stateNoCheck, errTotAllowed=20, errMaxAllowed=round(errTotAllowed/2)) {
    
    # Categorize the potential issues in the file (note to suppress or NA deaths)
    checkProblems <- df %>% 
        mutate(problem=(!is.na(Suppress) | is.na(deaths)), 
               noCheck=state %in% all_of(stateNoCheck)
        )
    
    # Print a list of the problems, excluding those in stateNoCheck
    cat("\nRows in states to be checked that have NA deaths or a note for suppression:\n")
    checkProblems %>%
        filter(problem, !noCheck) %>%
        arrange(desc(year), desc(week)) %>%
        select(state, weekEnding, year, week, age, Suppress, deaths) %>%
        as.data.frame() %>%
        print()
    
    # Summarize the problems
    cat("\n\nProblems by state:\n")
    checkProblems %>%
        group_by(noCheck, state, problem) %>%
        summarize(n=n(), deaths=specNA(sum)(deaths), .groups="drop") %>%
        filter(problem) %>%
        print()
    
    # Assess the amount of error
    errorState <- checkProblems %>%
        filter(problem, !noCheck) %>%
        count(state)
    
    # Error out if threshold for error by state OR total errors exceeded
    errMax <- errorState %>% pull(n) %>% max()
    errTot <- errorState %>% pull(n) %>% sum()
    cat("\n\nThere are", errTot, "rows with errors; maximum for any given state is", errMax, "errors\n")
    
    if ((errTot > errTotAllowed) | (errMax > errMaxAllowed)) {
        stop("\nToo many errors; thresholds are ", errTotAllowed, " total and ", errMaxAllowed, " maximum\n")
    }
    
}

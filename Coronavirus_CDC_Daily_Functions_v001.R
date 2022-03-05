# File to be sourced, contains functions for working with CDC daily data

# FUNCTIONS INCLUDED:
# 1.  readRunCDCDaily() - main function for working with CDC daily data
# 2.  getStateData() - function for getting a state population file (usually already saved)
# 3.  readQCRawCDCDaily() - function for read and QC of CDC daily data
# 4.  clusterStates() - function to run clustering on states
# 5.  getClusters() - helper function to get clusters from a processed list of CDC Daily data
# 6.  diagnoseClusters() - function to create plots and summaries of clusters
# 7.  integrateData() - function for creating integrated cluster data
# 8.  combineAggData() - function for combining and aggregating the cluster data
# 9.  createSummary() - function for creating the summary cluster dashboard page
# 10. helperSummaryMap() - function for creating summary-level maps
# 11. helperAggTotal() - function for creating plot of totals by cluster
# 12. helperAggTrend() - function for creating plot of trends by cluster
# 13. createDetailedSummaries() - function for creating detailed summaries by cluster
# 14. clustersToFrame() - function to convert useClusters to an appropriate tibble for further analysis
# 15. downloadReadHospitalData() - function to download hospital capacity data
# 16. postProcessCDCDaily() - post-processing for CDC daily data
# 17. createBurdenPivot() - create pivoted burden data
# 18. makeCaseHospDeath() - create case, hospital, and death file
# 19. cumulativeBurdenPlot() - plot of cumulative burden by time
# 20. cumulativeVaccinePlot() - plot of cumulative vaccines by time
# 21. hospAgePerCapita() - create per capita hospitalized by age
# 22. readPopStateAge() - read a previously downloaded file for population by state and age
# 23. filterPopStateAge() - filter a file of population by state and age for the relevant values
# 24. bucketPopStateAge() - bucket the population by state and age data
# 25. onePageCFRPlot() - plot all states on the same page
# 26. findCorrAlign() - find correlations and alignments of metrics
# 27. plotCFRLag() - plot the implied CFR and best lag/lead for dpm-cpm
# 28. makePeakValley() - mark the peaks and valleys in CDC daily data
# 29. plotHospitalUtilization() - create plots of hospital utilization
# 30. createGeoMap() - make state-level data map
# 31. skinnyHHS() - select and filter HHS data as needed
# 32. imputeNACapacity() - impute values for HHS capacity
# 33. sumImputedHHS() - sum the imputed HHS data to the state-week level

# Function to download/load, process, segment, and analyze data for CDC daily (last updated 02-AUG-2021)
readRunCDCDaily <- function(thruLabel, 
                            downloadTo=list("cdcDaily"=NA, "cdcHosp"=NA, "vax"=NA), 
                            readFrom=downloadTo, 
                            compareFile=list("cdcDaily"=NA, "cdcHosp"=NA, "vax"=NA),
                            writeLog=NULL,
                            ovrwriteLog=TRUE,
                            dfPerCapita=NULL,
                            useClusters=NULL,
                            hierarchical=TRUE,
                            returnList=!isTRUE(hierarchical), 
                            kCut=6,
                            reAssignState=vector("list", 0),
                            weightedMeanAggs=eval(formals(diagnoseClusters)$wm_aggVars),
                            detailedPlotAggs=weightedMeanAggs,
                            skipAssessmentPlots=FALSE,
                            brewPalette=NA,
                            ...
                            ) {
    
    # FUNCTION ARGUMENTS:
    # thruLabel: the label for when the data are through (e.g., "Aug 30, 2020")
    # donwloadTo: named list for locations to download data (cdcDaily and cdcHosp)
    #             NA means do not download data for that particular element
    # readFrom: named list for locations to read data from (defaults to donwloadTo)
    # compareFile: named list for the reference file to be used for cdcDaily and cdcHosp 
    #              NA means do not use a reference file for that element
    # dateChangePlot: boolean, should changes in dates be captured as a plot rather than as a list?
    # dateMetricPrint: boolean, should the changes by date and metric be printed to the main log?
    # writeLog: name of a separate log file for capturing detailed data on changes between files
    #           NULL means no detailed data captured
    # ovrwriteLog: boolean, should the log file be overwritten and started again from scratch?
    # dfPerCapita: file can be passed directly, which bypasses the loading and processing steps
    #              default NULL means create dfPerCapita using steps 2-4
    # useClusters: file containing clusters by state (NULL means make the clusters from the data)
    # hierarchical: boolean, should hierarchical clusters be produced (if FALSE, will be k-means)?
    # returnList: boolean, should a list be returned or just the cluster object?
    #             refers to what is returned by clusterStates(); the main function always returns a list
    # kCut: number of segments when cutting the hierarchical tree
    # reAssignState: mapping file for assigning a state to another state's cluster
    #                format list("stateToChange"="stateClusterToAssign")
    # weightedMeanAggs: variables where a population-weighted cluster mean should be created
    # detailedPlotAggs: variables that should be included in the cluster-level disease evolution detailed plots
    # skipAssessmentPlots: boolean to skip the plots for assessClusters()
    #                      especially useful if just exploring dendrograms or silhouette widths
    # brewPalette: create plots using this color scheme (needs to be valid in ggplot2::scale_*_brewer())
    #              NA means use R default color schemes
    # ...: arguments to be passed to clusterStates(), will be used only if useClusters is NULL
    
    # STEP 0: Function to create the return list
    createFinalList <- function(plots=TRUE) {
        list(stateData=stateData, 
             dfRaw=dfRawList, 
             dfProcess=dfProcessList, 
             dfPerCapita=dfPerCapita, 
             useClusters=useClusters, 
             plotDataList=if(plots) plotDataList else NULL
             )
    }
    
    # STEP 1: Get state data
    stateData <- getStateData()
    
    # If a log file is requested, create the log file (allows for append=TRUE for all downstream functions)
    if (!is.null(writeLog)) genNewLog(writeLog=writeLog, ovrwriteLog=ovrwriteLog)
    
    # Get the data types to be used (will be the elements of readFrom) and create a file storage list
    elemUsed <- names(readFrom)
    dfRawList <- vector("list", length=length(elemUsed)) %>% purrr::set_names(elemUsed)
    dfProcessList <- vector("list", length=length(elemUsed)) %>% purrr::set_names(elemUsed)
    
    # Steps 2-4 are run only is dfPerCapita has not been passed
    if (is.null(dfPerCapita)) {
        
        # Step 2: Download and QC all of the requested data
        for (elem in elemUsed) {
            dfRawList[[elem]] <- readQCRawCDCDaily(fileName=readFrom[[elem]], 
                                                   writeLog=writeLog, 
                                                   ovrwriteLog=FALSE,
                                                   urlType=elem, 
                                                   getData=if(is.na(downloadTo[[elem]])) FALSE else TRUE, 
                                                   dfRef=compareFile[[elem]]
                                                   )
            glimpseLog(dfRawList[[elem]], txt=paste0("\nRaw file for ", elem, ":\n"), logFile=writeLog)
        }
        
        # Step 3: Process all of the requested data
        for (elem in elemUsed) {
            dfProcessList[[elem]] <- processRawFile(dfRawList[[elem]], 
                                                    vecRename=c(), # already handled in readQCRawCDCDaily()
                                                    vecSelect=vecSelectMapper[[elem]], 
                                                    lstCombo=lstComboMapper[[elem]], 
                                                    lstFilter=lstFilterMapper[[elem]]
                                                    )
            glimpseLog(dfProcessList[[elem]], txt=paste0("\nProcessed for ", elem, ":\n"), logFile=writeLog)
        }
        
        # Step 4: Integrate in to a dfPerCapita file and glimpse (to specified log file)
        dfPerCapita <- createPerCapita(dfProcessList, 
                                       uqBy=c("state", "date"), 
                                       popData=stateData, 
                                       mapper=perCapMapper, 
                                       asIsVars=if(isTRUE(exists("asIsMapper"))) asIsMapper[[elem]] else c()
                                       )
        glimpseLog(dfPerCapita, txt="\nIntegrated per capita data file:\n", logFile=writeLog)
        
    } else {
        dfRawList <- NULL
        dfProcessList <- NULL
    }
    
    # STEP 5: Create the clusters (if they have not been passed)
    if (is.null(useClusters)) {
        clData <- clusterStates(df=dfPerCapita, hierarchical=hierarchical, returnList=returnList, ...)
        useClusters <- getClusters(clData, hier=hierarchical, kCut=kCut, reAssign=reAssignState)
    }
    
    # STEP 5a: Stop the process and return what is available if skipAssessmentPlots is TRUE
    if (skipAssessmentPlots) 
        return(createFinalList(plots=FALSE))
    
    # STEP 6: Create the cluster assessments
    lstFuns <- list("stateData"=function(x) colSelector(x, vecSelect=c("state", "pop")), 
                    "dfPerCapita"=NULL
                    )
    plotDataList <- diagnoseClusters(lst=list("stateData"=stateData, 
                                              "dfPerCapita"=dfPerCapita, 
                                              "useClusters"=useClusters
                                              ), 
                                     lstExtract=lstFuns,
                                     wm_aggVars=weightedMeanAggs,
                                     detailAggVars=detailedPlotAggs,
                                     brewPalette=brewPalette
                                     )
    
    # STEP 7: Return a list of the key data
    return(createFinalList(plots=TRUE))
    
}



# Function to extract and format key state data (last updated 02-AUG-2021)
getStateData <- function(df=readFromRDS("statePop2019"), 
                         renameVars=c("stateAbb"="state", "NAME"="name", "pop_2019"="pop"), 
                         keepVars=c("state", "name", "pop")
                         ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing state data
    # renameVars: variables to be renamed, using named list with format "originalName"="newName"
    # keepVars: variables to be kept in the final file
    
    # Rename variables where appropriate
    names(df) <- ifelse(is.na(renameVars[names(df)]), names(df), renameVars[names(df)])
    
    # Return file with only key variables kept
    df %>%
        select_at(vars(all_of(keepVars)))
    
}



# Function to read and check a raw data file (last updated 02-AUG-2021)
readQCRawCDCDaily <- function(fileName, 
                              writeLog=NULL,
                              ovrwriteLog=TRUE,
                              dfRef=NULL,
                              urlType=NULL,
                              url=NULL, 
                              getData=TRUE,
                              ovrWriteDownload=FALSE, 
                              vecRename=NULL, 
                              selfList=NULL,
                              fullList=NULL,
                              uniqueBy=NULL, 
                              step3Group=NULL,
                              step3Vals=NULL, 
                              step4KeyVars=NULL, 
                              step5PlotItems=NULL,
                              step6AggregateList=NULL,
                              inferVars=list("url"=urlMapper, 
                                             "vecRename"=renMapper, 
                                             "selfList"=selfListMapper, 
                                             "fullList"=fullListMapper, 
                                             "uniqueBy"=uqMapper, 
                                             "step3Group"=checkControlGroupMapper,
                                             "step3Vals"=checkControlVarsMapper, 
                                             "step4KeyVars"=checkSimilarityMapper, 
                                             "step5PlotItems"=plotSimilarityMapper,
                                             "step6AggregateList"=keyAggMapper
                                             )
                              ) {
    
    # FUNCTION ARGUMENTS
    # fileName: the location where downloaded data either is, or will be, stored
    # writeLog: the external file location for printing (NULL means use the main log stdout)
    # ovrwriteLog: boolean, if using an external log, should it be started from scratch (overwritten)?
    # dfRef: a reference data frame for comparison (either NULL or NA means do not run comparisons)
    # urlType: character vector that can be mapped using urlMapper and keyVarMapper
    # url: direct URL passed as character string
    #      NOTE that if both url and urlType are NULL, no file will be downloaded
    # getData: boolean, should an attempt be made to get new data using urlType or url?
    # ovrWriteDownload: boolean, if fileName already exists, should it be overwritten?
    # vecRename: vector for renaming c('existing name'='new name'), can be any length from 0 to ncol(df)
    #            NULL means infer from urlType, if not available there use c()
    # selfList: list for functions to apply to self, list('variable'=fn) will apply variable=fn(variable)
    #           processed in order, so more than one function can be applied to self
    #           NULL means infer from urlType, if not available in mapping file use list()
    # fullList: list for general functions to be applied, list('new variable'=expression(code))
    #           will create 'new variable' as eval(expression(code))
    #           for now, requires passing an expression
    #           NULL means infer from urlType, use list() if not in mapping file
    # uniqueBy: combination of variables for checking uniqueness
    #           NULL means infer from data, keep as NULL (meaning use-all) if cannot be inferred
    # step3Group: variable to be used as the x-axis (grouping) for step 3 plots
    #             NULL means infer from data
    # step3Vals: values to be plotted on the y-axis for step 3 plots
    #            NULL means infer from data
    # step4KeyVars: list of parameters to be passed as keyVars= in step 4
    #               NULL means infer from urlType
    # step5PlotItems: items to be plotted in step 5
    #                 NULL means infer from urlType
    # step6AggregateList: drives the elements to be passed to compareAggregate() and flagLargeDelta()
    #                     NULL means infer from urlType
    # inferVars: vector of c('variable'='mapper') for inferring parameter values when passed as NULL
    
    # Step 0a: Use urlType to infer key variables if passed as NULL
    for (vrbl in names(inferVars)) {
        mapper <- inferVars[[vrbl]]
        if (is.null(get(vrbl))) {
            if (urlType %in% names(mapper)) assign(vrbl, mapper[[urlType]])
            else if ("default" %in% names(mapper)) assign(vrbl, mapper[["default"]])
        }
    }
    
    # Step 1: Download a new file (if requested)
    if (!is.null(url) & isTRUE(getData)) fileDownload(fileName=fileName, url=url, ovrWrite=ovrWriteDownload)
    else cat("\nNo file has been downloaded, will use existing file:", fileName, "\n")
    
    # Step 2: Read file, rename and mutate variables, confirm uniqueness by expected levels
    dfRaw <- fileRead(fileName) %>% 
        colRenamer(vecRename) %>% 
        colMutater(selfList=selfList, fullList=fullList) %>%
        checkUniqueRows(uniqueBy=uniqueBy)
    
    # Step 3: Plot basic control totals for new cases and new deaths by month
    dfRaw %>%
        checkControl(groupBy=step3Group, useVars=step3Vals, printControls=FALSE, na.rm=TRUE) %>%
        helperLinePlot(x=step3Group, y="newValue", facetVar="name", facetScales="free_y", groupColor="name")
    
    # If there is no file for comparison, return the data
    if (is.null(dfRef) | if(length(dfRef)==1) is.na(dfRef) else FALSE) return(dfRaw)
    
    # Step 4b: Check similarity of existing and reference file
    # ovrWriteLog=FALSE since everything should be an append after the opening text line in step 0
    diffRaw <- checkSimilarity(df=dfRaw, 
                               ref=dfRef, 
                               keyVars=step4KeyVars, 
                               writeLog=writeLog, 
                               ovrwriteLog=FALSE
                               )
    
    # Step 5: Plot the similarity checks
    plotSimilarity(diffRaw, plotItems=step5PlotItems)
    
    # Step 6: Plot and report on differences in aggregates
    helperAggMap <- function(x) {
        h1 <- compareAggregate(df=dfRaw, ref=dfRef, grpVar=x$grpVar, numVars=x$numVars, 
                               sameUniverse=x$sameUniverse, plotData=x$plotData, isLine=x$isLine, 
                               returnDelta=x$returnDelta)
        if (isTRUE(x$flagLargeDelta)) {
            h2 <- flagLargeDelta(h1, pctTol=x$pctTol, absTol=x$absTol, sortBy=x$sortBy, 
                                 dropNA=x$dropNA, printAll=x$printAll
                                 )
            if (is.null(writeLog)) print(h2)
            else {
                cat(nrow(h2), " records", sep="")
                txt <- paste0("\n\n***Differences of at least ", 
                              x$absTol, 
                              " and at least ", 
                              round(100*x$pctTol, 3), "%\n\n"
                              )
                printLog(h2, txt=txt, writeLog=writeLog)
            }
        }
    }
    lapply(step6AggregateList, FUN=helperAggMap)
    
    cat("\n\n")
    
    # Return the raw data file
    dfRaw
    
}



# Updates to the clustering function
clusterStates <- function(df, 
                          caseVar="cpm", 
                          deathVar="dpm",
                          totCaseVar=NULL, 
                          totDeathVar=NULL,
                          shapeFunc=customYYYYMM, 
                          minShape=NULL, 
                          maxShape=NULL,
                          minDeath=0,
                          maxDeath=Inf,
                          minCase=0,
                          maxCase=Inf,
                          ratioTotalvsShape=1, 
                          ratioDeathvsCase=1, 
                          hierarchical=TRUE, 
                          hierMethod="complete", 
                          nCenters=3, 
                          iter.max=10,
                          nstart=1,
                          testCenters=NULL,
                          returnList=FALSE, 
                          hmlSegs=3, 
                          eslSegs=2,
                          seed=NULL
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing cases and deaths data
    # caseVar: the variable containing the daily cases per capita data
    # deathVar: the variable containing the daily deaths per capita data
    # totCaseVar: a variable containing total cases per capita (may differ from sum of new due to bulk adds)
    #             NULL means use sum(caseVar), otherwise use value of totCaseVar on last day of data
    # totDeathVar: a variable containing total deaths per capita (may differ from sum of new due to bulk adds)
    #             NULL means use sum(deathVar), otherwise use value of totDeathVar on last day of data
    # shapeFunc: the function to be used for creating the shape of the curve
    # minShape: the minimum value to be used for shape (to avoid very small amounts of data in Jan/Feb/Mar)
    #           shape is the month, so 4 means start with April data (NULL means keep everything)
    # maxShape: the maximum value to be used for shape (to avoid very small amounts of data in a partial month)
    #           shape is the month, so 9 means end with September data (NULL means keep everything)
    # minDeath: use this value as a floor for the death metric when calculating shape
    # maxDeath: use this value as a maximum when calculating distance using deaths 
    # minCase: use this value as a floor for the case metric when calculating shape
    # maxCase: use this value as a maximum when calculating distance using cases 
    # ratioTotalvsShape: amount of standard deviation to be kept in total variable vs shape variables
    # ratioDeathvsCase: amount of standard deviation to be kept in deaths vs cases 
    #                   (total death data will be scaled to have sd this many times higher than cases)
    #                   (death percentages by time period will be scaled directly by this amount)
    # hierarchical: whether to create hierarchical clusters
    #               TRUE means run hierarchical clustering
    #               FALSE means run kmeans clustering
    #               NA means run rules-based clustering
    # hierMethod: the method for hierarchical clustering (e.g., 'complete' or 'single')
    # nCenters: the number of centers to use for kmeans clustering
    # testCenters: integer vector of centers to test (will create an elbow plot); NULL means do not test
    # iter.max: maximumum number of kmeans iterations (default in kmeans algorithm is 10)
    # nstart: number of random sets chosen for kmeans (default in kmeans algorithm is 1)
    # returnList: boolean, if FALSE just the cluster object is returned
    #                      if TRUE, a list is returned with dfCluster and the cluster object
    # hmlSegs: number of segments to create for volume of burden integrated over time
    # eslSegs: number of segments to create for shape of burden over time
    # seed: set the seed to this value (NULL means no seed)
    
    # Create the timeBucket field, then filter the data to only the time periods of interest
    df <- df %>%
        mutate(timeBucket=shapeFunc(date))
    
    # Limit to only relevant time buckets if requested
    if (!is.null(minShape)) {
        df <- df %>%
            filter(timeBucket >= minShape)
    }
    if (!is.null(maxShape)) {
        df <- df %>%
            filter(timeBucket <= maxShape)
    }
    
    # Create an aggregate by state, scaled so that they have the proper ratio
    # If totCaseVar is NULL, use sum(cases), otherwise use max(cases)
    # If totDeathVar is NULL, use sum(cases), otherwise use max(cases)
    dfAgg <- df %>%
        group_by(state) %>%
        summarize(origTotalCases=if(is.null(totCaseVar)) sum(get(caseVar)) else max(get(totCaseVar)), 
                  origTotalDeaths=if(is.null(totDeathVar)) sum(get(deathVar)) else max(get(totDeathVar)), 
                  .groups="drop"
        ) %>%
        mutate(totalCases=pmin(origTotalCases, maxCase), totalDeaths=pmin(origTotalDeaths, maxDeath)) %>%
        ungroup() %>%
        mutate(totalDeaths=ratioDeathvsCase*totalDeaths*sd(totalCases)/sd(totalDeaths)) %>%
        select(-origTotalCases, -origTotalDeaths)  # fields are just for QC while writing function
    
    # Create shape of the curve by state
    dfShape <- df %>%
        select_at(vars(all_of(c("timeBucket", "state", caseVar, deathVar)))) %>%
        purrr::set_names(c("timeBucket", "state", "cases", "deaths")) %>%
        group_by(state, timeBucket) %>%
        summarize_if(is.numeric, .funs=sum) %>%
        ungroup() %>%
        pivot_longer(-c(state, timeBucket)) %>%
        group_by(state, name) %>%
        mutate(tot=pmax(sum(value), ifelse(name=="deaths", minDeath, minCase)), 
               value=ifelse(name=="deaths", ratioDeathvsCase, 1) * value / tot) %>%
        select(-tot) %>%
        pivot_wider(state, names_from=c(name, timeBucket), values_from=value) %>%
        ungroup()
    
    # Function to calculate SD of a subset of columns
    calcSumSD <- function(df) {
        df %>% 
            ungroup() %>% 
            select(-state) %>% 
            summarize_all(.funs=sd) %>% 
            as.vector() %>% 
            sum()
    }
    
    # Down-weight the aggregate data so that there is the proper sum of sd in aggregates and shapes
    aggSD <- calcSumSD(dfAgg)
    shapeSD <- calcSumSD(dfShape)
    dfAgg <- dfAgg %>%
        mutate_if(is.numeric, ~. * ratioTotalvsShape * shapeSD / aggSD)
    
    # Combine so there is one row per state
    dfCluster <- dfAgg %>%
        inner_join(dfShape, by="state")
    
    # convert 'state' to rowname
    keyData <- dfCluster %>% 
        column_to_rownames("state")
    
    # Create rules-based segments (NA) or hierarchical segments (TRUE) or kmeans segments (FALSE)
    if (is.na(hierarchical)) {
        # Create pseudo-rules-based segments
        if (!is.null(seed)) set.seed(seed)
        # STEP 1: Classify high-medium-low based on deaths and cases
        hml <- kmeans(select(keyData, starts_with("total")), 
                      centers=hmlSegs, iter.max=iter.max, nstart=nstart
        )
        # STEP 2: Classify early-late based on shape
        esl <- kmeans(select(keyData, -starts_with("total")), 
                      centers=eslSegs, iter.max=iter.max, nstart=nstart
        )
        # STEP 3: Create a final segment
        objCluster <- eslSegs*(hml$cluster-1) + esl$cluster
    } else if (isTRUE(hierarchical)) {
        # Create hierarchical segments
        objCluster <-  hclust(dist(keyData), method=hierMethod)
        plot(objCluster)
    } else {
        # Create k-means segments
        # Create an elbow plot if testCenters is not NULL
        if (!is.null(testCenters)) {
            helperElbow(keyData, testCenters=testCenters, iter.max=iter.max, nstart=nstart, silhouette=TRUE)
        }
        # Create the kmeans cluster object, setting a seed if requested
        if (!is.null(seed)) set.seed(seed)
        objCluster <- kmeans(keyData, centers=nCenters, iter.max=iter.max, nstart=nstart)
        cat("\nCluster means and counts\n")
        n=objCluster$size %>% cbind(objCluster$centers) %>% round(2) %>% t() %>% print()
    }
    
    # Return the data and object is a list if returnList is TRUE, otherwise return only the clustering object
    if (!isTRUE(returnList)) {
        objCluster
    } else {
        list(objCluster=objCluster, dfCluster=dfCluster)
    }
    
}



# Helper function to get the clusters from a clustering file
getClusters <- function(clData, hier=FALSE, kCut=0, reAssign=list()) {        
    
    # If hierarchical clusters, cut the tree, otherwise use the output object directly
    if (isTRUE(hier)) useClusters <- cutree(clData, k=kCut)
    else if (is.na(hier)) useClusters <- clData$objCluster
    else useClusters <- clData$objCluster$cluster
    
    # If requested, manually assign clusters to the cluster for another state
    for (xNum in seq_len(length(reAssign))) {
        useClusters[names(reAssign)[xNum]] <- useClusters[reAssign[[xNum]]]
    }
    
    # Return the clusters
    useClusters
    
}



# Function to create diagnoses and plots for clustering data (last updated 02-AUG-2021)
diagnoseClusters <- function(lst, 
                             lstExtract=fullListExtract,
                             clusterFrame=clustersToFrame(lst),
                             wm_aggVars=c("tcpm7", "tdpm7", "cpm7", "dpm7", "hpm7"),
                             summaryAggVars=c("wm_tcpm7", "wm_tdpm7", "wm_hpm7"),
                             detailAggVars=c("tcpm7", "tdpm7", "cpm7", "dpm7", "hpm7"),
                             brewPalette=NA, 
                             printSummary=TRUE, 
                             printDetailed=TRUE
                             ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a list containing processed clustering data
    # lstExtract: the elements to extract from lst with an optional function for converting the elements
    #             NULL means use the extracted element as-is
    # clusterFrame: the clusters to be plotted (default is to match to useClusters)
    # wm_aggVars: variables where a population-weighted mean should be produced for the cluster-aggregate
    # summaryAggVars: variables to be included in plot 4 for the overall summary
    # detailAggVars: variables to be included in plot 2 for the detailed summary
    # brewPalette: the color palette to use with scale_*_brewer()
    #              default NA means use the standard color/fill profile
    # printSummary: boolean, should summary plots be printed to the log?
    # printDetailed: boolean, should detailed plots be printed to the log?
    
    # Create the integrated and aggregate data from lst
    dfFull <- integrateData(lst, lstExtract=lstExtract, otherDF=list(clusterFrame))
    dfAgg <- combineAggData(dfFull, wm_aggVars=wm_aggVars)
    
    # Create the main summary plots
    summaryPlots <- createSummary(dfAgg, 
                                  stateClusterDF=clusterFrame, 
                                  brewPalette=brewPalette, 
                                  p4AggVars=summaryAggVars
                                  )
    
    # Create the detailed summaries
    detPlots <- createDetailedSummaries(dfDetail=dfFull, 
                                        dfAgg=dfAgg, 
                                        brewPalette=brewPalette, 
                                        p2DetMetrics=detailAggVars, 
                                        mapper=c("tcpm"="Cases per million\n(cumulative)", 
                                                 "tdpm"="Deaths per million\n(cumulative)", 
                                                 "cpm7"="Cases\nper million", 
                                                 "dpm7"="Deaths\nper million",
                                                 "hpm7"="Hospitalized\nper million",
                                                 "tdpm7"="Deaths (cum)\nper million",
                                                 "tcpm7"="Cases (cum)\nper million", 
                                                 "vxcpm7"="Fully vaccinated\nper million", 
                                                 "vxcgte65pct"="Fully vacinated\n65+ (%)"
                                                 )
    )
    
    # Print the summary plots if requested
    if (isTRUE(printSummary)) {
        gridExtra::grid.arrange(summaryPlots$p1 + theme(legend.position="none"), 
                                summaryPlots$p3 + theme(legend.position="left"), 
                                summaryPlots$p4, 
                                layout_matrix=rbind(c(1, 2), 
                                                    c(3, 3)
                                )
        )
    }
    
    # Print the detailed plots if requested
    if (isTRUE(printDetailed)) purrr::walk(detPlots, .f=print)
    
    # Return a list of the key plotting files
    list(dfFull=dfFull, 
         dfAgg=dfAgg, 
         plotClusters=clusterFrame, 
         summaryPlots=summaryPlots, 
         detPlots=detPlots
         )
    
}



# Function for creating an integrated data frame
integrateData <- function(lst=NULL, 
                          lstExtract=list("useClusters"=function(x) clustersToFrame(x), 
                                          "stateData"=function(x) colSelector(x, vecSelect=c("state", "pop")), 
                                          "dfPerCapita"=NULL
                                          ),
                          keyJoin="state",
                          fnJoin=dplyr::inner_join,
                          otherDF=list()
                          ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a list containing one or more data frame elements
    # lstExtract: the elements to extract from lst with an optional function for converting the elements
    #             NULL means use the extracted element as-is
    # keyJoin: the merge key for the data frames
    # fnJoin: the merge function, applied as reduce starting with lstExtract and ending with ...
    # otherDF: list of additional data frames to be joined
    
    dfList <- list()
    
    # If a list has been provided, extract the relevant elements
    if (!is.null(lst)) {
        for (elem in names(lstExtract)) {
            dfList[[elem]] <- lst[[elem]]
            if (!(is.null(lstExtract[[elem]]))) dfList[[elem]] <- lstExtract[[elem]](dfList[[elem]])
        }
    }
    
    # Append any other data frames passed
    dfList <- append(dfList, otherDF)
    
    # Return a consolidated data frame
    joinFrames(dfList, fnJoin=fnJoin, keyJoin=keyJoin)
    
}



# Function for creating an aggregate-level frame (last updated 02-AUG-2021)
combineAggData <- function(df, 
                           aggTo=c("cluster", "date"), 
                           wm_aggVars=c("tcpm7", "tdpm7", "cpm7", "dpm7", "hpm7"),
                           aggBy=list("agg1"=list(aggFunc=specNA(specSumProd), 
                                                  aggVars=c("pop"), 
                                                  wtVar=NULL, 
                                                  prefix=NULL
                                                  ), 
                                      "agg2"=list(aggFunc=specNA(weighted.mean), 
                                                  aggVars=wm_aggVars, 
                                                  wtVar="pop", 
                                                  prefix="wm_"
                                                  )
                                      ),
                           fnJoin=dplyr::full_join
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: a data frame containing data for summarizing to an aggregate
    # aggTo: the level to which data should be aggregated
    # wm_aggVars: variables for which a population weighted mean should be created for the cluster total
    # aggBy: a list of lists directing the creation of aggregates
    # fnJoin: a function for joining the relevant aggTo aggregates
    
    # Create an empty aggregation list
    aggList <- list()
    
    # Create the aggregates based on the instruction list
    # Potentially update later so that the instructions can be skipped when the defaults should be used
    for (ctr in seq_along(aggBy)) {
        aggList[[ctr]] <- df %>%
            createGroupAgg(aggTo=aggTo, 
                           aggFunc=aggBy[[ctr]]$aggFunc, 
                           aggVars=aggBy[[ctr]]$aggVars,
                           wtVar=aggBy[[ctr]]$wtVar, 
                           prefix=aggBy[[ctr]]$prefix
                           )
    }
    
    # Return the joined data
    joinFrames(aggList, fnJoin=fnJoin, keyJoin=aggTo)
    
}



# Function for creating plots that can be combined on a dashboard page (last updated 02-AUG-2021)
createSummary <- function(df, 
                          p4AggVars=c("wm_tcpm7", "wm_tdpm7", "wm_hpm7"),
                          stateClusterDF=NULL,
                          brewPalette=NA
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: an integrated data frame by cluster-date
    # p4AggVars: variables to be used for aggregates in plot 4
    # stateClusterDF: a data frame containing state-cluster (NULL means it can be found in df)
    # brewPalette: character string for a palette from RColorBrewer to be used (NA means default colors)
    
    # Create plots that can be relevant for a dashboard, including:
    # 1. Map of segments
    # 2. Bar plot of counts by segment
    # 3. Facetted bar plot of segment descriptors (e.g., population, burden per million)
    # 4. Facetted trend-line plot of burden by segments
    
    # Create a map of the clusters
    p1 <- helperSummaryMap(if(is.null(stateClusterDF)) df else stateClusterDF, 
                           discreteValues=TRUE, 
                           labelScale=is.na(brewPalette), 
                           textLabel=c("RI", "CT", "DE", "MD", "DC"),
                           extraArgs=if(is.na(brewPalette)) list() else 
                               list("arg1"=scale_fill_brewer("Cluster", palette=brewPalette))
                           )
    
    # Create a bar plot of counts by segment
    p2 <- helperSummaryMap(if(is.null(stateClusterDF)) df else stateClusterDF, 
                           discreteValues=TRUE, 
                           labelScale=is.na(brewPalette), 
                           countOnly=TRUE,
                           extraArgs=if(is.na(brewPalette)) list() else 
                               list("arg1"=scale_fill_brewer("Cluster", palette=brewPalette))
                           )
    
    # Create plot for population and burden by cluster
    p3 <- df %>%
        helperAggTotal(aggVars=c("pop", "wm_tcpm7", "wm_tdpm7"), 
                       mapper=c("pop"="Population (millions)", 
                                "wm_tcpm7"="Cases per thousand", 
                                "wm_tdpm7"="Deaths per million"
                                ), 
                       xLab=NULL, 
                       yLab=NULL, 
                       title=NULL,
                       divideBy=c("pop"=1000000, "wm_tcpm7"=1000), 
                       extraArgs=if(is.na(brewPalette)) list() else 
                           list("arg1"=scale_fill_brewer("Cluster", palette=brewPalette))
                       )
    
    # Create plot for cumulative burden per million over time
    p4xtra <- list(arg1=scale_x_date(date_breaks="2 months", date_labels="%b-%y"), 
                   arg2=theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                   )
    if(!is.na(brewPalette)) p4xtra$arg3 <- scale_color_brewer("Cluster", palette=brewPalette)
    p4 <- df %>%
        helperAggTrend(aggVars=p4AggVars, 
                       mapper=c("wm_tcpm7"="Cases per thousand\n(cumulative)", 
                                "wm_tdpm7"="Deaths per million\n(cumulative)", 
                                "wm_hpm7"="Hospitalized per million\n(current)", 
                                "wm_vxcpm7"="Fully vaccinated\n(% of total pop)",
                                "wm_vxcgte65pct"="Fully vaccinated\n(% of total pop 65+)"
                                ),
                       yLab=NULL,
                       title=NULL, 
                       divideBy=c("wm_tcpm7"=1000, "wm_vxcpm7"=1000000, "wm_vxcgte65pct"=100), 
                       linesize=0.75,
                       extraArgs=p4xtra
        )
    
    list(p1=p1, p2=p2, p3=p3, p4=p4)
    
}



# Helper function to make a summary map
helperSummaryMap <- function(df, 
                             mapLevel="states", 
                             keyCol="state",
                             values="cluster",
                             discreteValues=NULL,
                             legend.position="right",
                             labelScale=TRUE,
                             extraArgs=list(),
                             countOnly=FALSE,
                             textLabel=c(),
                             ...
                             ) {
    
    # FUNCTION ARGUMENTS:
    # df: a data frame containing a level of geography and an associated cluster
    # mapLevel: a parameter for whether the map is "states" or "counties"
    # keyCol: the key column for plotting (usmap::plot_usmap is particular, and this must be 'state' or 'fips')
    # values: the character name of the field containing the data to be plotted
    # discreteValues: boolean for whether the values are discrete (if not, use continuous)
    #                 NULL means infer from data
    # legend.position: character for the location of the legend in the plot
    # labelScale: boolean, should an scale_fill_ be created?  Use FALSE if contained in extraArgs
    # extraArgs: list of other arguments that will be appended as '+' to the end of the usmap::plot_usmap call
    # countOnly: should a bar plot of counts only be produced?
    # textLabel: a list of elements that should be labelled as text on the plot (too small to see)
    # ...: other parameters to be passed to usmap::plot_usmap (e.g., labels, include, exclude, etc.)
    
    # Modify the data frame to contain only the relevant data
    df <- df %>%
        select(all_of(c(keyCol, values))) %>%
        distinct()
    
    # Determine the type of data being plotted
    if (is.null(discreteValues)) discreteValues <- !is.numeric(df[[values]])
    
    # Convert data type if needed
    if (isTRUE(discreteValues) & is.numeric(df[[values]])) 
        df[[values]] <- factor(df[[values]])
    
    # If count only is needed, create a count map; otherwise create a map
    if (isTRUE(countOnly)) { 
        gg <- df %>%
            ggplot(aes(x=fct_rev(get(values)))) + 
            geom_bar(aes_string(fill=values)) + 
            stat_count(aes(label=..count.., y=..count../2), 
                       geom="text", 
                       position="identity", 
                       fontface="bold"
                       ) +
            coord_flip() + 
            labs(y="Number of members", x="")
    } else {
        gg <- usmap::plot_usmap(regions=mapLevel, data=df, values=values, ...)
        if (length(textLabel) > 0) {
            labDF <- df %>% 
                filter(get(keyCol) %in% textLabel) %>%
                mutate(rk=match(get(keyCol), textLabel)) %>%
                arrange(rk) %>%
                mutate(lon=-70.1-seq(0, 0.8*length(textLabel)-0.8, by=0.8), 
                       lat=40.1-seq(0, 1.5*length(textLabel)-1.5, by=1.5)
                       ) %>%
                select(lon, lat, everything()) %>%
                usmap::usmap_transform()
            gg <- gg + geom_text(data=labDF, 
                                 aes(x=lon.1, y=lat.1, label=paste(get(keyCol), get(values))), 
                                 size=3.25
                                 )
        }
    }
    
    # Position the legend as requested
    gg <- gg + theme(legend.position=legend.position)
    
    # Create the scale if appropriate
    if (isTRUE(labelScale)) gg <- gg + 
        if(isTRUE(discreteValues)) scale_fill_discrete(values) else scale_fill_continuous(values)
    
    # Apply extra arguments
    for (ctr in seq_along(extraArgs)) gg <- gg + extraArgs[[ctr]]
    
    # Return the map object
    gg
    
}



# Function to plot an aggregate (non-temporal)
helperAggTotal <- function(df, 
                           aggVars,
                           grpVar="cluster", 
                           xLab="Cluster", 
                           yLab="Totals", 
                           title=paste0(yLab, " by cluster"), 
                           subtitle=NULL, 
                           divideBy=c(),
                           mapper=varMapper,
                           extraArgs=list()
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: a data frame containing the relevant aggregate
    # aggVars: the aggregate variables to be plotted
    # grpVar: the intended facetting variable in the plot
    # xLab: the xlabel for the plot
    # yLab: the ylabel for the plot
    # title: the title for the plot
    # subtitle: the subtitle for the plot (NULL means no subtitle)
    # divideBy: a named list of c("variable"=divisor) where variable will be divided by divisor
    #           not included in list means no divisor (will be treated as 1)
    # mapper: mapping file for variable to descriptive name
    # extraArgs: list of other arguments that will be appended as '+' to the end of the ggplot call
    
    # Ensure that grpVar is not numeric (convert to cluster if needed)
    for (vrbl in grpVar) if(is.numeric(df[[vrbl]])) df[[vrbl]] <- factor(df[[vrbl]])
    
    # Create the aggregate plot
    gg <- df %>%
        select(all_of(c(grpVar, aggVars))) %>%
        pivot_longer(-all_of(c(grpVar))) %>%
        filter(!is.na(value)) %>%
        group_by_at(all_of(c(grpVar, "name"))) %>%
        filter(row_number()==n()) %>%
        ungroup() %>%
        mutate(useValue=ifelse(name %in% names(divideBy), value/divideBy[name], value)) %>%
        ggplot(aes(x=fct_rev(get(grpVar)), y=useValue)) + 
        geom_col(aes_string(fill=all_of(grpVar))) + 
        geom_text(aes(y=useValue/2, label=round(useValue))) +
        coord_flip() +
        facet_wrap(~mapper[name], scales="free_x") +
        labs(x=xLab, y=yLab, title=title)
    if (!is.null(subtitle)) gg <- gg + labs(subtitle=subtitle)
    for (ctr in seq_along(extraArgs)) gg <- gg + extraArgs[[ctr]]
    
    # Return the plot
    gg
    
}



# Function to plot an aggregate over time
helperAggTrend <- function(df, 
                           aggVars,
                           xVar="date",
                           grpVar="cluster", 
                           xLab="", 
                           yLab="Rolling 7-day mean per million", 
                           title=paste0(yLab, " by cluster"), 
                           subtitle=NULL,
                           divideBy=c(),
                           linesize=0,
                           mapper=varMapper,
                           extraArgs=list()
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: a data frame containing the relevant aggregate
    # aggVars: the aggregate variables to be plotted
    # xVar: the x-variable to be plotted (must be in df)
    # grpVar: the grouping variable in df
    #         note that df should be unique by xVar-grpVar
    # xLab: the xlabel for the plot
    # yLab: the ylabel for the plot
    # title: the title for the plot
    # subtitle: the subtitle for the plot (NULL means no subtitle)
    # divideBy: a named list of c("variable"=divisor) where variable will be divided by divisor
    #           not included in list means no divisor (will be treated as 1)
    # linesize: argument passed to geom_line(size=); zero will be the default thinnest line
    # mapper: mapping file for variable to descriptive name
    # extraArgs: list of other arguments that will be appended as '+' to the end of the ggplot call
    
    # Ensure that grpVar is not numeric (convert to cluster if needed)
    for (vrbl in grpVar) if(is.numeric(df[[vrbl]])) df[[vrbl]] <- factor(df[[vrbl]])
    
    # Create the aggregate plot
    gg <- df %>%
        select(all_of(c(xVar, grpVar, aggVars))) %>%
        pivot_longer(-all_of(c(xVar, grpVar))) %>%
        filter(!is.na(value)) %>%
        mutate(useValue=ifelse(name %in% names(divideBy), value/divideBy[name], value)) %>%
        ggplot(aes_string(x=xVar, y="useValue")) +
        geom_line(aes_string(group=grpVar, color=grpVar), size=linesize) +
        facet_wrap(~mapper[name], scales="free_y") +
        labs(x=xLab, y=yLab, title=title)
    if (!is.null(subtitle)) gg <- gg + labs(subtitle=subtitle)
    for (ctr in seq_along(extraArgs)) gg <- gg + extraArgs[[ctr]]
    
    # Return the plot
    gg
    
}



# Function to create detailed cluster summaries
createDetailedSummaries <- function(dfDetail, 
                                    dfAgg, 
                                    aggVar=c("cluster"), 
                                    detVar=c("state"),
                                    p1Metrics=c("tcpm", "tdpm"), 
                                    p1Order=c("tdpm"), 
                                    p2DetMetrics=c("tcpm7", "tdpm7", "cpm7", "dpm7", "hpm7"),
                                    p2AggMetrics=paste0("wm_", p2DetMetrics),
                                    p3Metrics=p1Metrics,
                                    p3Days=30,
                                    p3Slope=0.25,
                                    mapper=c("tcpm"="Cases per million\n(cumulative)", 
                                             "tdpm"="Deaths per million\n(cumulative)", 
                                             "cpm7"="Cases\nper million", 
                                             "dpm7"="Deaths\nper million",
                                             "hpm7"="Hospitalized\nper million",
                                             "tdpm7"="Deaths (cum)\nper million",
                                             "tcpm7"="Cases (cum)\nper million"
                                    ),
                                    brewPalette=NA
                                    ) {
    
    # FUNCTION ARGUMENTS:
    # dfDetail: data frame or tibble containing detailed (sub-cluster) data
    # dfAgg: data frame or tibble containing aggregated (cluster) data
    # aggVar: variable reflecting the aggregate level
    # detVar: variable reflecting the detailed level
    # p1Metrics: metrics to be shown for plot 1 (will be faceted)
    # p1Order: variable for ordering detVar in p1
    # p2DetMetrics: variables to be included from dfDetail for p2
    # p2AggMetrics: corresponding variables to be included from dfAgg for p2
    # p3Metrics: metrics to be included in the growth plots
    # p3Days: number of days to include for calculating growth
    # p3Slope: the slope for the dashed line for growth in p3
    # mapper mapping file for variable name to descriptive name
    # brewPalette: character string for a palette from RColorBrewer to be used (NA means default colors)    
    
    # Create plot for aggregates by sub-cluster
    if(detVar=="state") {
        p1 <- dfDetail %>%
            colSelector(vecSelect=c(detVar, aggVar, "date", p1Metrics)) %>%
            pivot_longer(all_of(p1Metrics)) %>%
            filter(!is.na(value)) %>%
            group_by_at(detVar) %>%
            filter(date==max(date)) %>%
            ungroup() %>%
            ggplot(aes(x=fct_reorder(get(detVar), 
                                     value, 
                                     .fun=function(x) { max(ifelse(name==p1Order, x, 0)) }
                                     ),
                       y=value
                       )
                   ) + 
            geom_col(aes(fill=get(aggVar))) + 
            facet_wrap(~mapper[name], scales="free_x") + 
            coord_flip() + 
            labs(title="Cumulative burden", x=NULL, y=NULL)
        if (!is.na(brewPalette)) p1 <- p1 + scale_fill_brewer("Cluster", palette=brewPalette)
    } else {
        # Do not create the plot for other than state-level data
        p1 <- NULL
    }
    
    # Create facetted burden trends by aggregate
    # For state-level, create each state as a line
    # For anything else, create a 10%-90% range
    if (detVar=="state") {
        p2 <- dfDetail %>%
            colSelector(vecSelect=c(detVar, "date", aggVar, p2DetMetrics)) %>%
            pivot_longer(all_of(p2DetMetrics)) %>%
            filter(!is.na(value)) %>%
            ggplot(aes(x=date, y=value)) + 
            geom_line(aes_string(group=detVar), color="grey", size=0.5) + 
            facet_grid(mapper[name] ~ get(aggVar), scales="free_y") + 
            scale_x_date(date_breaks="2 months", date_labels="%b-%y") + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
            labs(x=NULL, y=NULL, title="Burden by cluster and metric")
    } else {
        p2 <- dfDetail %>%
            colSelector(vecSelect=c(detVar, "date", aggVar, p2DetMetrics)) %>%
            pivot_longer(all_of(p2DetMetrics)) %>%
            filter(!is.na(value)) %>%
            group_by_at(c(aggVar, "name", "date")) %>%
            summarize(p10=unname(quantile(value, 0.1)), p90=unname(quantile(value, 0.9)), .groups="drop") %>%
            ggplot(aes(x=date)) + 
            geom_ribbon(aes(ymin=p10, ymax=p90), alpha=0.75) +
            facet_grid(mapper[name] ~ get(aggVar), scales="free_y") + 
            scale_x_date(date_breaks="2 months", date_labels="%b-%y") + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
            labs(x=NULL, 
                 y=NULL, 
                 title="Burden by cluster and metric", 
                 subtitle="Shaded region is 10%le through 90%le, solid line is weighted mean"
                 )
    }
    aggPlot <- dfAgg %>% 
        colSelector(vecSelect=c("date", aggVar, p2AggMetrics)) %>%
        colRenamer(vecRename=purrr::set_names(p2DetMetrics, p2AggMetrics)) %>%
        pivot_longer(all_of(p2DetMetrics)) %>%
        filter(!is.na(value))
    p2 <- p2 + 
        geom_line(data=aggPlot, aes_string(color=aggVar, group=aggVar, y="value"), size=1.5)
    if (!is.na(brewPalette)) p2 <- p2 + 
        scale_color_brewer("Cluster", palette=brewPalette) + 
        theme(legend.position="none")
    
    # Create growth trends plot
    if (TRUE) {
        p3 <- dfDetail %>%
            colSelector(vecSelect=c(detVar, aggVar, "date", p3Metrics)) %>%
            pivot_longer(all_of(p3Metrics)) %>%
            filter(!is.na(value)) %>%
            group_by_at(c(detVar, "name")) %>%
            filter(date %in% c(max(date), max(date)-lubridate::days(p3Days))) %>%
            mutate(growth=max(value)-min(value)) %>%  # not ideal way to calculate
            filter(date==max(date)) %>%
            ungroup() %>%
            ggplot(aes(x=value, y=growth))
        if(detVar=="state") p3 <- p3 + geom_text(aes_string(color=aggVar, label=detVar), fontface="bold")
        else p3 <- p3 + geom_point(aes_string(color=aggVar)) 
        p3 <- p3 + 
            facet_wrap(~mapper[name], scales="free") + 
            labs(title=paste0("Current vs growth"), 
                 subtitle=paste0("Dashed line represents ", 
                                 round(100*p3Slope), 
                                 "% growth rate over past ", 
                                 p3Days, 
                                 " days"
                                 ),
                 x="Most recent cumulative", 
                 y=paste0("Growth over past ", p3Days, " days")
                 ) + 
            lims(y=c(0, NA), x=c(0, NA)) + 
            theme(panel.background = element_rect(fill = "white", colour = "white"), 
                  panel.grid.major = element_line(size = 0.25, linetype = 'solid', color = "grey")
                  ) + 
            geom_abline(slope=p3Slope, intercept=0, lty=2)
        if (!is.na(brewPalette)) { 
            p3 <- p3 + scale_color_brewer(stringr::str_to_title(aggVar), palette=brewPalette)
        }
    } else {
        p3 <- NULL
    }
    
    # Return a list of plot objects
    list(p1=p1, p2=p2, p3=p3)
    
}



# Function to convert useClusters to a tibble for further analysis
clustersToFrame <- function(lst, 
                            colNameData="cluster", 
                            colNameName="state", 
                            convFactor=TRUE, 
                            fctLevels=NULL, 
                            fctLabels=NULL, 
                            fctOrdered=!is.null(fctLevels)
                            ) {
    
    # FUNCtION ARGUMENTS
    # lst: a list containing useClusters OR a vector of useClusters
    # colNameData: the column name in the tibble for the vector data
    # colNameName: the column name in the tibble for the vector names
    # convFactor: should colNameData be converted to a factor?
    # fctLevels: factor levels and order (NULL means use all sorted low to high)
    # fctLabels: factor labels (should be a one-to-one match with fctLevels)
    # fctOrdered: should an ordered factor be created?  default is 'yes if fctLevels have been passed
    
    # If lst is a list, extract the useClusters element
    if ("list" %in% class(lst)) lst <- lst[["useClusters"]]
    
    # Convert to tibble
    df <- vecToTibble(lst, colNameData=colNameData, colNameName=colNameName)
    
    # Convert to factor if requested
    if (isTRUE(convFactor)) {
        if (is.null(fctLevels)) fctLevels <- sort(unique(df[[colNameData]]))
        if (is.null(fctLabels)) fctLabels <- fctLevels
        df[[colNameData]] <- factor(df[[colNameData]], levels=fctLevels, labels=fctLabels, ordered=fctOrdered)
    }
    
    # Return the tibble
    df
    
}


# Function to download hospital capacity data
downloadReadHospitalData <- function(loc, 
                                     url="https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD",
                                     ovrWrite=FALSE, 
                                     mapper=hhsMapper
                                     ) {
    
    # FUNCTION ARGUMENTS:
    # loc: location for the downloaded data
    # url location for downloading data
    # ovrWrite: boolean, if loc exists, should it be overwritten?
    # mapper: character vector of form c("variable"="formatted name") of variables to run histograms for
    
    # Check if the file exists, download if appropriate
    tempDownload <- function(x=loc, y=url, z=ovrWrite) {
        
        if(file.exists(x)) {
            cat("\nFile", x, "already exists\n")
            if(!isTRUE(z)) {
                cat("File will not be downloaded since ovrWrite is not TRUE\n")
                return()
            }
        }
        
        # Download the file
        fileDownload(x, url=y, ovrWrite=z)
        
    }
    
    tempDownload()
    
    # Read the file and glimpse
    df <- fileRead(loc)
    glimpse(df)
    
    # Basic count checks
    cat("\nHospital Subtype Counts:\n")
    df %>% count(hospital_subtype) %>% print()
    cat("\nRecords other than 50 states and DC\n")
    df %>% count(state) %>% filter(!(state %in% c(state.abb, "DC"))) %>% print()
    
    # Counts of less than 0, NA, and -999999
    cat("\nRecord types for key metrics\n")
    df %>%
        select(names(mapper)) %>%
        pivot_longer(-c()) %>%
        mutate(type=case_when(is.na(value) ~ "NA", 
                              value==-999999 ~ "Value -999999", 
                              value < 0 ~ "Negative", 
                              TRUE ~ "Positive"
        )
        ) %>%
        count(name, type) %>%
        pivot_wider(name, names_from="type", values_from="n", values_fill=0) %>%
        group_by(name) %>%
        mutate(Total=sum(across(where(is.numeric)))) %>%
        ungroup() %>%
        print()
    
    # Basic Histograms (NA and -999999 are missing data)
    p1 <- df %>%
        select(names(mapper)) %>%
        pivot_longer(-c()) %>%
        filter(!is.na(value), value != -999999, value >= 0) %>%
        ggplot(aes(x=value/1000)) + 
        geom_histogram(fill="lightblue") + 
        scale_x_sqrt() +
        labs(x="Value (000s)", 
             y="# non-missing records", 
             title="Histogram for key metrics by record", 
             subtitle="Excludes values less than 0, as well as NA or -999999"
        ) +
        facet_wrap(~hhsMapper[name], scales="free")
    print(p1)
    
    # Return the file
    df
    
}


# Function for postr-processing CDC daily data
postProcessCDCDaily <- function(lst, 
                                dataThruLabel,
                                keyStates=c(state.abb, "DC"), 
                                keyDatesBurden=NULL, 
                                keyDatesVaccine=NULL,
                                returnData=FALSE,
                                ...
                                ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a processed list file from readRunCDCDaily
    # dataThruLabel: label for when the hospital data are through
    # keyStates: the list of states to be plotted (burden data will be created for all states)
    # keyDatesBurden: key dates to use for the burden plots (NULL means generate automatically)
    # keyDatesVaccine: key dates to use for the vaccine plots (NULL means generate automatically)
    # returnData: should the pivoted data be returned?
    # ...: other arguments passed through to cumulativeBurdenPlot()
    
    # Create the burden data
    burdenPivotList <- createBurdenPivot(lst, dataThru=dataThruLabel)
    
    # Create the cumulative burden plots
    cumulativeBurdenPlot(lst, 
                         keyStates=keyStates, 
                         keyDates=keyDatesBurden, 
                         ...
    )
    
    # Create the cumulative vaccines data
    cumulativeVaccinePlot(lst, 
                          keyStates=keyStates, 
                          keyDates=keyDatesVaccine, 
                          ...
    )
    
    if (isTRUE(returnData)) return(burdenPivotList)
    
}


# Create pivoted burden data
createBurdenPivot <- function(lst, 
                              dataThru,
                              minDatePlot="2020-08-01", 
                              plotByState=c(state.abb, "DC")
                              ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a processed list that includes sub-component $dfRaw$cdcHosp
    # dataThru: character string to be used for 'data through'; most commonly MMM-YY
    # minDatePlot: starting date for plots
    # plotByState: states to be facetted for plot of hospitaliztions by age (FALSE means do not create plot)
    
    # Convert minDatePlot to Date if passed as character
    if ("character" %in% class(minDatePlot)) minDatePlot <- as.Date(minDatePlot)
    
    # Create the hospitalized by age data
    hospAge <- lst[["dfRaw"]][["cdcHosp"]] %>%
        select(state, 
               date, 
               grep(x=names(.), pattern="ed_\\d.*[9+]$", value=TRUE), 
               grep(x=names(.), pattern="pediatric.*ed$", value=TRUE)
        ) %>% 
        pivot_longer(-c(state, date)) %>% 
        mutate(confSusp=ifelse(grepl(x=name, pattern="confirmed"), "confirmed", "suspected"), 
               adultPed=ifelse(grepl(x=name, pattern="adult"), "adult", "ped"), 
               age=ifelse(adultPed=="ped", 
                          "0-17", 
                          stringr::str_replace_all(string=name, pattern=".*_", replacement="")
               ), 
               age=ifelse(age %in% c("0-17", "18-19"), "0-19", age), 
               div=as.character(state.division)[match(state, state.abb)]
        )
    
    # Create the pivoted burden data
    dfPivot <- makeCaseHospDeath(dfHosp=hospAge, dfCaseDeath=lst[["dfPerCapita"]])
    
    # Plot for overall trends by age group
    p1 <- hospAge %>% 
        filter(state %in% c(state.abb, "DC"), !is.na(value)) %>% 
        mutate(ageBucket=age) %>% 
        group_by(date, ageBucket) %>% 
        summarize(value=sum(value), .groups="drop") %>% 
        arrange(date) %>%
        group_by(ageBucket) %>% 
        mutate(value7=zoo::rollmean(value, k=7, fill=NA)) %>% 
        filter(date >= minDatePlot) %>% 
        ggplot(aes(x=date, y=value7)) + 
        labs(x=NULL, 
             y="Confirmed or suspected COVID admissions (rolling-7 mean)", 
             title=paste0("Hospital admissions for COVID by age bucket (Aug 2020 - ", dataThru, ")"), 
             subtitle="50 states and DC (includes confirmed and suspected from CDC data)"
        ) + 
        lims(y=c(0, NA))
    
    # Create three main plots of hospitalized by age data
    print(p1 + geom_line(aes(group=ageBucket, color=ageBucket), size=1) + scale_color_discrete("Age\nbucket"))
    print(p1 + geom_col(aes(fill=ageBucket), position="stack") + scale_color_discrete("Age\nbucket"))
    print(p1 + geom_col(aes(fill=ageBucket), position="fill") + scale_color_discrete("Age\nbucket"))
    
    # Plot for trends by state and age group
    if (!isFALSE(plotByState)) {
        p2 <- hospAge %>% 
            filter(state %in% plotByState, !is.na(value)) %>% 
            mutate(ageBucket=ifelse(age >= "60", "60+", ifelse(age=="0-19", "0-19", "20-59"))) %>% 
            group_by(date, state, ageBucket) %>% 
            summarize(value=sum(value), .groups="drop") %>% 
            group_by(ageBucket, state) %>% 
            mutate(value7=zoo::rollmean(value, k=7, fill=NA)) %>% 
            filter(date >= minDatePlot) %>% 
            ggplot(aes(x=date, y=value7)) + 
            geom_line(aes(color=ageBucket, group=ageBucket)) + 
            scale_color_discrete("Age\nbucket") + 
            labs(x=NULL, 
                 y="Confirmed or suspected COVID admissions (rolling-7 mean)", 
                 title=paste0("Hospital admissions for COVID by age bucket (Aug 2020 - ", dataThru, ")")
            ) + 
            lims(y=c(0, NA)) + 
            facet_wrap(~state, scales="free_y")
        print(p2)
    }
    
    # Return key data (do not return plot objects)
    list(hospAge=hospAge, dfPivot=dfPivot)
    
}


# Function to create case-hospital-death file
makeCaseHospDeath <- function(dfHosp, dfCaseDeath) {
    
    # FUNCTION ARGUMENTS:
    # dfHosp: the tibble or data.frame containing the hospital data by date-state
    # dfCaseDeath: the tibble or data.frame containing the case and death data by date-state
    
    allHosp <- dfHosp %>%
        mutate(ageBucket=ifelse(age >= "60", "60+", ifelse(age=="0-19", "0-19", "20-59"))) %>% 
        group_by(date, state, ageBucket) %>% 
        summarize(value=sum(value), .groups="drop") %>% 
        group_by(ageBucket, state) %>% 
        mutate(value7=zoo::rollmean(value, k=7, fill=NA)) %>%
        ungroup() %>%
        left_join(getStateData(keepVars=c("state", "pop"))) %>%
        mutate(vpm7=1000000*value7/pop)
    
    allCaseDeath <- dfCaseDeath %>%
        select(state, date, new_cases, new_deaths, vxa, vxc, cpm7, dpm7, vxapm7, vxcpm7) %>%
        pivot_longer(-c(state, date))
    
    allPivot <- allHosp %>%
        select(state, date, name=ageBucket, value=vpm7) %>%
        bind_rows(allCaseDeath) %>%
        checkUniqueRows(uniqueBy=c("state", "date", "name"))
    
    allPivot
    
}


# Plot of cumulative burden by time
cumulativeBurdenPlot <- function(lst, 
                                 keyStates=c(state.abb, "DC"), 
                                 keyDates=NULL, 
                                 ...
                                 ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a processed list file containing dfPerCapita
    # keyStates: states to include in the plot
    # keyDates: dates to include in the burden plot
    #           NULL means default to max(date)-2 from current, 6 months ago, 12 months ago)
    # ...: other arguments to pass to tempStackPlot(), most commonly colorVector
    
    # Get the list of key dates
    if (is.null(keyDates)) {
        keyDates <- as.Date(max(lst[["dfPerCapita"]]$date)-2-lubridate::dmonths(c(0, 6, 12)), origin="1970-01-01")
    }
    
    # Convert to date if needed
    if (!("Date" %in% class(keyDates))) keyDates <- as.Date(keyDates)
    
    # Create data filtered for keyDates and keyStates
    burdenGrowth <- lst[["dfPerCapita"]] %>% 
        filter(date %in% all_of(keyDates), 
               state %in% all_of(keyStates)
        )
    
    # Create the naming vector for tempStackPlot
    vecName <- as.character(keyDates) %>% purrr::set_names(as.character(keyDates))
    
    # Create plot for cases
    p1 <- burdenGrowth %>%
        select(state, date, tcpm) %>% 
        mutate(tcpm=round(tcpm/1000)) %>%
        pivot_wider(state, names_from="date", values_from="tcpm") %>%
        tempStackPlot(yVars=vecName, 
                      yLab="Cumulative cases per thousand", 
                      plotTitle="Evolution of cumulative cases per thousand by state", 
                      addSuffix="",
                      scaleName="Date", 
                      ...
        )
    
    # Create plot for deaths
    p2 <- burdenGrowth %>%
        select(state, date, tdpm) %>% 
        mutate(tdpm=round(tdpm)) %>%
        pivot_wider(state, names_from="date", values_from="tdpm") %>%
        tempStackPlot(yVars=vecName, 
                      yLab="Cumulative deaths per million", 
                      plotTitle="Evolution of cumulative deaths per million by state", 
                      addSuffix="",
                      scaleName="Date", 
                      ...
        )
    
    # Print the plots
    gridExtra::grid.arrange(p1, p2, nrow=1)
    
    # Return the burden data
    burdenGrowth
    
}


# Cumulative plot for vaccines
cumulativeVaccinePlot <- function(lst, 
                                  keyStates=c(state.abb, "DC"), 
                                  keyDates=NULL, 
                                  returnData=FALSE,
                                  ...
                                  ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a processed list file containing dfPerCapita
    # keyStates: states to include in the plot
    # keyDates: dates to include in the burden plot
    #           NULL means default to max(date)-2 from current, 6 months ago, 12 months ago)
    # returnData: boolean, should the data be returned?
    # ...: other arguments to pass to tempStackPlot(), most commonly colorVector
    
    # Get the list of key dates
    if (is.null(keyDates)) {
        keyDates <- as.Date(max(lst[["dfRaw"]][["vax"]]$date)-2-lubridate::dmonths(c(0, 3, 6)), 
                            origin="1970-01-01"
        )
    }
    
    # Convert to date if needed
    if (!("Date" %in% class(keyDates))) keyDates <- as.Date(keyDates)
    
    # Chart for fully vaccinated by state
    p5 <- tempStackPlot(lst[["dfRaw"]][["vax"]] %>% filter(date==max(keyDates), state %in% keyStates), 
                        yVars=c("vxcgte65pct"="65+", 
                                "vxcgte18pct"="18+", 
                                "vxcpoppct"="All"
                        ), 
                        yLab="% Fully vaccinated", 
                        plotTitle=paste0("Fully vaccinated by age cohort and state\n(as of ", max(keyDates), ")"), 
                        makeDotPlot=TRUE, 
                        yLims = c(0, 105)
    )
    
    # Run for first dose
    p6 <- tempStackPlot(lst[["dfRaw"]][["vax"]] %>% filter(date==max(keyDates), state %in% keyStates), 
                        yVars=c("Administered_Dose1_Recip_65PlusPop_Pct"="65+", 
                                "Administered_Dose1_Recip_18PlusPop_Pct"="18+", 
                                "Administered_Dose1_Pop_Pct"="All"
                        ), 
                        yLab="% Receiving First Dose", 
                        plotTitle=paste0("First-dose vaccinated by age cohort and state\n(as of ", 
                                         max(keyDates), 
                                         ")"
                        ),
                        makeDotPlot=TRUE,
                        yLims=c(0, 105)
    )
    
    gridExtra::grid.arrange(p5, p6, nrow=1)
    
    # Create data filtered for keyDates and keyStates
    burdenGrowth <- lst[["dfRaw"]][["vax"]] %>% 
        filter(date %in% all_of(keyDates), 
               state %in% all_of(keyStates)
        )
    
    # Create the naming vector for tempStackPlot
    vecName <- as.character(keyDates) %>% purrr::set_names(as.character(keyDates))
    
    # Run for fully vaccinated
    p1 <- burdenGrowth %>%
        select(state, date, vxcpoppct) %>%
        pivot_wider(state, names_from="date", values_from="vxcpoppct") %>%
        tempStackPlot(yVars=vecName, 
                      yLab="% Fully Vaccinated (all population)", 
                      plotTitle="Evolution of fully vaccinated rate by state", 
                      ...
        )
    
    p2 <- burdenGrowth %>%
        select(state, date, vxcgte65pct) %>%
        pivot_wider(state, names_from="date", values_from="vxcgte65pct") %>%
        tempStackPlot(yVars=vecName, 
                      yLab="% Fully Vaccinated (65+)", 
                      plotTitle="Evolution of fully vaccinated rate by state", 
                      ...
        )
    
    gridExtra::grid.arrange(p1, p2, nrow=1)
    
    # Run for first dose
    p3 <- burdenGrowth %>%
        select(state, date, Administered_Dose1_Pop_Pct) %>%
        pivot_wider(state, names_from="date", values_from="Administered_Dose1_Pop_Pct") %>%
        tempStackPlot(yVars=vecName, 
                      yLab="% First-dose (all population)", 
                      plotTitle="Evolution of first dose rate by state", 
                      ...
        )
    
    p4 <- burdenGrowth %>%
        select(state, date, Administered_Dose1_Recip_65PlusPop_Pct) %>%
        pivot_wider(state, names_from="date", values_from="Administered_Dose1_Recip_65PlusPop_Pct") %>%
        tempStackPlot(yVars=vecName, 
                      yLab="% First-dose (65+)", 
                      plotTitle="Evolution of first dose rate by state", 
                      ...
        )
    
    gridExtra::grid.arrange(p3, p4, nrow=1)
    
    # Return the burden data
    if(isTRUE(returnData)) burdenGrowth
    
}


# Function to create per capita hospitalized by age
hospAgePerCapita <- function(dfBucket, 
                             lst, 
                             popVar, 
                             excludeState=c(), 
                             cumStartDate=NULL
                             ) {
    
    # FUNCTION ARGUMENTS:
    # dfBucket: data frame containing bucketed age data by state
    # lst: a processed list file containing $hospAge
    # popVar: name of the population variable in dfBucket
    # excludeState: list of states to exclude from cumulative plot
    # cumStateDate: data to start the cumulative plots (NULL means use earliest date in data)
    
    # Find cumStartDate if not passed
    if(is.null(cumStartDate)) cumStartDate <- lst[["hospAge"]]$date %>% min()
    
    # Create mapping from bucket10 to bucket03
    ageMap10to03 <- dfBucket %>% 
        count(bucket10, bucket03) %>%
        select(-n)
    
    # Create population by state and bucket03
    popStateBucket03 <- dfBucket %>%
        filter(sex=="Total", state != "US") %>%
        group_by(state, bucket03) %>% 
        summarize(pop=sum(get(popVar)), .groups="drop")
    
    # Create hospitalized by age bucket by state data
    dfUse <- lst[["hospAge"]] %>%
        left_join(ageMap10to03, by=c("age"="bucket10")) %>%
        filter(!is.na(value)) %>%
        group_by(state, date, bucket03) %>%
        summarize(value=sum(value), .groups="drop") %>%
        filter(state %in% c(state.abb, "DC")) %>%
        left_join(popStateBucket03, by=c("state", "bucket03")) %>%
        mutate(vpm=1000000*value/pop) %>%
        group_by(state, bucket03) %>%
        arrange(date) %>%
        mutate(vpm7=zoo::rollmean(vpm, k=7, fill=NA), vpmcum=cumsum(vpm)) %>%
        ungroup()
    
    p1 <- dfUse %>%
        ggplot(aes(x=date, y=vpm7)) + 
        geom_line(aes(group=bucket03, color=bucket03)) + 
        labs(x=NULL, 
             y="Newly hospitalized per million (rolling 7-day mean)", 
             title="Per million newly hospitalized by age bucket"
        ) +
        facet_wrap(~state, scales="free_y") + 
        scale_color_discrete("Age")
    print(p1)
    
    p2 <- dfUse %>%
        filter(!(state %in% all_of(excludeState)), date >= cumStartDate) %>%
        ggplot(aes(x=date, y=vpmcum/1000)) + 
        geom_line(aes(group=bucket03, color=bucket03)) + 
        labs(x=NULL, 
             y=paste0("Cumulative hospitalized per thousand since ", cumStartDate), 
             title="Cumulative newly hospitalized per thousand by age bucket", 
             subtitle=paste0("Since ", 
                             cumStartDate, 
                             if(length(excludeState) > 0) paste0(", excludes ", paste0(excludeState, collapse=", ")) 
                             else ""
             )
        ) +
        facet_wrap(~state, scales="free_y") + 
        scale_color_discrete("Age")
    print(p2)
    
    # Return the dataset
    dfUse
    
}


# Function to read a previously downloaded file for population by state and age
readPopStateAge <- function(loc) {
    
    # FUNCTION ARGUMENTS:
    # loc: file location on the local computer
    
    # Read the data
    df <- fileRead(loc) %>%
        checkUniqueRows(uniqueBy=c("NAME", "SEX", "AGE"))
    
    # Confirm that states, ages, and sexes are as expected
    a1 <- all.equal(sort(c(state.name, "District of Columbia", "United States")), 
                    df %>% pull(NAME) %>% unique() %>% sort()
    )
    print(a1)
    a2 <- all.equal(c(0:85, 999), df %>% pull(AGE) %>% unique() %>% sort())
    print(a2)
    a3 <- all.equal(0:2, df %>% pull(SEX) %>% unique() %>% sort())
    print(a3)
    if(!isTRUE(a1) | !isTRUE(a2) | !isTRUE(a3)) stop("\nUnexpected values for state, age, or sex\n")
    
    # Plot for total US population estimates
    p1 <- df %>%
        filter(AGE==999, NAME=="United States") %>%
        select(state=NAME, SEX, starts_with("POPEST")) %>%
        pivot_longer(-c(state, SEX)) %>%
        mutate(year=as.integer(stringr::str_extract(name, "\\d{4}")), 
               SEX=factor(SEX, levels=c("0", "1", "2"), labels=c("Total", "Male", "Female"))
        ) %>%
        ggplot(aes(x=factor(year))) + 
        geom_line(aes(y=value/1000000, group=SEX, color=SEX)) + 
        geom_text(aes(label=round(value/1000000, 1), 
                      y=value/1000000 + ifelse(SEX=="Male", -5, 5), 
                      color=SEX
        )
        ) +
        lims(y=c(0, NA)) + 
        labs(x=NULL, y="Population (millions)", title="US Population Estimates by Year")
    print(p1)
    
    componentCheck <- function(vrbl, sumName, descMessage) {
        otherVars <- setdiff(c("NAME", "SEX", "AGE"), vrbl)
        dfCheck <- df %>%
            select(-c(SUMLEV, REGION, DIVISION, STATE)) %>%
            pivot_longer(-c(NAME, SEX, AGE)) %>%
            mutate(across(.cols=all_of(vrbl), .fns=~ifelse(.x==sumName, "Total", "Component"), .names="type")) %>%
            group_by_at(c("type", all_of(otherVars), "name")) %>%
            summarize(value=sum(value), .groups="drop") %>%
            pivot_wider(c(all_of(otherVars), "name"), names_from="type", values_from="value") %>%
            mutate(diff=Total-Component) %>%
            arrange(-abs(diff))
        if(max(abs(dfCheck$diff)) > 0) {
            print(dfCheck)
            stop(paste0("\nFAILED CHECK: ", descMessage, "\n"))
        } else {
            cat("\nPASSED CHECK:", descMessage, "\n\n")
        }
    }
    
    componentCheck("NAME", sumName="United States", descMessage="United States total is the sum of states and DC")
    componentCheck("AGE", sumName=999, descMessage="Age 999 total is the sum of the ages")
    componentCheck("SEX", sumName=0, descMessage="Sex 0 total is the sum of the sexes")
    
    # Return the data
    df
    
}


# Function to filter a file of population by state and age for the relevant values
filterPopStateAge <- function(df, keyCol, keyColName=keyCol, yearLabel=NULL) {
    
    # FUNCTION ARGUENTS:
    # df: loaded data frame with columns
    # keyCol: the population column to select
    # keyColName: renaming to be applied for the population column (default will leave as-is)
    # yearLabel: label for year to use in plots (NULL means infer from keyCol)
    
    if (is.null(yearLabel)) yearLabel <- stringr::str_extract(keyCol, pattern="\\d{4}")
    
    # Create the selection and renaming vector
    useCols <- c("stateFull", "sex", "age", all_of(keyColName))
    names(useCols) <- c("NAME", "SEX", "AGE", all_of(keyCol))
    
    # Select the key variable and rename, add state abbreviation, convert sex to more interpretable factor
    dfFilter <- df %>%
        colSelector(names(useCols)) %>%
        colRenamer(useCols) %>%
        mutate(state=c(state.abb, "DC", "US")[match(stateFull, 
                                                    c(state.name, "District of Columbia", "United States")
        )
        ], 
        sex=factor(sex, levels=c(0, 1, 2), labels=c("Total", "Male", "Female"))
        )
    
    # Plot for total population in the key year
    p1 <- dfFilter %>%
        filter(age==999, state != "US") %>%
        ggplot(aes(x=fct_reorder(state, get(keyColName), max))) + 
        geom_text(data=~filter(., sex == "Total"), 
                  aes(label=paste0(round(get(keyColName)/1000000, 1), " (", state, ")"), 
                      y=get(keyColName)/1000000 + 0.1
                  ), 
                  size=3, 
                  hjust=0
        ) +
        geom_col(data=~filter(., sex != "Total"), aes(y=get(keyColName)/1000000, fill=sex), position="stack") + 
        coord_flip() + 
        labs(x=NULL, y="Population (millions)", title=paste0(yearLabel, " Population by State and Sex"))
    print(p1)
    
    # Population by Age in the key year
    p2 <- dfFilter %>%
        filter(sex=="Total", age != 999, state != "US") %>%
        group_by(age) %>%
        summarize(across(.cols=all_of(keyColName), sum)) %>%
        ggplot(aes(x=factor(age))) + 
        geom_text(aes(label=round(get(keyColName)/1000000, 1), y=get(keyColName)/1000000 + 0.1), 
                  size=3, 
                  hjust=0
        ) +
        geom_col(aes(y=get(keyColName)/1000000), fill="lightblue") + 
        labs(x=NULL, y="Population (millions)", title=paste0(yearLabel, " Population by Age")) + 
        coord_flip()
    print(p2)
    
    # Return the data
    dfFilter
    
}


# Function to bucket the population by state and age data
bucketPopStateAge <- function(df, popVar, popYearLabel=NULL) {
    
    # FUNCTION ARGUMENTS:
    # df: a filtered data frame containing the year of interest
    # popVar: name of the population variable
    # popYearLabel: year to use in the plot titles (NULL means infer from popVar)
    
    # Infer popYearLabel if not provided
    if(is.null(popYearLabel)) popYearLabel <- stringr::str_extract(popVar, pattern="\\d{4}")
    
    # Add age buckets to data
    dfBucket <- df %>%
        mutate(bucket10=case_when(age==999 ~ "Total", 
                                  age <= 19 ~ "0-19", 
                                  age >= 80 ~ "80+", 
                                  TRUE ~ paste0(floor(age/10)*10, "-", floor(age/10)*10+9)
        ), 
        bucket03=case_when(age==999 ~ "Total",age <= 19 ~ "0-19", age >= 60 ~ "60+", TRUE ~ "20-59"),
        bucketYMO=case_when(age==999 ~ "Total",age < 18 ~ "0-17", age >= 65 ~ "65+", TRUE ~ "18-64")
        )
    
    # Check that buckets worked as intended
    checkBucket <- function(keyVar) {
        dfBucket %>% 
            count(age, y=get(keyVar)) %>% 
            ggplot(aes(x=factor(age), y=y)) + 
            geom_tile(aes(fill=n)) + 
            coord_flip() + 
            labs(x=NULL, y=NULL, title=paste0("Age map for: ", keyVar))
    }
    p1 <- checkBucket(keyVar="bucket10")
    p2 <- checkBucket(keyVar="bucket03")
    p3 <- checkBucket(keyVar="bucketYMO")
    gridExtra::grid.arrange(p1, p2, p3, nrow=1)
    
    # Proportion by bucketYMO by state
    p4 <- dfBucket %>%
        filter(sex=="Total", age != 999, state != "US") %>%
        group_by(state, bucketYMO) %>%
        summarize(pop=sum(get(popVar)), .groups="drop") %>%
        ggplot(aes(x=fct_reorder2(state, .x=bucketYMO, .y=pop, .fun=function(x, y) -sum(y[x=="0-17"])/sum(y)))) + 
        geom_col(aes(y=pop, fill=fct_rev(bucketYMO)), position="fill") + 
        coord_flip() + 
        labs(x=NULL, 
             y=paste0("Proportion of ", popYearLabel, " population"), 
             title="Age distribution by state"
        ) + 
        scale_fill_discrete("Age Bucket")
    print(p4)
    
    # Proportion by bucket10 by state
    p5 <- dfBucket %>%
        filter(sex=="Total", age != 999, state != "US") %>%
        group_by(state, bucket10) %>%
        summarize(pop=sum(get(popVar)), .groups="drop") %>%
        ggplot(aes(x=fct_reorder2(state, .x=bucket10, .y=pop, .fun=function(x, y) -sum(y[x=="0-19"])/sum(y)))) + 
        geom_col(aes(y=pop, fill=fct_rev(bucket10)), position="fill") + 
        coord_flip() + 
        labs(x=NULL, 
             y=paste0("Proportion of ", popYearLabel, " population"), 
             title="Age distribution by state"
        ) + 
        scale_fill_discrete("Age Bucket")
    print(p5)
    
    # Mean age by state
    p6 <- dfBucket %>%
        filter(sex=="Total", age != 999, state != "US") %>%
        group_by(state) %>%
        mutate(age=ifelse(age==85, 90, age)) %>%
        summarize(ageMean=sum(age*get(popVar))/sum(get(popVar)), .groups="drop") %>%
        ggplot(aes(x=fct_reorder(state, -ageMean))) + 
        geom_text(aes(y=ageMean+0.2, label=round(ageMean, 1)), hjust=0, size=3) +
        geom_point(aes(y=ageMean)) + 
        coord_flip() + 
        labs(x=NULL, 
             y="Average age", 
             title="Mean age by state", 
             subtitle="(caution that all 85+ counted as 90 for mean calculation)"
        ) + 
        lims(y=c(0, NA))
    print(p6)
    
    # Return the bucketed data
    dfBucket
    
}


# Function to plot all states on the same page
onePageCFRPlot <- function(df, keyState, multDeath=100, cfrCap=0.06, ...) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing state-date-name-value
    # keyState: the key state to be analyzed
    # multDeath: multiplier for death in the death/lagged cases chart of plotCFRLag()
    # ...: other arguments to be passed to findCorrAlign()
    
    # Find the correlations data
    corrData <- findCorrAlign(df, 
                              keyState=keyState, 
                              yLab="Value per million\n(rolling 7-day mean)", 
                              printPlots=FALSE, 
                              returnPlots=TRUE, 
                              returnData=TRUE, 
                              ...
    )
    
    # Find CFR
    cfrData <- plotCFRLag(corrData, 
                          cfrCap=cfrCap, 
                          multDeath=multDeath, 
                          mainTitle=paste0(formals(plotCFRLag)$mainTitle, keyState), 
                          printPlots=FALSE, 
                          returnPlots=TRUE
    )
    
    # Create single-page summary
    gridExtra::grid.arrange(corrData$p1, corrData$p2, cfrData$p1, cfrData$p2, nrow=2)
    
}


# Find correlations and alignments of metrics
findCorrAlign <- function(df, 
                          keyState, 
                          varFix="dpm7", 
                          varMove="cpm7", 
                          lagLeads=-10:40,
                          minDate=NULL, 
                          maxDate=NULL,
                          varMapper=c("cpm7"="Cases per million", "dpm7"="Deaths per million"), 
                          yLab="Value per million (rolling 7-day mean)", 
                          printPlots=TRUE, 
                          returnPlots=FALSE,
                          returnData=FALSE
                          ) {
    
    # FUNCTION ARGUMENTS
    # df: pivoted data frame with state-date-name-value
    # keyState: state to include
    # varFix: metric to be held constant
    # varMove: metric to be lagged/led
    # lagLeads: lags and leads for the variable that moves
    # minDate: minimum date for lag/lead (NULL means data-driven)
    # maxDate: maximum date for lag/lead (NULL means data-driven)
    # varMapper: mapping file for varFix and varMove to descriptive labels
    # yLab: label for the y-axis in the first plot
    # printPlots: boolean, should the plots be printed?
    # returnPlots: boolean, should the plots be returned?
    # returnData: boolean, should the data frames be returned as a list?
    
    # Set minDate and maxDate to the actual minmax if passed as NULL 
    if (is.null(minDate)) minDate <- df %>% summarize(date=min(date)) %>% pull(date)
    if (is.null(maxDate)) maxDate <- df %>% summarize(date=max(date)) %>% pull(date)
    
    # Filter to relevant data
    df <- df %>%
        filter(state %in% all_of(keyState), name %in% all_of(c(varFix, varMove)), !is.na(value))
    
    # Plot core metrics for requested states
    p1 <- df %>%
        ggplot(aes(x=date, y=value)) + 
        geom_line(aes(group=name, color=name)) + 
        facet_wrap(~varMapper[name], scales="free_y") + 
        labs(x=NULL, y=yLab, title=paste0("Metrics by state for: ", paste0(keyState, collapse=", "))) + 
        theme(legend.position="none")
    if(isTRUE(printPlots)) print(p1)
    
    # Create dataset for correlations
    dfCorr <- df %>%
        select(date, name, value) %>%
        pivot_wider(date)
    
    # Find correlation by lag/lead for dataset
    dfRho <- tibble::tibble(lagLead=lagLeads, 
                            rho=sapply(lagLeads, 
                                       FUN=function(x) {
                                           lagCorrCheck(dfCorr %>% filter(date >= minDate, date <= maxDate), 
                                                        lagLead=x
                                           )
                                       }
                            )
    )
    
    # Find best correlation and lag/lead
    bestRho <- dfRho %>% 
        filter(rho==max(rho))
    
    # Plot correlations by lag/lead
    p2 <- dfRho %>% 
        ggplot(aes(x=lagLead, y=rho)) + 
        geom_point() + 
        geom_hline(data=bestRho, aes(yintercept=rho), lty=2) +
        geom_vline(data=bestRho, aes(xintercept=lagLead), lty=2) +
        labs(x=paste0("Lag or lead of ", varMapper[varMove]), 
             y=paste0("Correlation to ", varMapper[varFix]), 
             title=paste0("Correlations by lag/lead for state: ", keyState), 
             subtitle=paste0("Best correlation ", 
                             round(bestRho$rho, 3), 
                             " obtained at lag/lead of: ", 
                             bestRho$lagLead
             )
        )
    if(isTRUE(printPlots)) print(p2)
    
    if (isTRUE(returnData) | isTRUE(returnPlots)) {
        list(dfRho=if(isTRUE(returnData)) dfRho else NULL, 
             bestRho=if(isTRUE(returnData)) bestRho else NULL, 
             dfCorr=if(isTRUE(returnData)) dfCorr else NULL, 
             p1=if(isTRUE(returnPlots)) p1 else NULL, 
             p2=if(isTRUE(returnPlots)) p2 else NULL
        )
    }
    
}


# Plot the implied CFR and best lag/lead for dpm-cpm
plotCFRLag <- function(lst, 
                       lagUse=NULL, 
                       scaleUse=NULL, 
                       cfrCap=0.06, 
                       multDeath=50,
                       mainTitle="Coronavirus data for selected geography: ", 
                       printPlots=TRUE, 
                       returnPlots=FALSE
                       ) {
    
    # FUNCTION ARGUMENTS:
    # lst: data frame with date-cpm7-dpm7 OR list with both dfCorr and bestRho
    # lagUse: the lag to use (if NULL, use the value in bestRho$lagLead)
    # scaleUse: scalar for secondary y-axis (NULL means calculate from data)
    # cfrCap: the cap for all values of CFR
    # multDeath: multiplier for death data in plot 2
    # mainTitle: main title for plots
    # printPlots: boolean, should the plots be printed?
    # returnPlots: boolean, should the plots be returned?
    
    # Create dfCorr and lagUse
    if ("list" %in% class(lst)) {
        dfCorr <- lst[["dfCorr"]]
        if (is.null(lagUse)) lagUse <- lst[["bestRho"]]$lagLead
    } else {
        dfCorr <- lst
    }
    
    # Check that dfCorr is a data frame with date-cpm7-dpm7 and lagUse is not NULL
    if (!("data.frame" %in% class(dfCorr))) stop("\nMust have a data frame for lst/dfCorr\n")
    if (!(all(c("date", "cpm7", "dpm7") %in% names(dfCorr)))) stop("\ndfCorr must have date-cpm7-dpm7\n")
    if (is.null(lagUse)) stop("\nMust have a value for lagUse\n")
    
    # Create scaleUse if not passed
    if (is.null(scaleUse)) scaleUse <- 500*ceiling(max(dfCorr$cpm7)/cfrCap/500)
    
    # Create plot of CFR by date, showing lagged cases
    basePlot <- dfCorr %>%
        mutate(lagData=if(lagUse >= 0) lag(cpm7, lagUse) else lead(cpm7, -lagUse)) %>%
        filter(!is.na(lagData), lagData > 0) %>%
        ggplot(aes(x=date)) + 
        geom_line(aes(y=cpm7), color="navy") + 
        geom_line(aes(y=lagData), color="navy", lty=2)
    p1 <- basePlot + 
        geom_line(aes(y=scaleUse*pmin(cfrCap, dpm7/lagData)), color="red") + 
        scale_y_continuous(paste0("Cases per million\n(rolling 7-day mean)"), 
                           sec.axis = sec_axis(~ . / scaleUse, 
                                               name = paste0("Implied CFR (capped at ", 
                                                             round(100*cfrCap, 1), 
                                                             "%)"
                                               )
                           )
        ) +
        labs(x=NULL, 
             title=mainTitle, 
             subtitle=paste0("Red line (right axis) is implied fatality rate\n", 
                             "Blue line is cases with and without ", 
                             abs(lagUse), 
                             "-day ", 
                             if(lagUse > 0) "lag" else "lead"
             )
        )
    if (isTRUE(printPlots)) print(p1)
    
    # Apply a CFR to the data and show alignment
    p2 <- basePlot +
        geom_line(aes(y=multDeath*dpm7), color="red") + 
        labs(x=NULL, 
             y="Per million (7-day rolling mean)", 
             title=mainTitle, 
             subtitle=paste0("Red line is ", 
                             multDeath, 
                             "*deaths\n", 
                             "Blue line is cases with and without ", 
                             abs(lagUse), 
                             "-day ", 
                             if(lagUse > 0) "lag" else "lead"
             )
        )
    if (isTRUE(printPlots))print(p2)
    
    if (isTRUE(returnPlots)) list(p1=p1, p2=p2)
    
}


# Annotate peaks and valleys in CDC daily data
makePeakValley <- function(df, 
                           numVar, 
                           windowWidth,
                           rollMean=NULL, 
                           uqBy=c("date"), 
                           facetVar=c(), 
                           fnNumVar=function(x) x, 
                           fnPeak=function(x) x+100, 
                           fnValley=function(x) x-100, 
                           fnGroupFacet=FALSE,
                           useTitle="", 
                           yLab=""
                           ) {
    
    # FUNCTION ARGUMENTS
    # df: a data frame or tibble
    # numVar: the numeric variable of interest
    # windowWidth: width of the window for calculating peaks and valleys
    # rollMean: the number of days for rolling mean (NULL means no rolling mean)
    # uqBy: variable that the resutling data should be unique by
    # facetVar: variable for faceting (c() means no facets)
    # fnNumVar: what function should be applied to numVar (e.g., function(x) x/1000)
    # fnPeak: function for plotting the peak labels
    # fnValley: function for plotting the valley labels
    # fnGroupFacet: boolean, should the functions be run separatelt for each facet as a grouping variable?
    #               useful for labeling if the goal is to use 0.1*max(yVar) rather than a global peak and valley
    # useTitle: title for plots
    # yLab: y-axis label for plots
    
    # Create named vectors for useTitle and yLab if not passed
    if(is.null(names(useTitle))) 
        useTitle <- rep(useTitle, times=length(numVar)) %>% purrr::set_names(all_of(numVar))
    if(is.null(names(yLab))) 
        yLab <- rep(yLab, times=length(numVar)) %>% purrr::set_names(all_of(numVar))
    
    # Create named lists for fnNumVar, fnPeak, and fnValley
    tempMakeList <- function(f, n, nms) {
        tempList <- vector("list", length=n)
        for(a in 1:n) tempList[[a]] <- f
        names(tempList) <- nms
        tempList
    }
    if(is.null(names(fnNumVar))) fnNumVar <- tempMakeList(fnNumVar, n=length(numVar), nms=numVar)
    if(is.null(names(fnPeak))) fnPeak <- tempMakeList(fnPeak, n=length(numVar), nms=numVar)
    if(is.null(names(fnValley))) fnValley <- tempMakeList(fnValley, n=length(numVar), nms=numVar)
    
    # Create the relevant data frame
    newDF <- df %>% 
        group_by_at(all_of(c(uqBy, facetVar))) %>%
        summarize(across(all_of(numVar), .fns=sum, na.rm=TRUE), .groups="drop") %>%
        group_by_at(all_of(facetVar)) %>%
        mutate(if(!is.null(rollMean)) across(all_of(numVar), .fns=zoo::rollmean, k=rollMean, fill=NA),
               across(all_of(numVar), .fns=findPeaks, width=windowWidth, gt=1, .names="{.col}_isPeak"),
               across(all_of(numVar), 
                      .fns=findPeaks, 
                      width=windowWidth, 
                      FUN=min, 
                      gt=1, 
                      lt=NULL, 
                      fillVal=NA 
                      ,.names="{.col}_isValley"
               )
        ) %>% 
        ungroup()
    
    # Group by the facet variable(s) if not NULL and separate function by facet requested
    if(!is.null(facetVar) & isTRUE(fnGroupFacet)) newDF <- newDF %>% group_by_at(all_of(facetVar))
    
    # Create the relevant plots
    for(keyVar in numVar) {
        
        p1 <- newDF %>%
            mutate(posPeak=fnPeak[[keyVar]](fnNumVar[[keyVar]](get(keyVar))), 
                   posValley=fnValley[[keyVar]](fnNumVar[[keyVar]](get(keyVar)))
            ) %>%
            ggplot(aes(x=get(uqBy), y=fnNumVar[[keyVar]](get(keyVar)))) + 
            geom_line() + 
            geom_point(data=~filter(., get(paste0(keyVar, "_isPeak"))), color="red", size=3) +
            geom_point(data=~filter(., get(paste0(keyVar, "_isValley"))), color="green", size=3) + 
            geom_text(data=~filter(., get(paste0(keyVar, "_isPeak"))), 
                      aes(y=posPeak, 
                          label=paste0(get(uqBy), "\n", round(fnNumVar[[keyVar]](get(keyVar))))
                      ), 
                      color="red", 
                      size=3
            ) + 
            geom_text(data=~filter(., get(paste0(keyVar, "_isValley"))), 
                      aes(y=posValley, 
                          label=paste0(get(uqBy), "\n", round(fnNumVar[[keyVar]](get(keyVar))))
                      ), 
                      color="black", 
                      size=3
            ) + 
            labs(x=NULL, 
                 y=yLab[[keyVar]], 
                 title=useTitle[[keyVar]], 
                 subtitle="Red (peaks) and green (valleys)"
            )
        if(length(facetVar) > 0) p1 <- p1 + facet_wrap(~get(facetVar), scales="free_y")
        
        print(p1)
        
    }
    
    # Return the data, removing any grouping
    newDF %>% ungroup()
    
}


# Create plots of hospital utilization
plotHospitalUtilization <- function(df, 
                                    keyHosp=NULL, 
                                    plotTitle=NULL,
                                    seed=2112261542, 
                                    varMap=hhsMapper, 
                                    createFacets=TRUE,
                                    p2List=list("Adult Beds"=list("colsPlot"=c("adult_beds_occupied"="lightblue", 
                                                                               "adult_beds_covid"="red"
                                                                               ), 
                                                                  "linesPlot"=c("adult_beds"="black", 
                                                                                "total_beds"="green"
                                                                                )
                                                                  ), 
                                                "ICU Beds"=list("colsPlot"=c("icu_beds_occupied"="lightblue", 
                                                                             "adult_icu_covid"="red"
                                                                             ), 
                                                                "linesPlot"=c("icu_beds"="black")
                                                                )
                                                ),
                                    returnData=FALSE
                                    ) {
    
    # FUNCTION ARGUMENTS:
    # df: file containing hospital utilization data
    # keyHosp: character vector of hospital_pk to use (NULL means select one at random using seed)
    # plotTitle: title to use for plots (NULL means use a default based on keyHosp)
    # seed: random seed to use for selecting a hospital
    # varMap: character mapping file of format c("variable name"="plotting facet name")
    # createFacets: boolean, should the facetted plots be create?
    # returnData: boolean, should plot data be returned?
    
    # Sample a keyHosp if not provided
    if(is.null(keyHosp)) {
        set.seed(seed)
        keyHosp <- df %>% 
            pull(hospital_pk) %>% 
            sample(1)        
    }
    
    # Get plotTitle if not provided
    if(is.null(plotTitle)) {
        if(length(keyHosp) > 1) plotTitle <- "Multiple hospitals combined"
        else {
            plotTitle <- df %>% 
                filter(hospital_pk %in% all_of(keyHosp), collection_week==max(collection_week)) %>%
                mutate(useName=paste0(hospital_name, " (code: ", keyHosp, ") ",city, ", ", state, " ", zip)) %>%
                pull(useName)
        }
    }
    
    # Create key plot data
    p1Data <- df %>%
        filter(hospital_pk %in% all_of(keyHosp)) %>%
        select(date=collection_week, names(varMap)) %>%
        colRenamer(vecRename=varMap) %>%
        pivot_longer(-c(date)) %>%
        filter(!is.na(value), value != -999999) %>%
        group_by(date, name) %>%
        summarize(value=sum(value, na.rm=TRUE), n=n(), .groups="drop")
    
    # Create the facetted plots if requested
    if(isTRUE(createFacets)) {
        # Create the key plot
        p1 <- p1Data %>%
            ggplot(aes(x=date, y=value)) + 
            geom_line() + 
            facet_wrap(~name, scales="free_y") + 
            lims(y=c(0, NA)) + 
            labs(x=NULL, y="Average weekly value", title=plotTitle)
        print(p1)
    }
    
    # Create the stacked bar plots
    for(plotType in names(p2List)) {
        
        # Create the base plot
        p2 <- ggplot(data=p1Data, aes(x=date, y=value))
        
        # Add the columns
        for(vCol in names(p2List[[plotType]][["colsPlot"]])) {
            p2 <- p2 + geom_col(data=mutate(filter(p1Data, name %in% vCol), fill=vCol), aes(fill=fill))
        }
        
        # Add the lines
        p2 <- p2 + geom_line(data=filter(p1Data, name %in% names(p2List[[plotType]][["linesPlot"]])), 
                             aes(group=name, color=name), 
                             size=1.5
        )
        
        # Add the limits, labels, and scales
        p2 <- p2 + 
            lims(y=c(0, NA)) + 
            labs(x=NULL, y="Average weekly value", title=plotTitle, subtitle=plotType) +
            scale_color_manual("Capacity", values=p2List[[plotType]][["linesPlot"]]) + 
            scale_fill_manual("Occupied", values=p2List[[plotType]][["colsPlot"]])
        
        # Print the plot
        print(p2)
        
    }
    
    # Return the data if requested
    if(isTRUE(returnData)) return(p1Data)
    
}


# Create state-level data map
createGeoMap <- function(df, 
                         yVars,
                         xVar="collection_week",
                         facetVar="state",
                         lstFilter=list(), 
                         lstExclude=list(), 
                         vecSelect=NULL, 
                         vecRename=c(), 
                         selfList=list(), 
                         fullList=list(), 
                         plotTitle=NULL, 
                         plotSubtitle=NULL,
                         plotYLab=NULL, 
                         plotScaleLabel=NULL, 
                         createPlot=TRUE,
                         facetScaleType="fixed",
                         hLine=1,
                         noX=TRUE, 
                         noY=FALSE,
                         returnData=FALSE
) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the relevant data
    # yVars: list of the y-variables, of form list("yVar1"=c("label"="y1Label", "color"="y1Color"), "yVar2"=...)
    # xVar: the x-variable to use for the plots
    # facetVar: the variable for faceting the data
    # lstFilter: a list for filtering records, of form list("field"=c("allowed values"))
    # lstExclude: a list for filtering records, of form list("field"=c("disallowed values"))
    # vecSelect: vector for variables to keep c('keep1', "keep2", ...), NULL means keep all
    # vecRename: vector for renaming c('existing name'='new name'), can be any length from 0 to ncol(df)
    # selfList: list for functions to apply to self, list('variable'=fn) will apply variable=fn(variable)
    #           processed in order, so more than one function can be applied to self
    # fullList: list for general functions to be applied, list('new variable'=expression(code))
    #           will create 'new variable' as eval(expression(code))
    #           for now, requires passing an expression
    # plotTitle: title for plot
    # plotSubtitle: subtitle for plot
    # plotYLab: y-label for plot
    # plotScaleLabel: scale label for plot
    # createPlot: boolean, should the plot be created and printed?
    # facetScaleType: argument passed to facet_wrap - "fixed", "free", "free_y", "free_x"
    # hLine: height for a dashed horizontal line (NULL means none)
    # noX: boolean, should the x-axis be removed?
    # noY: boolean, should the y-axis be removed?
    # returnData: boolean, should the data frame dfMod be returned?
    
    # Create the modified data
    dfMod <- df %>%
        rowFilter(lstFilter=lstFilter, lstExclude=lstExclude) %>%
        colSelector(vecSelect=vecSelect) %>%
        colRenamer(vecRename=vecRename) %>%
        colMutater(selfList=selfList, fullList=fullList)
    
    if(isTRUE(createPlot)) {
        
        # Create the plot data frame
        dfPlot <- dfMod %>%
            colSelector(c(facetVar, xVar, names(yVars))) %>%
            pivot_longer(names(yVars))
        
        # Create the color mapper
        vecColor <- sapply(yVars, FUN=function(x) x[["color"]]) %>%
            purrr::set_names(names(yVars))
        
        # Create the plot
        p1 <- dfPlot %>%
            colRenamer(c("facetVar") %>% purrr::set_names(facetVar)) %>%
            ggplot(aes_string(x=xVar, y="value", group="name", color="name")) + 
            geom_line() +
            scale_color_manual(plotScaleLabel, 
                               values=vecColor, 
                               labels=sapply(yVars, FUN=function(x) x[["label"]])
            ) + 
            labs(x=NULL, y=plotYLab, title=plotTitle, subtitle=plotSubtitle) + 
            lims(y=c(0, NA)) +
            facet_geo(~facetVar, scales=facetScaleType)
        if(!is.null(hLine)) p1 <- p1 + geom_hline(yintercept=hLine, lty=2)
        if(isTRUE(noX)) p1 <- p1 + theme(axis.text.x = element_blank())
        if(isTRUE(noY)) p1 <- p1 + theme(axis.text.y = element_blank())
        
        # Print the plot
        print(p1)
        
    }
    
    # Return the data if requested(?)
    if(isTRUE(returnData)) return(dfMod)
    
}


# Select and filter HHS data as needed
skinnyHHS <- function(df, 
                      keyStates=c(state.abb, "DC"), 
                      idCols=c("state", "collection_week", "hospital_pk"),
                      varMapper=hhsMapper
                      ) {
    
    # FUNCTION ARGUMENTS:
    # df: the initial data frame
    # keyState: states to include for filtering
    # varMapper: variables to include and output names (named vector of form c("original name"="modified name"))
    
    df %>%
        filter(state %in% all_of(keyStates)) %>%
        colSelector(c(all_of(idCols), names(varMapper))) %>%
        colRenamer(varMapper)
    
}


# Impute values for hospital capacity
imputeNACapacity <- function(df, 
                             extraNA=c(-999999),
                             convertAllNA=TRUE,
                             idVars=c("hospital_pk"), 
                             sortVars=c("collection_week"),
                             varsToImpute=c("total_beds", "adult_beds"), 
                             varUsedToImpute=c("inpatient_beds")
                             ) {
    
    # FUNCTION ARGUMENTS:
    # df: the initial data frame
    # extraNA: values that should be treated as NA
    # convertAllNA: boolean, should all extraNA values be converted in all numeric columns?
    #               if FALSE, extraNA values will not be converted, though imputing will treat as NA
    # varsToImpute: variables to be imputed
    # varUsedToImpute: percent changes in this variable assumed to drive percent changes in varsToImpute if NA
    
    # Convert NA if requested
    if(isTRUE(convertAllNA)) {
        df <- df %>%
            mutate(across(where(is.numeric), 
                          .fns=function(x) ifelse(is.na(x), NA, ifelse(x %in% all_of(extraNA), NA, x))
            )
            )
    }
    
    # Impute values and return data
    df %>%
        arrange(across(all_of(c(idVars, sortVars)))) %>%
        group_by(across(all_of(idVars))) %>%
        mutate(across(all_of(varsToImpute), 
                      .fns=function(x) testImputeNA(x=x, y=get(varUsedToImpute), naValues=extraNA)
        )
        ) %>%
        ungroup()
    
}


# sum the imputed HHS data to the state-week level
sumImputedHHS <- function(df, 
                          groupVars=c("state", "collection_week")
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: the initial data frame
    # groupVars: variables for summing the data to
    
    df %>%
        group_by(across(all_of(groupVars))) %>%
        summarize(across(where(is.numeric), .fns=sum, na.rm=TRUE), n=n(),.groups="drop")
    
}

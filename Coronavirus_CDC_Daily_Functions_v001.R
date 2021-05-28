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

# Function to download/load, process, segment, and analyze data for CDC daily
readRunCDCDaily <- function(thruLabel, 
                            downloadTo=list("cdcDaily"=NA, "cdcHosp"=NA), 
                            readFrom=downloadTo, 
                            compareFile=list("cdcDaily"=NA, "cdcHosp"=NA),
                            writeLog=NULL,
                            ovrwriteLog=TRUE,
                            dfPerCapita=NULL,
                            useClusters=NULL,
                            hierarchical=TRUE,
                            returnList=!isTRUE(hierarchical), 
                            kCut=6,
                            reAssignState=vector("list", 0),
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
                                       mapper=perCapMapper
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
                                     brewPalette=brewPalette
                                     )
    
    # STEP 7: Return a list of the key data
    return(createFinalList(plots=TRUE))
    
}



# Function to extract and format key state data
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



# Function to read and check a raw data file
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



# Function to create diagnoses and plots for clustering data
diagnoseClusters <- function(lst, 
                             lstExtract=fullListExtract,
                             clusterFrame=clustersToFrame(lst),
                             brewPalette=NA, 
                             printSummary=TRUE, 
                             printDetailed=TRUE
                             ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a list containing processed clustering data
    # lstExtract: the elements to extract from lst with an optional function for converting the elements
    #             NULL means use the extracted element as-is
    # clusterFrame: the clusters to be plotted (default is to match to useClusters)
    # brewPalette: the color palette to use with scale_*_brewer()
    #              default NA means use the standard color/fill profile
    # printSummary: boolean, should summary plots be printed to the log?
    # printDetailed: boolean, should detailed plots be printed to the log?
    
    # Create the integrated and aggregate data from lst
    dfFull <- integrateData(lst, lstExtract=lstExtract, otherDF=list(clusterFrame))
    dfAgg <- combineAggData(dfFull)
    
    # Create the main summary plots
    summaryPlots <- createSummary(dfAgg, stateClusterDF=clusterFrame, brewPalette=brewPalette)
    
    # Create the detailed summaries
    detPlots <- createDetailedSummaries(dfDetail=dfFull, dfAgg=dfAgg, brewPalette=brewPalette)
    
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



# Function for creating an aggregate-level frame
combineAggData <- function(df, 
                           aggTo=c("cluster", "date"), 
                           aggBy=list("agg1"=list(aggFunc=specNA(specSumProd), 
                                                  aggVars=c("pop"), 
                                                  wtVar=NULL, 
                                                  prefix=NULL
                                                  ), 
                                      "agg2"=list(aggFunc=specNA(weighted.mean), 
                                                  aggVars=c("tcpm7", "tdpm7", "cpm7", "dpm7", "hpm7"), 
                                                  wtVar="pop", 
                                                  prefix="wm_"
                                                  )
                                      ),
                           fnJoin=dplyr::full_join
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: a data frame containing data for summarizing to an aggregate
    # aggTo: the level to which data should be aggregated
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



# Function for creating plots that can be combined on a dashboard page
createSummary <- function(df, 
                          stateClusterDF=NULL,
                          brewPalette=NA
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: an integrated data frame by cluster-date
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
        helperAggTrend(aggVars=c("wm_tcpm7", "wm_tdpm7", "wm_hpm7"), 
                       mapper=c("wm_tcpm7"="Cases per thousand\n(cumulative)", 
                                "wm_tdpm7"="Deaths per million\n(cumulative)", 
                                "wm_hpm7"="Hospitalized per million\n(current)"
                                ),
                       yLab=NULL,
                       title=NULL, 
                       divideBy=c("wm_tcpm7"=1000), 
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
        geom_col(aes(fill=cluster)) + 
        facet_wrap(~mapper[name], scales="free_x") + 
        coord_flip() + 
        labs(title="Cumulative burden", x=NULL, y=NULL)
    if (!is.na(brewPalette)) p1 <- p1 + scale_fill_brewer("Cluster", palette=brewPalette)
    
    # Create facetted burden trends by aggregate
    p2 <- dfDetail %>%
        colSelector(vecSelect=c(detVar, "date", aggVar, p2DetMetrics)) %>%
        pivot_longer(all_of(p2DetMetrics)) %>%
        filter(!is.na(value)) %>%
        ggplot(aes(x=date, y=value)) + 
        geom_line(aes_string(group=detVar), color="grey", size=0.5) + 
        facet_grid(mapper[name] ~ cluster, scales="free_y") + 
        scale_x_date(date_breaks="2 months", date_labels="%b-%y") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
        labs(x=NULL, y=NULL, title="Burden by cluster and metric")
    aggPlot <- dfAgg %>% 
        colSelector(vecSelect=c("date", aggVar, p2AggMetrics)) %>%
        colRenamer(vecRename=purrr::set_names(p2DetMetrics, p2AggMetrics)) %>%
        pivot_longer(all_of(p2DetMetrics)) %>%
        filter(!is.na(value))
    p2 <- p2 + 
        geom_line(data=aggPlot, aes_string(color=aggVar, group=aggVar), size=1.5)
    if (!is.na(brewPalette)) p2 <- p2 + 
        scale_color_brewer("Cluster", palette=brewPalette) + 
        theme(legend.position="none")
    
    # Create growth trends plot
    p3 <- dfDetail %>%
        colSelector(vecSelect=c(detVar, aggVar, "date", p3Metrics)) %>%
        pivot_longer(all_of(p3Metrics)) %>%
        filter(!is.na(value)) %>%
        group_by_at(c(detVar, "name")) %>%
        filter(date %in% c(max(date), max(date)-lubridate::days(p3Days))) %>%
        mutate(growth=max(value)-min(value)) %>%  # not ideal way to calculate
        filter(date==max(date)) %>%
        ungroup() %>%
        ggplot(aes(x=value, y=growth)) + 
        geom_text(aes_string(color=aggVar, label=detVar), fontface="bold") + 
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
    if (!is.na(brewPalette)) p3 <- p3 + scale_color_brewer(stringr::str_to_title(aggVar), palette=brewPalette)
    
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

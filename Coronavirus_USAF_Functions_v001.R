# File to be sourced, contains functions for working with CDC daily data

# FUNCTIONS INCLUDED:
# 1. getCountyClusters() - obtains the county clusters, adding the default cluster if requested
# 2. clusterCounties() - code for creating county clusters (calls clusterStates)
# 3. readRunUSAFacts() - main function for working with USA Facts data
# 4. diagnoseClusters() - function for assessing clusters and plotting key metrics
# 5. readQCRawUSAF() - function to read and QC relevant files from USA Facts
# 6. getCountyData() - function to get the county population data file, with proper column names

# Function to obtain county clusters and return the county clusters vector
getCountyClusters <- function(obj, 
                              hierarchical=FALSE, 
                              kCut=0, 
                              reAssign=list(), 
                              defaultCluster=NULL
                              ) {
    
    # FUNCTION ARGUMENTS
    # obj: a clustering object returned by clusterCounties()
    # hierarchical: whether the clustering object is based on hierarchical clusters
    #               TRUE means from hierarchical clustering
    #               FALSE means from kmeans clustering
    #               NA means from rules-based clustering
    # kCut; if hierarchical clustering is used, what k (number of clusters in cutree) should be used?
    # reAssign: mapping file to change segments, as list('entity'='other entity cluster to use')
    # defaultCluster: cluster label to be assigned to any county that is not in obj$objCluster
    #                 NULL means do not add these to the clustering vector
    
    # Get the clusters from obj$objCluster
    clust <- getClusters(obj$objCluster, hier=hierarchical, kCut=kCut, reAssign=reAssign)
    
    # Add the defaultCluster label to any county that does not have a cluster label
    if (!is.null(defaultCluster)) {
        ctyAdd <- obj$countyBelow %>% pull(countyFIPS) %>% unique() %>% sort()
        vecAdd <- rep(defaultCluster, length(ctyAdd)) %>% purrr::set_names(ctyAdd)
        clust <- c(clust, vecAdd)
    }
    
    # Return the cluster vector
    clust
    
}



# Function to take county-level data, prepare for clusterStates, and return resulting outputs
clusterCounties <- function(dfPerCapita, 
                            hierarchical,
                            vecRename=c(),
                            clusterBy=c("countyFIPS"),
                            arrangeBy=c("date"),
                            burdenMetrics=c("cpm", "dpm"),
                            popVar=c("pop"),
                            vecSelect=c(clusterBy, arrangeBy, burdenMetrics, popVar),
                            uniqueBy=c(clusterBy, arrangeBy),
                            minPopCluster=1,
                            returnList=TRUE, 
                            ...
                            ) {
    
    # FUNCTION ARGUMENTS:
    # dfPerCapita: a county-level file with per-capita metrics
    # hierarchical: whether to create hierarchical clusters
    #               TRUE means run hierarchical clustering
    #               FALSE means run kmeans clustering
    #               NA means run rules-based clustering
    # vecRename: renaming of input variables
    # clusterBy: the variable name used for clustering
    # arrangeBy: data will be sorted by this a mix of clusterBy and this variable
    # burdenMetrics: the metrics to be used for burden in clustering
    # popVar: the column containing population data
    # vecSelect: selection of input variables
    # uniqueBy: the input file must be unique by, and will then be sorted by, uniqueBy
    # minPopCluster: minimum population for including county in running cluster-level metrics
    # returnList: boolean, if FALSE just the cluster object is returned
    #                      if TRUE, a list is returned with dfCluster and the cluster object
    # ...: other arguments that will be passed to clusterStates
    
    # STEP 1: Select and rename variables from the dfPerCapita file
    countyData <- dfPerCapita %>%
        colRenamer(vecRename=vecRename) %>%
        colSelector(vecSelect=vecSelect) %>%
        checkUniqueRows(uniqueBy=uniqueBy, returnDF=TRUE) %>%
        arrange(across(all_of(uniqueBy))) %>%
        mutate(popThresh=(get(popVar)>=minPopCluster))
    
    # STEP 2: Split data based on population threshold
    countyFiltered <- countyData %>% filter(popThresh)
    countyBelow <- countyData %>% filter(!popThresh)
    
    # STEP 2a: Confirm that no county is in both data sets
    count(countyFiltered, a=get(clusterBy), popThresh) %>%
        bind_rows(count(countyBelow, a=get(clusterBy), popThresh)) %>%
        checkUniqueRows(uniqueBy=c("a"), returnDF=FALSE, noteUnique=FALSE)
    
    # STEP 3: Run county-level clusters
    objCluster <- clusterStates(colRenamer(countyFiltered, vecRename=c("countyFIPS"="state")), # should fix
                                hierarchical=hierarchical, 
                                returnList=returnList, 
                                ...
    )
    
    # Return all of the relevant objects
    list(objCluster=objCluster, 
         countyFiltered=countyFiltered, 
         countyBelow=countyBelow
    )
    
}



# Function to run the USA Facts (US county-level coronavirus data) clustering process
readRunUSAFacts <- function(maxDate, 
                            downloadTo=list("usafCase"=NA, "usafDeath"=NA),
                            readFrom=downloadTo,
                            compareFile=list("usafCase"=NA, "usafDeath"=NA),
                            writeLog=NULL,
                            ovrwriteLog=TRUE,
                            dfPerCapita=NULL,
                            useClusters=NULL,
                            showBurdenMinPop=10000, 
                            minPopCluster=25000,
                            defaultCluster=NULL,
                            hierarchical=FALSE,
                            kCut=6,
                            orderCluster=TRUE,
                            reAssignCounty=list(),
                            skipAssessmentPlots=FALSE,
                            brewPalette=NA,
                            ...
                            ) {
    
    # FUNCTION ARGUMENTS:
    # maxDate: the maximum data to use for data from the cases and deaths file
    # downloadTo: named list for locations to download data (usafCase, usafDeath, usafPop)
    #             NA means do not download data for that particular element
    # readFrom: named list for locations to read data from (defaults to donwloadTo)
    # compareFile: named list for the reference file to be used for usafCase, usafDeath, usafPop
    #              NA means do not use a reference file for that element
    # writeLog: name of a separate log file for capturing detailed data on changes between files
    #           NULL means no detailed data captured
    # ovrwriteLog: boolean, should the log file be overwritten and started again from scratch?
    # dfPerCapita: file can be passed directly, which bypasses the loading and processing steps
    #              default NULL means create dfPerCapita using steps 2-4
    # useClusters: named vector containing the clusters to use
    #              NULL means create clusters from this data
    # showBurdenMinPop: minimum population for showing in burden by cluster plots (NULL means skip plot)
    # minPopCluster: minimum population for including county in running cluster-level metrics
    # defaultCluster: cluster label to be assigned to any county that falls below minPopCluster
    #                 NULL means do not add these to the clustering vector
    # hierarchical: whether to create hierarchical clusters
    #               TRUE means run hierarchical clustering
    #               FALSE means run kmeans clustering
    #               NA means run rules-based clustering
    # kCut; if hierarchical clustering is used, what k (number of clusters in cutree) should be used?
    # orderCluster: if FALSE, ignore; if TRUE, order by "dpm"; if anything else, order by orderCluster
    # reAssignCounty: mapping file for assigning a county to another county's cluster
    #                format list("countyToChange"="countyClusterToAssign")
    # skipAssessmentPlots: boolean, should cluster assessment plots be skipped?
    # brewPalette: character vector length-1 referencing a color scheme from brewer_pal to use
    #              NA means use R default color schemes
    # ...: other arguments that will be passed to prepClusterCounties
    
    # STEP 1: Get a county-level population file, with fips as 5-digit character and non-zero population
    countyData <- getCountyData(selfList=list("countyFIPS"=zeroPad5), lstExclude=list("pop"=c(0)))
    
    # If a log file is requested, create the log file (allows for append=TRUE for all downstream functions)
    if (!is.null(writeLog)) genNewLog(writeLog=writeLog, ovrwriteLog=ovrwriteLog)
    
    # Get the data types to be used (elements of readFrom) and create a file storage list
    elemUsed <- names(readFrom)
    dfRawList <- vector("list", length=length(elemUsed)) %>% purrr::set_names(elemUsed)
    dfProcessList <- vector("list", length=length(elemUsed)) %>% purrr::set_names(elemUsed)
    
    # Steps 2-4 are required only if dfPerCapita has not been passed
    if (is.null(dfPerCapita)) {
        
        # STEP 2: Download and QC each requested data element
        for (elem in elemUsed) {
            dfRawList[[elem]] <- readQCRawUSAF(fileName=readFrom[[elem]],
                                               writeLog=writeLog,
                                               ovrwriteLog=FALSE,
                                               urlType=elem,
                                               getData=if(is.na(downloadTo[[elem]])) FALSE else TRUE,
                                               dfRef=compareFile[[elem]]
            )
            glimpseLog(dfRawList[[elem]], txt=paste0("\nRaw file for ", elem, ":\n"), logFile=writeLog)
        }
        
        # STEP 3: Process all requested data
        for (elem in elemUsed) {
            dfProcessList[[elem]] <- processRawFile(dfRawList[[elem]],
                                                    vecRename=c(), 
                                                    vecSelect=vecSelectMapper[[elem]],
                                                    lstCombo=lstComboMapper[[elem]],
                                                    lstFilter=lstFilterMapper[[elem]], 
                                                    lstExclude=lstExcludeMapper[[elem]]
            )
            glimpseLog(dfProcessList[[elem]], txt=paste0("\nProcessed for ", elem, ":\n"), logFile=writeLog)
        }
        
        # STEP 4: Integrate to create a per-capita data file
        dfPerCapita <- createPerCapita(dfProcessList,
                                       uqBy=c("countyFIPS", "state", "date"),
                                       popData=countyData,
                                       popJoinBy=c("countyFIPS", "state"),
                                       mapper=perCapMapper
        )
        glimpseLog(dfPerCapita, txt="\nIntegrated per capita data file:\n", logFile=writeLog)
        
    } else {
        dfRawList <- NULL
        dfProcessList <- NULL
    }
    
    # STEP 5: Create clusters (if passed as NULL)
    if (is.null(useClusters)) {
        # Add population back to dfPerCapita (should improve this process)
        dfPerUse <- countyData %>%
            select(countyFIPS, pop) %>%
            mutate(countyFIPS=zeroPad5(countyFIPS)) %>%
            right_join(dfPerCapita, by=c("countyFIPS"))
        clData <- clusterCounties(dfPerCapita=dfPerUse, 
                                  hierarchical=hierarchical, 
                                  minPopCluster=minPopCluster,
                                  ...
        )
        useClusters <- getCountyClusters(clData, 
                                         hier=hierarchical, 
                                         kCut=kCut, 
                                         reAssign=reAssignCounty, 
                                         defaultCluster=defaultCluster
        )
    }
    
    # STEP 6: Assess clusters
    if (skipAssessmentPlots) {
        plotDataList <- NULL
    } else {
        lstFuns <- list("stateData"=function(x) colSelector(x, vecSelect=c("countyFIPS", "pop")), 
                        "dfPerCapita"=NULL
        )
        clFrame <- useClusters %>%
            clustersToFrame(colNameName="countyFIPS") %>%
            colMutater(selfList=list("countyFIPS"=zeroPad5))
        plotDataList <- diagnoseClusters(lst=list("stateData"=countyData, 
                                                  "dfPerCapita"=dfPerCapita
        ), 
        lstExtract=lstFuns,
        clusterFrame=clFrame,
        brewPalette=brewPalette, 
        clusterType="county"
        )
    }
    
    # Return statement, still need to update Step 6 (cluster assessment)
    return(list(countyData=countyData, 
                dfRaw=dfRawList, 
                dfProcess=dfProcessList, 
                dfPerCapita=dfPerCapita, 
                useClusters=useClusters, 
                maxDate=maxDate,
                plotDataList=plotDataList
                )
    )
    
}



# Function to create diagnoses and plots for clustering data
diagnoseClusters <- function(lst, 
                             lstExtract=fullListExtract,
                             clusterFrame=NULL,
                             brewPalette=NA, 
                             clusterType="state",
                             printSummary=TRUE, 
                             printDetailed=TRUE
                             ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a list containing processed clustering data
    # lstExtract: the elements to extract from lst with an optional function for converting the elements
    #             NULL means use the extracted element as-is
    # clusterFrame: tibble of the clusters to be plotted
    #               NULL means create from lst
    # brewPalette: the color palette to use with scale_*_brewer()
    #              default NA means use the standard color/fill profile
    # clusterType: character variable of form "state" for state clusters and "county" for county
    # printSummary: boolean, should summary plots be printed to the log?
    # printDetailed: boolean, should detailed plots be printed to the log?
    
    # Get the key variable (used for joins and the like)
    if (clusterType=="state") keyVar <- "state"
    else if (clusterType=="county") keyVar <- "countyFIPS"
    else stop(paste0("\nThe passed clusterType: ", clusterType, " is not programmed\n"))
    
    # Create clusterFrame from lst if it has been passed as NULL
    if (is.null(clusterFrame)) clusterFrame <- clustersToFrame(lst, colNameName=keyVar)
    
    # Create the integrated and aggregate data from lst
    dfFull <- integrateData(lst, lstExtract=lstExtract, otherDF=list(clusterFrame), keyJoin=keyVar)
    dfAgg <- combineAggData(dfFull, aggBy=plotCombineAggByMapper[[clusterType]])
    
    # Create the main summary plots
    summaryPlots <- createSummary(dfAgg, 
                                  stateClusterDF=clusterFrame, 
                                  brewPalette=brewPalette, 
                                  dataType=clusterType
    )
    
    # Create the detailed summaries
    detPlots <- createDetailedSummaries(dfDetail=dfFull, 
                                        dfAgg=dfAgg, 
                                        detVar=keyVar,
                                        p2DetMetrics=plotCombineAggByMapper[[clusterType]]$agg2$aggVars,
                                        brewPalette=brewPalette
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



# Function to read and QC raw USA Facts data
readQCRawUSAF <- function(fileName, 
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
                          pivotBy=NULL,
                          rawMakeVar=NULL,
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
                                         "pivotBy"=pivotMapper,
                                         "rawMakeVar"=rawMakeVarMapper,
                                         "step3Group"=checkControlGroupMapper,
                                         "step3Vals"=checkControlVarsMapper,
                                         "step4KeyVars"=checkSimilarityMapper, 
                                         "step5PlotItems"=plotSimilarityMapper,
                                         "step6AggregateList"=keyAggMapper
                                         )
                          ) {
    
    # FUNCtiON ARGUMENTS:
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
    # pivotBy: combination of variables that should NOT be pivoted
    # uniqueBy: combination of variables for checking uniqueness of pivoted file
    #           NULL means infer from data, keep as NULL (meaning use-all) if cannot be inferred
    # rawMakeVar: variable name to be used for the numeric data pivoted down from columns
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
        checkUniqueRows(uniqueBy=pivotBy) %>%
        pivotData(pivotKeys=pivotBy, nameVar="date", valVar=rawMakeVar) %>%
        colMutater(selfList=list("date"=lubridate::mdy)) %>%
        checkUniqueRows(uniqueBy=uniqueBy) %>%
        arrange(across(c(setdiff(uniqueBy, "date"), "date"))) %>%
        group_by(across(setdiff(uniqueBy, "date"))) %>%
        mutate(newBurden=ifelse(row_number()==1, get(rawMakeVar), get(rawMakeVar)-lag(get(rawMakeVar)))) %>%
        ungroup() %>%
        colRenamer(vecRename=c("newBurden"=paste0("new_", rawMakeVar)))
    
    # Step 3: Plot basic control totals for new cases and new deaths by month
    dfRaw %>%
        checkControl(groupBy=step3Group, useVars=step3Vals, printControls=FALSE, na.rm=TRUE) %>%
        helperLinePlot(x=step3Group, y="newValue", facetVar="name", facetScales="free_y", groupColor="name")
    
    # If there is no file for comparison, return the data
    if (is.null(dfRef) | (if(length(dfRef)==1) is.na(dfRef) else FALSE)) return(dfRaw)
    
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
    
    dfRaw
    
}


# Function to get county-level population data
getCountyData <- function(df=readFromRDS("countyPop2021"), 
                          renameVars=c("State"="state", "County Name"="countyName", "population"="pop"), 
                          keepVars=c("countyFIPS", "countyName", "state", "pop"), 
                          selfList=list("countyFIPS"=zeroPad5),
                          fullList=list(), 
                          lstFilter=list(), 
                          lstExclude=list()
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing state data
    # renameVars: variables to be renamed, using named list with format "originalName"="newName"
    # keepVars: variables to be kept in the final file (NULL means keep all)
    # selfList: list for functions to apply to self, list('variable'=fn) will apply variable=fn(variable)
    #           processed in order, so more than one function can be applied to self
    # fullList: list for general functions to be applied, list('new variable'=expression(code))
    #           will create 'new variable' as eval(expression(code))
    #           for now, requires passing an expression
    # lstFilter: a list for filtering records, of form list("field"=c("allowed values"))
    # lstExclude: a list for filtering records, of form list("field"=c("disallowed values"))
    
    # Read the file, rename and keep variables, apply functions as appropriate
    df %>%
        colRenamer(vecRename=renameVars) %>%
        colSelector(vecSelect=keepVars) %>%
        colMutater(selfList=selfList, fullList=fullList) %>%
        rowFilter(lstFilter=lstFilter, lstExclude=lstExclude)
    
}

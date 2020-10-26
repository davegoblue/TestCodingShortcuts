# Function to download/load, process, segment, and analyze data from COVID Tracking Project
readRunCOVIDTrackingProject <- function(thruLabel, 
                                        downloadTo=NULL, 
                                        readFrom=downloadTo, 
                                        compareFile=NULL,
                                        dfPerCapita=NULL,
                                        useClusters=NULL,
                                        hierarchical=TRUE,
                                        returnList=!hierarchical, 
                                        kCut=6,
                                        reAssignState=vector("list", 0),
                                        makeCumulativePlots=TRUE,
                                        skipAssessmentPlots=FALSE,
                                        ...
                                        ) {
    
    # FUNCTION ARGUMENTS:
    # thruLabel: the label for when the data are through (e.g., "Aug 30, 2020")
    # donwloadTo: download the most recent COVID Tracking Project data to this location
    #             NULL means do not download any data
    # readFrom: location for reading in the COVID Tracking Project data (defaults to donwloadTo)
    # compareFile: name of the file to use for comparisons when reading in raw data (NULL means no comparison)
    # dfPerCapita: file can be passed directly, which bypasses the loading and processing steps
    # useClusters: file containing clusters by state (NULL means make the clusters from the data)
    # hierarchical: boolean, should hierarchical clusters be produced (if FALSE, will be k-means)?
    # returnList: boolean, should a list be returned or just the cluster object?
    #             refers to what is returned by clusterStates(); the main function always returns a list
    # kCut: number of segments when cutting the hierarchical tree
    # reAssignState: mapping file for assigning a state to another state's cluster
    #                format list("stateToChange"="stateClusterToAssign")
    # makeCumulativePlots: whether to make plots of cumulative metrics
    # skipAssessmentPlots: boolean to skip the plots for assessClusters()
    #                      especially useful if just exploring dendrograms or silhouette widths
    # ...: arguments to be passed to clusterStates(), will be used only if useClusters is NULL
    
    
    # STEP 1: Get state data
    stateData <- getStateData()
    
    
    # STEPS 2-4 are run only if dfPerCapita does not exist
    if (is.null(dfPerCapita)) {
        
        # STEP 2a: Download latest COVID Tracking Project data (if requested)
        if (!is.null(downloadTo)) downloadCOVIDbyState(fileName=downloadTo)
        
        # STEP 2b: Read-in COVID Tracking Project data
        dfRaw <- readCOViDbyState(readFrom, checkFile=compareFile)
        glimpse(dfRaw)
        
        # STEP 3: Process the data so that it includes all requested key variables
        varsFilter <- c("date", "state", "positiveIncrease", "deathIncrease", 
                        "hospitalizedCurrently", "totalTestResultsIncrease"
        )
        dfFiltered <- processCVData(dfRaw, 
                                    varsKeep=varsFilter, 
                                    varsRename=c(positiveIncrease="cases", 
                                                 deathIncrease="deaths", 
                                                 hospitalizedCurrently="hosp", 
                                                 totalTestResultsIncrease="tests"
                                    )
        )
        glimpse(dfFiltered)
        
        # STEP 4: Convert to per capita
        dfPerCapita <- helperMakePerCapita(dfFiltered, 
                                           mapVars=c("cases"="cpm", "deaths"="dpm", 
                                                     "hosp"="hpm", "tests"="tpm"
                                           ), 
                                           popData=stateData
        )
        glimpse(dfPerCapita)
        
    } else {
        dfRaw <- NULL
        dfFiltered <- NULL
    }
    
    
    # STEP 5: Create the clusters (if they have not been passed)
    if (is.null(useClusters)) {
        # Run the clustering process
        clData <- clusterStates(df=dfPerCapita, hierarchical=hierarchical, returnList=returnList, ...)
        # If hierarchical clusters, cut the tree, otherwise use the output object directly
        if (hierarchical) {
            useClusters <- cutree(clData, k=kCut)
        } else {
            useClusters <- clData$objCluster$cluster
        }
        # If requested, manually assign clusters to the cluster for another state
        for (xNum in seq_len(length(reAssignState))) {
            useClusters[names(reAssignState)[xNum]] <- useClusters[reAssignState[[xNum]]]
        }
        
    }
    
    
    # STEP 5a: Stop the process and return what is available if skipAssessmentPlots is TRUE
    if (skipAssessmentPlots) {
        return(list(stateData=stateData, 
                    dfRaw=dfRaw, 
                    dfFiltered=dfFiltered, 
                    dfPerCapita=dfPerCapita, 
                    useClusters=useClusters, 
                    plotData=NULL, 
                    consolidatedPlotData=NULL, 
                    clCum=NULL
        )
        )
    }
    
    
    # STEP 6: Create the cluster assessments
    plotData <- assessClusters(useClusters, 
                               dfState=stateData, 
                               dfBurden=dfPerCapita,
                               thruLabel=thruLabel,
                               plotsTogether=TRUE
    )
    
    
    # STEP 7: Plot the consolidated metrics
    subT <- "Cases: new cases, Deaths: new deaths, Hosp: total in hospital (not new), Tests: new tests"
    consolidatedPlotData <- plotConsolidatedMetrics(plotData, 
                                                    varMain=c("state", "cluster", "date", "pop",
                                                              "cases", "deaths", "hosp", "tests"
                                                    ), 
                                                    subT=subT, 
                                                    nrowPlot2=2
    )
    
    # STEP 8: Create cumulative metrics if requested
    if (makeCumulativePlots) {
        consPos <- consolidatedPlotData %>%
            ungroup() %>%
            select(state, cluster, date, name, vpm7) %>%
            arrange(state, cluster, date, name) %>%
            pivot_wider(-vpm7, names_from="name", values_from="vpm7") %>%
            mutate(pctpos=cases/tests) %>%
            pivot_longer(-c(state, cluster, date), values_to="vpm7") %>%
            filter(!is.na(vpm7))
        clCum <- makeCumulative(consPos)
        plotCumulativeData(clCum, 
                           keyMetricp2="", 
                           flagsp2="", 
                           makep1=TRUE, 
                           makep2=FALSE
        )
        plotCumulativeData(clCum, 
                           keyMetricp2="deaths", 
                           flagsp2=findFlagStates(clCum, keyMetricVal = "deaths")
        )
        plotCumulativeData(clCum, 
                           keyMetricp2="cases", 
                           flagsp2=findFlagStates(clCum, keyMetricVal = "cases")
        )
        plotCumulativeData(clCum, 
                           keyMetricp2="tests", 
                           flagsp2=findFlagStates(clCum, keyMetricVal = "tests")
        )
    } else {
        clCum <- NULL
    }
    
    
    # STEP 9: Return a list of the key data
    list(stateData=stateData, 
         dfRaw=dfRaw, 
         dfFiltered=dfFiltered, 
         dfPerCapita=dfPerCapita, 
         useClusters=useClusters, 
         plotData=plotData, 
         consolidatedPlotData=consolidatedPlotData, 
         clCum=clCum
    )
    
    
}



# Function to extract and format key state data
getStateData <- function(df=usmap::statepop, 
                         renameVars=c("abbr"="state", "full"="name", "pop_2015"="pop"), 
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



# Function to download data for COVID Tracking Project
downloadCOVIDbyState <- function(fileName, 
                                 api="https://api.covidtracking.com/v1/states/daily.csv", 
                                 ovrWrite=FALSE
                                 ) {
    
    # COVID Tracking Project API allows data downloads for personal, non-commercial use
    # https://covidtracking.com/data/api
    
    # FUNCTION ARGUMENTS:
    # fileName: the filename that the data will be saved to
    # api: The API link for data downloads
    # ovrWrite: whether to allow overwriting of the existing fileName
    
    # Check whether fileName already exists
    if (file.exists(fileName)) {
        cat("\nFile already exists at:", fileName, "\n")
        if (ovrWrite) cat("Will over-write with current data from", api, "\n")
        else stop("Exiting due to ovrWrite=FALSE and a duplicate fileName\n")
    }
    
    # Download the file 
    download.file(api, destfile=fileName)
    
    # Show statistics on downloaded file
    file.info(fileName)
    
}



# Function to read, convert, and sanity check a downloaded file
readCOViDbyState <- function(fileName, 
                             checkFile=NULL, 
                             controlFields=c("positiveIncrease", "deathIncrease", "hospitalizedCurrently"), 
                             controlBy=c("state")
                             ) {
    
    # FUNCTION ARGUMENTS:
    # fileName: the file name for reading the data
    # checkFile: a file that can be used for comparison purposes (NULL means do not compare to anything)
    # controlFields: fields that will be explicitly checked against checkFile
    # controlBy: level of aggregation at which fields will be explicitly checked against checkFile
    
    # Helper function to check for similarity of key elements
    helperSimilarity <- function(newData, refData, label) {
        cat("\n\nCheckin for similarity of:", label)
        cat("\nIn reference but not in current:", setdiff(refData, newData))
        cat("\nIn current but not in reference:", setdiff(newData, refData))
    }
    
    # Read in the file and convert the numeric date field to date using ymd format
    df <- readr::read_csv(fileName) %>% 
        mutate(date=lubridate::ymd(date))
    
    # Check that the file is unique by date-state
    if ((df %>% select(date, state) %>% anyDuplicated()) != 0) {
        stop("\nDuplicates by date and state, investigate and fix\n")
    } else {
        cat("\nFile is unique by state and date\n")
    }
    
    # Check for overall control totals in new file
    cat("\n\nOverall control totals in file:\n")
    df %>% 
        summarize_at(vars(all_of(controlFields)), .funs=sum, na.rm=TRUE) %>% 
        print()
    
    # Get control totals by date for new file
    dfByDate <- df %>% 
        group_by(date) %>%
        summarize_at(vars(all_of(controlFields)), .funs=sum, na.rm=TRUE) %>%
        ungroup() %>%
        pivot_longer(-date, values_to="newValue")
    
    # If there is no checkFile, then just produce a plot of the key metrics
    if (is.null(checkFile)) {
        p1 <- dfByDate %>% 
            ggplot(aes(x=date, y=newValue)) + 
            geom_line() + 
            facet_wrap(~name, nrow=1, scales="free_y") + 
            labs(title="Control totals by date for new file (no reference file)", x="", y="Summed Value")
        print(p1)
    } else {
        # Check for similarity of fields, dates, and states
        cat("\n*** COMPARISONS TO REFERENCE FILE:", deparse(substitute(checkFile)))
        helperSimilarity(newData=names(df), refData=names(checkFile), label="column names")
        helperSimilarity(newData=df %>% pull(state) %>% unique(), 
                         refData=checkFile %>% pull(state) %>% unique(), 
                         label="states"
        )
        helperSimilarity(newData=df %>% pull(date) %>% unique() %>% format("%Y-%m-%d"), 
                         refData=checkFile %>% pull(date) %>% unique() %>% format("%Y-%m-%d"), 
                         label="dates"
        )
        
        # Check for similarity of control totals by date in files
        checkByDate <- checkFile %>% 
            group_by(date) %>%
            summarize_at(vars(all_of(controlFields)), .funs=sum, na.rm=TRUE) %>%
            ungroup() %>%
            pivot_longer(-date, values_to="oldValue")
        cat("\n\n*** Difference of at least 5 and difference is at least 1%:\n\n")
        dfByDate %>% 
            inner_join(checkByDate) %>%
            filter(abs(newValue-oldValue)>=5, 
                   pmax(newValue, oldValue)>=1.01*pmin(newValue, oldValue)
            ) %>%
            as.data.frame() %>%
            print()
        p1 <- dfByDate %>% 
            full_join(checkByDate) %>%
            pivot_longer(-c(date, name), names_to="newOld") %>%
            ggplot(aes(x=date, y=value, group=newOld, color=newOld)) + 
            geom_line() + 
            facet_wrap(~name, nrow=1, scales="free_y") + 
            labs(title="Control totals by date for new and reference file", x="", y="Summed Value")
        print(p1)
        
        # Check for similarity of control totals by controlBy in files
        dfByControl <- df %>% 
            semi_join(select(checkFile, date), by="date") %>%
            group_by_at(vars(all_of(controlBy))) %>%
            summarize_at(vars(all_of(controlFields)), .funs=sum, na.rm=TRUE) %>%
            ungroup() %>%
            pivot_longer(-all_of(controlBy), values_to="newValue")
        checkByControl <- checkFile %>% 
            group_by_at(vars(all_of(controlBy))) %>%
            summarize_at(vars(all_of(controlFields)), .funs=sum, na.rm=TRUE) %>%
            ungroup() %>%
            pivot_longer(-all_of(controlBy), values_to="oldValue")
        cat("\n\n*** Difference of at least 5 and difference is at least 1%:\n\n")
        dfByControl %>% 
            inner_join(checkByControl) %>%
            filter(abs(newValue-oldValue)>=5, 
                   pmax(newValue, oldValue)>=1.01*pmin(newValue, oldValue)
            ) %>%
            as.data.frame() %>%
            print()
    }
    
    # Return the processed data file
    df
    
}



# Function to select relevant variables and observations, and report on control totals
processCVData <- function(dfFull, 
                          varsKeep=c("date", "state", "positiveIncrease", "deathIncrease"), 
                          varsRename=c("positiveIncrease"="cases", "deathIncrease"="deaths"), 
                          stateList=c(state.abb, "DC")
                          ) {
    
    # FUNCTION ARGUMENTS
    # dfFull: the full data file originally loaded
    # varsKeep: variables to keep from the full file
    # varsRename: variables to be renamed, using a named vector of form originalName=newName
    # stateList: variables for filtering state (NULL means do not run any filters)
    
    # Select only the key variables
    df <- dfFull %>%
        select_at(vars(all_of(varsKeep)))
    
    # Apply the renaming of variables
    names(df) <- ifelse(is.na(varsRename[names(df)]), names(df), varsRename[names(df)])
    
    # Designate each record as being either a valid state or not
    if (!is.null(stateList)) {
        df <- df %>%
            mutate(validState=state %in% stateList)
    } else {
        df <- df %>%
            mutate(validState=TRUE)
    }
    
    # Summarize the control totals for the data, based on whether the state is valid
    cat("\n\nControl totals - note that validState other than TRUE will be discarded\n\n")
    df %>%
        mutate(n=1) %>%
        group_by(validState) %>%
        summarize_if(is.numeric, sum) %>%
        print()
    
    # Return the file, filtered to where validState is TRUE, and deleting variable validState
    df %>%
        filter(validState) %>%
        select(-validState)
    
}



# Function to add per capita and rolling to the base data frame
# Updated function to take an arbitrary number of variables and convert them
helperMakePerCapita <- function(df, 
                                mapVars=c("cases"="cpm", "deaths"="dpm"),
                                popData=stateData,
                                k=7
                                ) {
    
    # FUNCTION ARGUMENTS:
    # df: the initial data frame for conversion
    # mapVars: named vector of variables to be converted 'original name'='converted name'
    # k: the rolling time period to use
    
    # Create the variables for per capita
    for (origVar in names(mapVars)) {
        df <- df %>% 
            helperPerCapita(origVar=origVar, newName=mapVars[origVar], popData=popData)
    }
    # Arrange the data by date in preparation for rolling aggregations
    df <- df %>% 
        group_by(state) %>% 
        arrange(date)
    # Create the rolling variables
    for (newVar in mapVars) {
        df <- df %>% 
            helperRollingAgg(origVar=newVar, newName=paste0(newVar, k), k=k)
    }
    
    # Return the updated data frame, ungrouped
    df %>%
        ungroup()
    
}



# Updates to the clustering function
clusterStates <- function(df, 
                          caseVar="cpm", 
                          deathVar="dpm",
                          shapeFunc=lubridate::month, 
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
    # caseVar: the variable containing the cases per capita data
    # deathVar: the variable containing the deaths per capita data
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
    
    # Extract key information (aggregates and by shapeFunc for each state)
    df <- df %>%
        select_at(vars(all_of(c("date", "state", caseVar, deathVar)))) %>%
        purrr::set_names(c("date", "state", "cases", "deaths")) %>%
        mutate(timeBucket=shapeFunc(date)) %>%
        group_by(state, timeBucket) %>%
        summarize(cases=sum(cases), deaths=sum(deaths)) %>%
        ungroup()
    
    # Limit to only relevant time buckets if requested
    if (!is.null(minShape)) {
        df <- df %>%
            filter(timeBucket >= minShape)
    }
    if (!is.null(maxShape)) {
        df <- df %>%
            filter(timeBucket <= maxShape)
    }
    
    # Extract an aggregate by state, scaled so that they have the proper ratio
    dfAgg <- df %>%
        group_by(state) %>%
        summarize(totalCases=sum(cases), totalDeaths=sum(deaths)) %>%
        mutate(totalCases=pmin(totalCases, maxCase), totalDeaths=pmin(totalDeaths, maxDeath)) %>%
        ungroup() %>%
        mutate(totalDeaths=ratioDeathvsCase*totalDeaths*sd(totalCases)/sd(totalDeaths))
    
    # Extract the percentages (shapes) by month, scaled so that they have the proper ratio
    dfShape <- df %>%
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
    keyData <- dfCluster %>% column_to_rownames("state")
    
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



assessClusters <- function(clusters, 
                           dfState=stateData, 
                           dfBurden=cvFilteredPerCapita,
                           thruLabel="Aug 20, 2020",
                           isCounty=FALSE,
                           plotsTogether=FALSE, 
                           clusterPlotsTogether=plotsTogether,
                           recentTotalTogether=plotsTogether, 
                           clusterAggTogether=plotsTogether, 
                           makeSummaryPlots=TRUE, 
                           makeTotalvsPerCapitaPlots=!isCounty, 
                           makeRecentvsTotalPlots=TRUE, 
                           makeTotalvsElementPlots=TRUE, 
                           showMap=!isCounty, 
                           orderCluster=FALSE
                           ) {
    
    # FUNCTION ARGUMENTS:
    # clusters: the named vector containing the clusters by state
    # dfState: the file containing the states and populations
    # dfBurden: the data containing the relevant per capita burden statistics by state-date
    # thruLabel: label for plots for 'data through'
    # isCounty: boolean, is this a plot of county-level data that have been named 'state'?
    #           FALSE means that it is normal state-level data
    # plotsTogether: boolean, should plots be consolidated on fewer pages?
    # clusterPlotsTogether: boolean, should plots p1-p4 be consolidated?
    # recentTotalTogether: boolean, should recent total plots p7-p8 be consolidated?
    # clusterAggTogether: boolean, should aggregate plots p9/p11 and p10/p12 be consolidated?
    # makeSummaryPlots: boolean, should the summary plots be made?
    # makeTotalvsPerCapitaPlots: boolean, should the total and per capita plots be produced?
    # makeRecentvsTotalPlots: boolean, should the recent vs. total plots be created?
    # makeTotalvsElementPlots: boolean, should the total vs. element plots be created?
    # showMap: boolean, whether to create a map colored by cluster (will show segment counts otherwise)
    # orderCluster: if FALSE, ignore; if TRUE, order by "dpm"; if anything else, order by orderCluster
    # ...: any additional arguments passed from a calling function
    #      most common would be orderCluster=TRUE, a request for the clusters to be ordered by disease burden
    
    # Attach the clusters to the state population data
    dfState <- as.data.frame(clusters) %>%
        set_names("cluster") %>%
        rownames_to_column("state") %>%
        inner_join(dfState, by="state") %>%
        mutate(cluster=factor(cluster))
    
    # Plot the rolling 7-day mean dialy disease burden by cluster
    dfPlot <- dfState %>%
        inner_join(dfBurden, by="state") %>%
        tibble::as_tibble()
    
    # Reorder the clusters if requested
    if (!isFALSE(orderCluster)) {
        if (isTRUE(orderCluster)) burdenParam <- "dpm" else burdenParam <- orderCluster
        dfPlot <- changeOrderLabel(dfPlot, grpVars="state", burdenVar=burdenParam)
    }
    
    # Call the helper function to make the overall summary statistics
    if (makeSummaryPlots) {
        helperClusterSummaryPlots(dfState=dfState, 
                                  dfPlot=dfPlot, 
                                  showMap=showMap, 
                                  clusterPlotsTogether=clusterPlotsTogether, 
                                  mapLevel=if(isCounty) "counties" else "states"
        )
    }
    
    # These are primarily useful for state-level data and default to FALSE when isCounty is TRUE
    if (makeTotalvsPerCapitaPlots) {
        helperCallTotalPerCapita(dfPlot=dfPlot, thruLabel=thruLabel)
    }
    
    # Call the helper function to make recent vs. total plots
    if (makeRecentvsTotalPlots) {
        helperCallRecentvsTotal(dfPlot=dfPlot, 
                                thruLabel=thruLabel, 
                                labelPoints=!isCounty, 
                                recentTotalTogether = recentTotalTogether
        )
    }
    
    # Call the total vs. elements helper function
    if (makeTotalvsElementPlots) {
        helperCallTotalvsElements(dfPlot=dfPlot, 
                                  aggAndTotal=!isCounty, 
                                  clusterAggTogether=clusterPlotsTogether
        )
    }
    
    # Return the plotting data frame
    dfPlot
    
}



# Function to create plots of consolidated metrics
plotConsolidatedMetrics <- function(dfMain, 
                                    dfHosp=NULL, 
                                    varMain=c("state", "cluster", "date", "pop", "cases", "deaths", "hosp"),
                                    varDropHosp=c("n", "pop"), 
                                    joinBy=c("state", "cluster", "date"), 
                                    subT=NULL, 
                                    nrowPlot2=1
                                    ) {
    
    # FUNCTION ARGUMENTS:
    # dfMain: the main file produced by assessClusters(), containing data by state-cluster-date
    # dfHosp: the separate file with hospital data (NULL means data already available in dfMain)
    # varMain: variables to keep from dfMain
    # varDropHosp: variables to drop from dfHosp
    # joinBy: variables for joining dfMain and dfHosp
    # subT: plot subtitle (NULL will use the defaults), 
    # nrowPlot2: number of rows for the facetting to use on plot 2
    
    if (is.null(subT)) {
        subT <- "Cases: new cases, Deaths: new deaths, Hosp: total in hospital (not new)"
    }
    
    # Filter dfMain to include only variables in varMain
    dfMain <- dfMain %>%
        select_at(vars(all_of(varMain)))
    
    # Left join dfMain to dfHosp unless dfHosp is NULL
    if (!is.null(dfHosp)) {
        dfHosp <- dfHosp %>%
            select_at(vars(all_of(names(dfHosp)[!(names(dfHosp) %in% varDropHosp)])))
        dfMain <- dfMain %>%
            left_join(dfHosp, by=all_of(joinBy))
    }
    
    # Check that variables state, cluster, date, pop are all available
    if (!(c("state", "cluster", "date", "pop") %in% names(dfMain) %>% all())) {
        stop("\nFunction requires variables state, cluster, date, and pop after processing\n")
    }
    
    # Create the relevant plotting data
    dfPlot <- dfMain %>%
        pivot_longer(-c(state, cluster, date, pop)) %>%
        filter(!is.na(value)) %>%
        rbind(mutate(., state="cluster")) %>%
        group_by_at(vars(all_of(c(joinBy, "name")))) %>%
        summarize(value=sum(value), pop=sum(pop)) %>%
        mutate(vpm=1000000*value/pop) %>%
        arrange(state, cluster, name, date) %>%
        group_by(state, cluster, name) %>%
        helperRollingAgg(origVar="vpm", newName="vpm7")    
    
    # Create facetted plots for totals by metric by segment
    p1 <- dfPlot %>%
        filter(!is.na(vpm7)) %>%
        ggplot(aes(x=date, y=vpm7)) + 
        geom_line(data=~filter(., state=="cluster"), aes(group=cluster, color=cluster), lwd=1.5) +
        geom_line(data=~filter(., state!="cluster"), aes(group=state), alpha=0.25) + 
        facet_grid(name ~ cluster, scales="free_y") + 
        labs(x="", 
             y="Rolling 7-day mean per million", 
             title="Key metrics by cluster (7-day rolling mean per million)", 
             subtitle=subT
        ) + 
        scale_x_date(date_breaks="1 months", date_labels="%b") + 
        theme(axis.text.x=element_text(angle=90))
    print(p1)
    
    # Create all-segment plot by metric
    p2 <- dfPlot %>%
        filter(!is.na(vpm7)) %>%
        ggplot(aes(x=date, y=vpm7)) + 
        geom_line(data=~filter(., state=="cluster"), aes(group=cluster, color=cluster), lwd=1.5) +
        facet_wrap(~ name, scales="free_y", nrow=nrowPlot2) + 
        labs(x="", 
             y="Rolling 7-day mean per million", 
             title="Key metrics by cluster (7-day rolling mean per million)", 
             subtitle=subT
        ) + 
        scale_x_date(date_breaks="1 months", date_labels="%b") + 
        theme(axis.text.x=element_text(angle=90))
    print(p2)
    
    # Create all-metric plot by segment (define 100% as peak for segment-metric)
    p3 <- dfPlot %>%
        filter(!is.na(vpm7)) %>%
        group_by(state, cluster, name) %>%
        mutate(spm7=vpm7/max(vpm7)) %>%
        ggplot(aes(x=date, y=spm7)) + 
        geom_line(data=~filter(., state=="cluster"), aes(group=name, color=name), lwd=1) +
        facet_wrap(~ cluster, scales="free_y") + 
        labs(x="", 
             y="% of Maximum", 
             title="Key metrics by cluster (% of cluster maximum for variable)", 
             subtitle=subT
        ) + 
        scale_x_date(date_breaks="1 months", date_labels="%b") + 
        scale_color_discrete("variable") +
        theme(axis.text.x=element_text(angle=90))
    print(p3)
    
    # Return the plotting data
    dfPlot
    
}

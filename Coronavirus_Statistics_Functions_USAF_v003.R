# Function to run the USA Facts (US county-level coronavirus data) clustering process
readRunUSAFacts <- function(maxDate, 
                            popLoc, 
                            caseLoc, 
                            deathLoc, 
                            dlPop=FALSE, 
                            dlCaseDeath=FALSE, 
                            ovrWrite=FALSE, 
                            ovrWriteError=TRUE, 
                            oldFile=NULL, 
                            showBurdenMinPop=10000, 
                            minPopCluster=25000,
                            existingStateClusters=NULL, 
                            existingCountyClusters=NULL, 
                            createClusters=FALSE, 
                            hierarchical=FALSE,
                            kCut=6,
                            orderCluster=TRUE,
                            ...
                            ) {
    
    # FUNCTION ARGUMENTS:
    # maxDate: the maximum data to use for data from the cases and deaths file
    # popLoc: location where the county-level population data are stored
    # caseLoc: location where the county-level cases data are stored
    # deathLoc: location where the county-level deaths data are stored
    # dlPop: boolean, should new population data be downloaded to popLoc
    # dlCaseDeath: boolean, should new case data and death data be downloaded to caseLoc and deathLoc
    # ovrWrite: boolean, if data are downloaded to an existing file, should it be over-written
    # ovrWriteError: boolean, if ovrWrite is FALSE and an attempt to overwrite is made, should it error out?
    # oldFile: old file for comparing metrics against (NULL means no old file for comarisons)
    # showBurdenMinPop: minimum population for showing in burden by cluster plots (NULL means skip plot)
    # minPopCluster: minimum population for including county in running cluster-level metrics
    # existingStateClusters: location of an existing named vector with clusters by state (NULL means none)
    # existingCountyClusters: location of an existing named vector with clusters by county (NULL means none)
    #                         if existingStateClusters is not NULL, then existingCountyClusters is ignored
    # createClusters: boolean, whether to create new clusters (only set up for kmeans)
    # hierarchical: whether to create hierarchical clusters
    #               TRUE means run hierarchical clustering
    #               FALSE means run kmeans clustering
    #               NA means run rules-based clustering
    # kCut; if hierarchical clustering is used, what k (number of clusters in cutree) should be used?
    # orderCluster: if FALSE, ignore; if TRUE, order by "dpm"; if anything else, order by orderCluster
    # ...: other arguments that will be passed to prepClusterCounties
    
    # STEP 0: Download new files (if requested)
    urlCase <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
    urlDeath <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
    urlPop <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv"
    
    # Helper function to download a file
    helperDownload <- function(url, loc, ovrWrite=ovrWrite, ovrWriteError=ovrWriteError) {
        # If the file exists, mention it and proceed as per the guard checks
        if (file.exists(loc)) {
            cat("\nFile:", loc, "already exists\n")
            if (!ovrWrite & ovrWriteError) stop("\nExiting due to ovrWrite=FALSE and ovrWriteError=TRUE\n")
            if (!ovrWrite & !ovrWriteError) {
                cat("\nFile is NOT downloaded again\n")
                return(NULL)
            }
        }
        # Download the file and change to read-only
        download.file(url, destfile=loc, method="curl")
        Sys.chmod(loc, mode="0555", use_umask = FALSE)
    }
    
    if (dlPop) helperDownload(urlPop, loc=popLoc)
    if (dlCaseDeath) helperDownload(urlCase, loc=caseLoc)
    if (dlCaseDeath) helperDownload(urlDeath, loc=deathLoc)
    
    # STEP 1: Read in the population file
    pop <- readr::read_csv(popLoc) %>%
        rename(countyName=`County Name`, state=State)
    
    # STEP 2: Read case and death data, combine, and add population totals and existing clusters
    burdenData <- readUSAFacts(
        caseFile=caseLoc, 
        deathFile=deathLoc, 
        countyPopFile=pop,
        oldFile=oldFile,
        showBurdenMinPop=showBurdenMinPop,
        maxDate=maxDate,
        stateClusters=existingStateClusters, 
        countyClusters=existingCountyClusters, 
        glimpseRaw=FALSE
    )
    
    # STEP 3: Create appropriately filtered data, and new clusters if requested
    clusterData <- prepClusterCounties(burdenFile=burdenData, 
                                       maxDate=maxDate, 
                                       minPop=minPopCluster,
                                       createClusters=createClusters, 
                                       hierarchical=hierarchical, 
                                       returnList=TRUE,
                                       ...
    )
    
    # STEP 4: Assess clusters against the new data
    # STEP 4a: Extract the county-level clusters (new clusters if created, existing otherwise)
    if (createClusters) {
        if (is.na(hierarchical)) clustVec <- clusterData$objCluster$objCluster
        else if (hierarchical) clustVec <- cutree(clusterData$objCluster$objCluster, k=kCut)
        else clustVec <- clusterData$objCluster$objCluster$cluster
    }
    else {
        clustVec <- existingCountyClusters
    }
    
    # STEP 4b: Show the cumulative data, order by cluster, and keep the plots together
    helperACC_county <- helperAssessCountyClusters(vecCluster=clustVec, 
                                                   dfPop=clusterData$countyFiltered, 
                                                   dfBurden=clusterData$countyFiltered, 
                                                   showCum=TRUE,
                                                   thruLabel=format(as.Date(maxDate), "%b %d, %Y"), 
                                                   plotsTogether=TRUE, 
                                                   orderCluster=orderCluster
    )
    
    # STEP 5: Add back clusters not used for analysis (code 999) and associated disease data
    # May want to change the approach to population data
    clusterStateData <- helperMakeClusterStateData(dfPlot=helperACC_county, 
                                                   dfPop=usmap::countypop,
                                                   dfBurden=clusterData$countyDailyPerCapita,
                                                   orderCluster=orderCluster
    )
    
    # STEP 6: Return a list of the key files
    list(pop=pop, 
         burdenData=burdenData, 
         clusterData=clusterData, 
         clustVec=clustVec, 
         helperACC_county=helperACC_county, 
         clusterStateData=clusterStateData,
         maxDate=maxDate
    )
    
}



# Function to read and convert raw data from USA Facts
readUSAFacts <- function(caseFile, 
                         deathFile, 
                         countyPopFile=filter(pop_usafacts, countyFIPS!=0),
                         oldFile=NULL,
                         showBurdenMinPop=NULL,
                         maxDate=NULL,
                         stateClusters=NULL, 
                         countyClusters=NULL, 
                         glimpseRaw=TRUE
                         ) {
    
    # FUNCTION ARGUMENTS:
    # caseFile: the location of the downloaded cases dataset
    # deathsFile: the location of the downloaded deaths dataset
    # countyopFile: the location of the county population file
    # oldFile: a file for comparing control totals against (NULL means do not compare)
    # showBurdenMinPop: minimum population for showing burden by cluster (NULL means skip plot)
    # maxDate: the date to use for the burden by cluster plot (ignored unless showBurdenMinPop is not NULL)
    # stateClusters: a field 'cluster' will be created from state using this named vector
    #                NULL means do not use state for finding clusters
    # countyClusters: a field 'cluster' will be created from countyFIPS using this named vector
    #                 NULL means do not use county for finding clusters
    # If both stateClusters and countyClusters are passed, only stateClusters will be used
    # glimpseRaw: boolean, whether to show a glimpse of the raw data when initially read in
    
    # Read cases file
    cnvCases <- helperReadConvert(caseFile, 
                                  valueName="cumCases", 
                                  glimpseRaw=glimpseRaw, 
                                  countyPopFile=countyPopFile
    )
    
    # Read deaths file
    cnvDeaths <- helperReadConvert(deathFile, 
                                   valueName="cumDeaths", 
                                   glimpseRaw=glimpseRaw, 
                                   countyPopFile=countyPopFile
    )
    
    # Join the files so there is countyFIPS-county-population-date and cumCases cumDeaths
    # Also, add the state segments as 'cluster' if requested
    dfBurden <- cnvDeaths %>% 
        select(-countyName.x, -countyName.y, -bold, cumDeathPer=burden) %>%
        inner_join(cnvCases %>% 
                       select(-countyName.x, -countyName.y, -bold, cumCasesPer=burden), 
                   by=c("countyFIPS", "stateFIPS", "county", "state", "date", "population")
        ) %>%
        mutate(cluster=if(is.null(stateClusters)) NA else stateClusters[state])
    
    # Attach county clusters if requested and if state clusters were not attached
    # County cluster file converted to 5-digit character
    str_pad5 <- function(x) stringr::str_pad(x, width=5, side="left", pad="0")
    if (is.null(stateClusters) & !is.null(countyClusters)) {
        names(countyClusters) <- str_pad5(as.character(names(countyClusters)))
        dfBurden <- dfBurden %>%
            mutate(cluster=countyClusters[str_pad5(countyFIPS)])
    }
    
    # Compare against an old file, if requested
    if (!is.null(oldFile)) {
        p1 <- bind_rows(oldFile, dfBurden, .id="source") %>%
            mutate(source=factor(case_when(source==1 ~ "Previous", source==2 ~ "New", TRUE ~ "Unknown"), 
                                 levels=c("New", "Previous", "Unknown")
            )
            ) %>%
            group_by(source, date) %>%
            summarize(cumDeaths=sum(cumDeaths), cumCases=sum(cumCases)) %>%
            pivot_longer(-c(source, date)) %>%
            ggplot(aes(x=date, y=value/1000, group=source, color=source)) + 
            geom_line() + 
            facet_wrap(~c("cumCases"="Cases", "cumDeaths"="Deaths")[name], scales="free_y") + 
            scale_x_date(date_breaks="1 months", date_labels="%m") + 
            labs(y="Burden (000s)", title="US National Coronavirus Burden by Source")
        print(p1)
    }
    
    if (!is.null(showBurdenMinPop)) {
        plotBurdenData(dfBurden, maxDate=maxDate, minPop=showBurdenMinPop)
    }
    
    # Return the burdens file
    dfBurden
    
}



# Helper function to read and convert 
helperReadConvert <- function(file, 
                              valueName, 
                              countyPopFile,
                              glimpseRaw=TRUE, 
                              glimpsePivot=TRUE
                              ) {
    
    # FUNCTION ARGUMENTS:
    # file: the file for reading and converting
    # valueName: name for the values column of the pivoted data
    # countyPopFile: file containing county population data
    # glimpseRaw: boolean, whether to show a glimpse of the raw data
    # glimpsePivot: boolean, whether to show a glimpse of the pivoted data
    
    # Read file
    df <- readr::read_csv(file)
    if (glimpseRaw) glimpse(df)
    
    # Conversion of the raw data
    dfPivot <- df %>%
        rename(countyName=`County Name`, state=State) %>%
        pivot_longer(-c(countyFIPS, countyName, state, stateFIPS), 
                     names_to="date", 
                     values_to=valueName
        ) %>%
        mutate(date=lubridate::mdy(date))
    if (glimpsePivot) glimpse(dfPivot)
    
    # Conversion of the pivoted data
    dfConverted <- countyLevelEvolution(dfPivot, 
                                        burdenVar=valueName, 
                                        countyPopFile=countyPopFile,
                                        inclStates=NULL, 
                                        topN=5,
                                        printPlot=FALSE, 
                                        returnData=TRUE
    )
    
    # Return the converted file
    dfConverted
    
}



# Function to plot evolution of county-level burdens
countyLevelEvolution <- function(dfBurden, 
                                 burdenVar, 
                                 countyPopFile,
                                 inclStates=NULL, 
                                 topN=10, 
                                 topNDate=NULL, 
                                 printPlot=TRUE, 
                                 returnData=TRUE, 
                                 plotTitle=NULL, 
                                 countyPopFloor=0, 
                                 subT=NULL
                                 ) {
    
    # FUNCTION ARGUMENTS:
    # dfBurden: file containing the relevant per-capita burden data
    # burdenVar: the name of the variable containing the burden per-capita data
    # countyPopFile: file containing population by county
    # inclStates: states to be included (default NULL means include all)
    # topN: integer, number of counties to flag as "top"
    # topNDate: the data to use as the topN cutpoint (NULL means most recent)
    # printPlot: boolean, whether to print the plot
    # returnData: boolean, if TRUE return the per-capita data file, otherwise return the plot object
    # plotTitle: title for the plot (NULL means assume from burdenVar)
    # countyPopFloor: floor for county population for the county to be plotted
    # subT: subtitle for the chart (NULL means none)
    
    # Adjust inclStates if NULL
    if (is.null(inclStates)) inclStates <- dfBurden %>% count(state) %>% pull(state)
    
    # Create plotTitle if needed
    if (is.null(plotTitle)) {
        plotTitle <- if(stringr::str_detect(string=stringr::str_to_lower(burdenVar), pattern="deaths")) {
            "Cumulative per-capita deaths by county" 
        } else {
            "Cumulative per-capita cases by county"
        }
    }
    
    # Get all possible dates
    allDates <- dfBurden %>% count(date) %>% pull(date)
    
    # Get the data for the counties in the relevant states (return data only and do not plot)
    perCapData <- plotCountyPerCapita(dfBurden, 
                                      burdenVar=burdenVar, 
                                      useDate=allDates, 
                                      inclStates=inclStates, 
                                      dfCounty=countyPopFile,
                                      printPlot=FALSE, 
                                      returnData=TRUE
                                      )
    
    # Get the relevant top-N date and convert to Date if not already of that type
    if (is.null(topNDate)) topNDate <- perCapData %>% pull(date) %>% max()
    if (!("Date" %in% class(topNDate))) topNDate <- as.Date(topNDate)
    
    # Get the top-n counties by burdenVar
    # Top 10 counties hit by FIPS, counting only those that exceed the population floor
    topN <- perCapData %>%
        filter(date==topNDate, population>=countyPopFloor) %>%
        arrange(-burden) %>%
        head(topN) %>%
        pull(countyFIPS)
    
    # Update perCapData with easy-read county name and bolding instructions
    perCapData <- perCapData %>%
        mutate(county=paste0(str_replace(countyName.x, " County", ""), " (", state, ")"), 
               bold=ifelse(countyFIPS %in% topN, 1, 0)
        ) %>%
        arrange(date)
    
    # Create the plot of all counties with the topN flagged
    # Evolution of per capita deaths by date in the northeast
    p1 <- perCapData %>%
        filter(population>=countyPopFloor) %>%
        ggplot(aes(x=date, y=burden)) + 
        geom_line(aes(group=countyFIPS, color=state, alpha=0.25 + 0.75*bold, size=0.5+0.5*bold)) + 
        scale_alpha_identity() + 
        scale_size_identity() + 
        geom_text(data=~filter(., bold==1, date==max(date)), 
                  aes(x=date+lubridate::days(2), label=paste0(county, ": ", round(burden)), color=state), 
                  size=3, 
                  fontface="bold", 
                  hjust=0
        ) +
        scale_x_date(date_breaks="1 months", date_labels="%m", expand=expand_scale(mult=c(0, 0.4))) + 
        labs(x="", y="Burden per million people", title=plotTitle) + 
        theme(legend.position="bottom")
    
    # Add the subtitle if passed
    if (!is.null(subT)) p1 <- p1 + labs(subtitle=subT)
    
    # Print the plot if requested
    if (printPlot) print(p1)
    
    # Return the relevant object
    if (returnData) perCapData else p1
    
}



# Function to plot per capita data by county
plotCountyPerCapita <- function(dfDisease, 
                                burdenVar,
                                useDate,
                                plotTitle="",
                                inclStates=NULL,
                                dfCounty=filter(pop_usafacts, countyFIPS!=0), 
                                popVar="population", 
                                highColor="darkblue", 
                                maxPerCap=NULL,
                                printPlot=TRUE, 
                                returnData=!printPlot
                                ) {
    
    # FUNCTION ARGUMENTS
    # dfDisease: file containing disease data
    # burdenVar: variable for disease burden (cumulative) on date
    # useDate: date for the analysis
    # plotTitle: title for the plot
    # inclStates: states to include (NULL means include all)
    # dfCounty: data for county-level population
    # popVar: variable for population in the dfCounty file
    # maxPerCap: the maximum amount to be used for per capita (everything at or above shaded the same)
    # highColor: the color to be used for high disease burden
    # printPlot: boolean, whether to print the plot
    # returnData: boolean, whether to return the underlying data (if FALSE, the plot object is returned)
    
    # Create the relevant data frame
    dfData <- dfDisease %>% 
        left_join(dfCounty, by=c("countyFIPS", "state")) %>% 
        filter(date %in% useDate, countyFIPS!=0, population>0) %>% 
        mutate(burden=1000000*get(burdenVar)/get(popVar))
    
    # Modify inclStates to be every state (if needed due to NULL)
    if (is.null(inclStates)) inclStates <- dfData %>% pull(state) %>% unique() %>% sort()
    
    # Create the relevant plot (this is necessary if printPlot is TRUE or returnData is FALSE)
    if (printPlot | !returnData) {
        p1 <- dfData %>%
            mutate(burden=if(is.null(maxPerCap)) burden else pmin(burden, maxPerCap)) %>%
            select(fips=countyFIPS, burden) %>% 
            usmap::plot_usmap(regions="counties", data=., values="burden", include=inclStates)
        if (is.null(maxPerCap)) 
            p1 <- p1 + scale_fill_continuous(plotTitle, low="white", high=highColor)
        else {
            p1 <- p1 + 
                scale_fill_continuous(plotTitle, low="white", high=highColor, limits=c(0, maxPerCap)) + 
                labs(caption=paste0("Values at/above ", maxPerCap, " plotted as ", maxPerCap))
        }
    }
    
    # Print the plot if requested
    if (printPlot) print(p1)
    
    # Return the data if requested, otherwise return the plot object
    if (returnData) dfData %>% filter(state %in% inclStates) else p1
    
}



# Function to plot the burden by cluster
plotBurdenData <- function(df, 
                           maxDate, 
                           minPop
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: the burden data
    # maxDate: the maximum date
    # minPop: the minimum population
    
    # Create a date label
    dateLabel <- format(as.Date(maxDate), "%b %d, %Y")
    
    # Plot the data as of maxDate
    p1 <- df %>%
        filter(date==maxDate, population>=minPop) %>%
        ggplot(aes(x=cumCasesPer, y=cumDeathPer)) + 
        geom_point(aes(size=log10(population), color=factor(cluster)), alpha=0.25) + 
        geom_smooth(method="lm", se=FALSE, aes(weight=population, color=factor(cluster))) +
        scale_size_continuous("log10\nCounty\nPop") + 
        labs(x=paste0("Cases per million as of ", dateLabel), 
             y=paste0("Deaths per million as of ", dateLabel), 
             title=paste0("County-level per-capita coronavirus burden as of ", dateLabel), 
             subtitle=paste0("Filtered to counties with population of at least ", minPop)
        ) + 
        facet_wrap(~cluster)
    print(p1)
    
    # Further, the total burden by cluster is plotted
    p2 <- df %>%
        filter(date==maxDate, population>=0) %>%
        group_by(cluster) %>%
        summarize(population=sum(population), 
                  cumCases=sum(cumCases), 
                  cumDeath=sum(cumDeaths), 
                  mdn_CasesPer=median(cumCasesPer), 
                  mdn_DeathPer=median(cumDeathPer)
        ) %>%
        mutate(mean_CasesPer=1000000*cumCases/population, mean_DeathPer=1000000*cumDeath/population) %>%
        select(cluster, starts_with("mdn"), starts_with("mean")) %>%
        pivot_longer(-cluster) %>%
        mutate(aggType=stringr::str_replace(name, "_.*", ""), 
               metType=stringr::str_replace(name, ".*_", "")
        ) %>%
        pivot_wider(c(cluster, aggType), names_from="metType", values_from="value") %>%
        ggplot(aes(x=CasesPer, y=DeathPer)) + 
        geom_point(aes(color=factor(cluster)), size=5) + 
        labs(x=paste0("Cases per million as of ", dateLabel), 
             y=paste0("Deaths per million as of ", dateLabel), 
             title=paste0("Cluster-level per-capita coronavirus burden as of ", dateLabel), 
             subtitle="State-level clusters based on hierarchical (method=`complete`)"
        ) + 
        scale_color_discrete("Cluster") +
        ylim(c(0, NA)) + 
        xlim(c(0, NA)) + 
        facet_wrap(~c("mdn"="Median of all counties in segment", "mean"="Segment-level metric")[aggType])
    print(p2)
    
}



# Updated preparation file for county clustering
prepClusterCounties <- function(burdenFile, 
                                maxDate,
                                minPop, 
                                createClusters=TRUE,
                                hierarchical=FALSE, 
                                returnList=!hierarchical, 
                                ...
                                ) {
    
    # FUNCTION ARGUMENTS:
    # burdenFile: the pivoted file containing the burdens data
    # maxDate: the latest date to be used from the data
    # minPop: the smallest population for a county to be included
    # createClusters: boolean, whether to create county-level clusters (if FALSE, object returned as NULL)
    #                 FALSE would be for prepping and converting data only (those objects are returned)
    # hierarchical: whether to create hierarchical clusters
    #               TRUE means run hierarchical clustering
    #               FALSE means run kmeans clustering
    #               NA means run rules-based clustering
    # returnList: whether the clustering call returns a list (only set up for TRUE as of now)
    # ...: other arguments to be passed to clusterStates
    
    # STEP 1: Select only desired variables from burden file
    countyCumPerCapita <- burdenFile %>%
        select(state=countyFIPS, date, cpm=cumCasesPer, dpm=cumDeathPer, population) %>%
        arrange(state, date)
    
    # STEP 2: Confirm that there are no duplicates and that every county has the same dates
    # This should be 1 provided that there are no duplicates
    countyCumPerCapita %>% 
        count(state, date) %>% 
        pull(n) %>% 
        max()
    
    # This should have no standard deviation if the same number of records exist on every day
    countyCumPerCapita %>%
        mutate(n=1) %>%
        group_by(date) %>%
        summarize(n=sum(n), population=sum(population)) %>%
        summarize_at(vars(all_of(c("n", "population"))), .funs=list(sd=sd, min=min, max=max))
    
    # STEP 3: Convert to daily new totals rather than cumulative data
    countyDailyPerCapita <- countyCumPerCapita %>%
        group_by(state) %>%
        arrange(date) %>%
        mutate_at(vars(all_of(c("cpm", "dpm"))), ~ifelse(row_number()==1, ., .-lag(.))) %>%
        ungroup()
    
    # STEP 4: Add rolling 7 aggregates and total cases/deaths
    countyDailyPerCapita <- countyDailyPerCapita %>%
        arrange(state, date) %>%
        group_by(state) %>%
        helperRollingAgg(origVar="cpm", newName="cpm7", k=7) %>%
        helperRollingAgg(origVar="dpm", newName="dpm7", k=7) %>%
        ungroup() %>%
        mutate(cases=cpm*population/1000000, deaths=dpm*population/1000000)
    
    # STEP 5: Filter the data prior to clustering
    countyFiltered <- countyDailyPerCapita %>%
        filter(population >= minPop, date <= as.Date(maxDate)) %>%
        mutate(state=as.character(state))
    
    # STEP 6: Check the implications of the filtering
    # Step 6a: Check whether minDeath and minCase have been passed
    ellipsisList <- (list(...))
    
    if ("minCase" %in% names(ellipsisList)) {
        minCase <- ellipsisList[["minCase"]]
        cat("\nShape curves will impose a floor of at least", minCase, "cases per million")
    } else {
        cat("\nShapes will be created without any floor on the number of cases per million")
        minCase <- 0
    }
    
    if ("minDeath" %in% names(ellipsisList)) {
        minDeath <- ellipsisList[["minDeath"]]
        cat("\nShape curves will impose a floor of at least", minDeath, "deaths per million")
    } else {
        cat("\nShapes will be created without any floor on the number of deaths per million")
        minDeath <- 0
    }
    
    # Check number of counties that fail the test for minDeath deaths per million or minCase cases per million
    is0 <- function(x) mean(x==0)
    isltn <- function(x, n) mean(x<n)
    isltDeath <- function(x) isltn(x, n=minDeath)
    isltCase <- function(x) isltn(x, n=minCase)
    
    cat("\n*** Counties with 0 cases/deaths or that fall below the floor for minCase/minDeath ***\n")
    countyFiltered %>% 
        group_by(state) %>% 
        summarize_at(c("cpm", "dpm"), sum) %>% 
        ungroup() %>%
        summarize_at(vars(all_of(c("cpm", "dpm"))), 
                     .funs=list(mean_is0=is0, mean_ltDeath=isltDeath, mean_ltCase=isltCase)
        ) %>%
        select(-cpm_mean_ltDeath, -dpm_mean_ltCase) %>%
        print()
    
    # Run county-level clusters if requested, otherwise store as NULL
    objCluster <- if(createClusters) {
        clusterStates(countyFiltered, hierarchical=hierarchical, returnList=returnList, ...)
    } else {
        NULL
    }
    
    # Return all of the relevant objects
    list(objCluster=objCluster, 
         countyFiltered=countyFiltered, 
         countyDailyPerCapita=countyDailyPerCapita, 
         countyCumPerCapita=countyCumPerCapita
    )
    
}



# Function for calling assessClusters() for county-level segments
helperAssessCountyClusters <- function(vecCluster, 
                                       dfPop, 
                                       dfBurden, 
                                       dfPopGeoVar="state", 
                                       dfPopPopVar="population", 
                                       showCum=FALSE,
                                       ...
                                       ) {
    
    # FUNCTION ARGUMENTS:
    # vecCluster: the named cluster vector
    # dfPop: the data frame containing the population data
    # dfBurden: the data frame containing the burden statistics by county and date
    # showCum: boolean, whether to create the cumulative burdens chart
    # ...: other arguments to pass to assessClusters()
    
    # Run the process    
    plotAssess <- assessClusters(vecCluster, 
                                 dfState=dfPop %>% 
                                     group_by_at(dfPopGeoVar) %>% 
                                     summarize(pop=mean(get(dfPopPopVar))) %>% 
                                     ungroup(), 
                                 dfBurden=select(dfBurden, -population), 
                                 isCounty=TRUE, 
                                 ...
    )
    
    # Create the cumulative burdens chart if requested
    if (showCum) {
        p1 <- plotAssess %>%
            select(cluster, date, pop, cases, deaths) %>%
            group_by(cluster, date) %>%
            summarize_if(is.numeric, sum, na.rm=TRUE) %>%
            arrange(date) %>%
            mutate(cpmcum=cumsum(cases)*1000000/pop, dpmcum=cumsum(deaths)*1000000/pop) %>%
            ungroup() %>%
            select(cluster, date, cases=cpmcum, deaths=dpmcum) %>%
            pivot_longer(-c(cluster, date)) %>%
            ggplot(aes(x=date, y=value, color=cluster)) + 
            geom_line(size=1) + 
            geom_text(data=~filter(., date==max(date)), 
                      aes(x=date+lubridate::days(2), label=round(value)), 
                      size=3, 
                      hjust=0
            ) +
            labs(x="", title="Cumulative burden per million people by segment", y="") +
            facet_wrap(~c("cases"="Cases per million", "deaths"="Deaths per million")[name], scales="free_y") +
            scale_x_date(date_breaks="1 months", date_labels="%b", expand=expand_scale(c(0, 0.1)))
        print(p1)
    }
    
    # Return the plot object
    plotAssess
    
}



# Create the cluster-state data
helperMakeClusterStateData <- function(dfPlot, 
                                       dfPop=usmap::countypop,
                                       dfBurden=countyDailyPerCapita, 
                                       orderCluster=FALSE
                                       ) {
    
    # FUNCTION ARGUMENTS:
    # dfPlot: the raw plotting data (which can have factors already reordered)
    # dfPop: source for full county data with population
    # dfBurden: source for disease burden by geography
    # orderCluster: if FALSE, ignore; if TRUE, order by "dpm"; if anything else, order by orderCluster
    
    # Merge in the counties that were previously excluded from segmentation
    df <- dfPlot %>%
        filter(date==max(date)) %>%
        select(fipsCounty=state, cluster) %>%
        mutate(fipsCounty=stringr::str_pad(fipsCounty, width=5, side="left", pad="0")) %>%
        right_join(select(dfPop, fipsCounty=fips, state=abbr, countyName=county, pop=pop_2015)) %>%
        mutate(cluster=factor(ifelse(is.na(cluster), 999, as.character(cluster))))
    
    # Merge in the disease data
    df <- dfBurden %>%
        mutate(fipsCounty=stringr::str_pad(state, width=5, side="left", pad="0")) %>%
        select(-state, -population) %>%
        right_join(df)
    
    # Reorder the clusters if requested
    if (!isFALSE(orderCluster)) {
        if (isTRUE(orderCluster)) burdenParam <- "dpm" else burdenParam <- orderCluster
        df <- changeOrderLabel(df, grpVars="fipsCounty", burdenVar=burdenParam)
    }
    
    # Return the relevant frame
    df
    
}

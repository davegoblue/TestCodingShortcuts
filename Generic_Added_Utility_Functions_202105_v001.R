# File to be sourced, contains generic utility functions

# FUNCTIONS INCLUDED:
#  1. saveToRDS() - take an R object and (by default) save it to an RDS file of the same name
#  2. readFromRDS() - take a character string, and readRDS() from that path
#  3. customYYYYMM() - format date object as 'YYYY-MM', with MM always being length 2 (e.g., '05')
#  4. glimpseFile() - provide a text description, then glimpse a file
#  5. fileDownload() - generic function for downloading a file, with checks for overwriting existing data
#  6. fileRead() - generic function for reading a downloaded file
#  7. colRenamer() - generic function for renaming columns, using c('existing name'='new name', ...)
#  8. colSelector() - generic function for selecting columns, using c('desired columns', ...)
#  9. colMutater() - generic function for mutating columns, driven by list('column'=function/expr)
# 10. checkUniqueRows() - generic function to confirm that a tibble is unique by a specified key
# 11. checkControl() - generic function to get control totals, by level, for specified variables
# 12. rowFilter() - generic function for filtering column(s), using list("field"=c("allowed values"), ...)
# 13. combineRows() - generic function to aggregate data from multiple rows to a single row
# 14. helperLinePlot() - generic function for creating a basic line plot (typically for control totals)
# 15. helperSimilarity() - generic function to compare distinct values in two different vectors
# 16. checkSimilarity() - generic function to compare specified columns in two files
# 17. plotSimilarity() - generic function to plot similarity results from checkSimilarity()
# 18. compareAggregate() - generic function to compare data in two files aggregated to a specified level
# 19. flagLargeDelta() - generic function to highlight values that differ by more than a specified tolerance
# 20. printLog() - generic function for printing to a specified log file
# 21. combineFiles() - generic function for combining a list of files using a passed reduce function
# 22. glimpseLog() - generic function for directing glimpseFile() output to an external log file
# 23. joinFrames() - join frames, can be passed as either indefinite length ... data frames or list(...)
# 24. vecToTibble() - generic function for converting named vector to tibble (names and values to columns)
# 25. specNA() - converts a function to return NA if everything is NA, apply na.rm=TRUE otherwise
# 26. specSumProd() - generic function to take a sumproduct (like the numerator for weighted.mean)
# 27. createGroupAgg() - generic function to aggregate data in specified columns by level using function
# 28. genNewLog() - generic function for generating a new external log file
# 29. processRawFile() - generic function for combining and filtering on a raw file; report QC totals
# 30. createPerCapita() - generic function for creating per capita metrics
# 31. helperMakePerCapita() - generic helper function for making rolling per-capita data
# 32. helperPerCapita() -  generic function for making rolling per-capita data for a column
# 33. helperRollingAgg() - generic function to make rolling-k data
# 34. helperElbow() - generic function to make elbow and silhouette plots from kmeans and distance data
# 35. zeroPad() - generic function for zero-padding a number/character (output as character)
# 36. zeroPad2() - zeroPad() with argument width=2 preset
# 37. zeroPad5() - zeroPad() with argument width=5 preset
# 38. tempStackPlot() - create stacked bar plots with a time dimension
# 39. lagCorrCheck() - find best correlation and lag/lead for two series of data
# 40. findPeaks() - function to find local extrema in a time series
# 40b. oldFindPeaks() - previous version of function to find local extrema in a time series
# 41. testImputeNA() - impute NA values for a vector based on another vector
# 42. pivotData() - helper function for pivoting data (longer or wider)
# 43. zeroNA() - convert NA data in a vector to 0
# 44. findDeltaFromMax() - find declines in a series that should be monotonically non-decreasing
# 45. cleanMem() - delete files and check memory impact
# 46. partialCSVRead() - read part of a CSV file
# 47. vecGaps() - Get break points for vector (e.g., c(0, 3, 5:8, 20) returns 0, 3, 5, 20 and 0, 3, 8, 20)
# 48. flatFileGaps() - find break points in flat file
# 49. readMultiCSV() - treat a single flat file with multiple gaps as a single CSV, and read it
# 50. plotClusterMeans() - plot means by cluster/variable for a k-means object
# 51. plotClusterPct() - variations in 1 or 2 key variables by k-means cluster
# 52. runKMeans() - run k-means on specified data OR use an existing k-means object, and plot key data
# 53. assignKMeans() - assign points to the nearest centroid of a provided k-means object
# 54. autoRound() - round every number in a vector, using automated rules if new rndTo parameter is passed
# 55. reorder_cormat() - Reorder output of correlation matrix for better heatmap plotting copied from STHDA)
# 56, makeHeatMap() - create a correlation heatmap, reordering variables as requested

# Function for saving an R object to RDS, including a check for whether the object already exists
saveToRDS <- function(obj, 
                      file=paste0(deparse(substitute(obj)), ".RDS"), 
                      dir="./RInputFiles/Coronavirus/", 
                      ovrWrite=FALSE, 
                      ovrWriteError=TRUE,
                      makeReadOnly=TRUE
                      ) {
    
    # FUNCTION ARGUMENTS:
    # obj: the R object to save
    # file: the file name to save as
    # dir: the directory to save in (file path will be paste0(dir, file))
    # ovrWrite: boolean, should the file be overwritten if it already exists?
    # ovrWriteError: boolean, should an error be thrown if an attempt is made to overwrite the file?
    # makeReadOnly: boolean, should the output file be made read-only?
    
    # Create the file name
    locFile <- paste0(dir, file)
    
    # Check if the file already exists and proceed as per options
    if (file.exists(locFile)) {
        cat("\nFile already exists:", locFile, "\n")
        if (!ovrWrite & ovrWriteError) stop("\nAborting due to ovrWrite=FALSE and ovrWriteError=TRUE")
        if (!ovrWrite) {
            cat("\nNot replacing the existing file since ovrWrite=FALSE\n")
            return(NULL)
        }
    }
    
    # Save the file and update the permissions to read-only (if flag is set)
    saveRDS(obj, file=locFile)
    if (makeReadOnly) Sys.chmod(locFile, mode="0555", use_umask = FALSE)
    
}



# Function for reading an R object from RDS
readFromRDS <- function(file, 
                        dir="./RInputFiles/Coronavirus/", 
                        addSuffix=".RDS", 
                        deparseSub=FALSE
                        ) {
    
    # FUNCTION ARGUMENTS:
    # file: the file name to read in
    # dir: the directory the file is in
    # addSuffix: the suffix that should be added to file (file path will be paste0(dir, file, addSuffix))
    # deparseSub: whether to deparse and substitute file (use it as the text name)
    
    # Convert file if needed
    if (deparseSub) file <- deparse(substitute(file))
    
    # Ensure that file is of type character
    if (!isTRUE(all.equal(class(file), "character"))) {
        stop("\nUnable to read since file is not a character\n")
    }
    
    # Create the file name
    locFile <- paste0(dir, file, addSuffix)
    
    # Read the file (will be the return)
    readRDS(locFile)
    
}



# Custom function for creating YYYY-MM for use as the shape of the curve function
customYYYYMM <- function(x) {
    paste0(lubridate::year(x), 
           "-", 
           stringr::str_pad(lubridate::month(x), width=2, side="left", pad="0")
           )
}



# Helper function for glimpsing
glimpseFile <- function(x, txt) {
    cat(txt)
    glimpse(x)
}



# Generic function to download data given a download location and URL
fileDownload <- function(fileName, 
                         url, 
                         ovrWrite=FALSE, 
                         ...
                         ) {
    
    # FUNCTION ARGUMENTS:
    # fileName: the filename that the data will be saved to
    # url: the URL to pull the data from
    # ovrWrite: whether to allow overwriting of the existing fileName
    # ...: other arguments to pass to download.file
    
    # Check whether fileName already exists
    if (file.exists(fileName)) {
        cat("\nFile already exists at:", fileName, "\n")
        if (ovrWrite) cat("Will over-write with current data from", url, "\n")
        else stop("Exiting due to ovrWrite=FALSE and a duplicate fileName\n")
    }
    
    # Download the file 
    download.file(url, destfile=fileName, ...)
    
    # Show statistics on downloaded file
    file.info(fileName)
    
}


# Generic function to read an existing file
fileRead <- function(fileName, 
                     fnRead=readr::read_csv, 
                     ...
                     ) {
    
    # FUNCTION ARGUMENTS:
    # fileName: file location for reading
    # fnRead: function for reading fileName
    # ...: other arguments to be passed to fnRead
    
    # Read the file and return
    fnRead(fileName, ...)
    
}



# Updated to handle length-zero inputs
# Generic function to rename columns in a file using an input vector
colRenamer <- function(df, 
                       vecRename=c(), 
                       ...
                       ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble
    # vecRename: vector for renaming c('existing name'='new name'), can be any length from 0 to ncol(df)
    # ...: additional arguments to be passed to rename_with
    
    # Rename the columns as requested
    if(length(vecRename)>0) dplyr::rename_with(df, .fn=function(x) vecRename[x], .cols=names(vecRename), ...)
    else df
    
}



# Generic function to select columns in a file using an input vector
colSelector <- function(df, 
                        vecSelect=NULL, 
                        ...
                        ) {
    
    # FUNCTION ARGUMENTS:    
    # df: the data frame or tibble
    # vecSelect: vector for variables to keep c('keep1', "keep2", ...), NULL means keep all
    # ...: additional arguments (not currently used)
    
    # If vecSelect is NULL, keep all the columns
    if (is.null(vecSelect)) vecSelect <- names(df)
    
    # Keep the requested columns
    select(df, all_of(vecSelect))
    
}



# Generic function to mutate columns in a file using an input list
colMutater <- function(df, 
                       selfList=list(), 
                       fullList=list(),
                       ...
                       ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble
    # selfList: list for functions to apply to self, list('variable'=fn) will apply variable=fn(variable)
    #           processed in order, so more than one function can be applied to self
    # fullList: list for general functions to be applied, list('new variable'=expression(code))
    #           will create 'new variable' as eval(expression(code))
    #           for now, requires passing an expression
    # ...: additional arguments to be passed to across() inside mutate()
    
    # Apply the self-functions sequentially
    for (ctr in seq_along(selfList)) 
        df <- mutate(df, across(.cols=names(selfList)[ctr], .fns=selfList[[ctr]], ...))
    
    # Apply the full-mutates sequentially
    for (ctr in seq_along(fullList))
        df <- mutate(df, !!names(fullList)[ctr]:=eval(fullList[[ctr]]))
    
    # Return the updated file
    df
    
}



# Generic function to check for uniqueness of rows in a tibble
checkUniqueRows <- function(df, 
                            uniqueBy=NULL, 
                            severity="stop", 
                            noteUnique=TRUE,
                            returnDF=TRUE
                            ) {
    
    # FUNCTION ARGUMENTS
    # df: tibble or data frame
    # uniqueBy: combination of variables for checking uniqueness (NULL means use all)
    # severity: passed to assertive, can be c("stop", "warning", "message", "none")
    # noteUnique: boolean, should a note be generated showing that the uniqueness check passed?
    # returnDF: should the data frame be returned (if TRUE, will allow for downstream chaining)
    
    # Use all variables if uniqueBy is NULL
    if (is.null(uniqueBy)) uniqueBy <- names(df)
    
    # Check for uniqueness
    df %>%
        select_at(vars(all_of(uniqueBy))) %>%
        assertive.properties::assert_has_no_duplicates(severity=severity)
    
    # Report back on findings if requested
    if (isTRUE(noteUnique)) cat("\n*** File", "has been checked for uniqueness by:", uniqueBy, "\n\n")
    
    # Return the data frame if requested
    if (returnDF) return(df)
    
}



# Generic function for checking control totals
checkControl <- function(df, 
                         groupBy=c(),
                         useVars=NULL, 
                         fn=sum, 
                         printControls=TRUE,
                         pivotData=!isTRUE(printControls),
                         returnData=!isTRUE(printControls),
                         ...
                         ) {
    
    # FUNCTION ARGUMENTS
    # df: the data frame or tibble
    # groupBy: control totals by level (c() means overall total)
    # useVars: variables to get control totals (NULL means all numeric)
    # fn: function that will be applied to create control totals
    # printControls: boolean, should the control file be printed?
    # pivotData: boolean, should data be pivoted so to be unique by groupBy with columns name and newValue?
    # returnData: boolean, should the control total data be returned?
    # ...: additional arguments (most common will be na.rm=TRUE)
    
    # Get the columns to summarize (use all numeric non-grouping variables if NULL passed)
    if (is.null(useVars)) useVars <- setdiff(names(df)[sapply(df, is.numeric)], groupBy)
    
    # Get the control totals by group
    dfControl <- df %>%
        group_by_at(vars(all_of(groupBy))) %>%
        summarize(across(.cols=all_of(useVars), .fns=fn, ...), .groups="drop")
    
    # Pivot data if requested
    if (pivotData) dfControl <- dfControl %>% pivot_longer(all_of(useVars), values_to="newValue")
    
    # Print control totals if requested
    if (printControls) print(dfControl)
    
    # Return data file if requested
    if (returnData) return(dfControl)
    
}



# Generic function for filtering rows based on criteria
rowFilter <- function(df, 
                      lstFilter=list(), 
                      lstExclude=list(),
                      ...
                      ) {
    
    # FUNCTION ARGUMENTS
    # df: tibble or data frame
    # lstFilter: a list for filtering records, of form list("field"=c("allowed values"))
    # lstExclude: a list for filtering records, of form list("field"=c("disallowed values"))
    # ...: additional arguments (not currently used)
    
    # Run the filtering for each element of lstFilter
    for (colName in names(lstFilter)) {
        df <- df %>% filter(.data[[colName]] %in% lstFilter[[colName]])
    }
    
    # Run the filtering for each element of lstExclude
    for (colName in names(lstExclude)) {
        df <- df %>% filter(!(.data[[colName]] %in% lstExclude[[colName]]))
    }
    
    # Return the filtered data frame
    df
    
}



# Generic function for combining data from multiple rows to a single row
combineRows <- function(df, 
                        comboVar, 
                        uqVars=c(),
                        vecCombo=c(), 
                        fn=sum, 
                        ...
                        ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble
    # comboVar: character string representing variable for being combined
    # uqVars: other variables that the final data should be unique by 
    #         (e.g., date for a state-date file mapping states together)
    # vecCombo: vector of combinations to be made of form c('old value'='new value')
    # fn: function for combining elements (only handles is.numeric for now)
    # ...: other arguments passed to fn
    
    # Add comboVar to uqVars if it is not already included
    if (!(comboVar %in% uqVars)) uqVars <- c(uqVars, comboVar)
    
    # Split data file in to a portion that needs modifying and a portfio for standalone
    df <- df %>%
        mutate(dummyVar=ifelse(get(comboVar) %in% names(vecCombo), 1, 0))
    dfKeep <- df %>% filter(dummyVar==0) %>% select(-dummyVar)
    dfMod <- df %>% filter(dummyVar==1) %>% select(-dummyVar)
    
    # Convert elements as appropriate
    dfMod <- dfMod %>%
        mutate(!!comboVar:=vecCombo[get(comboVar)])
    
    # Apply summary function at unique level
    dfMod <- dfMod %>%
        group_by_at(vars(all_of(uqVars))) %>%
        summarize(across(where(is.numeric), .fns=fn, ...), .groups="drop")
    
    # Return the modified data frame with the new records at the bottom
    dfKeep %>%
        select(names(dfMod)) %>%
        bind_rows(dfMod)
    
}



# Generic function to create line plots based on parameters
helperLinePlot <- function(df, 
                           x, 
                           y, 
                           groupColor=NULL,
                           facetVar=NULL, 
                           facetRows=NULL, 
                           facetScales="fixed", 
                           xLab="",
                           yLab="Summed Value", 
                           titleLab=paste0("Control totals by ", x)
                           ) {
    
    # FUNCTION ARGUMENTS
    # df: the data frame or tibble
    # x: the x variable
    # y: the y variable
    # groupColor: a variable to be used for grouping and coloring
    # facetVar: the faceting variable, NULL means do not facet
    # facetRows: the number of rows to include in faceting (NULL means allow facet_wrap to select)
    # facetScales: the scale for the facetting, can be "fixed" or "free" or "free_y" or "free_x"
    # xLab: label for the x-axis
    # yLab: label for the y-axis
    # titleLab: label for the title
    
    # Code to create the main plot
    p1 <- df %>% 
        ggplot(aes_string(x=x, y=y)) + 
        geom_line(if(!is.null(groupColor)) aes_string(group=groupColor, color=groupColor)) + 
        labs(title=titleLab, x=xLab, y=yLab)
    # Add faceting if not NULL
    if (!is.null(facetVar)) p1 <- p1 + facet_wrap(~get(facetVar), nrow=facetRows, scales=facetScales)
    print(p1)
    
}



# Helper function to check for similarity of key elements
helperSimilarity <- function(newData, 
                             refData, 
                             label, 
                             countOnly=FALSE, 
                             logFile=NULL, 
                             logAppend=TRUE, 
                             returnData=isTRUE(countOnly)
                             ) {
    
    # FUNCTION ARGUMENTS:
    # newData: a new data file
    # refData: a reference data file
    # label: a label for the check being run
    # countOnly: boolean, should only a count of differences be reported in the main log file?
    # logFile: external file for writing out detailed differences (NULL means do not write to external file)
    # logAppend: boolean, if the external log file exists should it be appended? (FALSE means overwrite)
    # returnData: should the differences data be returned (default is yes for countOnly, no otherwise
    
    # Find difference in set 1 and set 2    
    d1 <- setdiff(refData, newData)
    d2 <- setdiff(newData, refData)
    
    # Write the differences (counts or actual values) to the main log file
    cat("\n\nChecking for similarity of:", label)
    cat("\nIn reference but not in current:", if(countOnly) length(d1) else d1)
    cat("\nIn current but not in reference:", if(countOnly) length(d2) else d2)
    
    # If a logFile is provided and only counts are in the main file, write to the log file
    if (countOnly & !is.null(logFile)) {
        cat("\nDetailed differences available in:", logFile)
        capture.output(cat("\nDetailed differences for: ", label, "\n", sep=""), 
                       cat("\nIn reference but not in current:\n", paste(d1, collapse="\n"), sep=""), 
                       cat("\nIn current but not in reference:\n", paste(d2, collapse="\n"), sep=""), 
                       file=logFile, 
                       append=logAppend
        )
    }
    
    # Return data if requested
    if (returnData) return(list(d1=d1, d2=d2))
    
}



# Generic function to check similarity of specified columns in two files
checkSimilarity <- function(df, 
                            ref, 
                            keyVars=list(), 
                            writeLog=NULL, 
                            ovrwriteLog=TRUE
                            ) {
    
    # FUNCTION ARGUMENTS:
    # df: the new data frame
    # ref: the reference data frame
    # keyVars: the key variables to be tested, passed as a named list
    #          list('varName'=list(label='label', countOnly=boolean, externalLog=boolean, convChar=boolean))
    # writeLog: an external log file to be written to (NULL means main log file only)
    # ovrwriteLog: boolean, should the external log be overwritten?
    
    # Check for consistency of variable names (always output to main log file)
    helperSimilarity(newData=names(df), refData=names(ref), label="column names")
    
    # Create storage container for similarities
    changeList <- vector("list", length(keyVars))
    names(changeList) <- names(keyVars)
    
    # Check for consistency of variables in keyVars
    for (ctr in seq_along(keyVars)) {
        vrbl <- names(keyVars)[ctr]
        d1 <- df %>% pull(vrbl) %>% unique() %>% sort()
        d2 <- ref %>% pull(vrbl) %>% unique() %>% sort()
        lstData <- keyVars[[ctr]]
        changeList[[ctr]] <- helperSimilarity(if(isTRUE(lstData$convChar)) as.character(d1) else d1, 
                                              if(isTRUE(lstData$convChar)) as.character(d2) else d2, 
                                              label=lstData$label, 
                                              countOnly=lstData$countOnly, 
                                              logFile=if(isFALSE(lstData$externalLog)) NULL else writeLog, 
                                              logAppend=!ovrwriteLog, 
                                              returnData=TRUE
        )
    }
    
    # Return the list file of differences
    cat("\n\n")
    changeList
    
}



plotSimilarity <- function(lst, 
                           plotItems=NULL, 
                           nameMap=c("d1"="old file only", "d2"="new file only")
                           ) {
    
    # FUNCTION ARGUMENTS:
    # lst: a named list that includes setdiff() results in two directions
    # plotItems: character vector of the named list items to process (NULL means all)
    # nameMap: map from the list sub-component names to a descriptive name for plotting
    
    # If plotItems is NULL, plot everything 
    if (is.null(plotItems)) plotItems <- names(lst)
    
    # Loop through and plot
    for (vrbl in plotItems) {
        p1 <- lapply(lst[[vrbl]], FUN=function(x) tibble::tibble(value=x)) %>%
            bind_rows(.id="src") %>%
            mutate(src=factor(unname(nameMap[src]), levels=unname(nameMap))) %>%
            arrange(value, src) %>%
            ggplot(aes(x=value, y=src)) + 
            geom_point(aes(color=src), size=4) + 
            labs(x=vrbl, y="Mismatch cause", title=paste0("Values for ", vrbl, " only in one file")) + 
            coord_flip() + 
            scale_color_discrete("Mismatch\ncause")
        print(p1)
    }
    
}



compareAggregate <- function(df, 
                             ref, 
                             grpVar, 
                             numVars, 
                             sameUniverse=NA,
                             plotData=FALSE, 
                             isLine=TRUE, 
                             returnDelta=FALSE
                             ) {
    
    # FUNCTION ARGUMENTS:
    # df: the latest data frame or tibble
    # ref: the reference data frame or tibble
    # grpVar: character vector of the level for aggregation
    # numVars: character vector of the numeric vectors to explore
    # sameUniverse: character vector of variables where the files should be required to have the same universe
    #               NA means this is not enforced, helps limit to same date range when checking state
    # plotData: boolean, should the data be plotted?
    # isLine: boolean, should the plot be drawn as a line graph (FALSE means point graph)
    # returnDelta: boolean, should the delta aggregate file be returned?
    
    # Get the matching universe if requested
    if (!is.na(sameUniverse)) {
        univData <- df %>% select_at(vars(all_of(sameUniverse))) %>%
            bind_rows(select_at(ref, vars(all_of(sameUniverse))), .id="src") %>%
            count(date, src) %>%
            pivot_wider(names_from="src", values_from="n") %>%
            filter(complete.cases(.)) %>%
            select_at(vars(all_of(sameUniverse)))
        # Filter df and ref such that they only include matches
        df <- univData %>% left_join(df, by=names(univData))
        ref <- univData %>% left_join(ref, by=names(univData))
    }
    
    # Create a tibble of the aggregated data from df and ref
    dfAgg <- df %>%
        checkControl(groupBy=grpVar, useVars=numVars, printControls=FALSE, na.rm=TRUE)
    refAgg <- ref %>%
        checkControl(groupBy=grpVar, useVars=numVars, printControls=FALSE, na.rm=TRUE) %>%
        colRenamer(vecRename=c("newValue"="refValue"))
    deltaAgg <- full_join(dfAgg, refAgg, by=c(grpVar, "name")) %>%
        tibble::as_tibble()
    
    # Create a plot if requested
    if (plotData) {
        p1 <- deltaAgg %>%
            pivot_longer(c(newValue, refValue), names_to="src") %>%
            filter(!is.na(value)) %>%
            ggplot() + 
            labs(x="", y="", title=paste0("Aggregated data by ", grpVar, " across new and reference file")) + 
            scale_color_discrete("Source")
        if (isTRUE(isLine)) {
            p1 <- p1 + 
                geom_line(aes_string(x=grpVar, y="value", group="src", color="src")) + 
                facet_wrap(~name, scales="free_y")
        } else {
            p1 <- p1 + 
                geom_point(aes(x=fct_reorder(get(grpVar), value), y=value, color=src)) + 
                coord_flip() +
                facet_wrap(~name, scales="free_x", nrow=1)
        }
        if (!is.na(sameUniverse)) {
            p1 <- p1 + labs(subtitle=paste0("Data filtered to same universe on: ", sameUniverse))
        }
        print(p1)
    }
    
    # Return the aggregate data if requested
    if(isTRUE(returnDelta)) return(deltaAgg)
    
}



# Flag differences by more than a specified tolerance
flagLargeDelta <- function(df, 
                           col1="newValue", 
                           col2="refValue", 
                           pctTol=0.01, 
                           absTol=5, 
                           sortBy=c("pctDelta", "absDelta"),
                           dropNA=TRUE,
                           printAll=FALSE
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame or tibble
    # col1: the name of the first column being compared
    # col2: the name of the second column being compared
    # pctTol: values will be flagged as 'high percent' if abs(col1-col2) > pctTol*mean(col1, col2)
    #         setting pctTol to 0 means only absTol is relevant
    # absTol: values will be flagged as 'high volume' if abs(col1-col2) > absTol
    #         setting absTol to 0 means only pctTol is relevant
    #         if absTol==0 and pctTol==0 then any difference in col1 and col2 will be flagged
    # sortBy: final output should be sorted by descending of these variables (NULL means no sorting)
    # dropNA: boolean, should cases where col1 or col2 is NA be excluded from reporting?
    # printAll: print every record (as.data.frame) rather than just the top 10 (tibble)
    
    # Create val1 and val2 from col1 and col2
    tol <- df %>%
        colRenamer(vecRename=purrr::set_names(c("val1", "val2"), c(col1, col2))) 
    
    # Exclude NA if requested
    if (isTRUE(dropNA)) tol <- tol %>% filter(!is.na(val1), !is.na(val2))
    
    # Create absDelta and pctDelta and filter accordingly
    tol <- tol %>%
        mutate(use1=ifelse(is.na(val1), 0, val1), 
               use2=ifelse(is.na(val2), 0, val2), 
               absDelta=abs(use1-use2), 
               pctDelta=2*absDelta/(use1+use2)
        ) %>%
        select(-use1, -use2) %>%
        filter(absDelta > absTol, pctDelta > pctTol) %>%
        colRenamer(vecRename=purrr::set_names(c(col1, col2), c("val1", "val2")))
    
    # Sort the columns if requested
    if (!is.null(sortBy)) {
        tol <- tol %>%
            arrange(across(.cols=all_of(sortBy), .fns=desc))
    }
    
    # Convert to data frame if requested to print all
    if (isTRUE(printAll)) tol <- as.data.frame(tol)
    
    # Describe the upcoming report
    cat("\n\n***Differences of at least ", absTol, " and at least ", round(100*pctTol, 3), "%\n\n", sep="")
    
    # Return the data
    tol
    
}



# Function to allow for printing to a log file
printLog <- function(x, 
                     txt="",
                     writeLog=NULL, 
                     appendLog=TRUE
                     ) {
    
    # FUNCTION ARGUMENTS
    # x: an object to be printed
    # txt: a descriptor to include for the data in the log file or otherwise
    # writeLog: the external file location for printing (NULL means use the main log stdout)
    # appendLog: for an external log, should the file be appended rather than overwritten?
    
    if (is.null(writeLog)) {
        cat("\n", txt, "\n", sep="")
        print(x)
    }
    else {
        cat("\nDetailed output available in log:", writeLog)
        capture.output(cat("\n\n", txt, "\n", sep=""), print(x), file=writeLog, append=appendLog)
    }
}



# Generic function to combine a list of files using a reduce function
combineFiles <- function(lst, 
                         fn=dplyr::full_join, 
                         byVars=NULL,
                         ...
                         ) {
    
    # lst: A list containing one or more files to be joined
    # fn: A function for joining files
    # byVars: character string "by variables", which must be consistent across files
    #         NULL means infer from data as is standard in dplyr merges
    # ...: other arguments to be passed to fn by way of reduce
    
    purrr::reduce(lst, .f=fn, by=byVars, ...)
    
}



# Helper function to glimpse to a logfile
glimpseLog <- function(df, txt="", logFile=NULL, append=TRUE) {
    if (is.null(logFile)) glimpseFile(df, txt=txt)
    else capture.output(glimpseFile(df, txt=txt), file=logFile, append=append)
}



# Function to join any number of frames
joinFrames <- function(..., fnJoin=dplyr::inner_join, keyJoin=NULL) {
    
    # FUNCtION ARGUMENTS:
    # ...: data frames to be joined using purrr::reduce OR a length-1 list of data frames
    # fnJoin: dplyr function for join (full_join, inner_join, left_join, right_join), applied in order to ...
    # keyJoin: by= variable passed to fnJoin (NULL means anything that matches, the dplyr default)
    
    # Allow for ... to be already passed as a list
    # Convert ... to a list provisionally, then extract only element 1 if it is already a list
    x <- list(...)
    if (length(x)==1) {
        if ("list" %in% class(x[[1]])) 
            x <- x[[1]]
    }
    
    # Join the files and return
    purrr::reduce(.x=x, .f=fnJoin, by=keyJoin)
    
}



# Function to convert a named vector to a tibble
vecToTibble <- function(x, 
                        colNameData="value", 
                        colNameName="name"
                        ) {
    
    # FUNCTION ARGUMENTS:
    # x: a vector (typically would be named, otherwise rownames are 1:length(x))
    # colNameData: the column name in the tibble for the vector data
    # colNameName: the column name in the tibble for the vector names
    
    as.data.frame(x) %>%
        rownames_to_column(var=colNameName) %>%
        tibble::as_tibble() %>%
        purrr::set_names(c(colNameName, colNameData))
    
}



# Function to convert a regular function to one that returns NA for all NA and na.rm=TRUE otherwise
specNA <- function(fn=sum) function(x, ...) if (any(!is.na(x))) fn(x, ..., na.rm=TRUE) else NA



# Function to create a weighted sum (similar to weighted.mean, but not normalizing or dividing by weights)
specSumProd <- function(x, w=1, na.rm=FALSE) {
    
    # FUNCTION ARGUMENTS:
    # x: a numeric vector x
    # w: a weighting vector w, which should be either of length(x) or of length 1
    # na.rm: boolean, should missing vectors in x (and their corresponding weight) be excluded?
    
    # Check that x has an appropriate length
    if (!(length(w) %in% c(1, length(x)))) stop("\nParameter w must be either length 1 or same length as x\n")
    
    # If there are any missing values in x, manage x and w appropriately
    if (any(is.na(x))) {
        if(!isTRUE(na.rm)) return(NA)  # if there are missing values and na.rm is not TRUE, result is NA
        else {
            if(length(w) > 1) w <- w[!is.na(x)]  # eliminate the values of w that correspond to NA in x
            x <- x[!is.na(x)]
        }
    }
    
    # Return the sum of the product of x and w (note that if x is of length 0, this will return 0)
    return(sum(x*w))
    
}



# Function to create a group aggregate
createGroupAgg <- function(df, 
                           aggTo=c(),
                           aggVars=NULL,
                           aggFunc=specNA(mean),
                           wtVar=NULL, 
                           prefix=NULL
                           ) {
    
    # FUNCtION ARGUMENTS:
    # df: a data frame or tibble containing data suitable for aggregation
    # aggTo: level data will be aggregated to in the final result (c() means overall level)
    #        passed as character vector, such as c("cluster", "date") for aggregating to cluster-date
    # aggVars: variables to be aggregated (NULL means aggregate all that are of type is.numeric)
    # aggFunc: a function that can be used for aggregation to the aggTo level
    # wtVar: the weighting variable for the aggregation function (NULL means equal weigthing by record)
    #        passed as character, so "pop" means that values should be weighted by pop
    # prefix: a character prefix to be prepended to column names in summarize(across(...))
    #         NULL means use default naming convention
    
    # If aggVars has been passed as NULL, apply to all variables of type is.numeric
    # Ensure aggVars does not include any grouping variables
    if (is.null(aggVars)) aggVars <- sapply(df, is.numeric) %>% which() %>% names()
    aggVars <- setdiff(aggVars, aggTo)
    
    # Group by aggTo, then apply aggFunc using do.call passing w=get(wtVar) when wtVar is not NULL
    dfGroup <- df %>%
        group_by(across(.cols=all_of(aggTo))) %>%
        summarize(across(.cols=all_of(aggVars), 
                         .fns=~do.call(aggFunc, 
                                       append(list(x=.x), if(!is.null(wtVar)) list(w=get(wtVar)) else NULL)
                         ),
                         .names=if(is.null(prefix)) NULL else paste0(prefix, "{.col}")
        ), 
        .groups="drop"
        )
    
    # Return dfGroup
    dfGroup
    
}



# Function to generate a new log file
genNewLog <- function(writeLog, ovrwriteLog=TRUE) {
    
    # FUNCTION ARGUMENTS:
    # writeLog: path to a writeable connection
    # ovrWriteLog: should any existing log file be overwritten?
    
    txt <- paste0("\n\n*** Writing log at: ", 
                  Sys.time() %>% lubridate::with_tz(tzone="UTC"), 
                  "Z ***\n\n"
    )
    capture.output(cat(txt), file=writeLog, append=isFALSE(ovrwriteLog))
    
}



# Generic function for processing a raw file (last updated 02-AUG-2021)
processRawFile <- function(df, 
                           vecRename=c(), 
                           vecSelect=NULL,
                           lstCombo=list(), 
                           lstFilter=list(), 
                           lstExclude=list()
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: the raw data frame or tibble
    # vecRename: vector for renaming c('existing name'='new name'), can be any length from 0 to ncol(df)
    # vecSelect: vector of columns to select (run after vecRename), NULL means select all columns
    # lstCombo: a nested list of combinations to be applied
    #           each element of the list should include comboVar, uqVars, vecCombo, and fn
    # lstFilter: a list for filtering records, of form list("field"=c("allowed values"))
    # lstExclude: a list for filtering records, of form list("field"=c("disallowed values"))
    
    # STEP 1: Rename and select variables (selection occurs AFTER renaming)
    dfProcess <- df %>%
        colRenamer(vecRename=vecRename) %>%
        colSelector(vecSelect=vecSelect)
    
    # STEP 2: Combine multiple records to a single record
    for (ctr in seq_along(lstCombo)) {
        dfProcess <- dfProcess %>%
            combineRows(comboVar=lstCombo[[ctr]]$comboVar, 
                        uqVars=lstCombo[[ctr]]$uqVars, 
                        vecCombo=lstCombo[[ctr]]$vecCombo, 
                        fn=lstCombo[[ctr]]$fn
                        )
    }
    
    # STEP 3: Filter records
    qcOrig <- dfProcess %>% 
        summarize(across(where(is.numeric), sum, na.rm=TRUE), n=n()) %>% 
        mutate(isType="before")
    dfProcess <- dfProcess %>% 
        rowFilter(lstFilter=lstFilter, lstExclude=lstExclude)
    
    # STEP 4: Report on differences
    cat("\nColumn sums before and after applying filtering rules:\n")
    dfProcess %>% 
        summarize(across(where(is.numeric), sum, na.rm=TRUE), n=n()) %>% 
        mutate(isType="after") %>%
        bind_rows(qcOrig) %>%
        arrange(desc(isType)) %>%
        bind_rows(mutate(summarize(., across(where(is.numeric), function(x) (max(x)-min(x))/max(x))), 
                         isType="pctchg"
                         )
                  ) %>%
        select(isType, everything()) %>%
        print()
    cat("\n")
    
    # Return the processed data file
    dfProcess
    
}



# Generic function to create per-capita metrics using an existing file and source of population data
# (last updated 02-AUG-2021)
createPerCapita <- function(lst, 
                            uqBy,
                            popData,
                            mapper,
                            asIsVars=c(),
                            lstSortBy=uqBy,
                            fnJoin=dplyr::full_join, 
                            popJoinBy="state",
                            popVar="pop",
                            k=7,
                            mult=1000000,
                            ...
                            ) {
    
    # FUNCTION ARGUMENTS:
    # lst: A list containing one or more files to be joined OR a data frame that is already joined
    # uqBy: character string that the input file is unique by (will be the join keys if a list is passed)
    # popData: file containing population data that can be joined to the processed lst
    # mapper: mapping file of c('current name'='per capita name') for mapping variables
    # asIsVars: variables to be kept, but without creating pm or pm7
    # lstSortBy: the sorting that should be used for creating rolling metrics
    # fnJoin: The function to be used for joining files
    # popJoinBy: character string for the variable(s) to be used in joining popData to lst
    # popVar: character string for the variable in popData that represents population
    # k: time perior for rolling aggregations
    # mult: the unit for the per-capita data (default 1 million means make metrics per million)
    # ...: other arguments to be passed to combineFiles()
    
    # Step 1: If a list has been passed, use a joining process to create a data frame
    if ("list" %in% class(lst)) lst <- combineFiles(lst, byVars=uqBy, fn=fnJoin, ...)
    
    # Step 2: Sort the data using sortBy
    df <- dplyr::arrange(lst, across(all_of(lstSortBy)))
    
    # Step 3: Check that all variables other than uqBy and asIsVars can be mapped using mapper
    keyVars <- setdiff(names(df), c(uqBy, asIsVars))
    if (any(isFALSE(keyVars %in% mapper))) stop("\nVariable is missing in per capita mapper file\n")
    
    # Step 4: Run the per capita mapping process
    df <- helperMakePerCapita(df, 
                              mapVars=mapper[keyVars], 
                              popData=popData, 
                              k=k, 
                              byVar=popJoinBy, 
                              sortVar=setdiff(lstSortBy, popJoinBy), 
                              popVar=popVar, 
                              mult=mult
                              )
    
    # Return the data frame
    df
    
}



# Function to add per capita and rolling to the base data frame
helperMakePerCapita <- function(df, 
                                mapVars,
                                popData,
                                k=7, 
                                byVar="state", 
                                sortVar="date",
                                popVar="pop", 
                                mult=1000000
                                ) {
    
    # FUNCTION ARGUMENTS:
    # df: the initial data frame for conversion
    # mapVars: named vector of variables to be converted 'original name'='converted name'
    # k: the rolling time period to use
    # byVar: grouping variable for df
    # sortVar: each element of byVar should be sorted by sortVar prior to rolling aggregations
    # popVar: column name in popData the represents population
    # mult: unit for 'per capita' variable (1,000,000 will make 'per million' metrics)
    
    # Create the variables for per capita
    for (origVar in names(mapVars)) {
        df <- df %>% 
            helperPerCapita(origVar=origVar, 
                            newName=mapVars[origVar], 
                            popData=popData, 
                            byVar=byVar, 
                            popVar=popVar, 
                            mult=mult
            )
    }
    
    # Group and arrange the data prior to creating rolling aggregates
    df <- df %>% 
        group_by(across(.cols=all_of(byVar))) %>% 
        arrange(across(.cols=all_of(sortVar)))
    
    # Create the rolling variables
    for (newVar in mapVars) {
        df <- df %>% 
            helperRollingAgg(origVar=newVar, newName=paste0(newVar, k), k=k)
    }
    
    # Return the updated data frame, ungrouped
    df %>%
        ungroup()
    
}



# Helper function to create per capita metrics
helperPerCapita <- function(df, 
                            origVar, 
                            newName,
                            byVar="state",
                            popVar="pop",
                            popData=stateData,
                            mult=1000000
                            ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame currently being processed
    # origVar: the variables to be converted to per capita
    # newName: the new per capita variable name
    # byVar: the variable that will be merged by
    # popVar: the name of the population variable in the popData file
    # popData: the file containing the population data
    # mult: the multiplier, so that the metric is "per mult people"
    
    # Create the per capita variable
    df %>%
        inner_join(select_at(popData, vars(all_of(c(byVar, popVar)))), by=byVar) %>%
        mutate(!!newName:=mult*get(origVar)/get(popVar)) %>%
        select(-all_of(popVar))
    
}



# Helper function to create rolling aggregates
helperRollingAgg <- function(df, 
                             origVar, 
                             newName,
                             func=zoo::rollmean,
                             k=7, 
                             fill=NA, 
                             ...
                             ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the data
    # origVar: the original data column name
    # newName: the new variable column name
    # func: the function to be applied (zoo::rollmean will be by far the most common)
    # k: the periodicity (k=7 is rolling weekly data)
    # fill: how to fill leading.trailing data to maintain the same vector lengths
    # ...: any other arguments to be passed to func
    
    # Create the appropriate variable
    df %>%
        mutate(!!newName:=func(get(origVar), k=k, fill=fill, ...))
    
}



# Function to create an elbow plot for various numbers of clusters in the data
helperElbow <- function(mtx, 
                        testCenters, 
                        iter.max, 
                        nstart, 
                        silhouette=FALSE
                        ) {
    
    # FUNCTION ARGUMENTS:
    # mtx: a numeric matrix, or an object that can be coerced to a numeric matrix (no character fields)
    # testCenters: integer vector for the centers to be tested
    # iter.max: parameter passed to kmeans
    # nstart: parameter passed to kmeans
    # silhouette: whether to calculate the silhouette score
    
    # Create an object for storing tot.withinss and silhouetteScore
    totWithin <- vector("numeric", length(testCenters))
    silhouetteScore <- vector("numeric", length(testCenters))
    
    # Create the distancing data (required for silhouette score)
    if (silhouette) distData <- dist(mtx)
    
    # Run k-means for every value in testCenters, and store $tot.withinss (and silhouetteScore, if requested)
    n <- 1
    for (k in testCenters) {
        km <- kmeans(mtx, centers=k, iter.max=iter.max, nstart=nstart)
        totWithin[n] <- km$tot.withinss
        if (silhouette & (k > 1)) silhouetteScore[n] <- mean(cluster::silhouette(km$cluster, distData)[, 3])
        n <- n + 1
    }
    
    # Create the elbow plot
    p1 <- tibble::tibble(n=testCenters, wss=totWithin) %>%
        ggplot(aes(x=n, y=wss)) + 
        geom_point() + 
        geom_line() + 
        geom_text(aes(y=wss + 0.05*max(totWithin), x=n+0.2, label=round(wss, 1))) + 
        labs(x="Number of segments", y="Total Within Sum-Squares", title="Elbow plot") + 
        ylim(c(0, NA))
    
    # Create the silhouette plot if requested
    if (silhouette) {
        p2 <- tibble::tibble(n=testCenters, ss=silhouetteScore) %>%
            ggplot(aes(x=n, y=ss)) + 
            geom_point() + 
            geom_line() + 
            geom_text(aes(y=ss + 0.05*max(silhouetteScore), x=n+0.2, label=round(ss, 1))) + 
            labs(x="Number of segments", y="Mean silhouette width", title="Silhouette plot") + 
            ylim(c(-1, NA))
        gridExtra::grid.arrange(p1, p2, nrow=1)
    } else {
        print(p1)
    }
    
}



# Function for zero-padding a character string
zeroPad <- function(x, width, side="left", pad="0", convChar=TRUE) {
    stringr::str_pad(if(convChar) as.character(x) else x, width=width, side=side, pad=pad)
}
zeroPad2 <- function(x, ...) zeroPad(x, width=2, ...)
zeroPad5 <- function(x, ...) zeroPad(x, width=5, ...)


# Create a stacked bar plot with a time dimension
tempStackPlot <- function(df, 
                          yVars, 
                          xVar="state", 
                          yLab=NULL, 
                          plotTitle=NULL, 
                          colorVector=c("lightblue", "grey", "orange", "black"), 
                          addSuffix="%", 
                          scaleName="Cohort", 
                          textBuffer=0.5, 
                          makeDotPlot=FALSE, 
                          yLims=NULL
                          ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame or tibble
    # yVars: named vector with c("variable"="name"), in the desired order from right-most to left-most
    # xVar: the x variable
    # yLab: the y-axis label for the plot
    # plotTitle: the title for the plot
    # colorVector: colors to use for filled bars (sequentially, can have more, but not less, than length(yVars))
    # addSuffix: value to be appended to all values in plots (e.g., 96 would show as 96% in the text label)
    # scaleName: the name to use for the legend
    # textBuffer: distance from bar to text
    # makeDotPlot: boolean, should a dot-plot be made rather than stacked bars?
    # yLims: the limits for the y-axis passed as a length-2 vector such as c(0, 100) or c(0, NA)
    
    colorVector <- colorVector[1:length(yVars)]
    names(colorVector) <- names(yVars)
    
    # Function for the legend
    fnLegendKey <- if(isTRUE(makeDotPlot)) scale_color_manual else scale_fill_manual
    
    p1 <- df %>% 
        select(all_of(xVar), all_of(names(yVars))) %>%
        pivot_longer(-c(all_of(xVar))) %>%
        ggplot(aes(x=fct_reorder(get(xVar[1]), value, max))) + 
        coord_flip() + 
        labs(x=NULL, y=yLab, title=plotTitle) +
        (if(isTRUE(makeDotPlot)) geom_point(aes(y=value, color=name)) 
         else geom_col(aes(y=value, fill=name), position="identity")
        ) +
        geom_text(aes(y=value+textBuffer, 
                      label=paste0(value, 
                                   addSuffix, 
                                   ifelse(name==names(yVars)[1], paste0(" (", get(xVar[1]), ")"), "")
                      )
        ), 
        hjust=0, 
        size=3
        ) + 
        fnLegendKey(scaleName, 
                    breaks=rev(names(yVars)), 
                    labels=rev(unname(yVars)),
                    values=colorVector
        ) +
        theme(legend.position="bottom")
    
    # Add the y-limits if appropriate
    if (!is.null(yLims)) p1 <- p1 + lims(y=yLims)
    
    p1
    
}


# Create correlation for assigned lag/lead and variables in a data frame
lagCorrCheck <- function(df, lagLead=0, varFix="dpm7", varMove="cpm7") {
    df %>%
        mutate(lagVar=if(lagLead >= 0) lag(get(varMove), lagLead) else lead(get(varMove), abs(lagLead))) %>%
        filter(!is.na(lagVar)) %>%
        summarize(correl=cor(lagVar, get(varFix))) %>%
        pull(correl)
}


# Updated function to find local extrema in a vector
findPeaks <- function(x,
                      width=1,
                      FUN=max,
                      # gt=if(identical(FUN, max)) 0 else NULL,
                      # lt=if(identical(FUN, min)) 0 else NULL,
                      fillVal=NA,
                      epsTol=1e-12,
                      ...
                      ) {
    
    # FUNCTION ARGUMENTS:
    # x: a numeric vector
    # width: the width of the window to use
    # FUN: the function to be used (max to find peaks, min to find valleys)
    # gt: to be defined, the value must be greater than gt (NULL means use any value)
    # lt: to be defined, the value must be less than lt (NULL means use any value)
    # fillVal: value to use as output if a window does not exist (too close to boundary)
    # epsTol: the epsilon value for considering two values to be the same
    # ...: any other arguments to be passed to zoo::rollapply()
    
    # If width is 1 or under, everything is a maximum or minimum
    if(width <= 1) return(rep(TRUE, length(x)))
    
    # Check for window width (if even number, look further left than right)
    lookLeft <- ceiling((width-1)/2)
    lookRight <- floor((width-1)/2)
    
    # Run the look to the left
    xLeft <- x-zoo::rollapply(lag(x, 1), width=lookLeft, FUN=FUN, fill=fillVal, align="right")
    
    # Run the look to the right, unless lookRight is 0
    if(lookRight==0) xRight <- rep(0, length(x))
    else xRight <- x-zoo::rollapply(lead(x, 1), width=lookRight, FUN=FUN, fill=fillVal, align="left")
    
    # Return the boolean vector (only enabled for MAX and MIN for now)
    if(identical(FUN, max)) { !is.na(xLeft) & !is.na(xRight) & (xLeft>epsTol) & (xRight>=(-epsTol)) }
    else if(identical(FUN, min)) { !is.na(xLeft) & !is.na(xRight) & (xLeft<(-epsTol)) & (xRight<=epsTol) }
    else rep(NA, length(x))
    
}


# Previous function to find local extrema in a vector
oldFindPeaks <- function(x, 
                      width=1, 
                      align="center", 
                      FUN=max, 
                      gt=if(identical(FUN, max)) 0 else NULL, 
                      lt=if(identical(FUN, min)) 0 else NULL, 
                      fillVal=if(identical(FUN, max)) gt else if(identical(FUN, min)) lt else NA, 
                      epsTol=1e-12,
                      returnBool=TRUE, 
                      ...
                      ) {
    
    # FUNCTION ARGUMENTS:
    # x: a numeric vector
    # width: the width of the window to use
    # align: whether the window should be "center", "left", or "right"
    # FUN: the function to be used (max to find peaks, min to find valleys)
    # gt: to be defined, the value must be greater than gt (NULL means use any value)
    # lt: to be defined, the value must be less than lt (NULL means use any value)
    # fillVal: value to use as output if a window does not exist (too close to boundary)
    # epsTol: the epsilon value for considering two values to be the same
    # returnBool: should the boolean be returned? TRUE means return TRUE/FALSE for peaks, FALSE means return vector
    # ...: any other arguments to be passed to zoo::rollapply()
    
    # Create the rolling data
    rolls <- zoo::rollapply(x, width=width, align=align, FUN=FUN, fill=fillVal, ...)
    
    # No post-processing applied unless returnBool is TRUE
    if(!isTRUE(returnBool)) return(rolls)
    
    # Post-processing managed for gt and lt
    if(!is.null(gt)) rolls <- ifelse(rolls<=gt, NA, rolls)
    if(!is.null(lt)) rolls <- ifelse(rolls>=lt, NA, rolls)
    
    # Return the boolean vector
    !is.na(rolls) & (abs(rolls-x) <= epsTol)
    
}


# Impute NA values for a vector based on another vector
testImputeNA <- function(x, y=NULL, naValues=c()) {
    
    # FUNCTION ARGUMENTS
    # x: a numeric vector that may contain NA, or values in naValues that should be treated as NA
    # y: a numeric vector where percent changes in y should drive percent changes in x if x is NA
    #    NULL means just use zoo::na.locf(x, na.rm=TRUE)
    # naValues: any special values in x or y that should be treated as NA
    
    # Convert x to modified vector using naValues
    xMod <- ifelse(is.na(x), NA, ifelse(x %in% all_of(naValues), NA, x))
    
    # Create y as a vector of ones if NULL, otherwise convert to modified vector using naValues
    if(is.null(y)) y <- rep(1, length(x))
    yMod <- ifelse(is.na(y), NA, ifelse(y %in% all_of(naValues), NA, y))
    
    # Convert vector xMod using na.locf (need to keep NA values)
    xConv <- zoo::na.locf(xMod, na.rm=FALSE)
    
    # For any values where X was NA, scale up or down based on change in y
    # Works OK for a single NA, but not perfectly for repeating NA
    yMult <- yMod/lag(yMod)
    xMult <- yMult * lag(xConv)
    xConv[is.na(xMod)] <- xMult[is.na(xMod)]
    
    # Return the vector
    xConv
    
}


# Function to pivot the data file longer
pivotData <- function(df, 
                      pivotKeys, 
                      nameVar="name", 
                      valVar="value",
                      toLonger=TRUE, 
                      ...
                      ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame
    # pivotKeys: the keys (everything but cols for pivot_longer, id_cols for pivot_wider)
    # nameVar: variable name for names_to or names_from
    # valVar: variable name for values_to or values_from
    # toLonger: boolean, should pivot_longer() be used rather than pivot_wider()?
    # ...: other arguments to be passed to pivot_*()
    
    if (isTRUE(toLonger)) pivot_longer(df, -all_of(pivotKeys), names_to=nameVar, values_to=valVar, ...)
    else pivot_wider(df, all_of(pivotKeys), names_from=all_of(nameVar), values_from=all_of(valVar), ...)
    
}


# Convert NA data to 0
zeroNA <- function(x) ifelse(is.na(x), 0, x)


# Find change from maximum value
findDeltaFromMax <- function(df, groupBy=c(), timeVar="date", numVars=NULL) {
    
    # FUNCTION ARGUMENTS:
    # df: a data frame
    # groupBy: levels to which the final data should be aggregated
    # timeVar: time variable to which data should be aggregated
    # numVars: numeric variables to be summarized (NULL means all numeric variables)
    
    # Find numVars if not provided
    if(is.null(numVars)) numVars <- df %>% select(where(is.numeric)) %>% names %>% setdiff(groupBy)
    
    df %>%
        group_by_at(all_of(c(groupBy, timeVar))) %>%
        summarize(across(all_of(numVars), .fns=function(x) sum(x, na.rm=TRUE)), .groups="drop") %>%
        pivot_longer(all_of(numVars)) %>%
        arrange(across(all_of(c(groupBy, "name", timeVar)))) %>%
        group_by_at(all_of(c(groupBy, "name"))) %>%
        mutate(ratMax=value/max(value, na.rm=TRUE), 
               cumMax=ifelse(cummax(value)==0, 1, value/cummax(value)), 
               delMax=ifelse(cummax(value)==0, 1, (value-cummax(value))/max(value, na.rm=TRUE))
        ) %>%
        ungroup()
    
}


# Function for deleting files and checking memory impact
cleanMem <- function(objs=c(), runGC=TRUE, delObjs=FALSE) {
    
    # FUNCTION ARGUMENTS:
    # objs: character vector of objects to be removed
    # runGC: boolean, should gc() be run before and after deletion?
    # delObjs: boolean, should the objects be deleted?
    
    # Run gc() if requested
    if(isTRUE(runGC)) {
        cat("\nMemory usage prior to deleting files:\n")
        print(gc())
    }
    
    # Remove objects that exist, if requested
    if(isTRUE(delObjs)) {
        objs <- objs[sapply(objs, FUN=function(x) exists(x)) %>% unname]
        rm(list=objs, inherits=TRUE)
    }
    
    # Run gc() if requested
    if(isTRUE(runGC)) {
        cat("\nMemory usage after deleting files:\n")
        print(gc())
    }
    
}


# Helper function for reading a partial CSV file
partialCSVRead <- function(loc, firstRow=1L, lastRow=+Inf, col_names=TRUE, ...) {
    
    # FUNCTION arguments
    # loc: file location
    # firstRow: first row that is relevant to the partial file read (whether header line or data line)
    # last Row: last row that is relevant to the partial file read (+Inf means read until last line of file)
    # col_names: the col_names parameter passed to readr::read_csv
    #            TRUE means header=TRUE (get column names from file, read data starting on next line)
    #            FALSE means header=FALSE (auto-generate column names, read data starting on first line)
    #            character vector means use these as column names (read data starting on first line)
    # ...: additional arguments passed to read_csv
    
    # Read the file and return
    # skip: rows to be skipped are all those prior to firstRow
    # n_max: maximum rows read are lastRow-firstRow, with an additional data row when col_names is not TRUE
    readr::read_csv(loc, 
                    col_names=col_names,
                    skip=firstRow-1, 
                    n_max=lastRow-firstRow+ifelse(isTRUE(col_names), 0, 1), 
                    ...
    )
    
}


# Get the break points for gaps in a vector (e.g., 0, 3, 5:8, 20 has break points 0, 3, 5, 20 and 0, 3, 8, 30)
vecGaps <- function(x, addElements=c(), sortUnique=TRUE) {
    
    if(length(addElements)>0) x <- c(addElements, x)
    if(isTRUE(sortUnique)) x <- unique(sort(x))
    list("starts"=c(x[is.na(lag(x)) | x-lag(x)>1], +Inf), 
         "ends"=x[is.na(lead(x)) | lead(x)-x>1]
    )
    
}


# Find the break points in a single file
flatFileGaps <- function(loc) {
    
    which(stringr::str_length(readLines(loc))==0) %>% vecGaps(addElements=0)
    
}


# Read all relevant data as CSV with header
readMultiCSV <- function(loc, col_names=TRUE, ...) {
    
    gaps <- flatFileGaps(loc)
    
    lapply(seq_along(gaps$ends), 
           FUN=function(x) partialCSVRead(loc, 
                                          firstRow=gaps$ends[x]+1, 
                                          lastRow=gaps$starts[x+1]-1, 
                                          col_names=col_names, 
                                          ...
           )
    )
    
}


# Plot the means by cluster and variable for a k-means object
plotClusterMeans <- function(km, nrow=NULL, ncol=NULL, scales="fixed") {
    
    # FUNCTION ARGUMENTS
    # km: object returned by stats::kmeans(...)
    # nrow: number of rows for faceting (NULL means default)
    # ncol: number of columns for faceting (NULL means default)
    # scales: passed to facet_wrap as scales=scales
    
    # Assess clustering by dimension
    p1 <- km$centers %>%
        tibble::as_tibble() %>%
        mutate(cluster=row_number()) %>%
        pivot_longer(cols=-c(cluster)) %>%
        ggplot(aes(x=fct_reorder(name, 
                                 value, 
                                 .fun=function(a) ifelse(length(a)==2, a[2]-a[1], diff(range(a)))
                                 ), 
                   y=value
                   )
               ) + 
        geom_point(aes(color=factor(cluster))) + 
        scale_color_discrete("Cluster") + 
        facet_wrap(~factor(cluster), nrow=nrow, ncol=ncol, scales=scales) +
        labs(title=paste0("Cluster means (kmeans, centers=", nrow(km$centers), ")"), 
             x="Metric", 
             y="Cluster mean"
        ) + 
        geom_hline(yintercept=median(km$centers), lty=2) +
        coord_flip()
    print(p1)
    
}


# Plot percentage by cluster
plotClusterPct <- function(df, km, keyVars, nRowFacet=1, printPlot=TRUE) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame initially passed to stats::kmeans(...)
    # km: object returned by stats::kmeans(...)
    # keyVars: character vector of length 1 (y-only, x will be cl) or length 2 (x, y, cl will facet)
    # nRowFacet: number of rows for facetting (only relevant if length(keyVars) is 2)
    # printPlot: boolean, should plot be printed? (if not true, plot will be returned)
    
    # Check length of keyVars
    if(!(length(keyVars) %in% c(1, 2))) stop("\nArgument keyVars must be length-1 or length-2\n")
    
    p1 <- df %>%
        mutate(cl=factor(km$cluster)) %>%
        group_by(across(c(all_of(keyVars), "cl"))) %>%
        summarize(n=n(), .groups="drop") %>%
        group_by(across(all_of(keyVars))) %>%
        mutate(pct=n/sum(n)) %>%
        ungroup() %>%
        ggplot() + 
        scale_fill_continuous(low="white", high="green") + 
        labs(title=paste0("Percentage by cluster (kmeans with ", nrow(km$centers), " centers)"), 
             x=ifelse(length(keyVars)==1, "Cluster", keyVars[1]), 
             y=ifelse(length(keyVars)==1, keyVars[1], keyVars[2])
             )
    if(length(keyVars)==1) p1 <- p1 + geom_tile(aes(fill=pct, x=cl, y=get(keyVars[1])))
    if(length(keyVars)==2) {
        p1 <- p1 + 
            geom_tile(aes(fill=pct, x=get(keyVars[1]), y=get(keyVars[2]))) + 
            facet_wrap(~cl, nrow=nRowFacet)
    }
    
    if(isTRUE(printPlot)) print(p1)
    else return(p1)
    
}


# Run k-means (or use passed k-means object) and plot centers and percentages of observations
runKMeans <- function(df, 
                      km=NULL,
                      vars=NULL, 
                      centers=2, 
                      nStart=1L, 
                      iter.max=10L, 
                      seed=NULL, 
                      plotMeans=FALSE,
                      nrowMeans=NULL,
                      plotPct=NULL, 
                      nrowPct=1, 
                      returnKM=is.null(km)
                      ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame for clustering
    # km: k-means object (will shut off k-means processing and run as plot-only)
    # vars: variables to be used for clustering (NULL means everything in df)
    # centers: number of centers
    # nStart: passed to kmeans
    # iter.max: passed to kmeans
    # seed: seed to be set (if NULL, no seed is set)
    # plotMeans: boolean, plot variable means by cluster?
    # nrowMeans: argument passed as nrow for faceting rows in plotClusterMeans() - NULL is default ggplot2
    # plotPct: list of character vectors to be passed sequentially as keyVars to plotClusterPct()
    #          NULL means do not run
    #          pctByCluster=list(c("var1"), c("var2", "var3")) will run plotting twice
    # nrowPct: argument for faceting number of rows in plotClusterPct()
    # returnKM: boolean, should the k-means object be returned?
    
    # Set seed if requested
    if(!is.null(seed)) set.seed(seed)
    
    # Get the variable names if passed as NULL
    if(is.null(vars)) vars <- names(df)
    
    # Run the k-means process if the object has not been passed
    if(is.null(km)) {
        km <- df %>%
            select(all_of(vars)) %>% 
            kmeans(centers=centers, iter.max=iter.max, nstart=nStart)
    }
    
    # Assess clustering by dimension if requested
    if(isTRUE(plotMeans)) plotClusterMeans(km, nrow=nrowMeans)
    if(!is.null((plotPct))) 
        for(ctr in 1:length(plotPct)) 
            plotClusterPct(df=df, km=km, keyVars=plotPct[[ctr]], nRowFacet=nrowPct)
    
    # Return the k-means object
    if(isTRUE(returnKM)) return(km)
    
}


# Assign points to closest center of a passed k-means object
assignKMeans <- function(km, df, returnAllDistanceData=FALSE) {
    
    # FUNCTION ARGUMENTS:
    # km: a k-means object
    # df: data frame or tibble
    # returnAllDistanceData: boolean, should the distance data and clusters be returned?
    #                        TRUE returns a data frame with distances as V1, V2, ..., and cluster as cl
    #                        FALSE returns a vector of cluster assignments as integers
    
    # Select columns from df to match km
    df <- df %>% select(all_of(colnames(km$centers)))
    if(!all.equal(names(df), colnames(km$centers))) stop("\nName mismatch in clustering and frame\n")
    
    # Create the distances and find clusters
    distClust <- sapply(seq_len(nrow(km$centers)), 
                        FUN=function(x) sqrt(rowSums(sweep(as.matrix(df), 
                                                           2, 
                                                           t(as.matrix(km$centers[x,,drop=FALSE]))
                        )**2
                        )
                        )
    ) %>% 
        as.data.frame() %>% 
        tibble::as_tibble() %>% 
        mutate(cl=apply(., 1, which.min))
    
    # Return the proper file
    if(isTRUE(returnAllDistanceData)) return(distClust)
    else return(distClust$cl)
    
}


# Round every number in a vector, using automated rules if new rndTo parameter is passed
autoRound <- function(x, rndTo=-1L, rndBucketsAuto=100, nSig=NULL) {
    
    # FUNCTION ARGUMENTS
    # x: vector to be rounded
    # rndTo: every number in x should be rounded to the nearest rndTo
    #        NULL means no rounding
    #        -1L means make an estimate based on data (default)
    # rndBucketsAuto: integer, if rndTo is -1L, about how many buckets are desired for predictions?
    # nSig: number of significant digits for automatically calculated rounding parameter
    #       (NULL means calculate exactly)
    
    # If rndTo is passed as NULL, return x as-is
    if(is.null(rndTo)) return(x)
    
    # If rndTo is passed as -1L, make an estimate for rndTo
    if(isTRUE(all.equal(-1L, rndTo))) {
        # Get the number of unique values in x
        nUq <- length(unique(x))
        # If the number of unique values is no more than 150% of rndToBucketsAuto, return as-is
        if(nUq <= 1.5*rndBucketsAuto) return(x)
        # Otherwise, calculate a value for rndTo
        rndTo <- diff(range(x)) / rndBucketsAuto
        # Truncate to requested number of significant digits
        if(!is.null(nSig)) rndTo <- signif(rndTo, digits=nSig)
    }
    
    # Return the rounded vector if it was not already returned
    return(round(x/rndTo)*rndTo)
    
}


# Reorder the output of a correlation matrix for better heatmap plotting (Function copied from STHDA)
reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
}


# Create a correlation heatmap, reordering variables as requested
makeHeatMap <- function(df, 
                        vecSelect=NULL, 
                        groupSimilar=TRUE, 
                        upperTriOnly=TRUE, 
                        plotMap=TRUE, 
                        returnData=FALSE
                        ) {
    
    # FUNCTION ARGUMENTS:    
    # df: the data frame or tibble
    # vecSelect: vector for variables to keep c('keep1', "keep2", ...), NULL means keep all
    # groupSimilar: boolean, should similar (highly correlated) variables be placed nearby each other?
    # upperTriOnly: boolean, should only the upper triangle be kept?
    # plotMap: boolean, should the heatmap be plotted?
    # returnData: boolean, should the correlation data driving the heatmap be returned
    
    # Create correlations
    df <- df %>%
        colSelector(vecSelect=vecSelect) %>%
        cor()
    
    # Reorder if requested
    if(isTRUE(groupSimilar)) df <- df %>% reorder_cormat()
    
    # Use only the upper triangle if requested
    if(isTRUE(upperTriOnly)) df[upper.tri(df)] <- NA
    
    # Convert to tidy tibble
    df <- df %>%
        reshape2::melt(na.rm=TRUE) %>%
        tibble::as_tibble()
    
    # Plot map if requested
    if(isTRUE(plotMap)) {
        p1 <- df %>%
            ggplot(aes(x=Var2, y=Var1)) + 
            geom_tile(aes(fill=value)) +
            scale_fill_gradient2(NULL, low="blue", high="red", mid="white", midpoint=0, limit=c(-1, 1)) + 
            labs(x=NULL, y=NULL, title="Pearson correlation of key variables") + 
            theme(axis.text.x=element_text(angle = 90, vjust = 1, hjust = 1))
        print(p1)
    }
    
    # Return data if requested
    if(isTRUE(returnData)) return(df)
    
}


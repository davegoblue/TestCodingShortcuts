# genericGetASOSData() downloads METAR data from a specified station 
# with a specified start and end date to a specified file location
# Defaults are set as per Iowa State's html code when requesting a CSV.    
genericGetASOSData <- function(fileLoc, 
                               stationID,
                               startDate, 
                               endDate,
                               downloadMethod="curl", 
                               baseURL="https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?", 
                               dataFields="all", 
                               dataTZ="Etc%2FUTC", 
                               dataFormat="onlycomma", 
                               dataLatLon="no", 
                               dataMissing="M", 
                               dataTrace="T", 
                               dataDirect="no", 
                               dataType=2
                               ) {
    
    # Get the year, day, and hour of the key dates
    y1 <- lubridate::year(startDate)
    m1 <- lubridate::month(startDate)
    d1 <- lubridate::day(startDate)
    
    y2 <- lubridate::year(endDate)
    m2 <- lubridate::month(endDate)
    d2 <- lubridate::day(endDate)
    
    # Mimic the string shown below
    # https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=LAS
    # &data=all&year1=2015&month1=12&day1=31&year2=2017&month2=1&day2=2&tz=Etc%2FUTC
    # &format=onlycomma&latlon=no&missing=M&trace=T&direct=no&report_type=2
    
    useURL <- paste0(baseURL, "station=", stationID)  # add the desired station
    useURL <- paste0(useURL, "&data=", dataFields)  # default is "all
    useURL <- paste0(useURL, "&year1=", y1, "&month1=", m1, "&day1=", d1)  # Specify start ymd
    useURL <- paste0(useURL, "&year2=", y2, "&month2=", m2, "&day2=", d2)  # Specify end ymd
    useURL <- paste0(useURL, "&tz=", dataTZ)  # time zone (default UTC)
    useURL <- paste0(useURL, "&format=", dataFormat)  # file format (default CSV)
    useURL <- paste0(useURL, "&latlon=", dataLatLon)  # Whether to include lat-lon (default no)
    useURL <- paste0(useURL, "&missing=", dataMissing)  # How to handle missing data (default is 'M')
    useURL <- paste0(useURL, "&trace=", dataTrace)  # How to handle trace data (default is 'T')
    useURL <- paste0(useURL, "&direct=", dataDirect)  # Whether to directly get the data (default is 'no')
    useURL <- paste0(useURL, "&report_type=", dataType)  # Whether to get just METAR (2, default)
    
    # Download the file
    cat("\nDownloading from:", useURL, "\nDownloading to:", fileLoc, "\n")
    download.file(useURL, destfile=fileLoc, method=downloadMethod)
    
    return(TRUE)
}


# getASOSStationTime() automates parameter management for calling genericGetASOSData(), 
# with specific focus on converting a user's desired station and time to a sensible filename, 
# and converting target year to a sensible start and end time 
# (METAR are in Zulu/UTC time zone, so it is helpful to start early and end late to have 
# full data even if later converting to a local time zone)
getASOSStationTime <- function(stationID, 
                               startDate=NULL, 
                               endDate=NULL, 
                               analysisYears=NULL, 
                               fileLoc=NULL,
                               ovrWrite=FALSE,
                               ...) {
    
    # Get the relevant time period for the data
    if (is.null(analysisYears) & (is.null(startDate) | is.null(endDate))) {
        stop("Must provide either analysisYears or both of startDate and endDate")
    }
    if (!is.null(startDate) & !is.null(endDate) & !is.null(analysisYears)) {
        stop("Should specify EITHER both of startDate and endDate OR analysisYears BUT NOT both")
    }
    if (is.null(startDate)) {
        startDate <- ISOdate(min(analysisYears)-1, 12, 31, hour=0)
        endDate <- ISOdate(max(analysisYears)+1, 1, 2, hour=0)
    }
    
    # Create the file name
    if (!is.null(analysisYears)) {
        if (length(analysisYears) == 1) { timeDesc <- analysisYears }
        else { timeDesc <- paste0(min(analysisYears), "-", max(analysisYears)) }
    } else {
        timeDesc <- paste0(lubridate::year(startDate), 
                           stringr::str_pad(lubridate::month(startDate), 2, pad="0"),
                           stringr::str_pad(lubridate::day(startDate), 2, pad="0"), 
                           "-", 
                           lubridate::year(endDate), 
                           stringr::str_pad(lubridate::month(endDate), 2, pad="0"), 
                           stringr::str_pad(lubridate::day(endDate), 2, pad="0")
        )
    }
    
    if (is.null(fileLoc)) {
        fileLoc <- paste0("./RInputFiles/", 
                          "metar_k", 
                          stringr::str_to_lower(stationID), 
                          "_", 
                          timeDesc, 
                          ".txt"
        )
    }
    
    cat("\nData for station", stationID, "from", as.character(startDate), "to", 
        as.character(endDate), "will download to", fileLoc, "\n"
    )
    
    if (file.exists(fileLoc) & !ovrWrite) {
        stop("File already exists, aborting")
    }
    
    genericGetASOSData(fileLoc=fileLoc, stationID=stationID, startDate=startDate, endDate=endDate, ...)
    
}


# findHourlyReportingTimes() - get the hourly reporting time for use in the target METAR string
findHourlyReportingTimes <- function(metFile=NULL, 
                                     metStation=NULL, 
                                     metTime=NULL, 
                                     metPath="./RInputFiles/metar_k", 
                                     metSep="_", 
                                     metExt=".txt",
                                     stationToLower=TRUE, 
                                     returnAll=FALSE,
                                     naValues=c("", "NA", "M"), 
                                     colTypes=NULL, 
                                     threshError=0.75
                                     ) {
    
    # Find the file name if it has not been passed
    if (is.null(metFile)) {
        if (is.null(metStation) | is.null(metPath) | is.null(metSep) | is.null(metTime) | is.null(metExt)) {
            stop("If metFile is not passed, all other parameters must be passed so filename can be created")
        }
        if (stationToLower) { metStation <- stringr::str_to_lower(metStation) }
        metFile <- paste0(metPath, metStation, metSep, metTime, metExt)
    }
    
    # Find the most common Zulu time (this will be the METAR)
    zTimes <- readr::read_csv(metFile, na=naValues, col_types=colTypes) %>%
        pull(metar) %>%
        stringr::str_match(pattern="\\d{2}Z") %>%
        as.vector() %>%
        table() %>%
        sort(decreasing=TRUE)
    
    # Print the most common 10 times    
    cat("\nMost common 10 times for file:", metFile, "\n")
    print(zTimes[1:10])
    
    # Print the estimated Zulu time
    cat("\nThe most common Zulu time for", metFile, "is", names(zTimes)[1], "\nFrequency is",
        round(100*zTimes[1]/sum(zTimes), 1), "% (", zTimes[1], "of", sum(zTimes), ")\n"
    )
    
    # STOP if the file has less than threshError observations at its main point
    if (zTimes[1] / sum(zTimes) < threshError) {
        cat("\nThe following routines depend on a single, hourly METAR\n")
        cat("This file may not have that feature; investigate and fix\n")
        stop(paste0("Most frequent time occurs less than threshError: ", threshError))
    }
    
    # Return the fill vector if requested (likely only if a problem is detected), otherwise return the time
    if (returnAll) {
        return(zTimes)
    } else {
        return(names(zTimes)[1])
    }
    
}


# readMETAR() - extract only hourly observations, check for expected times and record uniqueness
readMETAR <- function(fileName, 
                      timeZ,
                      expMin, 
                      expDays,
                      colTypes=NULL,
                      naValues=c("", "NA", "M"), 
                      printSTR=FALSE,
                      errUnexpected=TRUE,
                      errNonUnique=TRUE, 
                      logFile=""
                      ) {
    
    # Read METAR data
    initRead <- readr::read_csv(fileName, na=naValues, col_types=colTypes)
    
    # Provide descriptions of the METAR read (str only if flag is set)
    if (printSTR) { 
        print(str(initRead, give.attr=FALSE))
        print(dim(initRead))
    }
    
    
    # Filter to only data that ends with times ending in timeZ
    filterRead <- initRead %>%
        filter(str_detect(metar, timeZ))
    if (printSTR) { print(dim(filterRead)) }
    
    # Check that the dates and times included are as expected
    expDate <- expMin + lubridate::hours(0:(24*expDays - 1))
    
    # Observations expected but not recorded
    expNotRecorded <- as.POSIXct(setdiff(expDate, filterRead$valid), origin="1970-01-01", tz="UTC")
    
    # Print summary to the main log file
    cat("\n*** OBSERVATIONS EXPECTED BUT NOT RECORDED:", length(expNotRecorded), "***\n")
    
    # Cat details to the provided logFile (if logFile="", these go the main log file)
    cat("\n*** OBSERVATIONS EXPECTED BUT NOT RECORDED: ***\n", 
        paste0("\n", paste0(format(expNotRecorded, "%Y-%m-%d %H:%M:%S", usetz=TRUE), collapse="\n")),
        file=logFile, 
        append=TRUE
    )
    
    
    # Observations recorded but not expected
    unexpected <- as.POSIXct(setdiff(filterRead$valid, expDate), origin="1970-01-01", tz="UTC")
    
    # Summary to the main log filr
    cat("\n\n*** OBSERVATIONS RECORDED BUT NOT EXPECTED:", length(unexpected), "***\n")
    
    # Cat details to the provided logFile (if logFile="", these go to the main log file)
    if (length(unexpected) > 0) {
        cat("\n*** OBSERVATIONS RECORDED BUT NOT EXPECTED: ***\n", 
            paste0(format(unexpected, "%Y-%m-%d %H:%M:%S", usetz=TRUE), collapse="\n"), 
            file=logFile, 
            append=TRUE
        )
    }
    if (errUnexpected) {
        stopifnot(length(unexpected)==0)
    }
    
    # Confirmation of uniqueness
    allUnique <- length(unique(filterRead$valid)) == length(filterRead$valid)
    
    # Send these to the main log file
    cat("\n*** Are the extracted records unique? ***\n", allUnique, "\n\n")
    if (errNonUnique) {
        stopifnot(allUnique)
    }
    
    # Return the dataset as a tibble
    tibble::as_tibble(filterRead)
}


# initialParseMETAR() - parse the raw METAR data based on an expected regex pattern
initialParseMETAR <- function(met, 
                              val, 
                              labs, 
                              showParseSummary=FALSE, 
                              glimpseFinal=FALSE, 
                              logFile=""
                              ) {
    
    # Pull the METAR data
    metAll <- met %>%
        pull(metar)
    
    # Find the number of matching elements
    matchSummary <- str_detect(metAll, pattern=val) %>% 
        table()
    
    # Print this to the main log file
    cat("\n*** Tentative Summary of Element Parsing *** \n")
    print(matchSummary)
    
    # The strings that do not match have errors in the raw data (typically, missing wind speed)
    mismatchData <- metAll[!str_detect(metAll, pattern=val)]
    
    # Print the to the logFile (if logFile="", then these print to the main log file)
    cat("\n*** Data Not Matched:", length(mismatchData),"*** \n",
        paste0("\n", paste0(mismatchData, collapse="\n"), "\n"), 
        file=logFile, 
        append=TRUE
    )
    
    
    # A matrix of string matches can be obtained
    mtxParse <- str_match(metAll, pattern=val)
    if (showParseSummary) {
        cat("\n*** Parsing matrix summary *** \n")
        print(dim(mtxParse))
        print(head(mtxParse))
    }
    
    # Create a data frame
    dfParse <- data.frame(mtxParse, stringsAsFactors=FALSE) %>%
        mutate(dtime=met$valid, origMETAR=met$metar)
    names(dfParse) <- c(labs, "dtime", "origMETAR")
    dfParse <- tibble::as_tibble(dfParse)
    if (glimpseFinal) {
        cat("\n*** Summary of the parsed data *** \n")
        glimpse(dfParse)
    }
    
    dfParse
}


# convertMETAR() - converts characters from the parsed METAR file to meaningful numbers for analysis
# Function is hard-coded to work on metrics passed as default; potentially generalize later
convertMETAR <- function(met, 
                         seed=NULL, 
                         showInvestigate=FALSE, 
                         showNACounts=TRUE
                         ) {
    
    # Convert to numeric where appropriate
    dfParse <- met %>%
        mutate(WindSpeed = as.integer(WindSpeed), 
               WindGust = as.numeric(WindGust), 
               Visibility = as.numeric(str_replace(Visibility, "SM", "")),
               TempC = as.integer(str_replace(TempC, "M", "-")), 
               DewC = as.integer(str_replace(DewC, "M", "-")), 
               Altimeter = as.numeric(str_replace(Altimeter, "A", "")) / 100, 
               SLP = as.integer(str_replace(SLP, "SLP", "")), 
               TempF = 32 + 1.8 * as.integer(str_replace(str_sub(FahrC, 2, 5), pattern="^1", "-"))/10, 
               DewF = 32 + 1.8 * as.integer(str_replace(str_sub(FahrC, 6, 9), pattern="^1", "-"))/10
        )
    
    # Investigate the data (print to main log File if requested)
    if (showInvestigate) {
        
        cat("\n *** Parsed data structure, head, tail, and random sample *** \n")
        str(dfParse)
        print(head(dfParse))
        print(tail(dfParse))
        
        if (!is.null(seed)) { set.seed(seed) }
        dfParse %>% 
            sample_n(20) %>%
            print()
    }
    
    if (showNACounts) {
        # Check for NA values (print to main log file)
        cat("\n *** Number of NA values *** \n")
        print(colSums(is.na(dfParse)))
    }
    
    # Return the parsed dataset
    dfParse
    
}


# Address the visibility issues - called by fixMETAR()
getVisibility <- function(curMet, origMet, var="metar", useNAforIncomplete=FALSE) {
    
    # Get the original METAR data
    metAll <- origMet %>%
        pull(var)
    
    # Correct for visibility
    # Type 1 has \\d/\\d{1,2}SM, such as 1/4SM or 1/16SM
    # Type 2 has \\d \\d/\\dSM, such as 1 1/2SM
    sm1 <- which(str_detect(metAll, pattern=" \\d/\\d{1,2}SM"))
    sm2 <- which(str_detect(metAll, pattern=" \\d \\d/\\dSM"))
    
    valSM1 <- str_match(metAll, pattern="\\d/\\d{1,2}SM")[sm1]
    valSM1 <- str_replace(valSM1, "SM", "")
    valSM1 <- as.integer(str_sub(valSM1, 1, 1)) / as.integer(str_sub(valSM1, 3, -1))
    
    valSM2 <- str_match(metAll, pattern=" \\d \\d/\\dSM")[sm2]
    valSM2 <- as.integer(str_sub(valSM2, 2, 2))
    
    curMet[sm1, "Visibility"] <- valSM1
    curMet[sm2, "Visibility"] <- curMet[sm2, "Visibility"] + valSM2
    
    # Reset to NA if the original METAR is NA and useNAForIncomplete=TRUE
    if (useNAforIncomplete) {
        curMet[is.na(curMet$METAR), "Visibility"] <- NA
    }
    
    visCounts <- curMet %>% 
        count(Visibility)
    visVector <- visCounts$n
    names(visVector) <- visCounts$Visibility
    cat("\nVisibilities after correcting for fractions:\n")
    print(visVector)
    cat("\n")
    
    curMet
}


# Correct for wind gusts - called by fixMETAR()
getWindGusts <- function(curMet, origMet, var="metar", useNAforIncomplete=FALSE) {
    
    metAll <- origMet %>%
        pull(var)
    
    gustCheck <- which(str_detect(metAll, pattern="\\d{5}G\\d{2}KT"))
    valGust <- str_match(metAll, pattern="\\d{5}G\\d{2}KT")[gustCheck]
    valGust <- as.integer(str_sub(valGust, 7, 8))
    
    curMet[gustCheck, "WindGust"] <- valGust
    
    # Reset to NA if the original METAR is NA and useNAForIncomplete=TRUE
    if (useNAforIncomplete) {
        curMet[is.na(curMet$METAR), "WindGust"] <- NA
    }
    
    gustCounts <- curMet %>% 
        count(WindGust)
    gustVector <- gustCounts$n
    names(gustVector) <- gustCounts$WindGust
    
    cat("\nCounts of wind gusts extracted:\n")
    print(gustVector)
    cat("\n")
    
    curMet
}


# Correct for SLP - called by fixMETAR()
fixSLP <- function(curMet, showSLPGraph=TRUE) {
    
    dfParse <- curMet %>%
        mutate(modSLP=ifelse(curMet$SLP < 500, 1000 + curMet$SLP/10, 900 + curMet$SLP/10))
    
    p <- dfParse %>%
        group_by(SLP, modSLP) %>%
        summarize(n=n()) %>%
        ggplot(aes(x=SLP, y=modSLP, size=n)) + 
        geom_point(alpha=0.3) + 
        labs(title="Correction of SLP in raw METAR to modSLP for further analysis", 
             x="SLP: Original Data", 
             y="modSLP: Converted for Analysis Data"
        )
    if (showSLPGraph) {print(p)}
    
    dfParse
}


# fixMETAR() - corrects for issues with extracting and converting visibility, wind gusts, and SLP
fixMETAR <- function(met, 
                     origMet=met, 
                     var="origMETAR",
                     modifyVisibility=TRUE, 
                     modifyWindGusts=TRUE, 
                     modifySLP=TRUE, 
                     useNAforIncomplete=FALSE,
                     showSLPGraph=TRUE
                     ) {
    
    if (modifyVisibility) {
        met <- getVisibility(met, origMet, var=var, useNAforIncomplete=useNAforIncomplete)
    }
    
    if (modifyWindGusts) {
        met <- getWindGusts(met, origMet, var=var, useNAforIncomplete=useNAforIncomplete)
    }
    
    if (modifySLP) {
        met <- fixSLP(met, showSLPGraph=showSLPGraph)
    }
    
    met
    
}


# Helper function to check if data are always ascending
# Subject to the rules that NA may follow anything but may only precede NA
# Called by extractAndWrangleClouds()
ascVectorChecker <- function(y, strictly=TRUE) {
    
    # Checks a vector for being ascending (strictly so if strictly=TRUE)
    # Allows for NA provided that they never precede a numeric value
    
    if (all(is.na(y))) {
        # This is OK, if there are no numeric values then they are all NA
    } else if (all(is.na(y[2:length(y)]))) {
        # This is OK, single value is by definition ascending
    } else {
        # Confirm that NA are non-descending and that values are non-descending
        if (any(is.na(y) != cummax(is.na(y)))) {
            cat("\nIssue with NA preceding value -", y)
        } else {
            nonNA <- y[!is.na(y)]
            if (strictly) {
                if (min(diff(nonNA)) <= 0) {
                    cat("\nIssue with following values same or lower then preceding:", y, "\n")
                }
            } else {
                if (min(diff(nonNA)) < 0) {
                    cat("\nIssue with following values lower then preceding:", y, "\n")
                }
            }
        }
    }
}


# Helper function to verify non-descending cloud types, ascending cloud heights, return highest cloud type
# Called by extractAndWrangleClouds()
validateClouds <- function(tbl, 
                           cloudStates,
                           metVar="origMETAR",
                           nCols=6, 
                           var1="cloud", 
                           var2="cType", 
                           var3="cLevel",
                           diagnoseVsError=5
                           ) {
    
    # STEP 1: Validate non-descending cloud types
    ascStates <- c(cloudStates, "")  # "" can be considered the highest cloud state; nothing should follow it
    cTypes <- tbl %>%
        select_at(vars(all_of(paste0(var2, 1:nCols)))) %>%
        mutate_all(~match(., ascStates))
    
    for (intCtr in 1:(nCols-1)) {
        descClouds <- as.vector(cTypes[, intCtr+1] - cTypes[, intCtr])
        mism <- sum(descClouds < 0)
        if (mism != 0) {
            cat("\nIssue with descending cloud types - ", mism, "records in column", intCtr+1, "\n")
            if (mism > diagnoseVsError) {
                stop("Too many issues, investigate and fix")
            } else {
                cat("Problematic METAR:\n")
                print(tbl[which(descClouds < 0), metVar])
            }
        }
    }
    
    
    # STEP 2: Validate ascending cloud types (NA is always fine, but cannot be followed by a number)
    cHeights <- tbl %>%
        select_at(vars(all_of(paste0(var3, 1:nCols))))
    
    apply(cHeights, 1, FUN=ascVectorChecker)
    
    
    # STEP 3: Extract the most obscured cloud type
    obscureTypes <- cTypes %>%
        mutate_all(~ifelse(. > length(cloudStates), 0, .)) %>%
        apply(1, FUN=max)
    
    obscureTypes <- ifelse(obscureTypes==0, length(cloudStates)+1, obscureTypes)
    ascStates[obscureTypes]
    
}


# extractAndWrangleClouds() - extract cloud data from the METAR; 
# determine cloud types, minimum heights, and minimum ceilings; 
# and bind the cloud results to the METAR frame  
extractAndWrangleClouds <- function(met, 
                                    metVar, 
                                    maxClouds=6,
                                    timeVar="dtime",
                                    soloStates=c("SKC", "CLR"), 
                                    numStates=c("FEW", "SCT", "BKN", "OVC", "VV"), 
                                    printDetails=FALSE, 
                                    logFile="", 
                                    useNAforIncomplete=FALSE, 
                                    parsedMETARVariable="METAR"  # variable is NA if regex failed to parse
                                    ) {
    
    # Check that maxClouds <= 9
    if ((maxClouds != round(maxClouds, 0)) | (maxClouds < 1) | (maxClouds > 9)) {
        cat("\nThe maxClouds parameter must be an integer from 1-9, invalid entry of:", maxClouds)
        stop("Investigate and fix")
    }
    
    # Extract the metar record
    metAll <- met %>%
        pull(metVar)
    
    # If METAR is NA and if useNAForIncomplete=TRUE, make metAll=""
    if (useNAforIncomplete) {
        metAll[is.na(met[, parsedMETARVariable])] <- ""
    }
    
    # Limit the METAR record to only clouds described prior to RMK
    metAll <- str_replace(metAll, pattern="RMK.*", replacement="")
    
    # Search for soloStates and bind them by column to the existing data
    for (soloState in soloStates) {
        ss <- str_extract_all(metAll, pattern=paste0(" ", soloState, " "), simplify=TRUE)
        if (dim(ss)[2] == 0) {
            resVec <- rep(0L, dim(ss)[1])
        } else {
            resVec <- apply(ss, 1, FUN=function(x) { sum(x!="") })
        }
        origNames <- names(met)
        met <- bind_cols(met, data.frame(resVec))
        names(met) <- c(origNames, paste0("n", soloState))
    }
    
    # Search and extract for numStates
    numPasted <- paste0("(", paste0(numStates, collapse="|"), ")")
    numRecords <- str_extract_all(metAll, pattern=paste0(numPasted, "\\d{3}"), simplify=TRUE)
    
    # Allow for up to maxClouds cloud types
    if (dim(numRecords)[2] > maxClouds) {
        cat("Too many clouds relative to passed paremeter maximum of", maxClouds, ": Dimensions -",
            dim(numRecords)
        )
        stop("Investigate and fix")
    }
    
    # Create columns for cType1-cType(maxClouds) and cLevel1-cLevel(maxClouds)
    for (intCtr in 1:maxClouds) {
        if (intCtr > dim(numRecords)[2]) {
            x <- rep("", dim(numRecords)[1])
        } else {
            x <- numRecords[, intCtr]
        }
        
        # Hard-coding that the heights are always three digits (as per the extraction)
        cType <- str_sub(x, 1, str_length(x)-3)
        cLevel <- str_sub(x, str_length(x)-2, -1)
        cLevel <- ifelse(cLevel=="", NA_integer_, 100*as.integer(cLevel))
        
        origNames <- names(met)
        met <- bind_cols(met, tibble::tibble(x, cType, cLevel))
        names(met) <- c(origNames, paste0(c("cloud", "cType", "cLevel"), intCtr))
    }
    
    # Validate clouds are sensible and return the most obscured state
    wType <- validateClouds(met, cloudStates=numStates, metVar=metVar, nCols=maxClouds)
    
    # Create a table of soloStates and wType
    tmp <- met %>%
        select_at(vars(all_of(paste0("n", soloStates)))) %>%
        mutate(obsc=wType)
    
    # Confirm that values in tmp are as expected
    # Should be either 0 or 1 total of soloStates counts
    # If 1 soloStates counts, should be wType=""
    # If 0 soloStates counts, should be wType!=""
    tmpSolos <- tmp %>%
        select_at(vars(all_of(paste0("n", soloStates)))) %>%
        apply(1, FUN=sum)
    
    tmpObsc <- ifelse(wType!="", 1, 0)
    tmpProbs <- (tmpSolos + tmpObsc != 1) & (!useNAforIncomplete | !is.na(met$METAR))
    
    if (sum(tmpProbs)==0) {
        # Write this to the main log file
        cat("\nwType as created from clouds data is consistent\n")
    } else {
        # Write this to the main log file
        xText <- ifelse(logFile=="", ":", "(see logfile):")
        cat("\nThere are", sum(tmpProbs), "issues with clouds data", xText, "\n\n")
        cat("\nIssues with clouds data:\n", file=logFile, append=TRUE)
        met %>%
            select_at(vars(all_of(c(timeVar, metVar)))) %>%
            filter(tmpProbs) %>%
            write.table(file=logFile, append=TRUE, row.names=FALSE, col.names=FALSE)
        cat("\n", file=logFile, append=TRUE)
    }
    
    # Print the summaries by key type
    if (printDetails) {
        tmp %>%
            group_by_all() %>%
            summarize(n=n()) %>%
            print()
    }
    
    # Create the final wType variable, attach to the met data, and convert to a factor
    # Take the wType variable and overwrite in the soloStates if they exist
    for (soloVar in rev(soloStates)) {
        soloVec <- tmp %>%
            pull(paste0("n", soloVar))
        wType <- ifelse(soloVec > 0, soloVar, wType)
    }
    wType <- ifelse(wType=="", "Error", wType)
    
    met <- met %>%
        mutate(wType=factor(wType, levels=c(rev(numStates), rev(soloStates), "Error")))
    
    # Show count by wType
    met %>%
        count(wType) %>%
        print()
    
    # Return the database
    met
    
}


# checkTimeProperties() - confirm that times are aligned between 'dtime' and 'origMETAR' 
# and add year, month (as integer and as factored mmm abbreviation), and day  
checkTimeProperties <- function(met, 
                                metVar, 
                                timeVar
                                ) {
    
    # Extract the Zulu days and times from metVar
    zTime <- met %>%
        pull(metVar) %>%
        str_extract(pattern="\\d{6}Z")
    
    # Extract the day, hour, and minute from timeVar
    dTime <- met %>%
        pull(timeVar)
    
    # Create the expected Zulu string from dTime
    expTime <- paste0(str_pad(lubridate::day(dTime), width=2, pad="0"), 
                      str_pad(lubridate::hour(dTime), width=2, pad="0"), 
                      str_pad(lubridate::minute(dTime), width=2, pad="0"), 
                      "Z"
    )
    
    # Check for consistency
    mism <- (zTime != expTime)
    if (sum(mism) == 0) {
        cat("\nAll Zulu times in the METAR are consistent with the datetime variable\n")
    } else {
        cat("\nThere are", 
            sum(mism), 
            "inconsistencies between Zulu times in the METAR and the datetime variable\n",
            "First 10 examples include:\n"
        )
        data.frame(zTime, dTime, expTime, mism) %>%
            filter(mism) %>%
            head(10)
    }
    
    # Create key variables and return them
    met <- met %>%
        mutate(year=lubridate::year(dTime), 
               monthint=lubridate::month(dTime), 
               month=factor(monthint, levels=1:12, labels=month.abb[1:12]),
               day=lubridate::day(dTime)
        )
    
    met
    
}


# integrateProcessingMETAR() integrated all of the processing steps and saves a file for later
integrateProcessingMETAR <- function(locRawMETAR, 
                                     startDay, 
                                     endDay, 
                                     genMET,
                                     labsMET,
                                     saveLoc=NULL,
                                     ovrWrite=FALSE,
                                     ovrWriteError=FALSE,
                                     colTypes=NULL, 
                                     threshError=0.75, 
                                     logFile="", 
                                     ovrWriteLog=TRUE,
                                     useNAforIncomplete=FALSE, 
                                     showSLPGraph=TRUE
                                     ) {
    
    # If the log file is to be written to, create a few details:
    if (logFile != "") {
        if (ovrWriteLog) { cat("", file=logFile, append=FALSE) }
        cat("\nProcessing for", locRawMETAR, "from", startDay, "to", endDay, "\n", 
            file=logFile, 
            append=TRUE
        )
    }
    
    # Convert startDay and endDay to date
    startDay=as.Date(startDay)
    endDay=as.Date(endDay)
    
    # Find the hourly reporting times, and validate that they have 'enough' data at the peak hour
    keyZulu <- findHourlyReportingTimes(metFile=locRawMETAR, 
                                        colTypes=colTypes, 
                                        threshError=threshError
    )
    
    # Use the Zulu time to create the expected minimum time and number of days
    zuluNumber <- as.numeric(str_sub(keyZulu, 1, -2))
    dtString <- paste0(as.character(startDay), 
                       " 00:", 
                       str_pad(as.character(zuluNumber), width=2, pad="0"), 
                       ":00"
    )
    
    # Read in the raw METAR file
    readFile <- readMETAR(locRawMETAR, 
                          timeZ=keyZulu, 
                          expMin=as.POSIXct(dtString, tz="UTC"), 
                          expDays=1 + as.integer(endDay - startDay), 
                          colTypes=colTypes, 
                          logFile=logFile
    )
    
    # Create the valMet string by pasting the key Zulu time to the front of it
    valMet <- paste0(keyZulu, genMET)
    
    # Make the initial pass at parsing the raw METAR file
    initFile <- initialParseMETAR(readFile, 
                                  val=valMet, 
                                  labs=labsMET, 
                                  glimpseFinal=FALSE, 
                                  logFile=logFile
    )
    
    # Convert the initially parsed METAR file
    # This process is quite hard-coded to a specific format (which is fine for now)
    convFile <- convertMETAR(initFile)
    
    # Fix the converted METAR file - visibility, wind gusts, SLP
    # This process is quite hard-coded to a specific format (which is fine for now)
    fixFile <- fixMETAR(convFile, 
                        useNAforIncomplete=useNAforIncomplete, 
                        showSLPGraph=showSLPGraph
    )
    
    # Extract and wrangle the clouds from the METAR
    # This process is quite hard-coded to a specific format (which is fine for now)
    fullFile <- extractAndWrangleClouds(fixFile, 
                                        metVar="origMETAR", 
                                        logFile=logFile,
                                        useNAforIncomplete=useNAforIncomplete
    )
    
    # Check the time properties and add year, month (as integer and factor), day as variables
    finalFile <- checkTimeProperties(fullFile, metVar="origMETAR", timeVar="dtime")
    
    # If saveLoc is provided, save the file using RDS to the provided location; return it otherwise
    if (!is.null(saveLoc)) {
        if (file.exists(saveLoc)) {
            if (ovrWrite) {
                cat("\nOver-writing existing file:", saveLoc, "\n\n")
                saveRDS(finalFile, file=saveLoc)
            } else {
                cat("\nFile already exists and NOT over-written:", saveLoc, "\n\n")
                if (ovrWriteError) {
                    stop("Aborting due to ovrWriteError=TRUE\n\n")
                }
            }
        } else {
            cat("\nCreating file:", saveLoc, "\n\n")
            saveRDS(finalFile, file=saveLoc)
        }
    } else {
        return(finalFile)
    }
    
}

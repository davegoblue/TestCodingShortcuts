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



# Function to reorder and relabel factors
changeOrderLabel <- function(df, 
                             fctVar="cluster",
                             grpVars=c(),
                             burdenVar="dpm", 
                             wgtVar="pop",
                             exclfct="999"
                             ) {
    
    # FUNCTION ARGUMENTS
    # df: the data frame
    # fctVar: the factor variable
    # grpVars: the variable that the data are aurrently at (e.g., "fipsCounty" for county-level in df)
    # burdenVar: the disease burden variable for sorting
    # wgtVar: the weight variable for sorting
    # exclfct: the factor level to be excluded from analysis
    
    # General approach
    # 1. Data are aggregated to c(fctVar, grpVars) as x=sum(burdenVar*wgtVar) and y=mean(wgtVar)
    # 2. Data are aggregated to fctVar as z=sum(x)/sum(y)
    # 3. Factors are reordered from high to low on z, with the excluded factor added back last (if it exists)
    
    # Check if exclfct exists in the factor variable
    fctDummy <- exclfct %in% levels(df[, fctVar, drop=TRUE])
    
    # Create the summary of impact by segment
    newLevels <- df %>%
        filter(get(fctVar) != exclfct) %>%
        group_by_at(vars(all_of(c(fctVar, grpVars)))) %>%
        summarize(x=sum(get(burdenVar)*get(wgtVar)), y=mean(get(wgtVar))) %>%
        group_by_at(vars(all_of(fctVar))) %>%
        summarize(z=sum(x)/sum(y)) %>%
        arrange(-z) %>%
        pull(fctVar) %>%
        as.character()
    
    # Add back the dummy factor at the end (if it exists)
    if (fctDummy) newLevels <- c(newLevels, exclfct)
    
    # Reassign the levels in df
    df %>% 
        mutate(!!fctVar:=factor(get(fctVar), levels=newLevels, labels=newLevels))
    
}



# Helper function to make the overall cluster summary statistics
helperClusterSummaryPlots <- function(dfState, 
                                      dfPlot,
                                      showMap, 
                                      clusterPlotsTogether,
                                      weightedMean=TRUE,
                                      mapLevel="states"
                                      ) {
    
    # FUNCTION ARGUMENTS:
    # dfState: contains the state/county-level data
    # dfPlot: contains the joined data for plotting
    # showMap: boolean for whether to create a map (if FALSE, segment membership counts are shown instead)
    # clusterPlotsTogether: boolean, whether to put all four plots on the same page
    # weightedMean: boolean, whether to create weighted mean by segment (if FALSE, median by segment is taken)
    # mapLevel: the level to be used on the map
    
    # Reorder the cluster levels in dfState to match dfPlot
    # Convert factor order to match dfPlot (can be reordered by argument to the calling function)
    dfState <- dfState %>%
        mutate(cluster=factor(cluster, levels=levels(dfPlot$cluster)))
    
    # Plot the segments on a map or show segment membership
    if (showMap) {
        if (mapLevel=="counties") {
            dfMap <- dfState %>%
                mutate(fips=stringr::str_pad(state, width=5, side="left", pad="0")) %>%
                select(fips, cluster)
        } else {
            dfMap <- dfState
        }
        # Create the map
        p1 <- usmap::plot_usmap(regions=mapLevel, data=dfMap, values="cluster") + 
            scale_fill_discrete("cluster") + 
            theme(legend.position="right")
    } else {
        p1 <- dfState %>%
            count(cluster) %>%
            ggplot(aes(x=fct_rev(cluster), y=n)) + 
            geom_col(aes(fill=cluster)) +
            geom_text(aes(y=n/2, label=n)) +
            coord_flip() + 
            labs(x="", y="# Counties", title="Membership by segment")
    }
    
    # Plot the population totals by segment
    # Show population totals by cluster
    p2 <- dfState %>%
        group_by(cluster) %>%
        summarize(pop=sum(pop)/1000000) %>%
        ggplot(aes(x=fct_rev(cluster), y=pop)) + 
        geom_col(aes(fill=cluster)) + 
        geom_text(aes(y=pop/2, label=round(pop))) + 
        labs(y="2015 population (millions)", x="Cluster", title="Population by cluster (millions)") +
        coord_flip()
    
    # Plot the rolling 7-day mean daily disease burden by cluster
    # Create the p3Data to be either median of all elements in cluster or weighted mean
    p3 <- dfPlot %>%        
        select(date, cluster, cases=cpm7, deaths=dpm7, pop) %>%
        pivot_longer((-c(date, cluster, pop))) %>%
        filter(!is.na(value)) %>%
        group_by(date, cluster, name) %>%
        summarize(mdnValue=median(value), wtdValue=sum(pop*value)/sum(pop)) %>%
        ggplot(aes(x=date, y=if(weightedMean) wtdValue else mdnValue)) +
        geom_line(aes(group=cluster, color=cluster)) +
        facet_wrap(~name, scales="free_y") +
        labs(x="",
             y="Rolling 7-day mean, per million",
             title="Rolling 7-day mean daily disease burden, per million",
             subtitle=paste0(if(weightedMean) "Weighted mean" else "Median", 
                             " per day for states assigned to cluster"
             )
        ) + 
        scale_x_date(date_breaks="1 months", date_labels="%b")
    
    # Plot the total cases and total deaths by cluster
    p4 <- dfPlot %>%
        group_by(cluster) %>%
        summarize(cases=sum(cases), deaths=sum(deaths)) %>%
        pivot_longer(-cluster) %>%
        ggplot(aes(x=fct_rev(cluster), y=value/1000)) + 
        geom_col(aes(fill=cluster)) + 
        geom_text(aes(y=value/2000, label=round(value/1000))) +
        coord_flip() + 
        facet_wrap(~varMapper[name], scales="free_x") + 
        labs(x="Cluster", y="Burden (000s)", title="Total cases and deaths by segment")
    
    # Place the plots together if plotsTogether is TRUE, otherwise just print
    if (isTRUE(clusterPlotsTogether)) {
        gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
    } else {
        print(p1); print(p2); print(p3); print(p4)
    }
    
}



# Helper function to make total and per capita by state (calls its own helper function)
helperCallTotalPerCapita <- function(dfPlot, 
                                     thruLabel
                                     ) {
    
    # FUNCTION ARGUMENTS:
    # dfPlot: the plotting data frame
    # thruLabel: the date that data are through
    
    # Plot total cases and total deaths by state, colored by cluster
    helperBarDeathsCases(dfPlot, 
                         numVars=c("cases", "deaths"), 
                         title=paste0("Coronavirus impact by state through ", thruLabel), 
                         xVar=c("state"), 
                         fillVar=c("cluster")
    )
    
    # Plot cases per million and deaths per million by state, colored by cluster
    helperBarDeathsCases(dfPlot, 
                         numVars=c("cpm", "dpm"), 
                         title=paste0("Coronavirus impact by state through ", thruLabel), 
                         xVar=c("state"), 
                         fillVar=c("cluster")
    )
    
}



# Helper function to make recent vs. total plots
helperCallRecentvsTotal <- function(dfPlot, 
                                    thruLabel, 
                                    labelPoints, 
                                    recentTotalTogether
                                    ) {
    
    # FUNCTION ARGUMENTS:
    # dfPlot: the plotting data frame
    # thruLabel: the date that data are through
    # labelPoints: boolean, whether to label the individual points
    # recentTotalTogether: boolean, whether to put these plots together on one page
    
    # Plot last-30 vs total for cases per million by state, colored by cluster
    p7 <- helperRecentvsTotal(dfPlot, 
                              xVar="cpm", 
                              yVar="newcpm", 
                              title=paste0("Coronavirus burden through ", thruLabel), 
                              labelPlot=labelPoints,
                              printPlot=FALSE
    )
    
    # Plot last-30 vs total for deaths per million by state, colored by cluster
    p8 <- helperRecentvsTotal(dfPlot, 
                              xVar="dpm", 
                              yVar="newdpm", 
                              title=paste0("Coronavirus burden through ", thruLabel), 
                              labelPlot=labelPoints,
                              printPlot=FALSE
    )
    
    # Print the plots either as a single page or separately
    if (isTRUE(recentTotalTogether)) {
        gridExtra::grid.arrange(p7, p8, nrow=1)
    } else {
        print(p7); print(p8)
    }    
    
}



# Helper function to create total vs. elements plots
helperCallTotalvsElements <- function(dfPlot, 
                                      aggAndTotal, 
                                      clusterAggTogether,
                                      ...
                                      ) {
    
    # FUNCTION ARGUMENTS:
    # dfPlot: the plotting data frame
    # aggAndTotal: boolean, should each individual line be plotted (if FALSE an 80% CI is plotted instead)
    # clusterAggTogether: boolean, whether to print the plots all on a single page
    # ...: any other arguments to pass to helperTotalvsElements (especially pctRibbon or aggFunc)
    
    # Plot the cases per million on a free y-scale and a fixed y-scale
    p9 <- helperTotalvsElements(dfPlot, 
                                keyVar="cpm7", 
                                aggAndTotal=aggAndTotal,
                                title="Cases per million, 7-day rolling mean", 
                                printPlot=FALSE, 
                                ...
    )
    p10 <- helperTotalvsElements(dfPlot, 
                                 keyVar="cpm7", 
                                 aggAndTotal=aggAndTotal,
                                 title="Cases per million, 7-day rolling mean", 
                                 facetScales="fixed", 
                                 printPlot=FALSE, 
                                 ...
    )
    
    # Plot the deaths per million on a free y-scale and a fixed y-scale
    p11 <- helperTotalvsElements(dfPlot, 
                                 keyVar="dpm7", 
                                 aggAndTotal=aggAndTotal,
                                 title="Deaths per million, 7-day rolling mean", 
                                 printPlot=FALSE, 
                                 ...
    )
    p12 <- helperTotalvsElements(dfPlot, 
                                 keyVar="dpm7", 
                                 aggAndTotal=aggAndTotal,
                                 title="Deaths per million, 7-day rolling mean", 
                                 facetScales="fixed", 
                                 printPlot=FALSE, 
                                 ...
    )
    
    if (isTRUE(clusterAggTogether)) {
        gridExtra::grid.arrange(p9, p11, nrow=1)
        gridExtra::grid.arrange(p10, p12, nrow=1)
    } else {
        print(p9); print(p10); print(p11); print(p12)
    }
    
}



# Function to create side-by-side plots for a deaths and cases metric
# Data in df will be aggregated to be unique by byVar using aggFunc
helperBarDeathsCases <- function(df, 
                                 numVars,
                                 title="",
                                 xVar="state",
                                 fillVar=NULL,
                                 aggFunc=sum, 
                                 mapper=varMapper
                                 ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the data
    # numVars: the relevant numeric variables for plotting
    # title: plot title, default is nothing
    # xVar: the x-axis variable for plotting
    # fillVar: the variable that will color the bars in the final plot (NULL means use "lightblue" for all)
    # aggFunc: the aggregate function (will be applied to create data unique by byVar)
    # mapper: mapping file to convert x/y variables to descriptive axes (named vector "variable"="label")
    
    # OVERALL FUNCTION PROCESS:
    # 1.  Variables in numVar are aggregated by aggFunc to be unique by c(xVar, fillVar)
    # 2.  Data are pivoted longer
    # 3.  Bar charts are created, with coloring by fillVar if provided
    
    # Create the byVar for summing
    byVar <- xVar
    if (!is.null(fillVar)) { byVar <- c(byVar, fillVar) }
    
    # Process the data and create the plot
    p1 <- df %>%
        select_at(vars(all_of(c(byVar, numVars)))) %>%
        group_by_at(vars(all_of(byVar))) %>%
        summarize_all(aggFunc) %>%
        pivot_longer(-all_of(byVar)) %>%
        ggplot(aes(x=fct_reorder(get(xVar), value, .fun=min), y=value)) + 
        coord_flip() + 
        facet_wrap(~mapper[name], scales="free_x") + 
        labs(x="", y="", title=title) + 
        if (is.null(fillVar)) geom_col(fill="lightblue") else geom_col(aes_string(fill=fillVar))
    
    # Print the plot
    print(p1)
    
}



# Helper function to assess 30-day change vs. total
helperRecentvsTotal <- function(df, 
                                xVar, 
                                yVar,
                                title,
                                recencyDays=30, 
                                ablineSlope=0.5, 
                                mapper=varMapper, 
                                labelPlot=TRUE,
                                printPlot=TRUE
                                ) {
    
    # FUNCTION ARGUMENTS:
    # df: the tibble containing data by state by day
    # xVar: the x-variable
    # yVar: the y-variable
    # title: the plot title
    # recencyDays: number of days to consider as recent
    # ablineSlope: dashed line will be drawn with this slope and intercept 0
    # mapper: mapping file to convert x/y variables to descriptive axes (named vector "variable"="label")
    # labelPlot: boolean, whether to show the labels for each point
    # printPlot: boolean, whether to display the plot (if FALSE, plot object is returned)
    
    # Get the most date cutoff
    dateCutoff <- df %>% pull(date) %>% max() - recencyDays + 1
    cat("\nRecency is defined as", format(dateCutoff, "%Y-%m-%d"), "through current\n")
    
    # Create the plot
    p1 <- df %>%
        mutate(newCases=ifelse(date >= dateCutoff, cases, 0), 
               newDeaths=ifelse(date >= dateCutoff, deaths, 0), 
               newcpm=ifelse(date >= dateCutoff, cpm, 0), 
               newdpm=ifelse(date >= dateCutoff, dpm, 0)
        ) %>%
        group_by(state, cluster) %>%
        summarize_if(is.numeric, .funs=sum) %>%
        ungroup() %>%
        ggplot(aes_string(x=xVar, y=yVar)) + 
        labs(x=mapper[xVar], 
             y=mapper[yVar], 
             title=title, 
             subtitle=paste0("Dashed line represents ", 
                             round(100*ablineSlope), 
                             "% of total is new in last ", 
                             recencyDays,
                             " days"
             )
        ) + 
        geom_abline(lty=2, slope=ablineSlope) + 
        lims(x=c(0, NA), y=c(0, NA)) + 
        theme(legend.position = "bottom")
    
    # Add the appropriate geom (scatterplot if labelPlot is FALSE)
    if (labelPlot) p1 <- p1 + geom_text(aes(color=cluster, label=state))
    else p1 <- p1 + geom_point(aes(color=cluster), alpha=0.5)
    
    if (isTRUE(printPlot)) {
        print(p1)
    } else {
        p1
    }
    
}



# Function to plot cluster vs. individual elements on a key metric
helperTotalvsElements <- function(df, 
                                  keyVar, 
                                  title,
                                  aggAndTotal=TRUE,
                                  pctRibbon=0.8,
                                  aggFunc=if(aggAndTotal) median else mean, 
                                  mapper=varMapper, 
                                  facetScales="free_y", 
                                  printPlot=TRUE
                                  ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing n-day rolling averages
    # keyVar: the variable to be plotted
    # title: the plot title
    # aggAndTotal: boolean, whether to plot every individual observation along with the cluster aggregate
    # pctRibbon: if aggAndTotal is FALSE, a ribbon covering this percentage of the data will be plotted
    # aggFunc: how to aggregate the elements to the segment
    #          CAUTION that this is an aggregate of averages, rather than a population-weighted aggregate
    # mapper: the variable mapping file to get the appropriate label for keyVar
    # facetScales: the scaling for the facets - "free_y" to let them all float, "fixed" to have them the same
    # printPlot: boolean, if TRUE print the plot (otherwise return the plot object)
    
    # Create an appropriate subtitle
    subtitle <- if(facetScales=="free_y") {
        "Caution that each facet has its own y axis with different scales"
    } else if (facetScales=="fixed") { 
        "All facets are on the same scale"
    } else {
        "Update subtitle code in function helperTotalvsElements"
    }
    
    # Create an appropriate caption
    xCap <- "1. Cluster aggregate is mean over all observations (NOT population-weighted)"
    xCap <- paste0(xCap, "\n2. Ribbons reflect range covering ", round(pctRibbon*100), "% of observations")
    caption <- if(aggAndTotal) {
        "Cluster aggregate is median, weighting each observation equally\n(NOT population-weighted)"
    } else {
        xCap
    }
    
    # Create the plots for segment-level data
    p1 <- df %>%
        rbind(mutate(., state="cluster")) %>%
        group_by(state, cluster, date) %>%
        summarize_at(vars(all_of(keyVar)), .funs=aggFunc) %>%
        ungroup() %>%
        filter(!is.na(get(keyVar))) %>%
        ggplot(aes_string(x="date")) + 
        geom_line(data=~filter(., state == "cluster"), 
                  aes(y=get(keyVar), group=state, color=cluster), 
                  lwd=1.5
        ) + 
        facet_wrap(~cluster, scales=facetScales) + 
        labs(x="", 
             y=mapper[keyVar], 
             title=title, 
             subtitle=subtitle,
             caption=caption
        ) + 
        ylim(c(0, NA)) + 
        theme(legend.position="bottom")
    
    # Add an appropriate individual metric (either every observation or quantiles)
    if (aggAndTotal) {
        p1 <- p1 + 
            geom_line(data=~filter(., state != "cluster"), 
                      aes(y=get(keyVar), group=state), 
                      alpha=0.25
            )
    } else {
        dfRibbon <- df %>%
            filter(!is.na(get(keyVar))) %>%
            group_by(cluster, date) %>%
            summarize(ylow=quantile(get(keyVar), 0.5-0.5*pctRibbon), 
                      yhigh=quantile(get(keyVar), 0.5+0.5*pctRibbon)
            )
        p1 <- p1 + 
            geom_ribbon(data=dfRibbon, 
                        aes(ymin=ylow, ymax=yhigh), 
                        alpha=0.25
            )
    }
    
    # Print plot if requested, otherwise return it
    if (isTRUE(printPlot)) {
        print(p1)
    } else {
        p1
    }
    
}



# Function to convert a file to cumulative totals
makeCumulative <- function(df, 
                           typeVar="name", 
                           typeKeep=c("cases", "deaths", "tests"), 
                           valVar="vpm7", 
                           groupVars=c("state", "cluster", "name"), 
                           arrangeVars=c("date"), 
                           newName="cum7"
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: data frame containing the metrics
    # typeVar: the variable holding the metric type (default is 'name')
    # typeKeep: the values of typeVar to be kept
    # valVar: the variable holding the metric value (default is 'vpm7')
    # groupVars: groups for calculating cumulative sum
    # arrangeVars: variables to be sorted by before calculating cumulative sum
    # newName: the name for the new variable
    
    # Create the cumulative data
    dfCum <- df %>%
        filter(get(typeVar) %in% typeKeep, !is.na(get(valVar))) %>%
        arrange_at(vars(all_of(c(groupVars, arrangeVars)))) %>%
        group_by_at(groupVars) %>%
        mutate(!!newName:=cumsum(get(valVar))) %>%
        ungroup()
    
    # Return the processed data
    dfCum
    
}



# Function to plot cumulative data
plotCumulativeData <- function(df, 
                               keyMetricp2,
                               flagsp2,
                               p2Desc=keyMetricp2,
                               keyVar="cum7", 
                               makep1=FALSE, 
                               makep2=TRUE
                               ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame of cumulative data
    # keyMetricp2: the key metric to be plotted in the second plot (e.g., "deaths", "cases", "tests")
    # flagsp2: states to be treated as flagged in the second plot
    # p2Desc: the description to be used in plot 2
    # keyVar: the key variable to plot
    # makep1: boolean, whether to make the first plot
    # makep2: boolean, whether to make the second plot
    
    # Plot the cumulative data by cluster (if flag is set for producing this)
    if (isTRUE(makep1)) {
        p1 <- df %>%
            filter(state=="cluster") %>%
            ggplot(aes(x=date, y=get(keyVar))) + 
            geom_line(aes(group=cluster, color=cluster)) + 
            facet_wrap(~name, nrow=1, scales="free_y") + 
            scale_x_date(date_breaks="1 months", date_labels="%m") + 
            labs(x="Month", 
                 y="Cumulative Burden (per million)", 
                 title="Cumulative burden by segment (per million)"
            )
        print(p1)
    }
    
    
    # Plot the cumulative totals over time for one metric, and flag the highest
    if (isTRUE(makep2)) {
        p2 <- df %>%
            filter(state!="cluster", name==keyMetricp2) %>%
            mutate(bold=ifelse(state %in% flagsp2, 1, 0)) %>%
            ggplot(aes(x=date, y=get(keyVar))) + 
            geom_line(aes(group=state, color=cluster, alpha=0.4+0.6*bold, size=0.5+0.5*bold)) + 
            geom_text(data=~filter(., bold==1, date==max(date)), 
                      aes(x=date+lubridate::days(10), 
                          label=paste0(state, ": ", round(get(keyVar), 0)), 
                          color=cluster
                      ), 
                      size=3, 
                      fontface="bold"
            ) +
            scale_x_date(date_breaks="1 months", date_labels="%m") + 
            scale_alpha_identity() +
            scale_size_identity() +
            labs(x="Month", 
                 y=paste0("Cumulative ", p2Desc, " (per million)"), 
                 title=paste0("Cumulative coronavirus ", p2Desc, " by state (per million)"), 
                 subtitle="Top 5 states for total and growth rate are bolded and labelled"
            )
        print(p2)
    }
    
}



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



# Function to find and flag states that are high on a key value or change in key value
findFlagStates <- function(df, 
                           keyMetricVal, 
                           keyMetricVar="name", 
                           cumVar="cum7", 
                           prevDays=30, 
                           nFlag=5
                           ) {
    
    # FUNCTION ARGUMENTS:
    # df: the data frame containing the cumulative data
    # keyMetricVal: the metric of interest (e.g., "deaths", "cases", "tests")
    # keyMetricVar: the variable name for the column containing the metric of interest
    # cumVar: variable containing the cumulative totals
    # prevDays: the number of days previous to use for defining growth
    # nFlag: the number of states to flag for either total or growth (top-n of each)
    
    # Find top-5 in either total or recent increase
    dfFlag <- df %>%
        filter(get(keyMetricVar)==keyMetricVal, state!="cluster") %>%
        select_at(vars(all_of(c("state", "date", cumVar)))) %>%
        group_by(state) %>%
        summarize(maxVal=max(get(cumVar)), 
                  tminus=sum(ifelse(date==max(date)-lubridate::days(prevDays), get(cumVar), 0))
        ) %>%
        ungroup() %>%
        mutate(growth=maxVal-tminus, 
               rkTotal=min_rank(-maxVal), 
               rkGrowth=min_rank(-growth), 
               flag=ifelse(pmin(rkTotal, rkGrowth)<=nFlag, 1, 0)
        ) %>%
        arrange(-flag, rkTotal)
    
    # Return the top values as a vector of states
    dfFlag %>%
        filter(flag==1) %>%
        pull(state)
    
}

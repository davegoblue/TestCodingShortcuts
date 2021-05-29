# Mapping list for combining data elements from raw files
# Formatted as one list per urlType, with that list having one list for each combination of data
lstComboMapper <- list("cdcDaily"=list("nyc"=list("comboVar"="state", 
                                                  "uqVars"="date", 
                                                  "vecCombo"=c("NY"="NY", "NYC"="NY"),
                                                  "fn"=specNA(sum)
                                                  )
                                       ), 
                       "cdcHosp"=list()
                       )

# Mapping for urlType to url
urlMapper <- c("cdcDaily"="https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD", 
               "cdcHosp"="https://beta.healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD"
               )

# Mapping for urlType to colRenamer(vecRename=...)
renMapper <- list("cdcDaily"=c('submission_date'='date', 'new_case'='new_cases', 
                               'tot_death'='tot_deaths', 'new_death'='new_deaths'
                               ), 
                  "cdcHosp"=c("inpatient_beds_used_covid"="inp", 
                              "total_adult_patients_hospitalized_confirmed_and_suspected_covid"="hosp_adult", 
                              "total_pediatric_patients_hospitalized_confirmed_and_suspected_covid"="hosp_ped"
                              ), 
                  "default"=c()
                  )

# Mapping for urlType to colMutater(selfList=...)
selfListMapper <- list("cdcDaily"=list('date'=lubridate::mdy), 
                       "cdcHosp"=list(), 
                       "default"=list()
                       )

# Mapping for urlType to colMutater(fullList=...)
fullListMapper <- list("cdcDaily"=list(), 
                       "cdcHosp"=list(), 
                       "default"=list()
                       )

# Mapping for urlType to checkUniqueRows(uniqueBy=...)
uqMapper <- list("cdcDaily"=c("state", "date"), 
                 "cdcHosp"=c("state", "date")
                 )

# Mapping list for rows to be filtered (typically, states to be kept)
# Formatted as named list per urlType, with name being the field and element being the allowed values
lstFilterMapper <- list("cdcDaily"=list("state"=c(state.abb, "DC")), 
                        "cdcHosp"=list("state"=c(state.abb, "DC"))
                        )

# Mapping list for vector selection in processed data
# Formatted as a named list where the names are urlType and the values are fields to be kept
vecSelectMapper <- list("cdcDaily"=c("date", "state", "tot_cases", "tot_deaths", "new_cases", "new_deaths"), 
                        "cdcHosp"=c("date", "state", "inp", "hosp_adult", "hosp_ped")
                        )

# Mapping file for group_by variable per urlType
checkControlGroupMapper <- list("cdcDaily"="date",
                                "cdcHosp"="date",
                                "default"=c()
                                )

# Mapping file for numerics to summarize by group_by variable per urlType
checkControlVarsMapper <- list("cdcDaily"=c("new_cases", "new_deaths"),
                               "cdcHosp"=c("inp", "hosp_adult", "hosp_ped")
                               )

# Mapping for urlType to checkSimilarity(..., keyVars=); universe similarity checks to perform and report
checkSimilarityMapper <- list("cdcDaily"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                              state=list(label='state', countOnly=FALSE)
                                              ), 
                              "cdcHosp"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                             state=list(label='state', countOnly=FALSE)
                                             ), 
                              "default"=list()
                              )

# Mapping for urlType to plotSimilarity(..., ); fields where change in universe should be reported
plotSimilarityMapper <- list("cdcDaily"=c("date"), 
                             "cdcHosp"=c("date"), 
                             "default"=c()
                             )

# Mapping file for aggregated control total checks to perform
# Formatted as one list per urlType
# Within each urlType list, sublists drive the grouping variable, numerical aggregates, and reporting
keyAggMapper <- list("cdcDaily"=list("l1"=list("grpVar"="date",
                                               "numVars"=c("new_cases", "new_deaths",
                                                           "tot_cases", "tot_deaths"
                                               ),
                                               "sameUniverse"=NA,
                                               "plotData"=TRUE,
                                               "isLine"=TRUE,
                                               "returnDelta"=TRUE,
                                               "flagLargeDelta"=TRUE,
                                               "pctTol"=0.05,
                                               "absTol"=5,
                                               "sortBy"=c("name", "pctDelta", "absDelta"),
                                               "dropNA"=TRUE,
                                               "printAll"=TRUE
                                               ),
                                     "l2"=list("grpVar"="state",
                                               "numVars"=c("new_cases", "new_deaths"),
                                               "sameUniverse"=NA,
                                               "plotData"=TRUE,
                                               "isLine"=FALSE,
                                               "returnDelta"=FALSE,
                                               "flagLargeDelta"=FALSE
                                               ),
                                     "l3"=list("grpVar"="state",
                                               "numVars"=c("new_cases", "new_deaths",
                                                           "tot_cases", "tot_deaths"
                                                           ),
                                               "sameUniverse"="date",
                                               "plotData"=TRUE,
                                               "isLine"=FALSE,
                                               "returnDelta"=TRUE,
                                               "flagLargeDelta"=TRUE,
                                               "pctTol"=0.001,
                                               "absTol"=0,
                                               "sortBy"=c("name", "pctDelta", "absDelta"),
                                               "dropNA"=TRUE,
                                               "printAll"=TRUE
                                               )
                                     ),
                     "cdcHosp"=list("l1"=list("grpVar"="date",
                                              "numVars"=c("inp", "hosp_adult", "hosp_ped"),
                                              "sameUniverse"=NA,
                                              "plotData"=TRUE,
                                              "isLine"=TRUE,
                                              "returnDelta"=TRUE,
                                              "flagLargeDelta"=TRUE,
                                              "pctTol"=0.05,
                                              "absTol"=5,
                                              "sortBy"=c("name", "pctDelta", "absDelta"),
                                              "dropNA"=TRUE,
                                              "printAll"=TRUE
                                              ),
                                    "l2"=list("grpVar"="state",
                                              "numVars"=c("inp", "hosp_adult", "hosp_ped"),
                                              "sameUniverse"=NA,
                                              "plotData"=TRUE,
                                              "isLine"=FALSE,
                                              "returnDelta"=FALSE,
                                              "flagLargeDelta"=FALSE
                                              ),
                                    "l3"=list("grpVar"="state",
                                              "numVars"=c("inp", "hosp_adult", "hosp_ped"),
                                              "sameUniverse"="date",
                                              "plotData"=TRUE,
                                              "isLine"=FALSE,
                                              "returnDelta"=TRUE,
                                              "flagLargeDelta"=TRUE,
                                              "pctTol"=0.001,
                                              "absTol"=0,
                                              "sortBy"=c("name", "pctDelta", "absDelta"),
                                              "dropNA"=TRUE,
                                              "printAll"=TRUE
                                              )
                                    )
                     )

# Mapping file for creating per-capita metrics
# Formatted as c('raw variable name'='associated per capita variable name')
perCapMapper <- c("tot_cases"="tcpm", 
                  "tot_deaths"="tdpm", 
                  "new_cases"="cpm", 
                  "new_deaths"="dpm", 
                  "inp"="hpm", 
                  "hosp_adult"="ahpm", 
                  "hosp_ped"="phpm"
                  )


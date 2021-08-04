# Mapping list for combining data elements from raw files (last updated 2021-AUG-03)
# Formatted as one list per urlType, with that list having one list for each combination of data
lstComboMapper <- list("cdcDaily"=list("nyc"=list("comboVar"="state", 
                                                  "uqVars"="date", 
                                                  "vecCombo"=c("NY"="NY", "NYC"="NY"),
                                                  "fn"=specNA(sum)
                                                  )
                                       ), 
                       "cdcHosp"=list(), 
                       "vax"=list()
                       )

# Mapping for urlType to url (last updated 2021-AUG-03)
urlMapper <- c("cdcDaily"="https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD", 
               "cdcHosp"="https://beta.healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD", 
               "vax"="https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD"
               )

# Last updated 2021-AUG-03
# Mapping for urlType to colRenamer(vecRename=...)
renMapper <- list("cdcDaily"=c('submission_date'='date', 'new_case'='new_cases', 
                               'tot_death'='tot_deaths', 'new_death'='new_deaths'
                               ), 
                  "cdcHosp"=c("inpatient_beds_used_covid"="inp", 
                              "total_adult_patients_hospitalized_confirmed_and_suspected_covid"="hosp_adult", 
                              "total_pediatric_patients_hospitalized_confirmed_and_suspected_covid"="hosp_ped"
                              ), 
                  "vax"=c("Location"="state", 
                          "Date"="date", 
                          "Series_Complete_Yes"="vxc", 
                          "Administered"="vxa", 
                          "Admin_Per_100K"="Admin_Per_100k",
                          "Series_Complete_Pop_Pct"="vxcpoppct",
                          "Series_Complete_65Plus"="vxcgte65",
                          "Series_Complete_65PlusPop_Pct"="vxcgte65pct", 
                          "Series_Complete_18Plus"="vxcgte18",
                          "Series_Complete_18PlusPop_Pct"="vxcgte18pct"
                          ),
                  "default"=c()
                  )


# Last updated 2021-AUG-03
# Mapping for urlType to colMutater(selfList=...)
selfListMapper <- list("cdcDaily"=list('date'=lubridate::mdy), 
                       "cdcHosp"=list(), 
                       "vax"=list("date"=lubridate::mdy),
                       "default"=list()
                       )

# Last updated 2021-AUG-03
# Mapping for urlType to colMutater(fullList=...)
fullListMapper <- list("cdcDaily"=list(), 
                       "cdcHosp"=list(), 
                       "vax"=list(),
                       "default"=list()
                       )

# Last updated 2021-AUG-03
# Mapping for urlType to checkUniqueRows(uniqueBy=...)
uqMapper <- list("cdcDaily"=c("state", "date"), 
                 "cdcHosp"=c("state", "date"), 
                 "vax"=c("state", "date")
                 )

# Last updated 2021-AUG-03
# Mapping list for rows to be filtered (typically, states to be kept)
# Formatted as named list per urlType, with name being the field and element being the allowed values
lstFilterMapper <- list("cdcDaily"=list("state"=c(state.abb, "DC")), 
                        "cdcHosp"=list("state"=c(state.abb, "DC")), 
                        "vax"=list("state"=c(state.abb, "DC"))
                        )


# Mapping list for vector selection in processed data (last updated 2021-AUG-03)
# Formatted as a named list where the names are urlType and the values are fields to be kept
vecSelectMapper <- list("cdcDaily"=c("date", "state", "tot_cases", "tot_deaths", "new_cases", "new_deaths"), 
                        "cdcHosp"=c("date", "state", "inp", "hosp_adult", "hosp_ped"), 
                        "vax"=c("date", "state", "vxa", 
                                "vxc", "vxcpoppct", 
                                "vxcgte65", "vxcgte65pct", 
                                "vxcgte18", "vxcgte18pct"
                                )
                        )

# Mapping file for group_by variable per urlType (last updated 2021-AUG-03)
checkControlGroupMapper <- list("cdcDaily"="date",
                                "cdcHosp"="date",
                                "vax"="date",
                                "default"=c()
                                )

# Mapping file for numerics to summarize by group_by variable per urlType (last updated 2021-AUG-03)
checkControlVarsMapper <- list("cdcDaily"=c("new_cases", "new_deaths"),
                               "cdcHosp"=c("inp", "hosp_adult", "hosp_ped"), 
                               "vax"=c("vxa", "vxc", "vxcgte65", "vxcgte18")
                               )

# Last updated 2021-AUG-03
# Mapping for urlType to checkSimilarity(..., keyVars=); universe similarity checks to perform and report
checkSimilarityMapper <- list("cdcDaily"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                              state=list(label='state', countOnly=FALSE)
                                              ), 
                              "cdcHosp"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                             state=list(label='state', countOnly=FALSE)
                                             ), 
                              "vax"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                         state=list(label='state', countOnly=FALSE)
                                         ),
                              "default"=list()
                              )

# Last updated 2021-AUG-03
# Mapping for urlType to plotSimilarity(..., ); fields where change in universe should be reported
plotSimilarityMapper <- list("cdcDaily"=c("date"), 
                             "cdcHosp"=c("date"), 
                             "vax"=c("date"),
                             "default"=c()
)

# Mapping file for aggregated control total checks to perform (last updated 2021-AUG-03)
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
                                    ), 
                     "vax"=list("l1"=list("grpVar"="date",
                                          "numVars"=c("vxa", "vxc", "vxcgte18", "vxcgte65"),
                                          "sameUniverse"=NA,
                                          "plotData"=TRUE,
                                          "isLine"=TRUE,
                                          "returnDelta"=TRUE,
                                          "flagLargeDelta"=TRUE,
                                          "pctTol"=0.01,
                                          "absTol"=1,
                                          "sortBy"=c("name", "pctDelta", "absDelta"),
                                          "dropNA"=TRUE,
                                          "printAll"=TRUE
                                          ),
                                "l3"=list("grpVar"="state",
                                          "numVars"=c("vxa", "vxc", "vxcgte18", "vxcgte65"),
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


# First added 2021-AUG-03
# Mapping file for variables to be kept "as is" (not converted to rolling-7 per million)
asIsMapper <- list("cdcDaily"=c(), 
                   "cdcHosp"=c(), 
                   "vax"=c("vxcpoppct", "vxcgte65", "vxcgte65pct", "vxcgte18", "vxcgte18pct")
                   )

# Mapping file for creating per-capita metrics (last updated 2021-AUG-03)
# Formatted as c('raw variable name'='associated per capita variable name')
perCapMapper <- c("tot_cases"="tcpm", 
                  "tot_deaths"="tdpm", 
                  "new_cases"="cpm", 
                  "new_deaths"="dpm", 
                  "inp"="hpm", 
                  "hosp_adult"="ahpm", 
                  "hosp_ped"="phpm", 
                  "vxa"="vxapm", 
                  "vxc"="vxcpm"
                  )

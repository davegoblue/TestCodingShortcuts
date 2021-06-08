# Mapping list for combining data elements from raw files
# Mapping for urlType to url
usafMainURL <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/"
urlMapper <- c("cdcDaily"="https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD", 
               "cdcHosp"="https://beta.healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD", 
               "usafCase"=paste0(usafMainURL, "covid_confirmed_usafacts.csv"), 
               "usafDeath"=paste0(usafMainURL, "covid_deaths_usafacts.csv"),
               "usafPop"=paste0(usafMainURL, "covid_county_population_usafacts.csv")
               )

# Mapping for urlType to colRenamer(vecRename=...)
renMapper <- list("cdcDaily"=c('submission_date'='date', 'new_case'='new_cases', 
                               'tot_death'='tot_deaths', 'new_death'='new_deaths'
                               ), 
                  "cdcHosp"=c("inpatient_beds_used_covid"="inp", 
                              "total_adult_patients_hospitalized_confirmed_and_suspected_covid"="hosp_adult", 
                              "total_pediatric_patients_hospitalized_confirmed_and_suspected_covid"="hosp_ped"
                              ),
                  "usafCase"=c("County Name"="countyName", "State"="state"), 
                  "usafDeath"=c("County Name"="countyName", "State"="state"),
                  "default"=c()
                  )

# Mapping for urlType to colMutater(selfList=...)
selfListMapper <- list("cdcDaily"=list('date'=lubridate::mdy), 
                       "cdcHosp"=list(), 
                       "usafCase"=list('countyFIPS'=zeroPad5, 'stateFIPS'=zeroPad2), 
                       "usafDeath"=list('countyFIPS'=zeroPad5, 'stateFIPS'=zeroPad2),
                       "default"=list()
                       )

# Mapping for urlType to colMutater(fullList=...)
fullListMapper <- list("cdcDaily"=list(), 
                       "cdcHosp"=list(), 
                       "usafCase"=list(),
                       "usafDeath"=list(),
                       "default"=list()
                       )

# Mapping for urlType to pivotData(pivotBy=...)
pivotMapper <- list("usafCase"=c("countyFIPS", "countyName", "state", "stateFIPS"), 
                    "usafDeath"=c("countyFIPS", "countyName", "state", "stateFIPS")
                    )

# Mapping for urlType to pivotted variable
rawMakeVarMapper <- list("usafCase"=c("cases"), 
                         "usafDeath"=c("deaths")
                         )

# Mapping for urlType to checkUniqueRows(uniqueBy=...)
uqMapper <- list("cdcDaily"=c("state", "date"), 
                 "cdcHosp"=c("state", "date"), 
                 "usafCase"=c("countyFIPS", "stateFIPS", "date"), 
                 "usafDeath"=c("countyFIPS", "stateFIPS", "date")
                 )

# Mapping list for rows to be filtered (typically, states to be kept)
# Formatted as named list per urlType, with name being the field and element being the allowed values
lstFilterMapper <- list("cdcDaily"=list("state"=c(state.abb, "DC")), 
                        "cdcHosp"=list("state"=c(state.abb, "DC")), 
                        "usafCase"=list(), 
                        "usafDeath"=list()
                        )

# Mapping list for rows to be filtered out (typically, unallocated counties to be deleted)
# Formatted as named list per urlType, with name being the field and element being the disallowed values
lstExcludeMapper <- list("cdcDaily"=list(), 
                         "cdcHosp"=list(), 
                         "usafCase"=list("countyFIPS"=c("00000", "00001")), 
                         "usafDeath"=list("countyFIPS"=c("00000", "00001"))
                         )

# Mapping list for vector selection in processed data
# Formatted as a named list where the names are urlType and the values are fields to be kept
vecSelectMapper <- list("cdcDaily"=c("date", "state", "tot_cases", "tot_deaths", "new_cases", "new_deaths"), 
                        "cdcHosp"=c("date", "state", "inp", "hosp_adult", "hosp_ped"), 
                        "usafCase"=c("countyFIPS", "state", "date", "cases", "new_cases"), 
                        "usafDeath"=c("countyFIPS", "state", "date", "deaths", "new_deaths")
                        )

# Mapping file for group_by variable per urlType
checkControlGroupMapper <- list("cdcDaily"="date",
                                "cdcHosp"="date",
                                "usafDeath"="date",
                                "usafCase"="date",
                                "default"=c()
                                )

# Mapping file for numerics to summarize by group_by variable per urlType
checkControlVarsMapper <- list("cdcDaily"=c("new_cases", "new_deaths"),
                               "cdcHosp"=c("inp", "hosp_adult", "hosp_ped"), 
                               "usafDeath"=c("deaths", "new_deaths"), 
                               "usafCase"=c("cases", "new_cases")
                               )

# Mapping for urlType to checkSimilarity(..., keyVars=); universe similarity checks to perform and report
checkSimilarityMapper <- list("cdcDaily"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                              state=list(label='state', countOnly=FALSE)
                                              ), 
                              "cdcHosp"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                             state=list(label='state', countOnly=FALSE)
                                             ), 
                              "usafCase"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                              countyFIPS=list(label='county', countOnly=FALSE)
                                              ), 
                              "usafDeath"=list(date=list(label='date', countOnly=TRUE, convChar=TRUE), 
                                               countyFIPS=list(label='county', countOnly=FALSE)
                                               ), 
                              "default"=list()
                              )

# Mapping for urlType to plotSimilarity(..., ); fields where change in universe should be reported
plotSimilarityMapper <- list("cdcDaily"=c("date"), 
                             "cdcHosp"=c("date"), 
                             "usafCase"=c("date"),
                             "usafDeath"=c("date"),
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
                                    ), 
                     "usafDeath"=list("l1"=list("grpVar"="date",
                                                "numVars"=c("deaths", "new_deaths"),
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
                                                "numVars"=c("deaths", "new_deaths"),
                                                "sameUniverse"=NA,
                                                "plotData"=TRUE,
                                                "isLine"=FALSE,
                                                "returnDelta"=FALSE,
                                                "flagLargeDelta"=FALSE
                                                ),
                                      "l3"=list("grpVar"=c("countyFIPS", "countyName", "state"),
                                                "numVars"=c("deaths", "new_deaths"),
                                                "sameUniverse"="date",
                                                "plotData"=FALSE,
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
                     "usafCase"=list("l1"=list("grpVar"="date",
                                               "numVars"=c("cases", "new_cases"),
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
                                               "numVars"=c("cases", "new_cases"),
                                               "sameUniverse"=NA,
                                               "plotData"=TRUE,
                                               "isLine"=FALSE,
                                               "returnDelta"=FALSE,
                                               "flagLargeDelta"=FALSE
                                               ),
                                     "l3"=list("grpVar"=c("countyFIPS", "countyName", "state"),
                                               "numVars"=c("cases", "new_cases"),
                                               "sameUniverse"="date",
                                               "plotData"=FALSE,
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

# Formatted as one list per urlType, with that list having one list for each combination of data
lstComboMapper <- list("cdcDaily"=list("nyc"=list("comboVar"="state", 
                                                  "uqVars"="date", 
                                                  "vecCombo"=c("NY"="NY", "NYC"="NY"),
                                                  "fn"=specNA(sum)
                                                  )
                                       ), 
                       "cdcHosp"=list()
                       )

# Mapping file for creating per-capita metrics
# Formatted as c('raw variable name'='associated per capita variable name')
perCapMapper <- c("tot_cases"="tcpm", 
                  "tot_deaths"="tdpm",
                  "cases"="tcpm", 
                  "deaths"="tdpm",
                  "new_cases"="cpm", 
                  "new_deaths"="dpm", 
                  "inp"="hpm", 
                  "hosp_adult"="ahpm", 
                  "hosp_ped"="phpm"
                  )

plotCombineAggByMapper <- list("state"=list(agg1 = list(aggFunc = specNA(specSumProd), 
                                                        aggVars = c("pop"), 
                                                        wtVar = NULL, 
                                                        prefix = NULL
                                                        ), 
                                            agg2 = list(aggFunc = specNA(weighted.mean), 
                                                        aggVars = c("tcpm7", "tdpm7", "cpm7", "dpm7", "hpm7"),
                                                        wtVar = "pop", 
                                                        prefix = "wm_"
                                                        )
                                            ), 
                               "county"=list("agg1"=list(aggFunc=specNA(specSumProd), 
                                                         aggVars=c("pop"), 
                                                         wtVar=NULL, 
                                                         prefix=NULL
                                                         ), 
                                             "agg2"=list(aggFunc=specNA(weighted.mean), 
                                                         aggVars=c("tcpm7", "tdpm7", "cpm7", "dpm7"), 
                                                         wtVar="pop", 
                                                         prefix="wm_"
                                                         )
                                             )
                               )

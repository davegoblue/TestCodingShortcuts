# TestCodingShortcuts
Testing some coding shortcuts that I will want to use later in R

A few things that I want to learn how to implement in R based on what I have seen on the internet

* equivalents for MATCH and VLOOKUP, including the interval (non-exact) portions
* SET BY and MERGE BY (using dplyr or plyr?)
* normalizing and denormalizing data (tidy principles, using tidyr)
* sorting
* expanding knowledge of the base graphing package


All simple things to do, but knowing these will advance my capabilities in R
  
### Contents as of 17-FEB-2016  
* Exercise001 is for a general algorithm for lookups, aggregation/calculation, and graphing  
* TaverasRforExcel is for expanding on tips and tricks from Taveras (author of "R for Excel") with some of my explorations  

### Expansions week of 22-FEB-2016  
* Exercise002 is for simplifying the Exercise001 code  
* Exercise003 is for adding to the Exercise002 code the ability to calculate all possible outcomes  
* CheckSDPopSamp is for showing the sample vs. population calculations for variance and standard deviation  
  
### Expansions week of 04-APR-2016  
* RiskRuin_v001 uses exponents to calculate risk of ruin for games with integer outcomes  
* RiskRuin_v001 compares theory with simulation  
* RiskRuin_v001 compares theory with two forms of simulation (simulations very close, theory somewhat more conservative); next step is to optimize memory so this can be run for a larger number of attempts  
* RiskRuin_v001 corrected algorithm for finding p to solve polynomial when first run finds an inflection point prior to nBuckets  
* RiskRuin_v002 created to split processing in to chunks for better memory management; can run for very large N now; next step is to investigate whether speed can be further optimized  
  
### Expansions week of 11-APR-2016  
* RiskRuin_v002 converts graph to log10 scale and adds Chi-squared goodness of fit test  
* RiskRuin_v002 increases nPerTrial to 2.0 million and uses exactly 1% for cb    
* RiskRuin_v002 shows initial details of the some behind n*rr1^n (component of mean) and n^2*rr1^n (component of sd)  
  
### Expansions week of 25-APR-2016  
* RiskRuin_v002 adds proc.time() -- total is ~150 seconds for 2 * 2000 * 200000  
* RiskRuin_v002 back to 2 * 2000 * 2000000 -- total is ~1600 seconds  
* Created RiskRuin_v002.Rmd and RiskRuin_v002.html  
* Modified for 2 simulations, each of 2000 trials on 3 million hands - 36 minutes  
* Created RiskRuin_v002a.Rmd and RiskRuin_v002a.html to diagnose time by sub-component of calcOutcomes  
* Created RiskRuin_v002b.Rmd and RiskRuin_v002b.html to pre-process calcOutcomes for parallel attempts  
* Modified RiskRuin_v002b.Rmd and RiskRuin_v002b.html to run parallel - using 3 clusters cuts total run time ~50%  
  
### Expansions week of 02-MAY-2016  
* RiskRuin_v002b adds user-supplied flag for runParallel and time diagnostics (only if runParallel==FALSE)    
* Paired loops of 40 million (one no cb, and one with cb) take ~16 seconds each if not in parallel    
* Created TestParallel_v001 to look at the "sunk cost" timing for parallel vs. sequential in a simulation  
* TestParallel_v001 runs sequential, 3 parallel clusters doing 300% of total sequential work, and 3 parallel clusters doing 100% of total sequential work  
* TestParallel_v001 expanded to do 300% base work (due vecLen x3) and 900% base work (due vecLen x3 and nRuns x3)  
* TestParallel_v002 created to set cache=TRUE for all the calculations in TestParallel_v001  
* TestParallel_v002 invetsigates using 1-4 parallel clusters and comparing run times  
* TestParallel_v002 invetsigates using 1-5 parallel clusters, caches results, and plots elapsed time vs. # clusters  
* TestParallel_v002 invetsigates using 1-8 parallel clusters, caches results, and plots elapsed time vs. # clusters  
* TestParallel_v003 re-runs using 1-8 parallel clusters and preps for randomForest benchmarking    
* TestParallel_v003 runs randomForest sequentially using both the 10-fold cv and the 10-run bootstrap  
* TestParallel_v003 runs randomForest with 2 parallel clusters using both the 10-fold cv and the 10-run bootstrap  
* TestParallel_v003 runs randomForest with 3 parallel clusters using both the 10-fold cv and the 10-run bootstrap  
* TestParallel_v003 graphs accuracy impact of cv vs. bootstrap and sequential vs. parallel  
* TestParallel_v003 investigates efficiency of parallel for 10-fold cv on a much larger randomForest  
* TestParallel_v003 summarized for future reference    
  
### Expansions week of 09-MAY-2016  
* AssociativeArrays_v001 is an initial exploration of using the hash table to speed up lookups in R      

### Expansions week of 16-MAY-2016  
* Wizard001_v001 is an initial exploration of an algorithm displayed on the Wizard of Odds page  
  
### Expansions week of 20-JUN-2016  
* AssociativeArrays_v001 creates an associative array of word counts, similar to a dictionary in Python, then puts back together the appropriate key-value paris  
* AssociativeArrays_v001 shows an example of character lookups in a hashed environment    
* AssociativeArrays_v001 reproduces the Python word count dictionary on romeo-full.txt  
* AssociativeArrays_v001 reproduces the Python word count dictionary on tomsawyer_74.txt  
* AssociativeArrays_v001 compares run times by approach on tomsawyer_74.txt  
* AssociativeArrays_v001 explores words available in OED_from_Github.txt  
* AssociativeArrays_v001 searched for words in tomsawyer_74.txt in OED_from_Github.txt  
  
### Expansions week of 27-JUN-2016  
* AssociativeArrays_v001 updates word searches in tomsawyer_74.txt in OED_from_Github.txt  
* AssociativeArrays_v001 changes regular expressions to improve word searches in tomsawyer_74.txt in OED_from_Github.txt  
* AssociativeArrays_v001 further improves word searches in tomsawyer_74.txt in OED_from_Github.txt  
* AssociativeArrays_v001 explores word mismatches from tomsawyer_74.txt in OED_from_Github.txt  
* AssociativeArrays_v001 refines word matching from tomsawyer_74.txt in OED_from_Github.txt  
* RiskRuin_v003_DDB96_0200cb refines the risk of ruin program to look at 9/6 DDB with 2.00% cb  
* RiskRuin_v003_DDB96_0250cb refines the risk of ruin program to look at 9/6 DDB with 2.50% cb  

### Expansions week of 04-JUL-2016  
* RiskRuin_v003_DDB96_0250cb slightly adjusted for fixing typos  
* RiskRuin_v003_JB96_0200cb refines the risk of ruin program to look at 9/6 JB with 2.00% cb  
* RiskRuin_v003_BP85_0200cb refines the risk of ruin program to look at 8/5 BP with 2.00% cb  
* RiskRuin_v003_JB95_0200cb refines the risk of ruin program to look at 9/5 JB with 2.00% cb  
* RiskRuin_v003_Comparisons refines the risk of ruin program to compare various base games and cb  
* DataCamp_Insights_v001 captures a few key insights from "Introduction to R"  
* DataCamp_Insights_v001 captures additional insights from "Introduction to R" and "Intermediate R"  
* DataCamp_Insights_v001 finalizes key insights from "Introduction to R" and "Intermediate R"  
* DataCamp_Insights_v001 captures initial insights from "Writing Functions in R"  

### Expansions week of 11-JUL-2016  
* DataCamp_Insights_v001 captures additional insights from "Writing Functions in R"  
* DataCamp_Insights_v001 captures library(purrr) insights from "Writing Functions in R"  
* DataCamp_Insights_v001 finalizes documenting insights from "Writing Functions in R"  

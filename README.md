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
* DataCamp_Insights_v001 captures initial notes on reading flat files and Excel files from "Reading Data in to R"  
* DataCamp_Insights_v001 captures initial notes on reading files from other statistical software in "Reading Data in to R"  
* DataCamp_Insights_v001 captures initial notes on using relational databases (SQL) in "Reading Data in to R"  
* DataCamp_Insights_v001 captures initial notes on reading web data in "Reading Data in to R"  

### Expansions week of 18-JUL-2016  
* DataCamp_Insights_v001 captures initial notes on "Cleaning Data in R"  
* DataCamp_Insights_v001 captures library(tidyr) notes from "Cleaning Data in R"  
* DataCamp_Insights_v001 captures remaining notes (lubridate, stringr) from "Cleaning Data in R"  
* DataCamp_Insights_v001 captures initial notes from "Data Manipulation with dplyr"  
* DataCamp_Insights_v001 captures additional notes from "Data Manipulation with dplyr"  
* DataCamp_Insights_v001 captures additional notes (filter, arrange) from "Data Manipulation with dplyr"  
* DataCamp_Insights_v001 captures additional notes (summarize, pipe/chain) from "Data Manipulation with dplyr"  
  
### Expansions week of 25-JUL-2016  
* DataCamp_Insights_v001 captures additional notes (group_by, database connections) from "Data Manipulation with dplyr"  
* DataCamp_Insights_v001 captures initial notes (basic syntax) from "Data Manipulation with data.table"  
* DataCamp_Insights_v001 captures additional notes from "Data Manipulation with data.table"  
* DataCamp_Insights_v001 captures remaining notes from "Data Manipulation with data.table"  
* DataCamp_Insights_v001 captures initial notes from "Data Visualization with ggplot (part 1)"  
* DataCamp_Insights_v001 captures additional notes from "Data Visualization with ggplot (part 1)"  
* DataCamp_Insights_v001 captures additional notes (aestehetics) from "Data Visualization with ggplot (part 1)"  

### Expansions week of 01-AUG-2016  
* DataCamp_Insights_v001 captures additional notes (geometries) from "Data Visualization with ggplot (part 1)"  
* DataCamp_Insights_v001 finalizes notes (qplot, wrap-up) from "Data Visualization with ggplot (part 1)"  
* DataCamp_Insights_v001 captures initial notes (statistics layer) from "Data Visualization with ggplot (part 2)"  
* DataCamp_Insights_v001 captures additional notes (coordinates, facets) from "Data Visualization with ggplot (part 2)"  
* DataCamp_Insights_v001 captures additional notes (themes) from "Data Visualization with ggplot (part 2)"  
* DataCamp_Insights_v001 captures additional notes (best practices) from "Data Visualization with ggplot (part 2)"  
* DataCamp_Insights_v001 captures additional notes (case study background) from "Data Visualization with ggplot (part 2)"  

### Expansions week of 08-AUG-2016  
* DataCamp_Insights_v001 captures initial notes (background) from "Data Visualization with ggvis"  
* DataCamp_Insights_v001 captures additional notes (layers) from "Data Visualization with ggvis"  
* DataCamp_Insights_v001 captures additional notes (visualizations) from "Data Visualization with ggvis"  
* DataCamp_Insights_v001 captures additional notes (interactivity) from "Data Visualization with ggvis"  
* DataCamp_Insights_v001 captures final notes (axes, scales, etc.) from "Data Visualization with ggvis"  
* DataCamp_Insights_v001 edited in preparation for additional notes going in to a new file  
* AdditionalCoding_v001 created to start looking at card games (Three Card Poker)  

### Expansions week of 15-AUG-2016  
* AdditionalCoding_v001 adapted for baseline of 5-card games  
* AdditionalCoding_v001 calculates all baseline 5-card values, and makes a random sample of 100 single-card replacements  
* AdditionalCoding_v001 experiments with data.table for key lookups  
* AdditionalCoding_v001 experiments with hash tables for key lookups  
* AdditionalCoding_v001 adds a few plots (ggplot2) for initial hand types  
* AdditionalCoding_v001 experiments further with data.table for the lookups   
* AdditionalCoding_v001 compares run times for data.table and hash tables for the lookups     

### Expansions week of 22-AUG-2016  
* DataCamp_Insights_v001 includes Module 01 practice examples from Intermediate R  
* AdditionalCoding_v001 experiments with additional methods for the hash table  
* AdditionalCoding_v001 experiments with array0, array4, and array5 from the Wizard of Odds Methodology  
* AdditionalCoding_v001 experiments with array3 from the Wizard of Odds Methodology  
* AdditionalCoding_v001 populates array3 by slightly adapting the Wizard of Odds Methodology  
* AdditionalCoding_v001 populates array2, though the process needs additional optimization for run-time  
* AdditionalCoding_v001 works through the conversion of any given 2-4 cards to an index for future functions    

### Expansions week of 29-AUG-2016  
* AdditionalCoding_v001 implements the re-draw of 1-4 cards using the key functions    
* AdditionalCoding_v002 keeps the most salient components of AdditionalCoding_v001 for next steps  
* AdditionalCoding_v002 populates and control-total checks array1-array5  
* AdditionalCoding_v002 assesses all hands assuming re-draws can replace the cards thrown away  
* AdditionalCoding_v002 assesses the 1-card redraws assuming no replacement  
* AdditionalCoding_v002 assesses the 2-card redraws assuming no replacement  
* AdditionalCoding_v002 assesses the combined impact of 1-card draws and 2-card redraws both assuming no replacement  

### Expansions week of 05-SEP-2016  
* AdditionalCoding_v002 assesses the 3-card redraws assuming no replacement  
* AdditionalCoding_v002 assesses the combined impact of 1/2/3-card redraws all assuming no replacement
* AdditionalCoding_v002 assesses the 4-card redraws assuming no replacement  
* AdditionalCoding_v002 assesses the combined impact of 1/2/3/4-card redraws all assuming no replacement  
* AdditionalCoding_v002 assesses the 5-card redraws assuming no replacement  
* AdditionalCoding_v002 assesses the combined impact of 1/2/3/4/5-card redraws all assuming no replacement  
* AdditionalCoding_v003 initialized to look at improvements to speed and readability  
* AdditionalCoding_v003 speed increased for initial set-up components  
  
### Expansions week of 12-SEP-2016  
* AdditionalCoding_v003 tests the indexing and combinatorics set-up  
* AdditionalCoding_v003 tests the no replacement hands  
* AdditionalCoding_v003 creates weighted non-duplicates for Quads, Full Houses, and Trips  
* AdditionalCoding_v003 creates weighted non-duplicates for Two Pair  
* AdditionalCoding_v003 creates weighted non-duplicates for One Pair  
* AdditionalCoding_v003 creates weighted non-duplicates for No Pair  
* AdditionalCoding_v003 integrates and summarized weighted non-duplicated hands (flushes not yet working right)  
* AdditionalCoding_v003 integrates and summarizes weighted non-duplicated hands (flushes fixed)  

### Expansions week of 19-SEP-2016  
* AdditionalCoding_v003 investigates optimal holds based on non-duplicated data  
* AdditionalCoding_v003 adjusts hand valuations  
* AdditionalCoding_v003 explores initial matrix creation  
* AdditionalCoding_v003 explores initial matrix classification  
* AdditionalCoding_v003 explores initial hand type classification  
* AdditionalCoding_v003 explores array creation  
* AdditionalCoding_v003 pre-sets the indices for the arrays  

### Expansions week of 26-SEP-2016  
* AdditionalCoding_v004 sets up the 11-step process  
* AdditionalCoding_v004 creates indices/arrays for array5 and array4  
* AdditionalCoding_v004 creates indices/arrays for array3  
* AdditionalCoding_v004 creates indices/arrays for array2 and array1 and array0  
* AdditionalCoding_v004 creates ev0-ev5 for expected values by index  
* AdditionalCoding_v004 creates weighted non-duplicate hands for assessing drawing potential  
* AdditionalCoding_v004 creates weighted non-duplicate hands for draw-0 and draw-1    
* AdditionalCoding_v004 creates weighted non-duplicate hands for draw-2    
  
### Expansions week of 03-OCT-2016  
* AdditionalCoding_v004 creates weighted non-duplicate hands for draw-3    
* AdditionalCoding_v004 creates weighted non-duplicate hands for draw-4    
* AdditionalCoding_v004 creates weighted non-duplicate hands for draw-5    
* AdditionalCoding_v004 calculates the best holds as well as the overall game EV    
* AdditionalCoding_v004 calculates the best holds and EV for dealt RF/SF/Quad/FH    
* AdditionalCoding_v004 calculates the best holds and EV for dealt Flush/Straight    

### Expansions week of 10-OCT-2016  
* AdditionalCoding_v004 calculates the best holds and EV for dealt Trips/Two Pair   
* AdditionalCoding_v004 calculates the best holds and EV for dealt Paying Pair   
* AdditionalCoding_v004 calculates the best holds and EV for dealt Nothing   
* AdditionalCoding_v004 calculates timing by component   
* AdditionalCoding_v004 improves creation time for array0 (shaves 25% or ~20 seconds off original run time)   
* DataCamp_Insights_v001 captures initial notes from "Data Manipulation with xts and zoo"  
* DataCamp_Insights_v001 captures Module 2 notes from "Data Manipulation with xts and zoo"  

### Expansions week of 17-OCT-2016  
* AdditionalCoding_v004 describes caching plan and next steps   
* AdditionalCoding_v005 created to attempt better caching for reduced run times   
* AdditionalCoding_v005 creates indices for the draw2/keep3 hands   
* AdditionalCoding_v005 creates function for the draw-x hands, stores indices, and applies hand-to-scores     
* AdditionalCoding_v005 creates EV for all possible holds assuming the redraw can return the discards     
* AdditionalCoding_v005 creates EV for hold-4 assuming the redraw canNOT return the discards     
* AdditionalCoding_v005 creates EV for hold-3 assuming the redraw canNOT return the discards     

### Expansions week of 24-OCT-2016  
* AdditionalCoding_v005 creates EV for hold-2 assuming the redraw canNOT return the discards  
* AdditionalCoding_v005 creates EV for hold-1/hold-0 assuming the redraw canNOT return the discards  
* AdditionalCoding_v005 calculates timing implications and opportunities to optimize  
* AdditionalCoding_v005 creates non-duplication and weighting  
* AdditionalCoding_v005 runs optimal holds and game summary statistics for the non-duplicated and weighted hands  
* DataCamp_Insights_v001 captures Module 3 notes from "Data Manipulation with xts and zoo"  
* DataCamp_Insights_v001 captures Module 4 notes from "Data Manipulation with xts and zoo"  

### Expansions week of 31-OCT-2016  
* AdditionalCoding_v005 creates index-match to help with lookup readability  
* DataCamp_Insights_v001 captures Module 5 notes from "Data Manipulation with xts and zoo"  
* DataCamp_Insights_v001 captures ggplot2 part3 Refresher and Box Plots  
* DataCamp_Insights_v001 captures ggplot2 part3 Density Plots  
* DataCamp_Insights_v001 captures ggplot2 part3 Multiple Groups/Variables  
* AdditionalCoding_v005 caches preparation and simulates only the small data (new run time ~10 seconds)  
* AdditionalCoding_v006 created to include only relevant components of "small data" runs  

### Expansions week of 07-NOV-2016  
* AdditionalCoding_v006 converts scoring and analysis to functional form  
* AdditionalCoding_v006 simulates using several input tables  
* AdditionalCoding_v006 adds function to apply one set of holds against another set of EV  
* AdditionalCoding_v006 reduces temporary file memory storage through additional functions  
* AdditionalCoding_v006 assesses JB 96, BP 85, and DDB 96  
* AdditionalCoding_v006 assesses TDB 97 and TDB 96  
* AdditionalCoding_v006 assesses using DDB 96 strategy on JB and TDB  

### Expansions week of 14-NOV-2016  
* AdditionalCoding_v006 assesses BPD 96 using JB 96 strategy  
* AdditionalCoding_v006 assesses variances on the deal for JB 96, JB 85, BP 85, DDB 96, TDB 97, TDB 96, and BPD 96    
* DataCamp_Insights_v001 captures ggplot2 part3 Large Data (Observations, Variables)  
* AdditionalCoding_v006 compares holds across games and displays differences    
* AdditionalCoding_v006 compares holds across JB 96, JB 85, and BP 85    
* AdditionalCoding_v006 compares holds across JB 96, DDB 96, and BPD 96    
* AdditionalCoding_v006 compares holds across DDB 96 and TDB 96    

### Expansions week of 21-NOV-2016  
* AdditionalCoding_v006 simulates deal-only EV for JB 96 and BP 85    
* AdditionalCoding_v006 reports percentile deal-only outcomes at 10,000 hands    
* AdditionalCoding_v006 adds flexibility to change number of deals simulated    
* AdditionalCoding_v006 reports DDB 96 percentiles (EV of deal-only) vs. number of hands    
* AdditionalCoding_v006 allows functional coding for calculating EV of deal-only percentiles    
* AdditionalCoding_v006 caches a few longer-running components and sets several random seeds    
* AdditionalCoding_v006 assesses distributions for JB 96, BP 85, DDB 96, BPD 96, and TDB 96    

### Expansions week of 28-NOV-2016  
* AdditionalCoding_v006 fixes duplicate simGame functions  
* AdditionalCoding_v006 begins exploring variance of the draw  
* AdditionalCoding_v006 continues exploring variance of the draw  
* AdditionalCoding_v006 continues exploring variance of the draw (4-card holds)  
* AdditionalCoding_v006 continues exploring variance of the draw (3/2/1-card holds)  
* AdditionalCoding_v006 continues exploring variance of the draw (0-card holds, results for JB 96, BP 85, TDB 96)  
* AdditionalCoding_v006 general code clean-up  
  
### Expansions week of 05-DEC-2016  
* AdditionalCoding_v006 explores mean and variance for JB 96, BP 85, BPD 96, DDB 96, TDB 96    
* AdditionalCoding_v006 explores mean and variance for starting hands of JB 96    
* AdditionalCoding_v006 adds functional form for mean and variance for starting hands  
* AdditionalCoding_v006 further explores JB 96, BP 85, BPD 96, DDB 96, TDB 96   
* AdditionalCoding_v006 begins to assess hand "flavors" for N-play   
* AdditionalCoding_v006 continues to assess hand "flavors" for N-play   
* AdditionalCoding_v006 creates summary tables for JB 96, BP 85, BPD 96, DDB 96, and TDB 96   

### Expansions week of 12-DEC-2016  
* AdditionalCoding_v006 begins to assess 3-play    
* AdditionalCoding_v006 vectorizes inner loop for N-play and assesses all games for n=1, 3    
* AdditionalCoding_v006 partially vectorizes outer loop for N-play and assesses all games for n=1, 3    
* AdditionalCoding_v006 further adapts methodology for N-play outcomes for n=1, 3    
* AdditionalCoding_v006 attempts arrays for assessing N-play outcomes for n=5    
* AdditionalCoding_v006 pre-multiplies frequencies to significantly speed up N-play outcome assessments for n=5    
* AdditionalCoding_v006 creates initial N-play outcome assessments for n=10    

### Expansions week of 19-DEC-2016  
* AdditionalCoding_v006 vectorizes solution for driving 5-play to 10-play  
* AdditionalCoding_v006 assesses 10-play percentiles for 16,000 hands  
* AdditionalCoding_v006 assesses 5-play percentiles for 16,000 hands  
* AdditionalCoding_v006 vectorizes solution for doubling hands (creating 2-play and 4-play)  
* AdditionalCoding_v006 vectorizes combining n-play and m-play while maintaining "by flavor" (up through 5-play)  
* AdditionalCoding_v006 cleans rare hands from matrices and then propagates "pseudo-complete" 6-10 play hands  
* AdditionalCoding_v006 implements the "make own" mean approach for very-low-odds hands  
  
### Expansions week of 26-DEC-2016  
* AdditionalCoding_v006 allows for mxn propagation in a much more memory-friendly manner  
* AdditionalCoding_v006 expands to 10/15 play for JB 96, BP 85, and TDB 96  
* AdditionalCoding_v006 experiments with propagation methodology  
* AdditionalCoding_v006 experiments with the microbenchmark library  
* AdditionalCoding_v006 experiments with two sapply approaches for propagation  
* AdditionalCoding_v006 caches for run-time improvement  
* AdditionalCoding_v006 runs the JB 95 pay table to generate all key N-play data (1/3/5/10)  

### Expansions week of 02-JAN-2017  
* AdditionalCoding_v006 includes function genGame() to convert pay tables to key files and statistics; run for JB 95 and BP 75  
* AdditionalCoding_v006 assesses combinations of nPlay and nDeal for JB 95  
* AdditionalCoding_v006 creates functional form for game simulation and applies to JB 95, BP 75, and DDB 96  
* AdditionalCoding_v006 assesses all-time minima for 5-play at 8k hands (JB 95, DDB 96)  
* AdditionalCoding_v006 assesses risk-of-ruin for 5-play JB 95 (8k, 2k deals)  
* AdditionalCoding_v006 assesses risk-of-ruin for 10-play JB 95 (8k, 2k deals)  
* AdditionalCoding_v006 converts to functional form for assessing minima, risk-of-ruin, and final results  
  
### Expansions week of 09-JAN-2017  
* AdditionalCoding_v006 assesses survival curves for JB 95 5-play by starting units  
* AdditionalCoding_v006 creates functional form for survival curves and assesses JB 95 10/5/3/1-play  
* AdditionalCoding_v006 assesses survival curves for BP 75 and DDB 96 (10/5/3/1-play)  
* AdditionalCoding_v006 makes initial integration of STP (JB 95 5-play)  
* AdditionalCoding_v006 assesses "very-low" (JB 85, JB 75, BP 65, DDB 95, DDB 85) games for 12k deals of 10-play  
* AdditionalCoding_v006 assesses several variants of 12k deals of JB and DDB  
* AdditionalCoding_v006 compares multiple game types given a common target and starting conditions  
  
### Expansions week of 16-JAN-2017  
* AdditionalCoding_v006 compares a higher deal target for multiple games with the same starting conditions  
* DataCamp_Insights_v001 captures ggplot2 part3 Choropleths (Maps and Animations)  
* AdditionalCoding_v006 compares a very high deal target for multiple games with the same starting conditions  
* AdditionalCoding_v006 calculates percentiles for units required by # deals  
* AdditionalCoding_v006 assesses 10-p DDB 95 STP  
* AdditionalCoding_v006 fixes legends for grUnitsDeals()  
* AdditionalCoding_v006 assesses 10-p BP 75 STP  
  
### Expansions week of 23-JAN-2017  
* AdditionalCoding_v006 assesses 1-p for JB 95, BP 75, DDB 95, and DDB 96  
* DataCamp_Insights_v001 captures ggplot2 part4 ggplot2 internals (first two portions)  
* DataCamp_Insights_v001 captures ggplot2 part4 ggplot2 internals (remaining two portions)  
* DataCamp_Insights_v001 captures ggplot2 part5 Munging and Visualization (bag plot)  
* DataCamp_Insights_v001 captures ggplot2 part5 Munging and Visualization (case studies)  
* AdditionalCoding_v006 initial pass at replicating grUnitsDeals() using ggplot2  
* AdditionalCoding_v006 replicates legend for grUnitsDeals() using ggplot2/grid  
  
### Expansions week of 30-JAN-2017  
* AdditionalCoding_v006 initial pass at grUnitsDealsGG() as ggplot2/grid version of grUnitsDeals()  
* AdditionalCoding_v006 modifies grUnitsDealsGG() to be slightly more flexible  
* AdditionalCoding_v006 explores/refines grid.text() and grid.rect() in grUnitsDealsGG()  
* AdditionalCoding_v006 explores fonts (extrafont, fontfamily, windowsFonts, etc.) and additional games  
  

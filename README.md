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
* AdditionalCoding_v006 combines two games in to a single overall minimum/results  
* AdditionalCoding_v006 creates function cmbTwoGames() for combining two games  
* AdditionalCoding_v006 runs cmbTwoGames() using random seeds to hold the first 24k deals constant across scenarios  
  
### Expansions week of 06-FEB-2017  
* DataCamp_Insights_v001 captures Data Visualization in R - Chapter 1 (Overview)  
* DataCamp_Insights_v001 captures Data Visualization in R - Chapter 2 (Plot Types)  
* DataCamp_Insights_v001 captures Data Visualization in R - Chapter 3 (Adding Details) and Chapter 4 (Too Much)  
* DataCamp_Insights_v001 captures Data Visualization in R - Chapter 5 (Customization)  
* DataCamp_Insights_v001 captures Geospatial Data - Chapter 1 (Basic Mapping) and Chapter 2 (Point and Polygon Data)  
* DataCamp_Insights_v001 creates a dummy countries_spdf for a few examples from Chapter 1  
* DataCamp_Insights_v001 attempts orphaned holes fix with maptools::checkPolygonsHoles()  
    
### Expansions week of 13-FEB-2017  
* DataCamp_Insights_v001 captures Geospatial Data - Chapter 3 (Raster Data and Color)  
* DataCamp_Insights_v001 captures Geospatial Data - Chapter 4 (Data Import and Projections)  
* Cleaned DataCamp_Insights_v001, then split in to DataCamp_Visualization_v002 and DataCamp_Insights_v002 (components other than Visualization)  
* Created DataCamp_ImportClean_v002 and removed those sections from DataCamp_Insights_v002   
* DataCamp_Insights_v002 captures Object Oriented Programming in R - Chapter 1 (Introduction) and Chapter 2 (Using S3)  
* DataCamp_Insights_v002 captures Object Oriented Programming in R - Chapter 3 (Using R6)  
* DataCamp_Insights_v002 captures Object Oriented Programming in R - Chapter 4 (R6 Inheritance) and Chapter 5 (Advanced R6 Usage)  
  
### Expansions week of 20-FEB-2017  
* DataCamp_Insights_v002 captures Joining Data with dplyr - Chapter 1 (Mutating Joins)  
* DataCamp_Insights_v002 captures Joining Data with dplyr - Chapter 2 (Filtering Joins, Set Operations) and Chapter 3 (Assembling Data)  
* DataCamp_Insights_v002 captures Joining Data with dplyr - Chapter 4 (Advanced Joining)  
* DataCamp_Insights_v002 captures Joining Data with dplyr - Chapter 5 (Case Study)  
* DataCamp_Insights_v002 captures Exploratory Data Analysis: Case Study - Chapter 1 (Data Cleaning) and Chapter 2 (Visualization)  
* DataCamp_Insights_v002 captures Exploratory Data Analysis: Case Study - Chapter 3 (Tidy Modeling with Broom)  
* DataCamp_Insights_v002 captures Exploratory Data Analysis: Case Study - Chapter 4 (Joining and Tidying)  
  
### Expansions week of 27-FEB-2017  
* DataCamp_Insights_v002 captures Manipulating Time Series Data: Case Study - Chapter 1 (Flight Data)  
* DataCamp_Insights_v002 captures Manipulating Time Series Data: Case Study - Chapter 2 (Weather Data), Chapter 3 (Economic Data), and Chapter 4 (Sports Data)  
* DataCamp_Insights_v002 captures Exploring Pitch Data in R: Chapter 1 (Pitch Velocities) and Chapter 2 (Pitch Types)  
* DataCamp_Insights_v002 captures Exploring Pitch Data in R: Chapter 3 (Pitch Locations) and Chapter 4 (Batted Ball Outcomes)  
* DataCamp_Insights_v002 captures Introduction to Data: Chapter 1 (Language) and Chapter 2 (Study Types and Cautions)  
* DataCamp_Insights_v002 captures Introduction to Data: Chapter 3 (Sampling Strategies and Experimental Design) and Chapter 4 (Case Study)  
* DataCamp_Insights_v002 captures Exploratory Data Analysis: Chapter 1 (Categorical Data) and Chapter 2 (Numerical Data)  
  
### Expansions week of 06-MAR-2017  
* DataCamp_Insights_v002 captures Exploratory Data Analysis: Chapter 3 (Numerical Studies) and Chapter 4 (Case Study)  
* DataCamp_Insights_v002 captures Foundations of Inference: Chapter 1 (Ideas of Inference) and Chapter 2 (Randomization Studies)  
* DataCamp_Insights_v002 captures Foundations of Inference: Chapter 3 (Hypothesis Testing Errors)  
* DataCamp_Insights_v002 captures Foundations of Inference: Chapter 4 (Confidence Intervals)  
* DataCamp_Insights_v002 captures Correlation and Regression: Chapter 1 (Correlation and Regression)  
* DataCamp_Insights_v002 captures Correlation and Regression: Chapter 2 (Correlation)  
* DataCamp_Insights_v002 captures Correlation and Regression: Chapter 3 (Simple Linear Regression)  
  
### Expansions week of 13-MAR-2017  
* DataCamp_Insights_v002 captures Correlation and Regression: Chapter 4 (Interpreting Regression Models)  
* DataCamp_Insights_v002 captures Correlation and Regression: Chapter 5 (Model Fit)  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part I): Chapter 1 (What is Statistical Modeling?)  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part I): Chapter 2 (Designing and Training Models)  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part I): Chapter 3 (Assessing Prediction Performance)  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part I): Chapter 4 (Exploring Data with Models)  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part I): Chapter 5 (Covariates and Effect Sizes)  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part II): Chapter 1 (Effect Size and Interaction) 

### Expansions week of 20-MAR-2017  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part II): Chapter 2 (Total and Partial Change)  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part II): Chapter 3 (Sampling Variability)  
* DataCamp_Insights_v002 captures Statistical Modeling in R (Part II): Chapter 4 (Variables Working Together)  
* DataCamp_Insights_v002 captures Introduction to Time Series Analysis: Chapter 1 (Exploratory Time Series Data Analysis)  
* DataCamp_Insights_v002 captures Introduction to Time Series Analysis: Chapter 2 (Predicting the Future)  
* DataCamp_Insights_v002 captures Introduction to Time Series Analysis: Chapter 3 (Correlation Analysis)  
* DataCamp_Insights_v002 captures Introduction to Time Series Analysis: Chapter 4 (Autoregression)  
  
### Expansions week of 27-MAR-2017  
* DataCamp_Insights_v002 captures Introduction to Time Series Analysis: Chapter 5 (Simple Moving Average)  
* DataCamp_Insights_v002 captures ARIMA Modeling with R: Chapter 1 (Time Series Data and Models)  
* DataCamp_Insights_v002 captures ARIMA Modeling with R: Chapter 2 (Fitting ARMA Models)  
* DataCamp_Insights_v002 captures ARIMA Modeling with R: Chapter 3 (ARIMA Models)  
* DataCamp_Insights_v002 captures ARIMA Modeling with R: Chapter 4 (Seasonal ARIMA)  
* DataCamp_Insights_v002 captures Beginning Bayes in R: Chapter 1 (Introduction to Bayesian Thinking)  
* DataCamp_Insights_v002 captures Beginning Bayes in R: Chapter 2 (Binomial Probability)  
  
### Expansions week of 03-APR-2017  
* DataCamp_Insights_v002 captures Beginning Bayes in R: Chapter 3 (Normal Mean)  
* DataCamp_Insights_v002 captures Beginning Bayes in R: Chapter 4 (Bayesian Comparisons)  
* DataCamp_Insights_v002 captures Introduction to Machine Learning: Chapter 1 (What is Machine Learning?)  
* DataCamp_Insights_v002 captures Introduction to Machine Learning: Chapter 2 (Performance Measures)  
* DataCamp_Insights_v002 captures Introduction to Machine Learning: Chapter 3 (Classification)  
* DataCamp_Insights_v002 captures Introduction to Machine Learning: Chapter 4 (Regression)  
* DataCamp_Insights_v002 captures Introduction to Machine Learning: Chapter 5 (Clustering)  
  
### Expansions week of 10-APR-2017  
* DataCamp_Insights_v002 captures Unsupervised Learning in R: Chapter 1 (Unsupervised Learning)  
* DataCamp_Insights_v002 captures Unsupervised Learning in R: Chapter 2 (Hierarchical Clustering)  
* DataCamp_Insights_v002 captures Unsupervised Learning in R: Chapter 3 (Dimensionality Reduction with PCA)  
* DataCamp_Insights_v002 captures Unsupervised Learning in R: Chapter 4 (Case Study)  
* DataCamp_Insights_v002 captures Machine Learning Toolbox: Chapter 1 (Regression Models)  
* DataCamp_Insights_v002 captures Machine Learning Toolbox: Chapter 2 (Classification Models)  
* DataCamp_Insights_v002 captures Machine Learning Toolbox: Chapter 3 (Tuning Model Parameters)  
  
### Expansions week of 17-APR-2017  
* DataCamp_Insights_v002 captures Machine Learning Toolbox: Chapter 4 (Pre-Processing Data)  
* DataCamp_Insights_v002 captures Machine Learning Toolbox: Chapter 5 (Case Study)  
* DataCamp_Insights_v002 captures RStudio IDE - Part I: Chapter 1 (Orientation)  
* DataCamp_Insights_v002 captures RStudio IDE - Part I: Chapter 2 (Programming)  
* DataCamp_Insights_v002 captures RStudio IDE - Part I: Chapter 3 (Packages)  
* Cleaned DataCamp_Insights_v002 with updated introductory bullets for the contents  
* DataCamp_Insights_v002 captures RStudio IDE - Part II: Chapter 1 (Packages)  

### Expansions week of 24-APR-2017  
* DataCamp_Insights_v002 captures RStudio IDE - Part II: Chapter 2 (Version Control)  
* DataCamp_Insights_v002 captures RStudio IDE - Part II: Chapter 3 (Reporting)  
* Created stand-alone, archived DataCamp_DataManipulation_v002 and DataCamp_Statistics_v002 from DataCamp_Insights_v002  
* Created DataCamp_Insights_v003 (contains portions of DataCamp_Insights_v002 other than Data Manipulation and Statistics)  
* DataCamp_Insights_v003 captures Reporting with R Markdown: Chapter 1 (Authoring R Markdown Reports)  
* DataCamp_Insights_v003 captures in-line R usage example from Reporting with R Markdown: Chapter 1 (Authoring R Markdown Reports)  
* DataCamp_Insights_v003 captures Reporting with R Markdown: Chapter 2 (Embedding Code)  
  
### Expansions week of 01-MAY-2017  
* DataCamp_Insights_v003 captures in-line R usage example from Reporting with R Markdown: Chapter 2 (Embedding Code)  
* DataCamp_Insights_v003 captures additional in-line R usage example from Reporting with R Markdown: Chapter 2 (Embedding Code)  
* DataCamp_Insights_v003 captures Reporting with R Markdown: Chapter 3 (Compiling Reports)  
* DataCamp_Insights_v003 captures Reporting with R Markdown: Chapter 4 (Configuring R Markdown)  
* Now using R 3.3.0 which allows for loading packages qdap and slam  
* DataCamp_Insights_v003 captures Text Mining: Bag of Words - Chapter 1 (Jumping In)  
* DataCamp_Insights_v003 includes coffee tweets examples from Text Mining: Bag of Words - Chapter 1 (Jumping In)  
* DataCamp_Insights_v003 captures Text Mining: Bag of Words - Chapter 2 (Word Clouds and Visuals)  
  
### Expansions week of 08-MAY-2017  
* DataCamp_Insights_v003 captures chardonnay tweets exampled from Text Mining: Bag of Words - Chapter 2 (Word Clouds and Visuals)  
* DataCamp_Insights_v003 captures Text Mining: Bag of Words - Chapter 3 (Additional Text Mining Skills - library tm)  
* DataCamp_Insights_v003 captures Text Mining: Bag of Words - Chapter 4 (Case Study)  
* DataCamp_PythonNotes_v001 created to test running simple Python code within an R Markdown document  
* DataCamp_PythonNotes_v001 captures Intro to Python for Data Science - Chapter 1 (Python Basics)  
* DataCamp_PythonNotes_v001 captures Intro to Python for Data Science - Chapter 2 (Lists)  
* DataCamp_PythonNotes_v001 captures Intro to Python for Data Science - Chapter 3 (Functions and Packages)  
* DataCamp_PythonNotes_v001 captures Intro to Python for Data Science - Chapter 4 (Numpy)  

### Expansions week of 15-MAY-2017  
* DataCamp_PythonNotes_v001 captures Intermediate Python for Data Science - Chapter 1 (Matplotlib for Data Visualization)  
* DataCamp_PythonNotes_v001 explores saving PNG using matplotlib.pyplot.savefig() and displaying them in an R Markdown document  
* DataCamp_PythonNotes_v001 captures Intermediate Python for Data Science - Chapter 2 (Dictionaries and Pandas)  
* DataCamp_PythonNotes_v001 captures Intermediate Python for Data Science - Chapter 3 (Logic, Control Flow, and Filtering)  
* DataCamp_PythonNotes_v001 captures Intermediate Python for Data Science - Chapter 4 (Loops)  
* DataCamp_PythonNotes_v001 captures Intermediate Python for Data Science - Chapter 5 (Case Study)  
* DataCamp_PythonNotes_v001 captures Python Data Science Toolbox (Part I) - Chapter 1 (Writing Functions)  
  
### Expansions week of 22-MAY-2017  
* DataCamp_PythonNotes_v001 captures Python Data Science Toolbox (Part I) - Chapter 2 (Functions: Default and Variable-Length Arguments)  
* DataCamp_PythonNotes_v001 captures Python Data Science Toolbox (Part I) - Chapter 3 (Lambda Functions and Error Handling)  
* DataCamp_PythonNotes_v001 captures Python Data Science Toolbox (Part II) - Chapter 1 (Iterators and Iterables)  
* DataCamp_PythonNotes_v001 captures Python Data Science Toolbox (Part II) - Chapter 2 (List Comprehensions and Generators)  
* DataCamp_PythonNotes_v001 captures Python Data Science Toolbox (Part II) - Chapter 3 (Case Study)  
* DataCamp_PythonNotes_v001 includes graphs for Python Data Science Toolbox (Part II) - Chapter 3 (Case Study)  
* DataCamp_PythonNotes_v001 captures Network Analysis in Python (Part I) - Chapter 1 (Introduction to Networks)  

### Expansions week of 29-MAY-2017  
* DataCamp_PythonNotes_v001 captures Network Analysis in Python (Part I) - Chapter 2 (Important Nodes)  
* DataCamp_PythonNotes_v001 captures Network Analysis in Python (Part I) - Chapter 3 (Structures)  
* DataCamp_PythonNotes_v001 captures Network Analysis in Python (Part I) - Chapter 4 (Case Study)  
* DataCamp_PythonNotes_v001 captures plots for Network Analysis in Python (Part I)  
* DataCamp_PythonNotes_v001 captures Importing Data in Python (Part I) - Chapter 1 (Introduction and Flat Files)  
* DataCamp_PythonNotes_v001 captures Importing Data in Python (Part I) - Chapter 2 (Importing Data from Other File Types)  
* DataCamp_PythonNotes_v001 captures Importing Data in Python (Part I) - Chapter 3 (Relational Databases)  
    
### Expansions week of 05-JUN-2017  
* DataCamp_PythonNotes_v001 captures Importing Data in Python (Part II) - Chapter 1 (Importing Data from the Internet)  
* DataCamp_PythonNotes_v001 captures Importing Data in Python (Part II) - Chapter 2 (Interacting with API)  
* DataCamp_PythonNotes_v001 captures Importing Data in Python (Part II) - Chapter 3 (Twitter API)  
* DataCamp_PythonNotes_v001 captures Cleaning Data in Python - Chapter 1 (Exploring Your Data)  
* DataCamp_PythonNotes_v001 captures Cleaning Data in Python - Chapter 2 (Tidying Data for Analysis)  
* DataCamp_PythonNotes_v001 captures Cleaning Data in Python - Chapter 3 (Combining Data for Analysis)  
* DataCamp_PythonNotes_v001 captures Cleaning Data in Python - Chapter 4 (Cleaning Data for Analysis)  
    
### Expansions week of 12-JUN-2017  
* DataCamp_PythonNotes_v001 captures Cleaning Data in Python - Chapter 5 (Case Study)  
* DataCamp_PythonNotes_v001 captures pandas Foundations - Chapter 1 (Data Ingestion and Inspection)  
* DataCamp_PythonNotes_v001 includes all graphs and captures KAUS METAR data for 2010-08  
* DataCamp_PythonNotes_v001 captures pandas Foundations - Chapter 2 (Exploratory Data Analysis)  
* DataCamp_PythonNotes_v001 captures graphs and additional examples for Chapter 2  
* DataCamp_PythonNotes_v001 captures weather component for pandas Foundations - Chapter 3 (Time Series in pandas)  
* DataCamp_PythonNotes_v001 captures graphs for pandas Foundations - Chapter 3 (Time Series in pandas)  
  
### Expansions week of 19-JUN-2017  
* DataCamp_PythonNotes_v001 captures flights time-series for pandas Foundations - Chapter 3 (Time Series in pandas)  
* DataCamp_PythonNotes_v001 captures 2011 component for pandas Foundations - Chapter 4 (Case Study - Sunlight in Austin)  
* DataCamp_PythonNotes_v001 captures graphs for pandas Foundations - Chapter 4 (Case Study - Sunlight in Austin)  
* DataCamp_PythonNotes_v001 captures Manipulating DataFrames with pandas - Chapter 1 (Extract and Transform Data)  
* DataCamp_PythonNotes_v001 captures Manipulating DataFrames with pandas - Chapter 2 (Advanced Indexing)  
* DataCamp_PythonNotes_v001 captures Manipulating DataFrames with pandas - Chapter 3 (Rearrange and Reshape Data)  
* DataCamp_PythonNotes_v001 captures Manipulating DataFrames with pandas - Chapter 4 (Grouping Data)  
  
### Expansions week of 26-JUN-2017  
* DataCamp_PythonNotes_v001 captures Manipulating DataFrames with pandas - Chapter 5 (Case Study - Summer Olympics Medals)  
* DataCamp_PythonNotes_v001 captures Merging DataFrames with pandas - Chapter 1 (Preparing Data)  
* DataCamp_PythonNotes_v001 captures additional examples from Merging DataFrames with pandas - Chapter 1 (Preparing Data)  
* DataCamp_PythonNotes_v001 captures Merging DataFrames with pandas - Chapter 2 (Concatenating Data)  
* DataCamp_PythonNotes_v001 captures Merging DataFrames with pandas - Chapter 3 (Merging Data)  
* DataCamp_PythonNotes_v001 captures Merging DataFrames with pandas - Chapter 4 (Case Study)  
* DataCamp_PythonNotes_v001 adds missing plots  
  
### Expansions week of 03-JUL-2017  
* DataCamp_PythonNotes_v001 captures Introduction to Databases in Python - Chapter 1 (Basics of Relational Databases)  
* DataCamp_PythonNotes_v001 captures Introduction to Databases in Python - Chapter 2 (Filtering, Ordering, etc.)  
* DataCamp_PythonNotes_v001 captures Introduction to Databases in Python - Chapter 3 (Advanced SQL Alchemy)  
* DataCamp_PythonNotes_v001 captures Introduction to Databases in Python - Chapter 4 (Create and Manipulate Databases)  
* DataCamp_PythonNotes_v001 captures Introduction to Databases in Python - Chapter 5 (Case Study)  
* DataCamp_PythonNotes_v001 captures Introduction to Databases in Python - added graphs  
* DataCamp_PythonNotes_v001 captures Network Analysis in Python (Part II) - Chapter 1 (Bipartite Graphs and Recommendation Systems)    

### Expansions week of 10-JUL-2017  
* DataCamp_PythonNotes_v001 captures Network Analysis in Python (Part II) - Chapter 2 (Graph Projections)  
* DataCamp_PythonNotes_v001 captures Network Analysis in Python (Part II) - Chapter 3 (Comparing Graphs and Time Dynamics)  
* DataCamp_PythonNotes_v001 captures Network Analysis in Python (Part II) - Chapter 4 (Case Study)  
* DataCamp_PythonNotes_v001 captures graphs for Network Analysis in Python (Part II)  
* DataCamp_PythonNotes_v001 includes outline and updated module summary ordering  
* DataCamp_PythonNotes_v001 captures Python Data Types for Data Science - Chapter 1 (Fundamental Data Types)  
* DataCamp_PythonNotes_v001 captures Python Data Types for Data Science - Chapter 2 (Dictionaries)  
  
### Expansions week of 17-JUL-2017  
* DataCamp_PythonNotes_v001 captures Python Data Types for Data Science - Chapter 3 (Collections Module)  
* DataCamp_PythonNotes_v001 captures Python Data Types for Data Science - Chapter 4 (Handling Dates and Times)  
* DataCamp_PythonNotes_v001 captures Python Data Types for Data Science - Chapter 5 (Case Study)  
* DataCamp_PythonNotes_v001 captures additional CTA data exploration (rides per month, rides by daytype)  
* DataCamp_PythonNotes_v001 captures additional CTA data exploration (rides by station and daytype)  
* DataCamp_PythonNotes_v001 captures additional CTA data exploration (relative popularity by station and daytype)  
* DataCamp_PythonNotes_v001 captures additional CTA data exploration (consistency and inconsistency by station and daytype)  
  
### Expansions week of 24-JUL-2017  
* DataCamp_PythonNotes_v001 captures additional CTA data exploration (seasonality by station)  
* DataCamp_PythonNotes_v001 captures additional CTA data exploration (updated axis labels, titles, legends)  
* DataCamp_PythonNotes_v001 captures additional CTA data exploration (weekday patterns by day of week)  
* DataCamp_PythonNotes_v001 captures additional CTA data exploration (saved plots as figures)  
* DataCamp_PythonNotes_v001 captures additional Chicago crime data exploration (trends by day and month, types by district)  
* DataCamp_PythonNotes_v001 captures additional Chicago crime data exploration (clearance rate and # cleared by crime type)  
* DataCamp_PythonNotes_v001 captures additional Chicago crime data exploration (crime by location description and district)  
  
### Expansions week of 31-JUL-2017  
* DataCamp_PythonNotes_v001 captures additional Chicago crime data exploration (saved plots as figures)  
* DataCamp_PythonNotes_v001 saved for a final time  
* DataCamp_Python_ImportClean_v002 created as a stand-alone version of import and clean data  
* DataCamp_Python_Programming_v002 created as a stand-alone version of programming  
* DataCamp_PythonNotes_v002 created for additional usage  
* DataCamp_PythonNotes_v002 captures Introduction to Python Visualization - Chapter 1 (Data Ingestion and Inspection)  
* DataCamp_PythonNotes_v002 - plots as figures for Introduction to Python Visualization - Chapter 1 (Data Ingestion and Inspection)  
* DataCamp_PythonNotes_v002 captures Introduction to Python Visualization - Chapter 2 (Plotting 2D Arrays - Raster Data)  
* DataCamp_PythonNotes_v002 - uses Anaconda3 Python engine in the most recent chunk for image reading and saving  
* DataCamp_PythonNotes_v002 captures Introduction to Python Visualization - Chapter 3 (Seaborn)  
  
### Expansions week of 07-AUG-2017  
* DataCamp_PythonNotes_v002 - plots as figures for Introduction to Python Visualization - Chapter 3 (Seaborn)  
* DataCamp_PythonNotes_v002 captures Introduction to Python Visualization - Chapter 4 (Time Series) - text notes  
* DataCamp_PythonNotes_v002 captures Introduction to Python Visualization - Chapter 4 (Time Series) - stock plots  
* DataCamp_PythonNotes_v002 captures Introduction to Python Visualization - Chapter 4 (Time Series) - image processing  
* DataCamp_PythonNotes_v002 - plots as figures for Introduction to Python Visualization - Chapter 4 (Time Series)  
* DataCamp_PythonNotes_v002 captures Interactive Data Visualization with Bokeh - Chapter 1 (Basic Plotting with Bokeh) - text notes  
* DataCamp_PythonNotes_v002 captures Interactive Data Visualization with Bokeh - Chapter 1 (Basic Plotting with Bokeh) - coding  
  
### Expansions week of 14-AUG-2017  
* DataCamp_PythonNotes_v002 captures Interactive Data Visualization with Bokeh - Chapter 2 (Layouts, Annotations, Interactions) - text notes  
* DataCamp_PythonNotes_v002 captures Interactive Data Visualization with Bokeh - Chapter 2 (Layouts, Annotations, Interactions) - coding  
* DataCamp_PythonNotes_v002 captures Interactive Data Visualization with Bokeh - Chapter 3 (High-Level Charts)  
* DataCamp_PythonNotes_v002 captures Interactive Data Visualization with Bokeh - Chapter 4 (Building Interactivity)  
* DataCamp_PythonNotes_v002 captures Interactive Data Visualization with Bokeh - Chapter 5 (Case Study)  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part I) - Chapter 1 (Graphical EDA)  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part I) - Chapter 2 (Quantitative EDA)  
  
### Expansions week of 21-AUG-2017  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part I) - Chapter 3 (Discrete Variables)  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part I) - Chapter 4 (Continuous Variables)  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part I) - plots as figures for Chapters 1/2  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part I) - plots as figures for Chapters 3/4  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part II) - Chapter 1 (Parameter Estimation by Optimization)  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part II) - Chapter 2 (Bootstrap Confidence Intervals)  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part II) - plots as figures for Chapters 1/2  
  
### Expansions week of 28-AUG-2017  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part II) - Chapter 3 (Hypothesis Testing)  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part II) - Chapter 4 (Hypothesis Testing Examples)  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part II) - plots as figures for Chapters 3/4  
* DataCamp_PythonNotes_v002 captures Statistical Thinking in Python (Part II) - Chapter 5 (Case Study)  
* DataCamp_PythonNotes_v002 updated for accurate outline and introduction  
* DataCamp_Python_DataManipulation_v003 created as a stand-alone version of data manipulation  
* DataCamp_Python_Visualization_v003 created as a stand-alone version of visualization  
* DataCamp_PythonNotes_v003 created for additional usage  
  
### Expansions week of 04-SEP-2017  
* DataCamp_PythonNotes_v003 captures Supervised Learning with scikit-learn - Chapter 1 (Classification)  
* DataCamp_PythonNotes_v003 captures Supervised Learning with scikit-learn - Chapter 2 (Regression)  
* DataCamp_PythonNotes_v003 captures Supervised Learning with scikit-learn - plots as figures for Chapters 1/2  
* DataCamp_PythonNotes_v003 captures Supervised Learning with scikit-learn - Chapter 3 (Model Fine-Tuning)  
* DataCamp_PythonNotes_v003 captures Supervised Learning with scikit-learn - Chapter 4 (Pre-Processing and Pipelines)  
* DataCamp_PythonNotes_v003 captures Supervised Learning with scikit-learn - plots as figures for Chapters 3/4  
* DataCamp_PythonNotes_v003 captures Unsupervised Learning in Python - Chapter 1 (Clustering for Dataset Exploration)  
  
### Expansions week of 11-SEP-2017  
* DataCamp_PythonNotes_v003 captures Unsupervised Learning in Python - Chapter 2 (Visualize with Hierarchical Clustering and t-SNE)  
* DataCamp_PythonNotes_v003 captures Unsupervised Learning in Python - plots as figures for Chapters 1/2  
* DataCamp_PythonNotes_v003 captures Unsupervised Learning in Python - Chapter 3 (Decorrelating Data and Dimension Reduction)  
* DataCamp_PythonNotes_v003 captures Unsupervised Learning in Python - Chapter 4 (Discovering Interpretable Features)  
* DataCamp_PythonNotes_v003 captures Unsupervised Learning in Python - plots as figures for Chapters 3/4  
* DataCamp_PythonNotes_v003 captures Deep Learning in Python - Chapter 1 (Basics of Deep Learning and Neural Networks)  
* DataCamp_PythonNotes_v003 captures Deep Learning in Python - Chapter 2 (Optimizing with Backward Propagation)  
  
### Expansions week of 18-SEP-2017  
* DataCamp_PythonNotes_v003 captures Deep Learning in Python - plots as figures for Chapters 1/2  
* DataCamp_PythonNotes_v003 captures Deep Learning in Python - Chapter 3 (Building Deep Learning Modules)  
* DataCamp_PythonNotes_v003 captures Deep Learning in Python - Chapter 4 (Fine-Tuning keras Modules)  
* DataCamp_PythonNotes_v003 captures Machine Learning: School Budgets - Chapter 1 (Exploring Raw Data)  
* DataCamp_PythonNotes_v003 captures Machine Learning: School Budgets - Chapter 2 (Create Simple First Model)  
* DataCamp_PythonNotes_v003 captures Deep Learning in Python - Chapter 3 (Building Deep Learning Modules) with keras installed  
* DataCamp_PythonNotes_v003 captures Machine Learning: School Budgets - Chapter 3 (Improving Models)  
* DataCamp_PythonNotes_v003 captures Machine Learning: School Budgets - Chapter 4 (Learning from Experts)  
  
### Expansions week of 25-SEP-2017  
* DataCamp_PythonNotes_v003 captures Deep Learning in Python - Chapter 4 (Fine-Tuning keras Modules) with keras installed  
* DataCamp_PythonNotes_v003 captures Deep Learning in Python - plots as figures for Chapters 3/4  
* DataCamp_PythonNotes_v003 captures NLP Fundamentals in Python - Chapter 1 (Regular Expressions and Word Tokenization)  
* DataCamp_PythonNotes_v003 captures NLP Fundamentals in Python - Chapter 2 (Simple Topic Identification)  
* DataCamp_PythonNotes_v003 captures NLP Fundamentals in Python - plots as figures for Chapters 1/2  
* DataCamp_PythonNotes_v003 captures NLP Fundamentals in Python - Chapter 3 (Named Entity Recognition)  
* DataCamp_PythonNotes_v003 captures NLP Fundamentals in Python - notes for Chapter 4 (Fake News Classifier)  
  
### Expansions week of 02-OCT-2017  
* DataCamp_PythonNotes_v003 captures NLP Fundamentals in Python - sample data for Chapter 4 (Fake News Classifier)  
* DataCamp_PythonNotes_v003 captures NLP Fundamentals in Python - plots as figures for Chapters 3/4  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - notes for Chapter 1 (Fish Sleep and Bacteria Growth)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - code for Chapter 1 (Fish Sleep and Bacteria Growth)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - plots for Chapter 1 (Fish Sleep and Bacteria Growth)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - notes for Chapter 2 (2015 FINA Results)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - first half of code for Chapter 2 (2015 FINA Results)  
  
### Expansions week of 09-OCT-2017  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - remainder of code for Chapter 2 (2015 FINA Results)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - plots as figures for Chapter 2 (2015 FINA Results)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - notes for Chapter 3 (Current Controversy - 2013 FINA)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - first code chunk for Chapter 3 (Current Controversy - 2013 FINA)  
  

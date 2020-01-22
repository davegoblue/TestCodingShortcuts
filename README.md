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
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - second code chunk for Chapter 3 (Current Controversy - 2013 FINA)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - plots as figures for Chapter 3 (Current Controversy - 2013 FINA)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - notes for Chapter 4 (Statistical Seismology and the Parkfield Experiment)  
  
### Expansions week of 16-OCT-2017  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - first code chunk for Chapter 4 (Statistical Seismology and the Parkfield Experiment)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - second code chunk for Chapter 4 (Statistical Seismology and the Parkfield Experiment)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - plots as figures for Chapter 4 (Statistical Seismology and the Parkfield Experiment)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - notes for Chapter 5 (Earthquakes and Oil Mining in Oklahoma)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - first code chunk for Chapter 5 (Earthquakes and Oil Mining in Oklahoma)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - second code chunk for Chapter 5 (Earthquakes and Oil Mining in Oklahoma)  
* DataCamp_PythonNotes_v003 captures Case Studies in Statistical Thinking - plots as figures for Chapter 5 (Earthquakes and Oil Mining in Oklahoma)  
  
### Expansions week of 23-OCT-2017  
* DataCamp_Insights_v003 captures R for SAS - Chapter 1 (Introduction)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 2 (Installing and Maintaining R)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 3 (Help and Documentation)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 4 (R Studio Basics)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 5 (Programming Language Basics)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 6 (Data Structures)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 7 (Managing Files and Workspace)  
  
### Expansions week of 30-OCT-2017  
* DataCamp_Insights_v003 captures R for SAS - Chapter 8 (Controlling Functions)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 9 (Data Acquisition)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 10 (Missing Values)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 11 (Selecting Variables)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 12 (Selecting Observations)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 13 (Selecting Variables and Observations)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 14 (Transformations)  
  
### Expansions week of 06-NOV-2017  
* DataCamp_Insights_v003 captures R for SAS - Chapter 15 (Graphics)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 16 (Writing Functions)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 17 (Basic Statistics)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 18 (Correlation and Regression)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 19 (Comparing Groups)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 20 (High Quality Output)  
* DataCamp_Insights_v003 captures R for SAS - Chapter 21 (Ways to Run R)  

### Expansions week of 13-NOV-2017  
* DataCamp_Insights_v003 captures R for SAS - added datasets for Chapter 17 (Basic Statistics)  
* DataCamp_Insights_v003 captures R for SAS - added datasets for Chapter 18 (Correlation and Regression)  
* DataCamp_Insights_v003 captures R for SAS - added datasets for Chapter 19 (Comparing Groups)  
* AdditionalCoding_HCF_v001 created to start looking at a card game (High Card Flush)  
* AdditionalCoding_HCF_v001 makes a simplifying first order approximation to reduce calculations by ~450x  
* AdditionalCoding_HCF_v001 creates all the potential sub-hands of length 1, 2, 3, 4, 5, 6, and 7  
* AdditionalCoding_HCF_v001 creates ranks for the main game for all the potential sub-hands  

### Expansions week of 20-NOV-2017  
* AdditionalCoding_HCF_v001 creates scores for the straight flush side bet for all the potential sub-hands  
* AdditionalCoding_HCF_v001 creates a dummy hand for assessing impact on other possible hands  
* AdditionalCoding_HCF_v001 runs pre-work for cross-hand assessment  
* AdditionalCoding_HCF_v001 creates routine for calculating number of potential hands    
* AdditionalCoding_HCF_v001 runs routine on dummy hand    
* AdditionalCoding_HCF_v001 compares likelihoods of other hand categories based on dummy hand  
* AdditionalCoding_HCF_v001 makes initial assessment of qualification rules  
  
### Expansions week of 27-NOV-2017  
* AdditionalCoding_HCF_v001 compares likelihoods of other hand categories based on multiple dummy hands  
* AdditionalCoding_HCF_v001 compares likelihoods of other hand categories based on additional dummy hands  
* AdditionalCoding_HCF_v001 creates card deck and random draw  
* AdditionalCoding_HCF_v001 creates multiple random draws  
* AdditionalCoding_HCF_v001 scores and ranks a selection of hands  
* AdditionalCoding_HCF_v001 plays out a selection of hand pairs  
* AdditionalCoding_HCF_v001 plays out 10,000 hand pairs  
  
### Expansions week of 04-DEC-2017  
* AdditionalCoding_HCF_v001 plays out 50,000 hand pairs  
* AdditionalCoding_HCF_v001 plays out 100,000 hand pairs  
* AdditionalCoding_HCF_v001 includes proc.time() benchmarking  
* AdditionalCoding_HCF_v001 plays out 500,000 hand pairs  
* AdditionalCoding_HCF_v001 fixes numSF() error and re-plays out 500,000 hand pairs  
* AdditionalCoding_HCF_v001 first pass at alternate hand drawing and scoring methodology  
* AdditionalCoding_HCF_v001 refines alternate hand drawing and scoring methodology  
  
### Expansions week of 11-DEC-2017  
* AdditionalCoding_HCF_v001 runs alternate methodology for 500,000 hand pairs  
* AdditionalCoding_HCF_v001 runs alternate methodology for 5 permutations of 1,000,000 hand pairs  
* AdditionalCoding_HCF_v001 optimizes suit drawing component by ~100x    
* AdditionalCoding_HCF_v001 optimizes hand playing component by ~20x    
* AdditionalCoding_HCF_v001 optimizes hand scoring component by ~5x  
* AdditionalCoding_HCF_v001 adds split/partition capability for hand scoring component  
* AdditionalCoding_HCF_v001 includes initial prep for cross-hand eliminations  
  
### Expansions week of 18-DEC-2017  
* AdditionalCoding_HCF_v001 further optimizes hand scoring component  
* AdditionalCoding_HCF_v001 doubles number of hands played  
* AdditionalCoding_HCF_v001 codifies former hard-coding for suit permutations  
* DataCamp_Insights_v003 captures Introduction to Tidyverse - Chapter 1 (Data Wrangling)  
* DataCamp_Insights_v003 captures Introduction to Tidyverse - Chapter 2 (Data Visualization)  
* DataCamp_Insights_v003 captures Introduction to Tidyverse - Chapter 3 (Grouping and Summarizing)  
* DataCamp_Insights_v003 captures Introduction to Tidyverse - Chapter 4 (Types of Visualizations)  

### Expansions week of 25-DEC-2017  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 1 (Introduction and Shiny Basics) - notes  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 1 (Introduction and Shiny Basics) - code for Shiny App #1  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 1 (Introduction and Shiny Basics) - code for Shiny App #2  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 1 (Introduction and Shiny Basics) - code for Shiny App #3  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - notes  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.1  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.2  

### Expansions week of 01-JAN-2018  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.3  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.4  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.5  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.6  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.7  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.8  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.9  
  
### Expansions week of 08-JAN-2018  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.10  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 2 (Inputs, Outputs, Rendering) - code for Shiny App #2.11  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 3 (Reactive Programming) - notes  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 3 (Reactive Programming) - code for Shiny App #3.1  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 3 (Reactive Programming) - code for Shiny App #3.2  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 3 (Reactive Programming) - code for Shiny App #3.3  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 3 (Reactive Programming) - code for Shiny App #3.4  
  
### Expansions week of 15-JAN-2018  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 3 (Reactive Programming) - code for Shiny App #3.5  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 3 (Reactive Programming) - code for Shiny App #3.6  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 3 (Reactive Programming) - code for Shiny App #3.7  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - notes  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.1  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.2  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.3  
  
### Expansions week of 22-JAN-2018  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.4  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.5  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.6  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.7  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.8  
* DataCamp_Insights_v003 captures Building Web Applications in R with Shiny - Chapter 4 (Customizing Appearance) - code for Shiny App #4.9  
* DataCamp_Insights_v003 captures Writing Efficient R Code - Chapter 1 (Art of Benchmarking)  
  
### Expansions week of 29-JAN-2018  
* DataCamp_Insights_v003 captures Writing Efficient R Code - Chapter 2 (Fine Tuning: Efficient Base R)  
* DataCamp_Insights_v003 captures Writing Efficient R Code - Chapter 3 (Diagnosing Problems: Code Profiling)  
* DataCamp_Insights_v003 captures Writing Efficient R Code - Chapter 4 (Turbo-Charged Code: Parallel Programming)  
* DataCamp_Insights_v003 captures String Manipulation in R with stringr - Chapter 1 (String Basics)  
* DataCamp_Insights_v003 captures String Manipulation in R with stringr - Chapter 2 (Introduction to stringr)  
* DataCamp_Insights_v003 captures String Manipulation in R with stringr - Chapter 3 (Pattern Matching with Regualar Expressions)  
* DataCamp_Insights_v003 captures String Manipulation in R with stringr - Chapter 4 (Advanced Mapping and Manipulation)  
  
### Expansions week of 05-FEB-2018  
* DataCamp_Insights_v003 captures String Manipulation in R with stringr - Chapter 5 (Case Studies)  
* DataCamp_Insights_v003 captures Working with Dates and Times in R - Chapter 1 (Dates and Times in R)  
* DataCamp_Insights_v003 captures Working with Dates and Times in R - Chapter 2 (Parsing and Manipulating Dates with lubridate)  
* DataCamp_Insights_v003 captures Working with Dates and Times in R - Chapter 3 (Arithmetic with Dates and Times)  
* DataCamp_Insights_v003 captures Working with Dates and Times in R - Chapter 4 (Problems in Practice)  
* DataCamp_Insights_v003 captures Scalable Data Processing in R - Chapter 1 (Working with Increasingly Large Datasets)  
* Split DataCamp_Insights_v003 so new components can go in DataCamp_Insights_v003_a to stay under DLL limit  

### Expansions week of 12-FEB-2018  
* DataCamp_Insights_v003_a captures Scalable Data Processing in R - Chapter 2 (Processing and Analyzing Data with bigmemory)  
* DataCamp_Insights_v003_a captures Scalable Data Processing in R - Chapter 3 (Working with iotools)  
* DataCamp_Insights_v003_a captures Scalable Data Processing in R - Chapter 4 (Case Study: Preliminary Analysis of Housing Data)  
* DataCamp_Insights_v003_a captures Working with Web Data in R - Chapter 1 (Downloading Files and Using API Clients)  
* DataCamp_Insights_v003_a captures Working with Web Data in R - Chapter 2 (Using httr to Interact with API Directly)  
* DataCamp_Insights_v003_a captures Working with Web Data in R - Chapter 3 (Handling JSON and XML)  
* DataCamp_Insights_v003_a captures Working with Web Data in R - Chapter 4 (Web Scraping with XPATH)  
  
### Expansions week of 19-FEB-2018  
* DataCamp_Insights_v003_a captures Working with Web Data in R - Chapter 5 (CSS Web Scraping and Final Case Study)  
* DataCamp_Insights_v003_a captures Data Visualization in R with lattice - Chapter 1 (Basic Plotting with lattice)  
* DataCamp_Insights_v003_a captures Data Visualization in R with lattice - Chapter 2 (Conditioning and the Formula Interface)  
* DataCamp_Insights_v003_a captures Data Visualization in R with lattice - Chapter 3 (Controlling Scales and Graphical Parameters)  
* DataCamp_Insights_v003_a captures Data Visualization in R with lattice - Chapter 4 (Customizing Plots Using Panel Functions)  
* DataCamp_Insights_v003_a captures Data Visualization in R with lattice - Chapter 5 (Extensions and the lattice Ecosystem)  
* DataCamp_Insights_v003_a captures Visualizing Time Series Data in R - Chapter 1 (R Time Series Visualization Tools)  
  
### Expansions week of 26-FEB-2018  
* DataCamp_Insights_v003_a captures Visualizing Time Series Data in R - Chapter 2 (Univariate Time Series)  
* DataCamp_Insights_v003_a captures Visualizing Time Series Data in R - Chapter 3 (Multivariate Time Series)  
* DataCamp_Insights_v003_a captures Visualizing Time Series Data in R - Chapter 4 (Case Study: Stock Picking for Portfolios)  
* DataCamp_Insights_v003_a captures Communicating with the Tidyverse - Chapter 1 (Custom ggplot2 Themes)  
* DataCamp_Insights_v003_a captures Communicating with the Tidyverse - Chapter 2 (Creating Custom and Unique Visualization)  
* DataCamp_Insights_v003_a captures Communicating with the Tidyverse - Chapter 3 (Introduction to R Markdown)  
* DataCamp_Insights_v003_a captures Communicating with the Tidyverse - Chapter 4 (Customizing R Markdown Reports)  
  
### Expansions week of 05-MAR-2018  
* DataCamp_Insights_v003_a captures Foundations of Probability in R - Chapter 1 (Binomial Distribution)  
* DataCamp_Insights_v003_a captures Foundations of Probability in R - Chapter 2 (Laws of Probability)  
* DataCamp_Insights_v003_a captures Foundations of Probability in R - Chapter 3 (Bayesian Statistics)  
* DataCamp_Insights_v003_a captures Foundations of Probability in R - Chapter 4 (Related Distributions)  
* DataCamp_Insights_v003_a captures Inference for Numerical Data - Chapter 1 (Bootstrapping for Parameter Estimates)  
* DataCamp_Insights_v003_a captures Inference for Numerical Data - Chapter 2 (Introducing the t-distribution)  
* DataCamp_Insights_v003_a captures Inference for Numerical Data - Chapter 3 (Inference for Difference in Two Parameters)  
  
### Expansions week of 12-MAR-2018  
* DataCamp_Insights_v003_a captures Inference for Numerical Data - Chapter 4 (Comparing Many Means)  
* DataCamp_Insights_v003_a captures Introduction to Statistics with R: Correlation and Linear Regression - Chapter 1 (Introduction to Correlation Coefficients)  
* DataCamp_Insights_v003_a captures Introduction to Statistics with R: Correlation and Linear Regression - Chapter 2 (Introduction to Linear Regression)  
* DataCamp_Insights_v003_a captures Introduction to Statistics with R: Correlation and Linear Regression - Chapter 3 (Linear Regression cont.)  
* DataCamp_Insights_v003_a captures Inference for Linear Regression - Chapter 1 (Inferential Ideas)  
* DataCamp_Insights_v003_a captures Inference for Linear Regression - Chapter 2 (Simulation Based Inference for Slope)  
* DataCamp_Insights_v003_a captures Inference for Linear Regression - Chapter 3 (t-Based Inference for Slope)  
  
### Expansions week of 19-MAR-2018  
* DataCamp_Insights_v003_a captures Inference for Linear Regression - Chapter 4 (Technical Conditions in Linear Regression)  
* DataCamp_Insights_v003_a captures Inference for Linear Regression - Chapter 5 (Building on Inference in Simple Regression)  
* DataCamp_Insights_v003_a captures Multiple and Logistic Regression - Chapter 1 (Parallel Slopes)  
* DataCamp_Insights_v003_a captures Multiple and Logistic Regression - Chapter 2 (Evaluating and Extending Parallel Slopes)  
* DataCamp_Insights_v003_a captures Multiple and Logistic Regression - Chapter 3 (Multiple Regression)  
* DataCamp_Insights_v003_a captures Multiple and Logistic Regression - Chapter 4 (Logistic Regression)  
* DataCamp_Insights_v003_a captures Multiple and Logistic Regression - Chapter 5 (Case Study)  
  
### Expansions week of 26-MAR-2018  
* DataCamp_Insights_v003_a captures Forecasting Using R - Chapter 1 (Exploring and Visualizing Time Series in R)  
* DataCamp_Insights_v003_a captures Forecasting Using R - Chapter 2 (Benchmark Methods and Forecast Accuracy)  
* DataCamp_Insights_v003_a captures Forecasting Using R - Chapter 3 (Exponential Smoothing)  
* DataCamp_Insights_v003_a captures Forecasting Using R - Chapter 4 (Forecasting with ARIMA Models)  
* DataCamp_Insights_v003_a captures Forecasting Using R - Chapter 5 (Advanced Methods)  
* DataCamp_Insights_v003_a captures Network Analysis in R - Chapter 1 (Introduction to Networks)  
* DataCamp_Insights_v003_a captures Network Analysis in R - Chapter 2 (Identifying Important Vertices in a Network)  
  
### Expansions week of 02-APR-2018  
* DataCamp_Insights_v003_a captures Network Analysis in R - Chapter 3 (Characterizing Network Structures)  
* DataCamp_Insights_v003_a captures Network Analysis in R - Chapter 4 (Identifying Special Relationships)  
* DataCamp_Insights_v003_a captures Spatial Statistics in R - Chapter 1 (Introduction)  
* DataCamp_Insights_v003_a captures Spatial Statistics in R - Chapter 2 (Point Pattern Analysis)  
* DataCamp_Insights_v003_a captures Spatial Statistics in R - Chapter 3 (Areal Statistics)  
* DataCamp_Insights_v003_a captures Spatial Statistics in R - Chapter 4 (Geostatistics)  
* Split DataCamp_Insights_v003_a so new components can go in DataCamp_Insights_v003_b to stay under DLL limit  
* DataCamp_Insights_v003_a captures everything prior to Spatial Statistics in R  
* DataCamp_Insights_v003_b captures everything starting with Spatial Statistics in R  
* DataCamp_Insights_v003_b captures Spatial Analysis in R with sf and raster - Chapter 1 (Vector and Raster Spatial Data in R)  
  
### Expansions week of 09-APR-2018  
* DataCamp_Insights_v003_b captures Spatial Analysis in R with sf and raster - Chapter 2 (Preparing Layers for Spatial Analysis)  
* DataCamp_Insights_v003_b captures Spatial Analysis in R with sf and raster - Chapter 3 (Conducting Spatial Analysis)  
* DataCamp_Insights_v003_b captures Spatial Analysis in R with sf and raster - Chapter 4 (Combine Skills in Mini-Analysis)  
* DataCamp_Insights_v003_b captures Sentiment Analysis in R - Chapter 1 (Fast and Dirty - Polarity Scoring)  
* DataCamp_Insights_v003_b captures Sentiment Analysis in R - Chapter 2 (Sentiment Analysis the tidytext Way)  
* DataCamp_Insights_v003_b captures Sentiment Analysis in R - Chapter 3 (Visualizing Sentiment)  
* DataCamp_Insights_v003_b captures Sentiment Analysis in R - Chapter 4 (Case Study: Airbnb)  
  
### Expansions week of 16-APR-2018  
* DataCamp_Insights_v003_b captures Sentiment Analysis in R: The Tidy Way - Chapter 1 (Tweets Across the United States)  
* DataCamp_Insights_v003_b captures Sentiment Analysis in R: The Tidy Way - Chapter 2 (Shakespeare Sentiments)  
* DataCamp_Insights_v003_b captures Sentiment Analysis in R: The Tidy Way - Chapter 3 (Analyzing TV News)  
* DataCamp_Insights_v003_b captures Sentiment Analysis in R: The Tidy Way - Chapter 4 (Happy Songs)  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Regression - Chapter 1 (What is Regression)  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Regression - Chapter 2 (Training and Evaluating Regression Models)  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Regression - Chapter 3 (Issues to Consider)  
  
### Expansions week of 23-APR-2018  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Regression - Chapter 4 (Dealing with Non-Linear Responses)  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Regression - Chapter 5 (Tree Based Models)  
* DataCamp_Insights_v003_b captures Machine Learning with Tree-Based Models in R - Chapter 1 (Classification Trees)  
* DataCamp_Insights_v003_b captures Machine Learning with Tree-Based Models in R - Chapter 2 (Regression Trees)  
* DataCamp_Insights_v003_b captures Machine Learning with Tree-Based Models in R - Chapter 3 (Bagged Trees)  
* DataCamp_Insights_v003_b captures Machine Learning with Tree-Based Models in R - Chapter 4 (Random Forests)  
* DataCamp_Insights_v003_b captures Machine Learning with Tree-Based Models in R - Chapter 5 (Boosted Trees)  
  
### Expansions week of 30-APR-2018  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Classification - Chapter 1 (k-Nearest Neighbors kNN)  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Classification - Chapter 2 (Naive Bayes)  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Classification - Chapter 3 (Logistic Regression)  
* DataCamp_Insights_v003_b captures Supervised Learning in R: Classification - Chapter 4 (Classification Trees)  
* DataCamp_Insights_v003_b captures Introduction to R Using sparklyr - Chapter 1 (Starting Spark with dplyr)  
* DataCamp_Insights_v003_b captures Introduction to R Using sparklyr - Chapter 2 (Advanced dplyr)  
* DataCamp_Insights_v003_b captures Introduction to R Using sparklyr - Chapter 3 (Native Interfaces to Manipulate Spark DataFrames)  
  
### Expansions week of 07-MAY-2018  
* DataCamp_Insights_v003_b captures Introduction to R Using sparklyr - Chapter 4 (Case Study)  
* DataCamp_Insights_v003_b captures Building Web Applications in R with Shiny: Case Studies - Chapter 1 (Shiny Review) - notes  
* DataCamp_Insights_v003_b captures Building Web Applications in R with Shiny: Case Studies - Chapter 1 (Shiny Review) - code  
* DataCamp_Insights_v003_b captures Building Web Applications in R with Shiny: Case Studies - Chapter 2 (Making good plots with Shiny) - notes  
* DataCamp_Insights_v003_b captures Building Web Applications in R with Shiny: Case Studies - Chapter 2 (Making good plots with Shiny) - code  
* DataCamp_Insights_v003_b captures Building Web Applications in R with Shiny: Case Studies - Chapter 3 (Explore datasets interactively with Shiny) - notes  
* DataCamp_Insights_v003_b captures Building Web Applications in R with Shiny: Case Studies - Chapter 3 (Explore datasets interactively with Shiny) - code  
  
### Expansions week of 14-MAY-2018  
* DataCamp_Insights_v003_b captures Building Web Applications in R with Shiny: Case Studies - Chapter 4 (Shiny Word Clouds) - notes  
* DataCamp_Insights_v003_b captures Building Web Applications in R with Shiny: Case Studies - Chapter 4 (Shiny Word Clouds) - code  
* DataCamp_Insights_v003_b captures Introduction to Statistics with R: Moderation and Mediation - Chapter 1 (Moderation)  
* DataCamp_Insights_v003_b captures Introduction to Statistics with R: Moderation and Mediation - Chapter 2 (Introduction to Centering Predictors)  
* DataCamp_Insights_v003_b captures Introduction to Statistics with R: Moderation and Mediation - Chapter 3 (Intro to Mediation)  
* DataCamp_Insights_v003_b captures Introduction to Statistics with R: Multiple Regression - Chapter 1 (Introduction to Multiple Regression)  
* DataCamp_Insights_v003_b captures Introduction to Statistics with R: Multiple Regression - Chapter 2 (Intuition Behind Estimation of Multiple Regression Coefficients)  
  
### Expansions week of 21-MAY-2018  
* DataCamp_Insights_v003_b captures Introduction to Statistics with R: Multiple Regression - Chapter 3 (Dummy Coding)  
* DataCamp_Insights_v003_b captures Cluster Analysis in R - Chapter 1 (Calculating Distance Between Objects)  
* DataCamp_Insights_v003_b captures Cluster Analysis in R - Chapter 2 (Hierarchical Clustering)  
* DataCamp_Insights_v003_b captures Cluster Analysis in R - Chapter 3 (K-Means Clustering)  
* DataCamp_Insights_v003_b captures Cluster Analysis in R - Chapter 4 (Case Study: National Occupation Mean Wage)  
* DataCamp_Insights_v003_b captures Building Dashboards with shinydashboard - Chapter 1 (Building Static Dashboards)  
* DataCamp_Insights_v003_b captures Building Dashboards with shinydashboard - Chapter 2 (Building Dynamic Dashboards)  
  
### Expansions week of 28-MAY-2018  
* DataCamp_Insights_v003_b captures Building Dashboards with shinydashboard - Chapter 3 (Customizing Style)  
* DataCamp_Insights_v003_b captures Building Dashboards with shinydashboard - Chapter 4 (Case Study)  
* Split DataCamp_Insights_v003_b so new components can go in DataCamp_Insights_v003_c to stay under DLL limit  
* DataCamp_Insights_v003_b captures everything prior to Hierarchical and Mixed Effects Models  
* DataCamp_Insights_v003_c captures Hierarchical and Mixed Effects Models - Chapter 1 (Overview and Introduction)  
* DataCamp_Insights_v003_c captures Hierarchical and Mixed Effects Models - Chapter 2 (Linear Mixed Effect Models)  
* DataCamp_Insights_v003_c captures Hierarchical and Mixed Effects Models - Chapter 3 (Generalized Linear Mixed Effect Models)  
* DataCamp_Insights_v003_c captures Hierarchical and Mixed Effects Models - Chapter 4 (Repeated Measures)  
* DataCamp_Insights_v003_c captures Forecasting Product Demand in R - Chapter 1 (Forecasting Demand with Time Series)  
  
### Expansions week of 04-JUN-2018  
* DataCamp_Insights_v003_c captures Forecasting Product Demand in R - Chapter 2 (Components of Demand)  
* DataCamp_Insights_v003_c captures Forecasting Product Demand in R - Chapter 3 (Blending Regression with Time Series)  
* DataCamp_Insights_v003_c captures Forecasting Product Demand in R - Chapter 4 (Hierarchical Forecasting)  
* DataCamp_Insights_v003_c captures HR Analytics in R - Chapter 1 (Identifying Best Recruiting Sources)  
* DataCamp_Insights_v003_c captures HR Analytics in R - Chapter 2 (Drivers of Low Engagement)  
* DataCamp_Insights_v003_c captures HR Analytics in R - Chapter 3 (New Hire Pay)  
* DataCamp_Insights_v003_c captures HR Analytics in R - Chapter 4 (Performance Rating Consistency)  
  
### Expansions week of 11-JUN-2018  
* DataCamp_Insights_v003_c captures HR Analytics in R - Chapter 5 (Employee Safety Data)  
* DataCamp_Insights_v003_c captures Supervised Learning in R: Case Studies - Chapter 1 (Cars Data)  
* DataCamp_Insights_v003_c captures Supervised Learning in R: Case Studies - Chapter 2 (Stack Overflow Developer Data)  
* DataCamp_Insights_v003_c captures Supervised Learning in R: Case Studies - Chapter 3 (Voting)  
* DataCamp_Insights_v003_c captures Supervised Learning in R: Case Studies - Chapter 4 (Nuns)  
* DataCamp_Insights_v003_c captures Business Process Analytics in R - Chapter 1 (Introduction to Process Analysis)  
* DataCamp_Insights_v003_c captures Business Process Analytics in R - Chapter 2 (Analysis Techniques) - notes  
  
### Expansions week of 18-JUN-2018  
* DataCamp_Insights_v003_c captures Business Process Analytics in R - Chapter 2 (Analysis Techniques) - code  
* DataCamp_Insights_v003_c captures Business Process Analytics in R - Chapter 3 (Event Data Processing)  
* DataCamp_Insights_v003_c captures Business Process Analytics in R - Chapter 4 (Case Study)  
* DataCamp_Insights_v003_c captures Network Science in R - A Tidy Approach - Chapter 1 (Hubs of the Network)  
* DataCamp_Insights_v003_c captures Network Science in R - A Tidy Approach - Chapter 2 (Weakness and Strength)  
* DataCamp_Insights_v003_c captures Network Science in R - A Tidy Approach - Chapter 3 (Connection Patterns)  
* DataCamp_Insights_v003_c captures Network Science in R - A Tidy Approach - Chapter 4 (Similarity Clusters)  
  
### Expansions week of 25-JUN-2018  
* DataCamp_Insights_v003_c captures Data Privacy and Anonymization in R - Chapter 1 (Introduction to Data Privacy)  
* DataCamp_Insights_v003_c captures Data Privacy and Anonymization in R - Chapter 2 (Introduction to Differential Privacy)  
* DataCamp_Insights_v003_c captures Data Privacy and Anonymization in R - Chapter 3 (Differentially Private Properties)  
* DataCamp_Insights_v003_c captures Data Privacy and Anonymization in R - Chapter 4 (Differentially Private Data Synthesis)  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Statistical Modeling - Chapter 1 (Modeling Customer Lifetime Value with Linear Regression)  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Statistical Modeling - Chapter 2 (Logistic Regression for Churn Prevention)  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Statistical Modeling - Chapter 3 (Modeling Time to Reorder with Survival Analysis) - notes  
  
### Expansions week of 02-JUL-2018  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Statistical Modeling - Chapter 3 (Modeling Time to Reorder with Survival Analysis) - code  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Statistical Modeling - Chapter 4 (Reducing Dimensionality with Principal Component Analysis) - notes  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Statistical Modeling - Chapter 4 (Reducing Dimensionality with Principal Component Analysis) - code  
* DataCamp_Insights_v003_c captures Interactive Maps with leaflet in R - Chapter 1 (Setting Up Interactive Web Maps)  
* DataCamp_Insights_v003_c captures Interactive Maps with leaflet in R - Chapter 2 (Plotting Points) - notes  
* DataCamp_Insights_v003_c captures Interactive Maps with leaflet in R - Chapter 3 (Groups, Layers, Extras) - notes  
* DataCamp_Insights_v003_c captures Interactive Maps with leaflet in R - Chapter 2 (Plotting Points) - code  
  
### Expansions week of 09-JUL-2018  
* DataCamp_Insights_v003_c captures Interactive Maps with leaflet in R - Chapter 3 (Groups, Layers, Extras) - code  
* DataCamp_Insights_v003_c captures Interactive Maps with leaflet in R - Chapter 4 (Plotting Polygons) - notes  
* DataCamp_Insights_v003_c captures Interactive Maps with leaflet in R - Chapter 4 (Plotting Polygons) - code  
* DataCamp_Insights_v003_c captures Support Vector Machines in R - Chapter 1 (Introduction)  
* DataCamp_Insights_v003_c captures Support Vector Machines in R - Chapter 2 (Support Vector Classifiers - Linear Kernels) - notes  
* DataCamp_Insights_v003_c captures Support Vector Machines in R - Chapter 2 (Support Vector Classifiers - Linear Kernels) - code  
* DataCamp_Insights_v003_c captures Support Vector Machines in R - Chapter 3 (Polynomial Kernels) - notes  
  
### Expansions week of 16-JUL-2018  
* DataCamp_Insights_v003_c captures Support Vector Machines in R - Chapter 3 (Polynomial Kernels) - code  
* DataCamp_Insights_v003_c captures Support Vector Machines in R - Chapter 4 (Radial Basis Kernel Functions)  
* DataCamp_Insights_v003_c captures Experimental Design in R - Chapter 1 (Introduction to Experimental Design)  
* DataCamp_Insights_v003_c captures Experimental Design in R - Chapter 2 (Basic Experiments)  
* DataCamp_Insights_v003_c captures Experimental Design in R - Chapter 3 (Randomized Complete and Balanced Incomplete Block Design)  
* DataCamp_Insights_v003_c captures Experimental Design in R - Ch. 4 (Latin Squares, Graeco-Latin Squares, Factorial Experiments)  
* DataCamp_Insights_v003_c captures Structural Equation Modeling with lavaan in R - Chapter 1 (One-Factor Models) - notes  
  
### Expansions week of 23-JUL-2018  
* DataCamp_Insights_v003_c captures Structural Equation Modeling with lavaan in R - Chapter 1 (One-Factor Models) - code  
* DataCamp_Insights_v003_c captures Structural Equation Modeling with lavaan in R - Chapter 2 (Multi-Factor Models)  
* DataCamp_Insights_v003_c captures Structural Equation Modeling with lavaan in R - Chapter 3 (Troubleshooting Model Errors and Diagrams)  
* DataCamp_Insights_v003_c captures Structural Equation Modeling with lavaan in R - Chapter 4 (Full Example and Extension)  
* DataCamp_Insights_v003_c captures Working with Data in the Tidyverse - Chapter 1 (Explore Data)  
* DataCamp_Insights_v003_c captures Working with Data in the Tidyverse - Chapter 2 (Tame Data)  
* DataCamp_Insights_v003_c captures Working with Data in the Tidyverse - Chapter 3 (Tidy Data)  
  
### Expansions week of 30-JUL-2018  
* DataCamp_Insights_v003_c captures Working with Data in the Tidyverse - Chapter 4 (Transform Data)  
* DataCamp_Insights_v003_c captures Modeling Data in the Tidyverse - Chapter 1 (Introduction to Modeling)  
* DataCamp_Insights_v003_c captures Modeling Data in the Tidyverse - Chapter 2 (Modeling with Regression)  
* DataCamp_Insights_v003_c captures Modeling Data in the Tidyverse - Chapter 3 (Modeling with Multiple Regression)  
* DataCamp_Insights_v003_c captures Modeling Data in the Tidyverse - Chapter 4 (Model Selection and Assessment) - notes  
* DataCamp_Insights_v003_c captures Modeling Data in the Tidyverse - Chapter 4 (Model Selection and Assessment) - code  
* DataCamp_Insights_v003_c captures Analyzing Survey Data in R - Chapter 1 (Introduction to Survey Data) - notes  
  
### Expansions week of 06-AUG-2018  
* DataCamp_Insights_v003_c captures Analyzing Survey Data in R - Chapter 1 (Introduction to Survey Data) - code  
* DataCamp_Insights_v003_c captures Analyzing Survey Data in R - Chapter 2 (Exploring Categorical Data)  
* DataCamp_Insights_v003_c captures Analyzing Survey Data in R - Chapter 3 (Exploring Quantitative Data)  
* DataCamp_Insights_v003_c captures Analyzing Survey Data in R - Chapter 4 (Modeling Quantitative Data)  
* DataCamp_Insights_v003_c captures Inference for Categorical Data - Chapter 1 (Inference for a Single Parameter)  
* DataCamp_Insights_v003_c captures Inference for Categorical Data - Chapter 2 (Proportions - Testing and Power)  
* DataCamp_Insights_v003_c captures Inference for Categorical Data - Chapter 3 (Comparing Many Parameters - Independence)  
  
### Expansions week of 13-AUG-2018  
* DataCamp_Insights_v003_c captures Inference for Categorical Data - Chapter 4 (Comparing Many Parameters - Goodness of Fit)  
* DataCamp_Insights_v003_c captures Building Dashboards with flexdashboard - Chapter 1 (Dashboard Layouts)  
* DataCamp_Insights_v003_c captures Building Dashboards with flexdashboard - Chapter 2 (Data Visualization for Dashboards)  
* DataCamp_Insights_v003_c captures Building Dashboards with flexdashboard - Chapter 3 (Dashboard Components)  
* DataCamp_Insights_v003_c captures Building Dashboards with flexdashboard - Chapter 4 (Adding Interactivity with Shiny)  
* DataCamp_Insights_v003_c captures Network Analysis in R: Case Studies - Chapter 1 (Exploring Graphs Through Time)  
* DataCamp_Insights_v003_c captures Network Analysis in R: Case Studies - Chapter 2 (Talk About R on Twitter) - notes  
  
### Expansions week of 20-AUG-2018  
* DataCamp_Insights_v003_c captures Network Analysis in R: Case Studies - Chapter 2 (Talk About R on Twitter) - code  
* DataCamp_Insights_v003_c captures Network Analysis in R: Case Studies - Chapter 3 (Bike Sharing in Chicago) - notes  
* DataCamp_Insights_v003_c captures Network Analysis in R: Case Studies - Chapter 3 (Bike Sharing in Chicago) - code  
* DataCamp_Insights_v003_c captures Network Analysis in R: Case Studies - Chapter 4 (Other Ways to Visualize Graph Data) - notes  
* DataCamp_Insights_v003_c captures Network Analysis in R: Case Studies - Chapter 4 (Other Ways to Visualize Graph Data) - code  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 1 (What is Bayesian Analysis?) - notes  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 1 (What is Bayesian Analysis?) - code  
  
### Expansions week of 27-AUG-2018  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 2 (How Does Bayesian Inference Work?) - notes  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 2 (How Does Bayesian Inference Work?) - code  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 3 (Why Use Bayesian Data Analysis?) - notes  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 3 (Why Use Bayesian Data Analysis?) - code  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 4 (Bayesian Inference with Bayes' Theorem) - notes  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 4 (Bayesian Inference with Bayes' Theorem) - code  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 5 (More Parameters, Data, and Bayes) - notes  
  
### Expansions week of 03-SEP-2018  
* DataCamp_Insights_v003_c captures Fundamentals of Bayesian Analysis in R - Chapter 5 (More Parameters, Data, and Bayes) - code  
* DataCamp_Insights_v003_c captures Categorical Data in the Tidyverse - Chapter 1 (Introduction to Factor Variables) - notes  
* DataCamp_Insights_v003_c captures Categorical Data in the Tidyverse - Chapter 1 (Introduction to Factor Variables) - code  
* DataCamp_Insights_v003_c captures Categorical Data in the Tidyverse - Chapter 2 (Manipulating Factor Variables) - notes  
* DataCamp_Insights_v003_c captures Categorical Data in the Tidyverse - Chapter 2 (Manipulating Factor Variables) - code  
* DataCamp_Insights_v003_c captures Categorical Data in the Tidyverse - Chapter 3 (Creating Factor Variables) - notes  
* DataCamp_Insights_v003_c captures Categorical Data in the Tidyverse - Chapter 4 (Case Study on Flight Etiquette) - notes  
  
### Expansions week of 10-SEP-2018  
* DataCamp_Insights_v003_c captures Bayesian Modeling with RJAGS - Chapter 1 (Introduction to Bayesian Modeling) - notes  
* DataCamp_Insights_v003_c captures Categorical Data in the Tidyverse - Chapter 3 (Creating Factor Variables) - code  
* DataCamp_Insights_v003_c captures Bayesian Modeling with RJAGS - Chapter 2 (Bayesian Models and Markov Chains) - notes  
* DataCamp_Insights_v003_c captures Bayesian Modeling with RJAGS - Chapter 3 (Bayesian Inference and Prediction) - notes  
* DataCamp_Insights_v003_c captures Bayesian Modeling with RJAGS - Chapter 4 (Multivariate and Generalized Linear Models) - notes  
* DataCamp_Insights_v003_c captures Categorical Data in the Tidyverse - Chapter 4 (Case Study on Flight Etiquette) - code  
* DataCamp_Insights_v003_c captures Parallel Programming in R - Chapter 1 (Can I Run My Application in Parallel?) - notes  
  
### Expansions week of 17-SEP-2018  
* DataCamp_Insights_v003_c captures Parallel Programming in R - Chapter 1 (Can I Run My Application in Parallel?) - code  
* DataCamp_Insights_v003_c captures Parallel Programming in R - Chapter 2 (The parallel package) - notes  
* DataCamp_Insights_v003_c captures Parallel Programming in R - Chapter 2 (The parallel package) - code  
* DataCamp_Insights_v003_c captures Parallel Programming in R - Chapter 3 (foreach, future.apply, Load Balancing) - notes  
* DataCamp_Insights_v003_c captures Parallel Programming in R - Chapter 4 (Random Numbers and Reproducibility) - notes  
* DataCamp_Insights_v003_c captures Parallel Programming in R - Chapter 3 (foreach, future.apply, Load Balancing) - code  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Choice Modeling - Chapter 1 (Quickstart Guide) - notes  
  
### Expansions week of 24-SEP-2018  
* DataCamp_Insights_v003_c captures Parallel Programming in R - Chapter 4 (Random Numbers and Reproducibility) - code  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Choice Modeling - Chapter 2 (Managing and Summarizing Choice Data) - notes  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Choice Modeling - Chapter 3 (Building Choice Models) - notes  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Choice Modeling - Chapter 2 (Managing and Summarizing Choice Data) - code  
* DataCamp_Insights_v003_c captures Marketing Analytics in R: Choice Modeling - Chapter 4 (Hierarchical Choice Models) - notes  
* DataCamp_Insights_v003_c captures Single-Cell RNA-Seq Workflows in R - Chapter 1 (What is Single-Cell RNA Seq?) - notes  
* DataCamp_Insights_v003_c captures Single-Cell RNA-Seq Workflows in R - Chapter 2 (Quality Control and Normalization) - notes  
  
### Expansions week of 01-OCT-2018  
* DataCamp_Insights_v003_c captures Single-Cell RNA-Seq Workflows in R - Chapter 3 (Visualization and Dimensionality Reduction) - notes  
* DataCamp_Insights_v003_c captures Single-Cell RNA-Seq Workflows in R - Chapter 4 (Cell Clustering and Differential Expression Analysis) - notes  
* DataCamp_Insights_v003_c captures Differential Expression Analysis in R with limma - Chapter 1 (Differential Expression Analysis) - notes  
* DataCamp_Insights_v003_c captures Differential Expression Analysis in R with limma - Chapter 2 (Flexible Models for Common Study Designs) - notes  
* DataCamp_Insights_v003_c captures Differential Expression Analysis in R with limma - Chapter 3 (Pre-processing and Post-processing) - notes  
* DataCamp_Insights_v003_c captures Differential Expression Analysis in R with limma - Chapter 4 (Case Study: Effect of Doxorubicin Treatment) - notes  
* DataCamp_Insights_v003_c captures Interactive Data Visualization with bokeh - Chapter 1 (rbokeh Introduction)  
  
### Expansions week of 08-OCT-2018  
* DataCamp_Insights_v003_c captures Interactive Data Visualization with bokeh - Chapter 2 (rbokeh Aestehtic Attributes and Figure Options) - notes  
* DataCamp_Insights_v003_c captures Interactive Data Visualization with bokeh - Chapter 3 (Data Manipulation for Visualization and More rbokeh Layers) - notes  
* DataCamp_Insights_v003_c captures Interactive Data Visualization with bokeh - Chapter 4 (Grid Plots and Maps) - notes  
* DataCamp_Insights_v003_c captures A/B Testing in R - Chapter 1 (Mini Case Study in A/B Testing) - notes  
* DataCamp_Insights_v003_c captures A/B Testing in R - Chapter 1 (Mini Case Study in A/B Testing) - code  
* DataCamp_Insights_v003_c captures A/B Testing in R - Chapter 2 (Mini Case Study in A/B Testing - Part II) - notes  
* DataCamp_Insights_v003_c captures A/B Testing in R - Chapter 2 (Mini Case Study in A/B Testing - Part II) - code  
  
### Expansions week of 15-OCT-2018  
* DataCamp_Insights_v003_c captures A/B Testing in R - Chapter 3 (Experimental Design in A/B Testing) - notes  
* DataCamp_Insights_v003_c captures A/B Testing in R - Chapter 4 (Statistical Analyses in A/B Testing) - notes  
* DataCamp_Insights_v003_c captures Mixture Models in R - Chapter 1 (Introduction to Mixture Models) - notes  
* DataCamp_Insights_v003_c captures Mixture Models in R - Chapter 1 (Introduction to Mixture Models) - code  
* DataCamp_Insights_v003_c captures Mixture Models in R - Chapter 2 (Structure of Mixture Models and Parameter Estimation) - notes  
* DataCamp_Insights_v003_c captures Mixture Models in R - Chapter 2 (Structure of Mixture Models and Parameter Estimation) - code  
* DataCamp_Insights_v003_c captures Mixture Models in R - Chapter 3 (Mixture of Gaussians with flexmix) - notes  
  
### Expansions week of 22-OCT-2018  
* DataCamp_Insights_v003_c captures Mixture Models in R - Chapter 3 (Mixture of Gaussians with flexmix) - code  
* DataCamp_Insights_v003_c captures Mixture Models in R - Chapter 4 (Mixture Models Beyond Gaussians) - notes  
* DataCamp_Insights_v003_c captures Mixture Models in R - Chapter 4 (Mixture Models Beyond Gaussians) - code  
* DataCamp_Insights_v003_c captures Developing R Packages - Chapter 1 (The R Package Structure) - notes  
* DataCamp_Insights_v003_c captures Developing R Packages - Chapter 2 (Documenting Packages) - notes  
* DataCamp_Insights_v003_c captures Developing R Packages - Chapter 3 (Checking and Building Packages) - notes  
* DataCamp_Insights_v003_c captures Developing R Packages - Chapter 4 (Adding Unit Tests to R Packages) - notes  
  
### Expansions week of 29-OCT-2018  
* DataCamp_Insights_v003_c captures Factor Analysis in R - Chapter 1 (Evaluating Your Measure with Factor Analysis) - notes  
* DataCamp_Insights_v003_c captures Factor Analysis in R - Chapter 2 (Multidimensional EFA) - notes  
* DataCamp_Insights_v003_c captures Factor Analysis in R - Chapter 3 (Confirmatory Factor Analysis) - notes  
* DataCamp_Insights_v003_c captures Factor Analysis in R - Chapter 4 (Refining Your Measure and Model) - notes  
* DataCamp_Insights_v003_c captures Factor Analysis in R - Chapter 1 (Evaluating Your Measure with Factor Analysis) - code  
* DataCamp_Insights_v003_c captures Factor Analysis in R - Chapter 2 (Multidimensional EFA) - code  
* DataCamp_Insights_v003_c captures Factor Analysis in R - Chapter 3 (Confirmatory Factor Analysis) - code  
  
### Expansions week of 05-NOV-2018  
* DataCamp_Insights_v003_c captures Factor Analysis in R - Chapter 4 (Refining Your Measure and Model) - code  
* DataCamp_Insights_v003_c captures Generalized Linear Models in R - Chapter 1 (GLM - Extension of Regression Toolbox) - notes  
* DataCamp_Insights_v003_c captures Generalized Linear Models in R - Chapter 2 (Logistic Regression) - notes  
* DataCamp_Insights_v003_c captures Generalized Linear Models in R - Chapter 3 (Interpreting and Visualizing GLMs) - notes  
* DataCamp_Insights_v003_c captures Generalized Linear Models in R - Chapter 4 (Multiple Regression with GLMs) - notes  
* DataCamp_Insights_v003_c captures Introduction to Bioconductor - Chapter 1 (What is Bioconductor?) - notes  
* DataCamp_Insights_v003_c captures Introduction to Bioconductor - Chapter 2 (Biostrings and When to use Them) - notes  
  
### Expansions week of 12-NOV-2018  
* DataCamp_Insights_v003_c captures Introduction to Bioconductor - Chapter 3 (IRanges and GenomicRanges) - notes  
* DataCamp_Insights_v003_c captures Introduction to Bioconductor - Chapter 4 (Introducing ShortRead) - notes  
* DataCamp_Insights_v003_c captures Generalized Linear Models in R - Chapter 1 (GLM - Extension of Regression Toolbox) - code  
* DataCamp_Insights_v003_c captures Generalized Linear Models in R - Chapter 2 (Logistic Regression) - code  
* DataCamp_Insights_v003_c captures Generalized Linear Models in R - Chapter 3 (Interpreting and Visualizing GLMs) - code  
* DataCamp_Insights_v003_c captures Generalized Linear Models in R - Chapter 4 (Multiple Regression with GLMs) - code  
* DataCamp_Insights_v003_c captures Non-Linear Modeling in R with GAM - Chapter 1 (Intro to Generalized Additive Models) - notes  
  
### Expansions week of 19-NOV-2018  
* DataCamp_Insights_v003_c captures Non-Linear Modeling in R with GAM - Chapter 2 (Interpreting and Visualizing GAMs) - notes  
* DataCamp_Insights_v003_c captures Non-Linear Modeling in R with GAM - Chapter 3 (Spatial GAMs and Interactions) - notes  
* DataCamp_Insights_v003_c captures Non-Linear Modeling in R with GAM - Chapter 4 (Logistic GAM for Classification) - notes  
* DataCamp_Insights_v003_c captures Non-Linear Modeling in R with GAM - Chapter 1 (Intro to Generalized Additive Models) - code  
* DataCamp_Insights_v003_c captures Non-Linear Modeling in R with GAM - Chapter 2 (Interpreting and Visualizing GAMs) - code  
* DataCamp_Insights_v003_c captures Non-Linear Modeling in R with GAM - Chapter 3 (Spatial GAMs and Interactions) - code  
* DataCamp_Insights_v003_c captures Non-Linear Modeling in R with GAM - Chapter 4 (Logistic GAM for Classification) - code  
  
### Expansions week of 26-NOV-2018  
* DataCamp_Insights_v003_c captures Machine Learning in the Tidyverse - Chapter 1 (Foundations of Tidy Machine Learning) - notes  
* DataCamp_Insights_v003_c captures Machine Learning in the Tidyverse - Chapter 2 (Multiple Models with Broom) - notes  
* DataCamp_Insights_v003_c captures Machine Learning in the Tidyverse - Chapter 3 (Build, Tune, and Evaluate Regression Models) - notes  
* DataCamp_Insights_v003_c captures Machine Learning in the Tidyverse - Chapter 4 (Build, Tune, and Evaluate Classification Models) - notes  
* DataCamp_Insights_v003_c captures Predictive Analytics Using Networked Data in R - Chapter 1 (Introductions, Networks, and Labeled Networks) - notes  
* DataCamp_Insights_v003_c captures Predictive Analytics Using Networked Data in R - Chapter 2 (Homophily) - notes  
* DataCamp_Insights_v003_c captures Predictive Analytics Using Networked Data in R - Chapter 3 (Network Featurization) - notes  
  
### Expansions week of 03-DEC-2018  
* DataCamp_Insights_v003_c captures Predictive Analytics Using Networked Data in R - Chapter 4 (Putting it all Together) - notes  
* DataCamp_Insights_v003_c captures Predictive Analytics Using Networked Data in R - Chapter 1 (Introductions, Networks, and Labeled Networks) - code  
* DataCamp_Insights_v003_c captures Predictive Analytics Using Networked Data in R - Chapter 2 (Homophily) - code  
* DataCamp_Insights_v003_c captures Predictive Analytics Using Networked Data in R - Chapter 3 (Network Featurization) - code  
* DataCamp_Insights_v003_c captures Predictive Analytics Using Networked Data in R - Chapter 4 (Putting it all Together) - code  
* DataCamp_Insights_v003_c captures Bayesian Regression with rstanarm - Chapter 1 (Introduction to Bayesian Linear Models) - notes  
* DataCamp_Insights_v003_c captures Bayesian Regression with rstanarm - Chapter 2 (Modifying a Bayesian Model) - notes  
  
### Expansions week of 10-DEC-2018  
* DataCamp_Insights_v003_c captures Bayesian Regression with rstanarm - Chapter 3 (Assessing Model Fit) - notes  
* DataCamp_Insights_v003_c captures Bayesian Regression with rstanarm - Chapter 4 (Presenting and Using Bayesian Regression) - notes  
* DataCamp_Insights_v003_c captures CnIP-seq Workflows in R - Chapter 1 (Introduction to ChIP-seq) - notes  
* DataCamp_Insights_v003_c captures CnIP-seq Workflows in R - Chapter 2 (Preparing ChIP-seq Data) - notes  
* DataCamp_Insights_v003_c captures CnIP-seq Workflows in R - Chapter 3 (Comparing ChIP-seq Samples) - notes  
* DataCamp_Insights_v003_c captures CnIP-seq Workflows in R - Chapter 4 (Peaks to Genes to Function) - notes  
* DataCamp_Insights_v003_c captures Designing and Analyzing Clinical Trials in R - Chapter 1 (Principles) - notes  
  
### Expansions week of 17-DEC-2018  
* DataCamp_Insights_v003_c captures Designing and Analyzing Clinical Trials in R - Chapter 2 (Trial Designs) - notes  
* DataCamp_Insights_v003_c captures Designing and Analyzing Clinical Trials in R - Chapter 3 (Sample Size and Power) - notes  
* DataCamp_Insights_v003_c captures Designing and Analyzing Clinical Trials in R - Chapter 4 (Statistical Analysis) - notes  
* DataCamp_Insights_v003_c captures Designing and Analyzing Clinical Trials in R - Chapter 1 (Principles) - code  
* DataCamp_Insights_v003_c captures Designing and Analyzing Clinical Trials in R - Chapter 2 (Trial Designs) - code  
* DataCamp_Insights_v003_c captures Designing and Analyzing Clinical Trials in R - Chapter 3 (Sample Size and Power) - code  
* DataCamp_Insights_v003_c captures Designing and Analyzing Clinical Trials in R - Chapter 4 (Statistical Analysis) - code  
  
### Expansions week of 24-DEC-2018  
* DataCamp_Insights_v003_c captures Financial Analytics in R - Chapter 1 (Intro to Valuations) - notes  
* DataCamp_Insights_v003_c captures Financial Analytics in R - Chapter 1 (Intro to Valuations) - code  
* DataCamp_Insights_v003_c captures Financial Analytics in R - Chapter 2 (Key Financial Concepts) - notes  
* DataCamp_Insights_v003_c captures Financial Analytics in R - Chapter 2 (Key Financial Concepts) - code  
* DataCamp_Insights_v003_c captures Financial Analytics in R - Chapter 3 (Prioritizing Profitability) - notes  
* DataCamp_Insights_v003_c captures Financial Analytics in R - Chapter 3 (Prioritizing Profitability) - code  
* DataCamp_Insights_v003_c captures Financial Analytics in R - Chapter 4 (Understanding Outcomes) - notes  
  
### Expansions week of 31-DEC-2018  
* DataCamp_Insights_v003_c captures Financial Analytics in R - Chapter 4 (Understanding Outcomes) - code  
* DataCamp_Insights_v003_c captures Visualizing Big Data with Trelliscope - Chapter 1 (General Strategies for Visualizing Big Data) - notes  
* DataCamp_Insights_v003_c captures Visualizing Big Data with Trelliscope - Chapter 2 (ggplot2 and Trelliscope JS) - notes  
* DataCamp_Insights_v003_c captures Visualizing Big Data with Trelliscope - Chapter 3 (Trelliscope in the Tidyverse) - notes  
* DataCamp_Insights_v003_c captures Visualizing Big Data with Trelliscope - Chapter 4 (Case Study: Montreal BIXI Bike Data) - notes  
* DataCamp_Insights_v003_c captures Visualizing Big Data with Trelliscope - Chapter 1 (General Strategies for Visualizing Big Data) - code  
* DataCamp_Insights_v003_c captures Visualizing Big Data with Trelliscope - Chapter 4 (Case Study: Montreal BIXI Bike Data) - code  
  
### Expansions week of 07-JAN-2019  
* DataCamp_Insights_v003_c captures Visualization Best Practices in R - Chapter 1 (Proportions of a Whole) - notes  
* DataCamp_Insights_v003_c captures Visualization Best Practices in R - Chapter 2 (Point Data) - notes  
* DataCamp_Insights_v003_c captures Visualization Best Practices in R - Chapter 3 (Single Distributions) - notes  
* DataCamp_Insights_v003_c captures Visualization Best Practices in R - Chapter 4 (Comparing Distributions) - notes  
* DataCamp_Insights_v003_c captures Visualization Best Practices in R - Chapter 1 (Proportions of a Whole) - code  
* DataCamp_Insights_v003_c captures Visualization Best Practices in R - Chapter 2 (Point Data) - code  
* DataCamp_Insights_v003_c captures Visualization Best Practices in R - Chapter 3 (Single Distributions) - code  
  
### Expansions week of 14-JAN-2019  
* DataCamp_Insights_v003_c captures Visualization Best Practices in R - Chapter 4 (Comparing Distributions) - code  
* DataCamp_Insights_v003_c captures Linear Algebra for Data Science in R - Chapter 1 (Introduction to Linear Algebra) - notes  
* DataCamp_Insights_v003_c captures Linear Algebra for Data Science in R - Chapter 2 (Matrix-Vector Equations) - notes  
* DataCamp_Insights_v003_c captures Linear Algebra for Data Science in R - Chapter 3 (Eigenvalues and Eigenvectors) - notes  
* DataCamp_Insights_v003_c captures Linear Algebra for Data Science in R - Chapter 4 (Principal Component Analysis) - notes  
* DataCamp_Insights_v003_c captures Linear Algebra for Data Science in R - Chapter 1 (Introduction to Linear Algebra) - code  
* DataCamp_Insights_v003_c captures Linear Algebra for Data Science in R - Chapter 2 (Matrix-Vector Equations) - code  
  
### Expansions week of 21-JAN-2019  
* DataCamp_Insights_v003_c captures Linear Algebra for Data Science in R - Chapter 3 (Eigenvalues and Eigenvectors) - code  
* DataCamp_Insights_v003_c captures Linear Algebra for Data Science in R - Chapter 4 (Principal Component Analysis) - code  
* DataCamp_Insights_v003_c captures HR Analytics in R: Predicting Employee Churn - Chapter 1 (Introduction) - notes  
* DataCamp_Insights_v003_c captures HR Analytics in R: Predicting Employee Churn - Chapter 2 (Feature Engineering) - notes  
* DataCamp_Insights_v003_c captures HR Analytics in R: Predicting Employee Churn - Chapter 3 (Predicting Turnover) - notes  
* DataCamp_Insights_v003_c captures HR Analytics in R: Predicting Employee Churn - Chapter 4 (Model Validation, Intervention, and ROI) - notes  
* DataCamp_Insights_v003_c captures Dealing with Missing Data in R - Chapter 1 (Rationale) - notes  
  
### Expansions week of 28-JAN-2019  
* DataCamp_Insights_v003_c captures Dealing with Missing Data in R - Chapter 2 (Wrangling and Tidying Missing Values) - notes  
* DataCamp_Insights_v003_c captures Dealing with Missing Data in R - Chapter 3 (Testing Missing Relationships) - notes  
* DataCamp_Insights_v003_c captures Dealing with Missing Data in R - Chapter 4 (Imputation) - notes  
* DataCamp_Insights_v003_c captures Dealing with Missing Data in R - Chapter 1 (Rationale) - code  
* DataCamp_Insights_v003_c captures Dealing with Missing Data in R - Chapter 2 (Wrangling and Tidying Missing Values) - code  
* DataCamp_Insights_v003_c captures Dealing with Missing Data in R - Chapter 3 (Testing Missing Relationships) - code  
* DataCamp_Insights_v003_c captures Dealing with Missing Data in R - Chapter 4 (Imputation) - code  
  
### Expansions week of 04-FEB-2019  
* DataCamp_Insights_v003_c captures Analyzing Election and Polling Data in R - Chapter 1 (Presidential Job Approval Polls) - notes  
* DataCamp_Insights_v003_c captures Analyzing Election and Polling Data in R - Chapter 2 (US House and Senate Polling) - notes  
* DataCamp_Insights_v003_c captures Analyzing Election and Polling Data in R - Chapter 3 (Election Results and Political Demography) - notes  
* DataCamp_Insights_v003_c captures Analyzing Election and Polling Data in R - Chapter 4 (Predicting the Future of Politics) - notes  
* DataCamp_Insights_v003_c captures Analyzing Election and Polling Data in R - Chapter 1 (Presidential Job Approval Polls) - code  
* DataCamp_Insights_v003_c captures Analyzing Election and Polling Data in R - Chapter 2 (US House and Senate Polling) - code  
* DataCamp_Insights_v003_c captures Analyzing Election and Polling Data in R - Chapter 3 (Election Results and Political Demography) - code  
  
### Expansions week of 11-FEB-2019  
* DataCamp_Insights_v003_c captures Analyzing Election and Polling Data in R - Chapter 4 (Predicting the Future of Politics) - code  
* DataCamp_Insights_v003_c captures Analyzing US Census Data in R - Chapter 1 (Census Data with R in tidycensus) - notes  
* DataCamp_Insights_v003_c captures Analyzing US Census Data in R - Chapter 2 (Wrangling US Census Data) - notes  
* DataCamp_Insights_v003_c captures Analyzing US Census Data in R - Chapter 3 (US Census Geographic Data in R) - notes  
* DataCamp_Insights_v003_c captures Analyzing US Census Data in R - Chapter 4 (Mapping US Census Data) - notes  
* DataCamp_Insights_v003_c captures Multivariate Probability Distributions in R - Chapter 1 (Reading and Plotting Multivariate Data) - notes  
* DataCamp_Insights_v003_c captures Multivariate Probability Distributions in R - Ch 2 (Multivariate Normal Distribution) - notes  
  
### Expansions week of 18-FEB-2019  
* DataCamp_Insights_v003_c captures Multivariate Probability Distributions in R - Ch 3 (Other Multivariate Distributions) - notes  
* DataCamp_Insights_v003_c captures Multivariate Probability Distributions in R - Chapter 4 (Principal Component Analysis and Multidimensional Scaling) - notes  
* DataCamp_Insights_v003_c captures Multivariate Probability Distributions in R - Chapter 1 (Reading and Plotting Multivariate Data) - code  
* DataCamp_Insights_v003_c captures Multivariate Probability Distributions in R - Ch 2 (Multivariate Normal Distribution) - code  
* DataCamp_Insights_v003_c captures Multivariate Probability Distributions in R - Ch 3 (Other Multivariate Distributions) - code  
* DataCamp_Insights_v003_c captures Multivariate Probability Distributions in R - Chapter 4 (Principal Component Analysis and Multidimensional Scaling) - code  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 1 (Programming with purrr) - notes  
  
### Expansions week of 25-FEB-2019  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 2 (Functional Programming from Theory to Practice) - notes  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 3 (Better Code with purrr) - notes  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 4 (Case Study) - notes  
* DataCamp_Insights_v003_c captures Foundations of Functional Programming with purrr - Chapter 1 (Simplifying Iteration and Lists with purrr) - notes  
* DataCamp_Insights_v003_c captures Foundations of Functional Programming with purrr - Chapter 2 (More Complex Iterations) - notes  
* DataCamp_Insights_v003_c captures Foundations of Functional Programming with purrr - Chapter 3 (Troubleshooting Lists with purrr) - notes  
* DataCamp_Insights_v003_c captures Foundations of Functional Programming with purrr - Chapter 4 (Problem Solving w/ purrr) - notes  
  
### Expansions week of 04-MAR-2019  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 1 (Programming with purrr) - code  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 2 (Functional Programming from Theory to Practice) - code  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 3 (Better Code with purrr) - code  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 4 (Case Study) - code  
* DataCamp_Insights_v003_c captures Foundations of Functional Programming with purrr - Chapter 1 (Simplifying Iteration and Lists with purrr) - code  
* DataCamp_Insights_v003_c captures Foundations of Functional Programming with purrr - Chapter 2 (More Complex Iterations) - code  
* DataCamp_Insights_v003_c captures Foundations of Functional Programming with purrr - Chapter 3 (Troubleshooting Lists with purrr) - code  
  
### Expansions week of 11-MAR-2019  
* DataCamp_Insights_v003_c captures Foundations of Functional Programming with purrr - Chapter 4 (Problem Solving w/ purrr) - code  
* DataCamp_Insights_v003_c captures Foundations of Joining Data in R with data.table - Chapter 1 (Join Multiple data.tables) - notes  
* DataCamp_Insights_v003_c captures Foundations of Joining Data in R with data.table - Chapter 2 (Joins Using data.table Syntax) - notes  
* DataCamp_Insights_v003_c captures Foundations of Joining Data in R with data.table - Chapter 3 (Diagnosing and Fixing Common Join Problems) - notes  
* DataCamp_Insights_v003_c captures Foundations of Joining Data in R with data.table - Chapter 4 (Concatenating and Reshaping data.table) - notes  
* DataCamp_Insights_v003_c captures Fraud Detection in R - Chapter 1 (Introduction and Motivation) - notes  
* DataCamp_Insights_v003_c captures Fraud Detection in R - Chapter 2 (Social Network Analysis) - notes  
  
### Expansions week of 18-MAR-2019  
* DataCamp_Insights_v003_c captures Fraud Detection in R - Chapter 3 (Imbalanced Class Distributions) - notes  
* DataCamp_Insights_v003_c captures Fraud Detection in R - Chapter 4 (Digit Analysis and Robust Statistics) - notes  
* DataCamp_Insights_v003_c captures Dimensionality Reduction in R - Chapter 1 (Principal Component Analysis) - notes  
* DataCamp_Insights_v003_c captures Dimensionality Reduction in R - Chapter 2 (Advanced PCA and Non-Negative Matrix Factorization) - notes  
* DataCamp_Insights_v003_c captures Dimensionality Reduction in R - Chapter 3 (Exploratory Factor Analysis) - notes  
* DataCamp_Insights_v003_c captures Dimensionality Reduction in R - Chapter 4 (Advanced EFA) - notes  
* DataCamp_Insights_v003_c captures Foundations of Joining Data in R with data.table - Chapter 1 (Join Multiple data.tables) - code  
  
### Expansions week of 25-MAR-2019  
* DataCamp_Insights_v003_c captures Foundations of Joining Data in R with data.table - Chapter 2 (Joins Using data.table Syntax) - code  
* DataCamp_Insights_v003_c captures Foundations of Joining Data in R with data.table - Chapter 3 (Diagnosing and Fixing Common Join Problems) - code  
* DataCamp_Insights_v003_c captures Foundations of Joining Data in R with data.table - Chapter 4 (Concatenating and Reshaping data.table) - code  
* DataCamp_Insights_v003_c captures Fraud Detection in R - Chapter 1 (Introduction and Motivation) - code  
* DataCamp_Insights_v003_c captures Fraud Detection in R - Chapter 2 (Social Network Analysis) - code  
* DataCamp_Insights_v003_c captures Fraud Detection in R - Chapter 3 (Imbalanced Class Distributions) - code  
* DataCamp_Insights_v003_c captures Fraud Detection in R - Chapter 4 (Digit Analysis and Robust Statistics) - code  
  
### Expansions week of 01-APR-2019  
* DataCamp_Insights_v003_c captures Anomaly Detection in R - Chapter 1 (Statistical Outlier Detection) - notes  
* DataCamp_Insights_v003_c captures Anomaly Detection in R - Chapter 2 (Distance and Density Based Anomaly Detection) - notes  
* DataCamp_Insights_v003_c captures Anomaly Detection in R - Chapter 3 (Isolation Forest) - notes  
* DataCamp_Insights_v003_c captures Anomaly Detection in R - Chapter 4 (Comparing Performance) - notes  
* DataCamp_Insights_v003_c captures Dimensionality Reduction in R - Chapter 1 (Principal Component Analysis) - code  
* DataCamp_Insights_v003_c captures Dimensionality Reduction in R - Chapter 2 (Advanced PCA and Non-Negative Matrix Factorization) - code  
* DataCamp_Insights_v003_c captures Dimensionality Reduction in R - Chapter 3 (Exploratory Factor Analysis) - code  
  
### Expansions week of 08-APR-2019  
* DataCamp_Insights_v003_c captures Dimensionality Reduction in R - Chapter 4 (Advanced EFA) - notes  
* DataCamp_Insights_v003_c captures GARCH Models in R - Chapter 1 (Standard GARCH Model as the Workhorse) - notes  
* DataCamp_Insights_v003_c captures GARCH Models in R - Chapter 2 (Improvements of the Normal GARCH Model) - notes  
* DataCamp_Insights_v003_c captures GARCH Models in R - Chapter 3 (Performance Evaluation) - notes  
* DataCamp_Insights_v003_c captures GARCH Models in R - Chapter 4 (Applications) - notes  
* DataCamp_Insights_v003_c captures Anomaly Detection in R - Chapter 1 (Statistical Outlier Detection) - code  
* DataCamp_Insights_v003_c captures Anomaly Detection in R - Chapter 2 (Distance and Density Based Anomaly Detection) - code  
  
### Expansions week of 15-APR-2019  
* DataCamp_Insights_v003_c captures Anomaly Detection in R - Chapter 3 (Isolation Forest) - code  
* DataCamp_Insights_v003_c captures Anomaly Detection in R - Chapter 4 (Comparing Performance) - code  
* DataCamp_Insights_v003_c captures RNA-Seq Differential Expression Analysis - Chapter 1 (Introduction to RNA-Seq Theory and Workflow) - notes  
* DataCamp_Insights_v003_c captures RNA-Seq Differential Expression Analysis - Chapter 2 (Exploratory Data Analysis) - notes  
* DataCamp_Insights_v003_c captures RNA-Seq Differential Expression Analysis - Chapter 3 (Differential Expression Analysis with DESeq2) - notes  
* DataCamp_Insights_v003_c captures RNA-Seq Differential Expression Analysis - Chapter 4 (Exploration of Differential Expression Results) - notes  
* DataCamp_Insights_v003_c captures Survival Analysis in R - Chapter 1 (What is Survival Analysis?) - notes  
  
### Expansions week of 22-APR-2019  
* DataCamp_Insights_v003_c captures Survival Analysis in R - Chapter 2 (Estimation of Survival Curves) - notes  
* DataCamp_Insights_v003_c captures Survival Analysis in R - Chapter 3 (Weibull Model) - notes  
* DataCamp_Insights_v003_c captures Survival Analysis in R - Chapter 4 (Cox Model) - notes  
* DataCamp_Insights_v003_c captures Building Reponse Models in R - Chapter 1 (Building Response Models for Product Sales) - notes  
* DataCamp_Insights_v003_c captures Building Reponse Models in R - Chapter 2 (Extended Sales Response Modeling) - notes  
* DataCamp_Insights_v003_c captures Building Reponse Models in R - Chapter 3 (Response Models for Individual Demand) - notes  
* DataCamp_Insights_v003_c captures Building Reponse Models in R - Chapter 4 (Extended Demand Modeling) - notes  
  
### Expansions week of 29-APR-2019  
* DataCamp_Insights_v003_c captures Survival Analysis in R - Chapter 1 (What is Survival Analysis?) - code  
* DataCamp_Insights_v003_c captures Survival Analysis in R - Chapter 2 (Estimation of Survival Curves) - code  
* DataCamp_Insights_v003_c captures Survival Analysis in R - Chapter 3 (Weibull Model) - code  
* DataCamp_Insights_v003_c captures Survival Analysis in R - Chapter 4 (Cox Model) - code  
* DataCamp_Insights_v003_c captures Building Reponse Models in R - Chapter 1 (Building Response Models for Product Sales) - code  
* DataCamp_Insights_v003_c captures Building Reponse Models in R - Chapter 2 (Extended Sales Response Modeling) - code  
* DataCamp_Insights_v003_c captures Building Reponse Models in R - Chapter 3 (Response Models for Individual Demand) - code  
  
### Expansions week of 06-MAY-2019  
* DataCamp_Insights_v003_c captures Building Reponse Models in R - Chapter 4 (Extended Demand Modeling) - code  
* DataCamp_Insights_v003_c captures Time Series with data.table in R - Chapter 1 (Review of data.table) - notes  
* DataCamp_Insights_v003_c captures Time Series with data.table in R - Chapter 2 (Getting Time Series Data into data.table) - notes  
* DataCamp_Insights_v003_c captures Time Series with data.table in R - Chapter 3 (Generating Lags, Differences, and Windowed Aggregations) - notes  
* DataCamp_Insights_v003_c captures Time Series with data.table in R - Chapter 4 (Case Study: Financial Data) - notes  
* DataCamp_Insights_v003_c captures Interactive Data Visualization with plotly in R - Chapter 1 (Introduction to plotly) - notes  
* DataCamp_Insights_v003_c captures Interactive Data Visualization with plotly in R - Chapter 2 (Styling and Customizing Graphics) - notes  
  
### Expansions week of 13-MAY-2019  
* DataCamp_Insights_v003_c captures Interactive Data Visualization with plotly in R - Chapter 3 (Advanced Charts) - notes  
* DataCamp_Insights_v003_c captures Interactive Data Visualization with plotly in R - Chapter 4 (Case Study) - notes  
* DataCamp_Insights_v003_c captures Hyperparameter Tuning in R - Chapter 1 (Introduction to Hyperparameters) - notes  
* DataCamp_Insights_v003_c captures Hyperparameter Tuning in R - Chapter 2 (Hyperparameter Tuning with caret) - notes  
* DataCamp_Insights_v003_c captures Hyperparameter Tuning in R - Chapter 3 (Hyperparameter Tuning with mlr) - notes  
* DataCamp_Insights_v003_c captures Hyperparameter Tuning in R - Chapter 4 (Hyperparameter Tuning with h2o) - notes  
* DataCamp_Insights_v003_c captures Time Series with data.table in R - Chapter 1 (Review of data.table) - code  
  
### Expansions week of 20-MAY-2019  
* DataCamp_Insights_v003_c captures Time Series with data.table in R - Chapter 2 (Getting Time Series Data into data.table) - code  
* DataCamp_Insights_v003_c captures Time Series with data.table in R - Chapter 3 (Generating Lags, Differences, and Windowed Aggregations) - code  
* DataCamp_Insights_v003_c captures Time Series with data.table in R - Chapter 4 (Case Study: Financial Data) - code  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 1 (Programming with purrr) - notes  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 2 (Functional Programming from Theory to Practice) - notes  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 3 (Better Code with purrr) - notes  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 4 (Case Study) - notes  
  
### Expansions week of 27-MAY-2019  
* DataCamp_Insights_v003_c captures Hyperparameter Tuning in R - Chapter 1 (Introduction to Hyperparameters) - code  
* DataCamp_Insights_v003_c captures Hyperparameter Tuning in R - Chapter 2 (Hyperparameter Tuning with caret) - code  
* DataCamp_Insights_v003_c captures Hyperparameter Tuning in R - Chapter 3 (Hyperparameter Tuning with mlr) - code  
* DataCamp_Insights_v003_c captures Hyperparameter Tuning in R - Chapter 4 (Hyperparameter Tuning with h2o) - code  
* DataCamp_Insights_v003_c captures Longitudinal Analysis in R - Chapter 1 (Introduction to Longitudinal Data) - notes  
* DataCamp_Insights_v003_c captures Longitudinal Analysis in R - Chapter 2 (Modeling Continuous Longitudinal Outcomes) - notes  
* DataCamp_Insights_v003_c captures Longitudinal Analysis in R - Chapter 3 (Add Fixed Predictor Variables) - notes  
  
### Expansions week of 03-JUN-2019  
* DataCamp_Insights_v003_c captures Longitudinal Analysis in R - Chapter 4 (Modeling Longitudinal Dichotomous Outcomes) - notes  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 1 (Programming with purrr) - code  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 1 (Introduction to data.table) - notes  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 2 (Selecting and Computing on Columns) - notes  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 3 (Groupwise Operations) - notes  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 4 (Reference Semantics) - notes  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 5 (Importing and Exporting Data) - notes  
  
### Expansions week of 10-JUN-2019  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 2 (Functional Programming from Theory to Practice) - code  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 3 (Better Code with purrr) - code  
* DataCamp_Insights_v003_c captures Intermediate Functional Programming with purrr - Chapter 4 (Case Study) - code  
* DataCamp_Insights_v003_c captures Probability Puzzles in R - Chapter 1 (Introduction and Classic Puzzles) - notes  
* DataCamp_Insights_v003_c captures Probability Puzzles in R - Chapter 2 (Games with Dice) - notes  
* DataCamp_Insights_v003_c captures Probability Puzzles in R - Chapter 3 (Inspired from the Web) - notes  
* DataCamp_Insights_v003_c captures Probability Puzzles in R - Chapter 4 (Poker) - notes  
  
### Expansions week of 17-JUN-2019  
* DataCamp_Insights_v003_c captures Longitudinal Analysis in R - Chapter 1 (Introduction to Longitudinal Data) - code  
* DataCamp_Insights_v003_c captures Longitudinal Analysis in R - Chapter 2 (Modeling Continuous Longitudinal Outcomes) - code  
* DataCamp_Insights_v003_c captures Longitudinal Analysis in R - Chapter 3 (Add Fixed Predictor Variables) - code  
* DataCamp_Insights_v003_c captures Longitudinal Analysis in R - Chapter 4 (Modeling Longitudinal Dichotomous Outcomes) - code  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 1 (Introduction to data.table) - code  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 2 (Selecting and Computing on Columns) - code  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 3 (Groupwise Operations) - code  
  
### Expansions week of 24-JUN-2019  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 4 (Reference Semantics) - code  
* DataCamp_Insights_v003_c captures Data Manipulation in R with data.table - Chapter 5 (Importing and Exporting Data) - code  
* DataCamp_Insights_v003_c captures Highcharter for Finance in R - Chapter 1 (Introduction to Highcharter) - notes  
* DataCamp_Insights_v003_c captures Highcharter for Finance in R - Chapter 2 (Highcharter for xts Data) - notes  
* DataCamp_Insights_v003_c captures Highcharter for Finance in R - Chapter 3 (Highcharter for Wide tibble Data) - notes  
* DataCamp_Insights_v003_c captures Highcharter for Finance in R - Chapter 4 (Highcharter for Tidy tibble Data) - notes  
* DataCamp_Insights_v003_c captures Advanced Dimensionality Reduction in R - Chapter 1 (Introduction) - notes  
  
### Expansions week of 01-JUL-2019  
* DataCamp_Insights_v003_c captures Advanced Dimensionality Reduction in R - Chapter 2 (Introduction to t-SNE) - notes  
* DataCamp_Insights_v003_c captures Advanced Dimensionality Reduction in R - Chapter 3 (Using t-SNE with Predictive Models) - notes  
* DataCamp_Insights_v003_c captures Advanced Dimensionality Reduction in R - Chapter 4 (Generalized Low-Rank Models) - notes   
* DataCamp_Insights_v003_c captures Probability Puzzles in R - Chapter 1 (Introduction and Classic Puzzles) - code  
* DataCamp_Insights_v003_c captures Probability Puzzles in R - Chapter 2 (Games with Dice) - code  
* DataCamp_Insights_v003_c captures Probability Puzzles in R - Chapter 3 (Inspired from the Web) - code  
* DataCamp_Insights_v003_c captures Probability Puzzles in R - Chapter 4 (Poker) - code  
* DataCamp_Insights_v003_c captures Optimizing R Code with Rcpp - Chapter 1 (Introduction) - notes  
  
### Expansions week of 08-JUL-2019  
* DataCamp_Insights_v003_c captures Optimizing R Code with Rcpp - Chapter 2 (Functions and Control Flow) - notes  
* DataCamp_Insights_v003_c captures Optimizing R Code with Rcpp - Chapter 3 (Vector Classes) - notes  
* DataCamp_Insights_v003_c captures Optimizing R Code with Rcpp - Chapter 4 (Case Studies) - notes  
* DataCamp_Insights_v003_c captures Regression Modeling in R: Case Studies - Chapter 1 (GLM) - notes  
* DataCamp_Insights_v003_c captures Regression Modeling in R: Case Studies - Chapter 2 (Extending GLM) - notes  
* DataCamp_Insights_v003_c captures Regression Modeling in R: Case Studies - Chapter 3 (Mixed Effects Models - Part I) - notes  
* DataCamp_Insights_v003_c captures Regression Modeling in R: Case Studies - Chapter 4 (Mixed Effects Models - Part II) - notes  
  
### Expansions week of 15-JUL-2019  
* DataCamp_Insights_v003_c captures Highcharter for Finance in R - Chapter 1 (Introduction to Highcharter) - code  
* DataCamp_Insights_v003_c captures Highcharter for Finance in R - Chapter 2 (Highcharter for xts Data) - code  
* DataCamp_Insights_v003_c captures Highcharter for Finance in R - Chapter 3 (Highcharter for Wide tibble Data) - code  
* DataCamp_Insights_v003_c captures Highcharter for Finance in R - Chapter 4 (Highcharter for Tidy tibble Data) - code  
* DataCamp_Insights_v003_c captures Advanced Dimensionality Reduction in R - Chapter 1 (Introduction) - code  
* DataCamp_Insights_v003_c captures Advanced Dimensionality Reduction in R - Chapter 2 (Introduction to t-SNE) - code  
* DataCamp_Insights_v003_c captures Advanced Dimensionality Reduction in R - Chapter 3 (Using t-SNE with Predictive Models) - code  
  
### Expansions week of 22-JUL-2019  
* DataCamp_Insights_v003_c captures Advanced Dimensionality Reduction in R - Chapter 4 (Generalized Low-Rank Models) - code   
* DataCamp_Insights_v003_c captures Topic Modeling in R - Chapter 1 (Introduction to the Workflow) - notes  
* DataCamp_Insights_v003_c captures Topic Modeling in R - Chapter 2 (Word Clouds, Stop Words, Control Arguments) - notes  
* DataCamp_Insights_v003_c captures Topic Modeling in R - Ch 3 (Named Entity Recognition as Unsupervised Classification) - notes  
* DataCamp_Insights_v003_c captures Topic Modeling in R - Chapter 4 (Number of Topics) - notes  
* DataCamp_Insights_v003_c captures Regression Modeling in R: Case Studies - Chapter 1 (GLM) - code  
* DataCamp_Insights_v003_c captures Regression Modeling in R: Case Studies - Chapter 2 (Extending GLM) - code  
  
### Expansions week of 29-JUL-2019  
* DataCamp_Insights_v003_c captures Regression Modeling in R: Case Studies - Chapter 3 (Mixed Effects Models - Part I) - code  
* DataCamp_Insights_v003_c captures Regression Modeling in R: Case Studies - Chapter 4 (Mixed Effects Models - Part II) - code  
* DataCamp_Insights_v003_c captures Intermediate Interactive Data Visualization with plotly in R - Chapter 1 (Introduction and Review of plotly) - notes  
* DataCamp_Insights_v003_c captures Intermediate Interactive Data Visualization with plotly in R - Chapter 2 (Animating Graphics) - notes  
* DataCamp_Insights_v003_c captures Intermediate Interactive Data Visualization with plotly in R - Chapter 3 (Linking Graphics) - notes  
* DataCamp_Insights_v003_c captures Intermediate Interactive Data Visualization with plotly in R - Chapter 4 (Case Study) - notes  
* DataCamp_Insights_v003_c captures Intermediate Interactive Defensive R Programming - Chapter 1 (Avoiding Conflict) - notes  
  
### Expansions week of 05-AUG-2019  
* DataCamp_Insights_v003_c captures Intermediate Interactive Defensive R Programming - Chapter 2 (Early Warning Systems) - notes  
* DataCamp_Insights_v003_c captures Intermediate Interactive Defensive R Programming - Chapter 3 (Preparing Defenses) - notes  
* DataCamp_Insights_v003_c captures Intermediate Interactive Defensive R Programming - Chapter 4 (Creating a Battle Plan) - notes  
* DataCamp_Insights_v003_c captures Feature Engineering in R - Chapter 1 (Creating Features from Categorical Data) - notes  
* DataCamp_Insights_v003_c captures Feature Engineering in R - Chapter 2 (Creating Features from Numeric Data) - notes  
* DataCamp_Insights_v003_c captures Feature Engineering in R - Chapter 3 (Transforming Numerical Features) - notes  
* DataCamp_Insights_v003_c captures Feature Engineering in R - Chapter 4 (Advanced Methods) - notes  
  
### Expansions week of 12-AUG-2019  
* DataCamp_Insights_v003_c captures Topic Modeling in R - Chapter 1 (Introduction to the Workflow) - code  
* DataCamp_Insights_v003_c captures Topic Modeling in R - Chapter 2 (Word Clouds, Stop Words, Control Arguments) - code  
* DataCamp_Insights_v003_c captures Topic Modeling in R - Chapter 3 (Named Entity Recognition as Unsupervised Classification) - code  
* DataCamp_Insights_v003_c captures Introduction to Text Analysis in R - Chapter 1 (Wrangling Text) - notes  
* DataCamp_Insights_v003_c captures Introduction to Text Analysis in R - Chapter 2 (Visualizing Text) - notes  
* DataCamp_Insights_v003_c captures Introduction to Text Analysis in R - Chapter 3 (Sentiment Analysis) - notes  
* DataCamp_Insights_v003_c captures Introduction to Text Analysis in R - Chapter 4 (Topic Modeling) - notes  
  
### Expansions week of 19-AUG-2019  
* DataCamp_Insights_v003_c_extend captures Survey and Measure Development in R - Chapter 1 (Prepare to Analyze Survey Data) - notes  
* DataCamp_Insights_v003_c_extend captures Survey and Measure Development in R - Chapter 2 (EFA and Survey Development) - notes  
* DataCamp_Insights_v003_c_extend captures Survey and Measure Development in R - Chapter 3 (CFA and Construct Validation) - notes  
* DataCamp_Insights_v003_c_extend captures Survey and Measure Development in R - Chapter 4 (Criterion Validity and Replication) - notes  
* DataCamp_Insights_v003_c captures Feature Engineering in R - Chapter 1 (Creating Features from Categorical Data) - code  
* DataCamp_Insights_v003_c captures Feature Engineering in R - Chapter 2 (Creating Features from Numeric Data) - code  
* DataCamp_Insights_v003_c captures Feature Engineering in R - Chapter 3 (Transforming Numerical Features) - code  
  
### Expansions week of 26-AUG-2019  
* DataCamp_Insights_v003_c captures Feature Engineering in R - Chapter 4 (Advanced Methods) - code  
* DataCamp_Insights_v003_c captures Data Science for Managers - Chapter 1 (Introduction to Data Science) - notes  
* DataCamp_Insights_v003_c captures Data Science for Managers - Chapter 2 (Data Sources and Risks) - notes  
* DataCamp_Insights_v003_c captures Data Science for Managers - Chapter 3 (Analysis and Visualization) - notes  
* DataCamp_Insights_v003_c captures Data Science for Managers - Chapter 4 (Prediction) - notes  
* DataCamp_Insights_v003_c captures R for SAS Users - Chapter 1 (Getting Started with R) - notes  
* DataCamp_Insights_v003_c captures R for SAS Users - Chapter 2 (Data Wrangling) - notes  
  
### Expansions week of 02-SEP-2019  
* DataCamp_Insights_v003_c captures R for SAS Users - Chapter 3 (Data Exploration) - notes  
* DataCamp_Insights_v003_c captures R for SAS Users - Chapter 4 (Models and Presentation) - notes  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 1 (The Basics) - notes  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 2 (Vectors and Matrices) - notes  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 3 (Data Frames) - notes  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 4 (Factors) - notes  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 5 (Lists) - notes  
  
### Expansions week of 09-SEP-2019  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 1 (Dates) - notes  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 2 (If Statements and Operators) - notes  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 3 (Loops) - notes  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 4 (Functions) - notes  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 5 (Apply) - notes  
* DataCamp_Insights_v003_c captures Importing and Managing Financial Data R - Chapter 1 (Introduction and Downloading Data) - notes  
* DataCamp_Insights_v003_c captures Importing and Managing Financial Data R - Chapter 2 (Extracting and Transforming Data) - notes  
  
### Expansions week of 16-SEP-2019  
* DataCamp_Insights_v003_c captures Importing and Managing Financial Data R - Chapter 3 (Data from Multiple Sources) - notes  
* DataCamp_Insights_v003_c captures Importing and Managing Financial Data R - Chapter 4 (Aligning Data with Different Periodicities) - notes  
* DataCamp_Insights_v003_c captures Importing and Managing Financial Data R - Chapter 5 (Importing Text Data; Adjusting for Corporate Actions) - notes  
* DataCamp_Insights_v003_c captures Introduction to Text Analysis in R - Chapter 1 (Wrangling Text) - code  
* DataCamp_Insights_v003_c captures Introduction to Text Analysis in R - Chapter 2 (Visualizing Text) - code  
* DataCamp_Insights_v003_c captures Introduction to Text Analysis in R - Chapter 3 (Sentiment Analysis) - code  
* DataCamp_Insights_v003_c captures Introduction to Text Analysis in R - Chapter 4 (Topic Modeling) - code  
  
### Expansions week of 23-SEP-2019  
* DataCamp_Insights_v003_c_extend captures Survey and Measure Development in R - Chapter 1 (Preparing to Analyze Survey Data) - code  
* DataCamp_Insights_v003_c_extend captures Survey and Measure Development in R - Chapter 2 (EFA and Survey Development) - code  
* DataCamp_Insights_v003_c_extend captures Survey and Measure Development in R - Chapter 3 (CFA and Construct Validation) - code  
* DataCamp_Insights_v003_c_extend captures Survey and Measure Development in R - Ch 4 (Criterion Validity and Replication) - code  
* DataCamp_Insights_v003_c captures R for SAS Users - Chapter 1 (Getting Started with R) - code  
* DataCamp_Insights_v003_c captures R for SAS Users - Chapter 2 (Data Wrangling) - code  
* DataCamp_Insights_v003_c captures R for SAS Users - Chapter 3 (Data Exploration) - code  
  
### Expansions week of 30-SEP-2019  
* DataCamp_Insights_v003_c captures R for SAS Users - Chapter 4 (Models and Presentation) - code  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 1 (The Basics) - code  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 2 (Vectors and Matrices) - code  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 3 (Data Frames) - code  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 4 (Factors) - code  
* DataCamp_Insights_v003_c captures Introduction to R for Finance - Chapter 5 (Lists) - code  
* DataCamp_Insights_v003_c captures Introduction to Portfolio Analysis in R - Chapter 1 (Building Blocks) - notes  
  
### Expansions week of 07-OCT-2019  
* DataCamp_Insights_v003_c captures Introduction to Portfolio Analysis in R - Chapter 2 (Analyzing Performance) - notes  
* DataCamp_Insights_v003_c captures Introduction to Portfolio Analysis in R - Chapter 3 (Performance Drivers) - notes  
* DataCamp_Insights_v003_c captures Introduction to Portfolio Analysis in R - Chapter 4 (Optimizing the Portfolio) - notes  
* DataCamp_Insights_v003_c captures Introduction to Portfolio Analysis in R - Chapter 1 (Building Blocks) - code  
* DataCamp_Insights_v003_c captures Introduction to Portfolio Analysis in R - Chapter 2 (Analyzing Performance) - code  
* DataCamp_Insights_v003_c captures Introduction to Portfolio Analysis in R - Chapter 3 (Performance Drivers) - code  
* DataCamp_Insights_v003_c captures Introduction to Portfolio Analysis in R - Chapter 4 (Optimizing the Portfolio) - code  
  
### Expansions week of 14-OCT-2019  
* DataCamp_Insights_v003_c captures Intermediate Portfolio Analysis in R - Chapter 1 (Introduction to Portfolio Theory) - notes  
* DataCamp_Insights_v003_c captures Intermediate Portfolio Analysis in R - Chapter 2 (Portfolio Optimization Workflow) - notes  
* DataCamp_Insights_v003_c captures Intermediate Portfolio Analysis in R - Chapter 3 (Objective Functions and Moment Estimation) - notes  
* DataCamp_Insights_v003_c captures Intermediate Portfolio Analysis in R - Chapter 4 (Application) - notes  
* DataCamp_Insights_v003_c captures Bond Valuation and Analysis in R - Chapter 1 (Introduction to Bond Valuation) - notes  
* DataCamp_Insights_v003_c captures Bond Valuation and Analysis in R - Chapter 2 (Yield to Maturity) - notes  
* DataCamp_Insights_v003_c captures Bond Valuation and Analysis in R - Chapter 3 (Duration and Convexity) - notes  
  
### Expansions week of 21-OCT-2019  
* DataCamp_Insights_v003_c captures Bond Valuation and Analysis in R - Chapter 4 (Comprehensive Example) - notes  
* DataCamp_Insights_v003_c captures Importing and Managing Financial Data R - Chapter 1 (Introduction and Downloading Data) - code  
* DataCamp_Insights_v003_c captures Importing and Managing Financial Data R - Chapter 2 (Extracting and Transforming Data) - code  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 1 (Dates) - code  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 2 (If Statements and Operators) - code  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 3 (Loops) - code  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 4 (Functions) - code  
  
### Expansions week of 28-OCT-2019  
* DataCamp_Insights_v003_c captures Intermediate R for Finance - Chapter 5 (Apply) - code  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 1 (Present Value Approaches) - notes  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 2 (Perpetuity Growth Rate, Analyzing Projections, Dividend Discount Model) - notes  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 3 (Discount Rate and Cost of Capital) - notes  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 4 (Relative Valuation) - notes  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 5 (Comprehensive Valuation) - notes  
* DataCamp_Insights_v003_c captures Credit Risk Modeling in R - Chapter 1 (Introduction and Data Pre-Processing) - notes  
  
### Expansions week of 04-NOV-2019  
* DataCamp_Insights_v003_c captures Credit Risk Modeling in R - Chapter 2 (Logistic Regression) - notes  
* DataCamp_Insights_v003_c captures Credit Risk Modeling in R - Chapter 3 (Decision Trees) - notes  
* DataCamp_Insights_v003_c captures Credit Risk Modeling in R - Chapter 4 (Evaluating Credit Risk Models) - notes  
* DataCamp_Insights_v003_c captures Financial Trading in R - Chapter 1 (Trading Basics) - notes  
* DataCamp_Insights_v003_c captures Financial Trading in R - Chapter 2 (Boilerplate quantsrat Strategies) - notes  
* DataCamp_Insights_v003_c captures Financial Trading in R - Chapter 3 (Indicators) - notes  
* DataCamp_Insights_v003_c captures Financial Trading in R - Chapter 4 (Signals) - notes  
  
### Expansions week of 11-NOV-2019  
* DataCamp_Insights_v003_c captures Financial Trading in R - Chapter 5 (Rules) - notes  
* DataCamp_Insights_v003_c captures Financial Trading in R - Chapter 6 (Analyzing Results) - notes  
* DataCamp_Insights_v003_c captures Quantitative Risk Management in R - Chapter 1 (Exploring Market Risk-Factor Data) - notes  
* DataCamp_Insights_v003_c captures Quantitative Risk Management in R - Ch 2 (Real World Returns are Riskier than Normal) - notes  
* DataCamp_Insights_v003_c captures Quantitative Risk Management in R - Chapter 3 (Real World Returns are Volatile and Correlated) - notes  
* DataCamp_Insights_v003_c captures Quantitative Risk Management in R - Chapter 4 (Estimating Portfolio Value at Risk - VaR) - notes  
* DataCamp_Insights_v003_c captures Valuation of Insurance products in R - Chapter 1 (Valuation of Cash flows) - notes  
  
### Expansions week of 18-NOV-2019  
* DataCamp_Insights_v003_c captures Valuation of Insurance products in R - Chapter 2 (Life Tables) - notes  
* DataCamp_Insights_v003_c captures Valuation of Insurance products in R - Chapter 3 (Life Annuities) - notes  
* DataCamp_Insights_v003_c captures Valuation of Insurance products in R - Chapter 4 (Life Insurances) - notes  
* DataCamp_Insights_v003_c captures Intermediate Portfolio Analysis in R - Chapter 1 (Introduction to Portfolio Theory) - code  
* DataCamp_Insights_v003_c captures Intermediate Portfolio Analysis in R - Chapter 2 (Portfolio Optimization Workflow) - code  
* DataCamp_Insights_v003_c captures Intermediate Portfolio Analysis in R - Ch 3 (Objective Functions and Moment Estimation) - code  
* DataCamp_Insights_v003_c captures Intermediate Portfolio Analysis in R - Chapter 4 (Application) - code  
  
### Expansions week of 25-NOV-2019  
* DataCamp_Insights_v003_c captures Bond Valuation and Analysis in R - Chapter 1 (Introduction to Bond Valuation) - code  
* DataCamp_Insights_v003_c captures Bond Valuation and Analysis in R - Chapter 2 (Yield to Maturity) - code  
* DataCamp_Insights_v003_c captures Bond Valuation and Analysis in R - Chapter 3 (Duration and Convexity) - code  
* DataCamp_Insights_v003_c captures Bond Valuation and Analysis in R - Chapter 4 (Comprehensive Example) - code  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 1 (Present Value Approaches) - code  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 2 (Perpetuity Growth Rate, Analyzing Projections, Dividend Discount Model) - code  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 3 (Discount Rate and Cost of Capital) - code  
  
### Expansions week of 02-DEC-2019  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 4 (Relative Valuation) - code  
* DataCamp_Insights_v003_c captures Equity Valuation in R - Chapter 5 (Comprehensive Valuation) - code  
* DataCamp_Insights_v003_c captures Data Manipulation with dplyr in R - Chapter 1 (Transforming Data with dplyr) - notes  
* DataCamp_Insights_v003_c captures Data Manipulation with dplyr in R - Chapter 2 (Aggregating Data) - notes  
* DataCamp_Insights_v003_c captures Data Manipulation with dplyr in R - Chapter 3 (Selecting and Transforming Data) - notes  
* DataCamp_Insights_v003_c captures Data Manipulation with dplyr in R - Chapter 4 (Case Study) - notes  
* DataCamp_Insights_v003_c captures Introduction to Function Writing in R - Chapter 1 (How to Write a Function) - notes  
  
### Expansions week of 09-DEC-2019  
* DataCamp_Insights_v003_c captures Introduction to Function Writing in R - Chapter 2 (Arguments) - notes  
* DataCamp_Insights_v003_c captures Introduction to Function Writing in R - Chapter 3 (Return Values and Scope) - notes  
* DataCamp_Insights_v003_c captures Introduction to Function Writing in R - Chapter 4 (Case Study on Grain Yields) - notes  
* DataCamp_Insights_v003_c captures Credit Risk Modeling in R - Chapter 1 (Introduction and Data Pre-Processing) - code  
* DataCamp_Insights_v003_c captures Credit Risk Modeling in R - Chapter 2 (Logistic Regression) - code  
* DataCamp_Insights_v003_c captures Credit Risk Modeling in R - Chapter 3 (Decision Trees) - code  
* DataCamp_Insights_v003_c captures Credit Risk Modeling in R - Chapter 4 (Evaluating Credit Risk Models) - code  
  
### Expansions week of 16-DEC-2019  
* DataCamp_Insights_v003_c captures Introduction to Data Visualization in R - Chapter 1 (Introduction) - notes  
* DataCamp_Insights_v003_c captures Introduction to Data Visualization in R - Chapter 2 (Aesthetics) - notes  
* DataCamp_Insights_v003_c captures Introduction to Data Visualization in R - Chapter 3 (Geometries) - notes  
* DataCamp_Insights_v003_c captures Introduction to Data Visualization in R - Chapter 4 (Themes) - notes  
* DataCamp_Insights_v003_c captures Quantitative Risk Management in R - Chapter 1 (Exploring Market Risk-Factor Data) - code  
* DataCamp_Insights_v003_c captures Quantitative Risk Management in R - Ch 2 (Real World Returns are Riskier than Normal) - code  
* DataCamp_Insights_v003_c captures Quantitative Risk Management in R - Ch 3 (Real World Returns are Volatile and Correlated) - code  
  
### Expansions week of 23-DEC-2019  
* DataCamp_Insights_v003_c captures Quantitative Risk Management in R - Chapter 4 (Estimating Portfolio Value at Risk - VaR) - code  
* DataCamp_Insights_v003_c captures Preparing for Machine Learning Questions in R - Chapter 1 (Data Pre-Processing and Visualization) - notes  
* DataCamp_Insights_v003_c captures Preparing for Machine Learning Questions in R - Chapter 2 (Supervised Learning) - notes  
* DataCamp_Insights_v003_c captures Preparing for Machine Learning Questions in R - Chapter 3 (Unsupervised Learning) - notes  
* DataCamp_Insights_v003_c captures Preparing for Machine Learning Questions in R - Chapter 4 (Model Evaluaion) - notes  
* DataCamp_Insights_v003_c captures Valuation of Insurance products in R - Chapter 1 (Valuation of Cash flows) - code  
* DataCamp_Insights_v003_c captures Valuation of Insurance products in R - Chapter 2 (Life Tables) - code  
  
### Expansions week of 30-DEC-2019  
* DataCamp_Insights_v003_c captures Valuation of Insurance products in R - Chapter 3 (Life Annuities) - code  
* DataCamp_Insights_v003_c captures Valuation of Insurance products in R - Chapter 4 (Life Insurances) - code  
* DataCamp_Insights_v003_c captures Introduction to Natural Language Processing in R - Chapter 1 (True Fundamentals) - notes  
* DataCamp_Insights_v003_c captures Introduction to Natural Language Processing in R - Chapter 2 (Representations of Text) - notes  
* DataCamp_Insights_v003_c captures Introduction to Natural Language Processing in R - Chapter 3 (Applications: Classification and Topic Modeling) - notes  
* DataCamp_Insights_v003_c captures Introduction to Natural Language Processing in R - Chapter 4 (Advanced Techniques) - notes  
* DataCamp_Insights_v003_c captures Data Manipulation with dplyr in R - Chapter 1 (Transforming Data with dplyr) - code  
  
### Expansions week of 06-JAN-2020  
* DataCamp_Insights_v003_c captures Data Manipulation with dplyr in R - Chapter 2 (Aggregating Data) - code  
* DataCamp_Insights_v003_c captures Data Manipulation with dplyr in R - Chapter 3 (Selecting and Transforming Data) - code  
* DataCamp_Insights_v003_c captures Data Manipulation with dplyr in R - Chapter 4 (Case Study) - code  
* DataCamp_Insights_v003_c captures Joining Data with dplyr in R - Chapter 1 (Joining Tables) - notes  
* DataCamp_Insights_v003_c captures Joining Data with dplyr in R - Chapter 2 (Left and Right Joins) - notes  
* DataCamp_Insights_v003_c captures Joining Data with dplyr in R - Chapter 3 (Full, Semi, and Anti Joins) - notes  
* DataCamp_Insights_v003_c captures Joining Data with dplyr in R - Chapter 4 (Case Study: Stack Overflow) - notes  
  
### Expansions week of 13-JAN-2020  
* DataCamp_Insights_v003_c captures Introduction to TensorFlow in R - Chapter 1 (Introducing TensorFlow in R) - notes  
* DataCamp_Insights_v003_c captures Introduction to TensorFlow in R - Chapter 2 (Linear Regression Using Two TensorFlow API) - notes  
* DataCamp_Insights_v003_c captures Introduction to TensorFlow in R - Chapter 3 (Deep Learning in TensorFlow: Creating a Deep Neural Network) - notes  
* DataCamp_Insights_v003_c captures Introduction to TensorFlow in R - Chapter 4 (Deep Learning in TensorFlow: Increasing Model Accuracy) - notes  
* DataCamp_Insights_v003_c captures Introduction to Function Writing in R - Chapter 1 (How to Write a Function) - code  
* DataCamp_Insights_v003_c captures Introduction to Function Writing in R - Chapter 2 (Arguments) - code  
* DataCamp_Insights_v003_c captures Introduction to Function Writing in R - Chapter 3 (Return Values and Score) - code  
  
### Expansions week of 20-JAN-2020  
* DataCamp_Insights_v003_c captures Introduction to Function Writing in R - Chapter 4 (Case Study on Grain Yields) - code  
* DataCamp_Insights_v003_c captures Market Basket Analysis in R - Chapter 1 (Introduction to Market Basket Analysis) - notes  
* DataCamp_Insights_v003_c captures Market Basket Analysis in R - Ch 2 (Metrics and Techniques in Market Basket Analysis) - notes  
  

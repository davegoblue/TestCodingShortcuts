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
* DataCamp_Insights_v003_c captures Market Basket Analysis in R - Chapter 3 (Visualization in Market Basket Analysis) - notes  
* DataCamp_Insights_v003_c captures Market Basket Analysis in R - Chapter 4 (Case Study: Market Basket with Movies) - notes  
* DataCamp_Insights_v003_c captures Introduction to Data Visualization in R - Chapter 1 (Introduction) - code  
* DataCamp_Insights_v003_c captures Introduction to Data Visualization in R - Chapter 2 (Aesthetics) - code  
  
### Expansions week of 27-JAN-2020  
* DataCamp_Insights_v003_c captures Introduction to Data Visualization in R - Chapter 3 (Geometries) - code  
* DataCamp_Insights_v003_c captures Introduction to Data Visualization in R - Chapter 4 (Themes) - code  
* DataCamp_Insights_v003_c captures Analyzing Social Media Data in R - Chapter 1 (Understanding Twitter Data) - notes  
* DataCamp_Insights_v003_c captures Analyzing Social Media Data in R - Chapter 2 (Analyzing Twitter Data) - notes  
* DataCamp_Insights_v003_c captures Analyzing Social Media Data in R - Chapter 3 (Visualize Tweet Texts) - notes  
* DataCamp_Insights_v003_c captures Analyzing Social Media Data in R - Chapter 4 (Network Analysis and Mapping) - notes  
* DataCamp_Insights_v003_c captures Preparing for Machine Learning Questions in R - Chapter 1 (Data Pre-Processing and Visualization) - code  
  
### Expansions week of 03-FEB-2020  
* DataCamp_Insights_v003_c captures Preparing for Machine Learning Questions in R - Chapter 2 (Supervised Learning) - code  
* DataCamp_Insights_v003_c captures Preparing for Machine Learning Questions in R - Chapter 3 (Unsupervised Learning) - code  
* DataCamp_Insights_v003_c captures Preparing for Machine Learning Questions in R - Chapter 4 (Model Evaluation) - code  
* DataCamp_Insights_v003_c captures Building Web Applications with Shiny in R - Chapter 1 (Get Started with Shiny) - notes  
* DataCamp_Insights_v003_c captures Building Web Applications with Shiny in R - Chapter 2 (Inputs, Outputs, and Layouts) - notes  
* DataCamp_Insights_v003_c captures Building Web Applications with Shiny in R - Chapter 3 (Reactive Programming) - notes  
* DataCamp_Insights_v003_c captures Building Web Applications with Shiny in R - Chapter 4 (Build Shiny Apps) - notes  
  
### Expansions week of 10-FEB-2020  
* DataCamp_Insights_v003_c captures Intermediate Data Visualization with ggplot2 - Chapter 1 (Statistics) - notes  
* DataCamp_Insights_v003_c captures Intermediate Data Visualization with ggplot2 - Chapter 2 (Coordinates) - notes  
* DataCamp_Insights_v003_c captures Intermediate Data Visualization with ggplot2 - Chapter 3 (Facets) - notes  
* DataCamp_Insights_v003_c captures Intermediate Data Visualization with ggplot2 - Chapter 4 (Best Practices) - notes  
* DataCamp_Insights_v003_c captures Introduction to Natural Language Processing in R - Chapter 1 (True Fundamentals) - code  
* DataCamp_Insights_v003_c captures Introduction to Natural Language Processing in R - Chapter 2 (Representations of Text) - code  
* DataCamp_Insights_v003_c captures Introduction to Natural Language Processing in R - Chapter 3 (Applications: Classification and Topic Modeling) - code  
  
### Expansions week of 17-FEB-2020  
* DataCamp_Insights_v003_c captures Introduction to Natural Language Processing in R - Chapter 4 (Advanced Techniques) - code  
* DataCamp_Insights_v003_c captures Statistics Interview Questions in R - Chapter 1 (Probability Distributions) - notes  
* DataCamp_Insights_v003_c captures Statistics Interview Questions in R - Chapter 2 (Exploratory Data Analysis) - notes  
* DataCamp_Insights_v003_c captures Statistics Interview Questions in R - Chapter 3 (Statistical Tests) - notes  
* DataCamp_Insights_v003_c captures Statistics Interview Questions in R - Chapter 4 (Regression Models) - notes  
* DataCamp_Insights_v003_c captures Joining Data with dplyr in R - Chapter 1 (Joining Tables) - code  
* DataCamp_Insights_v003_c captures Joining Data with dplyr in R - Chapter 2 (Left and Right Joins) - code  
  
### Expansions week of 24-FEB-2020  
* DataCamp_Insights_v003_c captures Joining Data with dplyr in R - Chapter 3 (Full, Semi, and Anti Joins) - code  
* DataCamp_Insights_v003_c captures Joining Data with dplyr in R - Chapter 4 (Case Study: Stack Overflow) - code  
* DataCamp_Insights_v003_c captures Intermediate Regular Epressions in R - Chapter 1 (Regular Epressions: Writing Custom Patterns) - notes  
* DataCamp_Insights_v003_c captures Intermediate Regular Epressions in R - Chapter 2 (Creating Strings with Data) - notes  
* DataCamp_Insights_v003_c captures Intermediate Regular Epressions in R - Chapter 3 (Extracting Structured Data from Text) - notes  
* DataCamp_Insights_v003_c captures Intermediate Regular Epressions in R - Chapter 4 (Similarities Between Strings) - notes  
* DataCamp_Insights_v003_c captures Market Basket Analysis in R - Chapter 1 (Introduction to Market Basket Analysis) - code  
  
### Expansions week of 02-MAR-2020  
* DataCamp_Insights_v003_c captures Market Basket Analysis in R - Ch 2 (Metrics and Techniques in Market Basket Analysis) - code  
* DataCamp_Insights_v003_c captures Market Basket Analysis in R - Chapter 3 (Visualization in Market Basket Analysis) - code  
* DataCamp_Insights_v003_c captures Market Basket Analysis in R - Chapter 4 (Case Study: Market Basket with Movies) - code  
* DataCamp_Insights_v003_c truncated to everything through Valuation of Insurance Products in R, and then archived  
* DataCamp_Insights_v003_d created as a split ffrom v003_c; starts with Data Manipulation with dplyr in R  
* DataCamp_Insights_v003_d captures Intermediate Data Visualization with ggplot2 - Chapter 1 (Statistics) - code  
* DataCamp_Insights_v003_d captures Intermediate Data Visualization with ggplot2 - Chapter 2 (Coordinates) - code  
* DataCamp_Insights_v003_d captures Intermediate Data Visualization with ggplot2 - Chapter 3 (Facets) - code  
  
### Expansions week of 09-MAR-2020  
* DataCamp_Insights_v003_d captures Intermediate Data Visualization with ggplot2 - Chapter 4 (Best Practices) - code  
* AdditionalCoding_202003_v001 created for some mapping and plotting examples  
* AdditionalCoding_202003_v001 contains base state and county maps from usmap::plot_usmap(), CRS conversions from usmap::usmap_transform(), and data from the included usmap data sets  
* AdditionalCoding_202003_v001 contains filtering and coloring by US census region  
* AdditionalCoding_202003_v001 contains default and custom labelling by geography  
* AdditionalCoding_202003_v001 contains population centers as points and text  
* AdditionalCoding_202003_v001 contains custom coloring for geographies  
  
### Expansions week of 16-MAR-2020  
* AdditionalCoding_202003_v001 contains custom labelling for geographies  
* AdditionalCoding_202003_v001 contains plotting for weather data (temperature and dewpoint)  
* AdditionalCoding_202003_v001 contains examples for combining xts and ggplot2  
* AdditionalCoding_202003_v001 contains plotting for weather data (humidity)  
* AdditionalCoding_202003_v001 contains plotting for weather data (wind)  
* AdditionalCoding_202003_v001 contains archived granular weather data (METAR)  
* AdditionalCoding_202003_v001 contains extraction and parsing of METAR data  
  
### Expansions week of 23-MAR-2020  
* AdditionalCoding_202003_v001 contains relationships between METAR variables  
* AdditionalCoding_202003_v001 contains extracting cloud data from METAR  
* AdditionalCoding_202003_v001 contains plotting by factor variables  
* AdditionalCoding_202003_v001 contains functions for METAR read-in and initial wind data processing  
* AdditionalCoding_202003_v001 contains functions for extracting key information from METAR  
* AdditionalCoding_202003_v001 contains functions for analyzing relationships between METAR variables  
* AdditionalCoding_202003_v001 contains functions for extracting cloud data from METAR  
  
### Expansions week of 30-MAR-2020  
* AdditionalCoding_202003_v001 contains functions for plotting by factor variables  
* AdditionalCoding_202003_v001 contains integrated functions for running METAR analyses  
* AdditionalCoding_202003_v001 contains integrated functions run for a different station  
* AdditionalCoding_202003_v001 contains functions for METAR data download  
* AdditionalCoding_202003_v001 contains more generic functions for METAR data download  
* AdditionalCoding_202003_v001 contains functions for comparative wind directions by location  
* DataCamp_Insights_v003_d captures Data Cleaning in R - Chapter 1 (Common Data Problems) - notes  
  
### Expansions week of 06-APR-2020  
* DataCamp_Insights_v003_d captures Data Cleaning in R - Chapter 2 (Categorical and Text Data) - notes  
* DataCamp_Insights_v003_d captures Data Cleaning in R - Chapter 3 (Advanced Data Problems) - notes  
* DataCamp_Insights_v003_d captures Data Cleaning in R - Chapter 4 (Record Linkage) - notes  
* DataCamp_Insights_v003_d captures Handling Missing Data with Imputations in R - Chapter 1 (Problem of Missing Data) - notes  
* DataCamp_Insights_v003_d captures Handling Missing Data with Imputations in R - Chapter 2 (Donor Based Imputation) - notes  
* DataCamp_Insights_v003_d captures Handling Missing Data with Imputations in R - Chapter 3 (Model Based Imputation) - notes  
* DataCamp_Insights_v003_d captures Handling Missing Data with Imputations in R - Chapter 4 (Uncertainty From Imputation) - notes  
  
### Expansions week of 13-APR-2020  
* DataCamp_Insights_v003_c captures Statistics Interview Questions in R - Chapter 1 (Probability Distributions) - code  
* DataCamp_Insights_v003_c captures Statistics Interview Questions in R - Chapter 2 (Exploratory Data Analysis) - code  
* DataCamp_Insights_v003_c captures Statistics Interview Questions in R - Chapter 3 (Statistical Tests) - code  
* DataCamp_Insights_v003_c captures Statistics Interview Questions in R - Chapter 4 (Regression Models) - code  
* DataCamp_Insights_v003_c captures Intermediate Regular Epressions in R - Chapter 1 (Regular Epressions: Writing Custom Patterns) - code  
* DataCamp_Insights_v003_c captures Intermediate Regular Epressions in R - Chapter 2 (Creating Strings with Data) - code  
* DataCamp_Insights_v003_c captures Intermediate Regular Epressions in R - Chapter 3 (Extracting Structured Data from Text) - code  
  
### Expansions week of 20-APR-2020  
* DataCamp_Insights_v003_c captures Intermediate Regular Epressions in R - Chapter 4 (Similarities Between Strings) - code  
* AdditionalCoding_202003_v001 contains extraction capability for rain data  
* AdditionalCoding_202003_v001 contains functions for extraction capability for rain data  
* AdditionalCoding_202003_v001 combines functions and extends to other locales and precipitation types  
* AdditionalCoding_202003_v001 checks precipitation intervals for consistency  
* AdditionalCoding_202003_v001 detects all precipitation types in specified METAR data  
* AdditionalCoding_202003_v001 extracts precipitation amounts from specified METAR data  
  
### Expansions week of 27-APR-2020  
* AdditionalCoding_202003_v001 checks consistency of precipitation amounts from specified METAR data  
* AdditionalCoding_202003_v001 checks for gaps and reported sensor anomalies in specified METAR data  
* AdditionalCoding_202003_v001 checks other time periods and locales  
* AdditionalCoding_202003_v001 automtates interval checks to suggest precipitation begin and end times  
* AdditionalCoding_202003_v001 includes functions to automtate suggestions for precipitation begin and end times  
* AdditionalCoding_202003_v001 extends precipitation consistency checks to other locales  
* AdditionalCoding_202003_v001 runs consistency checks for precipitation amounts  
  
### Expansions week of 04-MAY-2020  
* AdditionalCoding_202003_v001 runs comparisons against an external source  
* AdditionalCoding_202003_v001 investigates wind by season and locale  
* AdditionalCoding_202003_v001 predicts locale by wind and month  
* AdditionalCoding_202003_v001 expands on predictions for locale by wind and month  
* AdditionalCoding_202003_v001 predicts locale by wind, temperature, dewpoint, and month  
* AdditionalCoding_202003_v001 includes a function for predictions by locale  
* AdditionalCoding_202003_v001 explores relationships between prediction accuracy and variables  
  
### Expansions week of 11-MAY-2020  
* AdditionalCoding_202003_v001 predicts between two similar locales using wind, temperature, dewpoint, and month  
* AdditionalCoding_202003_v001 extends predictions to years not included in modeling  
* AdditionalCoding_202003_v001 predicts using multiple locales, some from similar climates  
* AdditionalCoding_202003_v001 includes additional diagnostics of accuracy  
* AdditionalCoding_202003_v001 includes diagnostics by month and locale  
* AdditionalCoding_202003_v001 includes accuracy by metric and locale  
* AdditionalCoding_202003_v002 created  
* AdditionalCoding_202003_v002 includes extensions to accuracy by metric and locale  
  
### Expansions week of 18-MAY-2020  
* AdditionalCoding_202003_v002 explores archetype weather conditions  
* AdditionalCoding_202003_v002 includes functional form for exploring archetype weather conditions  
* AdditionalCoding_202003_v002 further explores the Wintry archetype  
* WeatherDownloads_202005_v001 created to isolate METAR downloads and initial processing  
* WeatherDownloads_202005_v001 contains code for METAR downloads and determination of hourly read times  
* WeatherDownloads_202005_v001 contains code for processing a downloaded METAR file  
* WeatherDownloads_202005_v001 contains updated METAR cloud processing code  
* WeatherDownloads_202005_v001 checks for consistency of recorded times  
  
### Expansions week of 25-MAY-2020  
* WeatherDownloads_202005_v001 integrates processing steps  
* WeatherDownloads_202005_v001 updates core functions and integration  
* WeatherDownloads_202005_v002 contains final versions of the download and initial processing functions  
* WeatherEDA_202005_v001 contains functions for running exploratory data analysis on downloaded weather files  
* WeatherDownloads_202005_v002 contains final versions of the download and initial processing functions (updated to correct for 1/16 SM Visibility and convert Altimeter to inches of mercury)  
* WeatherEDA_202005_v001 combines functions for running exploratory data analysis on downloaded weather files  
* WeatherEDA_202005_v001 directs text and graphical output to .log and .pdf files  
* WeatherEDA_202005_v001 compares counts by metrics across multiple locales  
  
### Expansions week of 01-JUN-2020  
* WeatherEDA_202005_v001 updates EDA functions for comparisons across multiple locales  
* WeatherEDA_202005_v001 compares numeric vs. numeric and numeric vs. factors for multiple locales  
* WeatherEDA_202005_v001 creates Q1-Q2-Q3 plot (modified boxplot) and runs initial cloud exploration  
* WeatherEDA_202005_v001 compares maximum obscuration and minimum ceiling height by locale    
* WeatherEDA_202005_v001 segments locales based on distributions of clouds by month  
* WeatherEDA_202005_v001 explores precipitation data in METAR  
* WeatherEDA_202005_v001 combines functions for extracting and exploring precipitation data in METAR  
  
### Expansions week of 08-JUN-2020  
* WeatherEDA_202005_v001 runs precipitation functions for rain across locales in METAR  
* WeatherEDA_202005_v001 explores fixes to the precipitation extraction functions  
* WeatherEDA_202005_v001 includes re-writes for the precipitation extraction functions  
* WeatherEDA_202005_v001 includes combines precipitation extraction functions  
* WeatherEDA_202005_v001 includes runs precipitation extraction functions for all locales  
* WeatherEDA_202005_v001 extracts daily summaries for high-low temperature and precipitation amount  
* WeatherEDA_202005_v001 directs precipitation processing logs and plots to files  
  
### Expansions week of 15-JUN-2020  
* WeatherEDA_202005_v002 created to hold final functions from _v001  
* WeatherEDA_202005_v002 extracts daily temperature and precipitation summaries and runs cross-locale comparisons  
* WeatherEDA_202005_v002 wraps facetted plots across multiple pages  
* WeatherEDA_202005_v002 runs EDA for clouds data and stores output files  
* WeatherModeling_202006_v001 created to run models on the processed METAR data  
* WeatherModeling_202006_v001 makes predictions based on temperature and dew point  
* WeatherModeling_202006_v001 makes predictions for all locales  
  
### Expansions week of 22-JUN-2020  
* WeatherModeling_202006_v001 makes predictions by locale type  
* WeatherModeling_202006_v001 makes predictions for four climate archetypes  
* WeatherModeling_202006_v001 explores prediction confidence  
* WeatherModeling_202006_v001 explores impact of incremental variable addition and number of trees  
* WeatherModeling_202006_v001 explores impact of incremental variable addition and number of trees  
* WeatherDownloadFunctions_v001.R created as source file to hold weather download functions  
* WeatherEDAFunctions_v001.R created as source file to hold weather EDA functions  
* WeatherIntegrate_202006_v001 created to download and process weather data from additional stations  
* WeatherIntegrate_202006_v001 converted to functional form  
* WeatherEDAFunctions_v002.R contains a more complete set of weather EDA functions  
* WeatherIntegrate_202006_v002 created to leverage the functions in WeatherEDAFunctions_v002.R  
  
### Expansions week of 29-JUN-2020  
* WeatherModelingFunctions_v001.R created as source file to hold weather modeling functions  
* WeatherModeling_202006_v002 created for conversion to functional form  
* WeatherModelingFunctions_v001.R holds updated weather modeling functions  
* WeatherModeling_202006_v002 explores archetype cities for weather modeling  
* WeatherModeling_202006_v002 extends archetype city exploration  
* WeatherModeling_202006_v002 explores predicting year for a city  
* WeatherModeling_202006_v002 explores the impact of Altimeter and SLP  
* WeatherModeling_202006_v002 continues arechetype city exploration  
* WeatherModeling_202006_v002 continues arechetype consolidation  
  
### Expansions week of 06-JUL-2020  
* WeatherModeling_202006_v002 explores warmer weather locales  
* DataCamp_Insights_v003_d captures Reporting with R Markdown - Chapter 1 (Getting Started with R Markdown) - notes  
* DataCamp_Insights_v003_d captures Reporting with R Markdown - Chapter 2 (Adding Analyses and Visualizations) - notes  
* DataCamp_Insights_v003_d captures Reporting with R Markdown - Chapter 3 (Improving Reports) - notes  
* DataCamp_Insights_v003_d captures Reporting with R Markdown - Chapter 4 (Customizing Reports) - notes  
* DataCamp_Insights_v003_d captures Introduction to Regression in R - Chapter 1 (Simple Linear Regression) - notes  
* DataCamp_Insights_v003_d captures Introduction to Regression in R - Chapter 2 (Predictions and Model Objects) - notes  
  
### Expansions week of 13-JUL-2020  
* DataCamp_Insights_v003_d captures Introduction to Regression in R - Chapter 3 (Assessing Model Fit) - notes  
* DataCamp_Insights_v003_d captures Introduction to Regression in R - Chapter 4 (Simple Logistic Regression) - notes  
* WeatherModeling_202006_v002 makes an initial exploration of random forest regression for temperature  
* WeatherModeling_202006_v002 converts random forest regression to functional form  
* WeatherEDAFunctions_v002.R contains rfRegression() for running random forest regressions on weather data  
* WeatherModeling_202006_v002 creates random forest regression evaluation in functional form  
* WeatherEDAFunctions_v002.R contains functions for random forest regression evaluation  
* WeatherModeling_202006_v002 expands random forest regression evaluation  
* WeatherModeling_202006_v002 runs random forest regression with only categorical variables  
  
### Expansions week of 20-JUL-2020  
* WeatherModeling_202006_v002 runs random forest regression with mix of categorical and numerical variables  
* WeatherModeling_202006_v002 runs random forest regression separately by locale  
* WeatherModeling_202006_v002 evaluates random forest regression accuracy on two dimensions  
* WeatherModeling_202006_v002 evaluates evolution of RMSE using only categorical means  
* WeatherModeling_202006_v002 evaluates RMSE for random forest vs. categorical means  
* WeatherModeling_202006_v002 uses functional form to evaluate RMSE changes by model  
* WeatherModeling_202006_v002 evaluates RMSE volution by locale by explanatory variable  
  
### Expansions week of 27-JUL-2020  
* WeatherModeling_202006_v002 uses 2016 model to assess predictions on 2014-2019 data  
* WeatherModeling_202006_v002 creates model using 2014-2019 data  
* WeatherModeling_202006_v002 models using month as an explanatory variable rather separate models by month  
* WeatherModeling_202006_v002 models using locale as an explanatory variable rather separate models by locale  
* WeatherModeling_202006_v002 explores random forest performance using a dummy dataset  
* WeatherModeling_202006_v002 explores random forest performance using a dummy dataset  
* WeatherModeling_202006_v002 explores extreme gradient boosting (XGB)  
* WeatherModeling_202006_v002 runs XGB for temperature predictions  
  
### Expansions week of 03-AUG-2020  
* WeatherModeling_202006_v002 includes functions to run and evaluate XGB  
* WeatherModeling_202006_v002 explores test vs. train RMSE for XGB regression  
* WeatherModeling_202006_v002 adds CV capability for XGB training  
* WeatherModeling_202006_v002 adds XGB capability for binary classification  
* WeatherModeling_202006_v002 uses XGB to classify each locale as self vs all-others  
* WeatherModeling_202006_v002 uses XGB to classify locales one vs one  
* WeatherModeling_202006_v002 uses XGB for multi-class classification  
  
### Expansions week of 10-AUG-2020  
* WeatherModeling_202006_v002 uses XGB to train on 2015-2018 and predict on 2014 and 2019  
* WeatherModeling_202006_v002 creates initial attempt at integrated functions for XGB modeling  
* WeatherModeling_202006_v002 uses integrated XGB functons to model 10 locales with 2016 data  
* WeatherModeling_202006_v002 creates initial attempt at integrated functions for XGB model assessment  
* WeatherModeling_202006_v002 expands integrated functions for XGB model assessment  
* WeatherModeling_202006_v002 runs integrated XGB modeling and assessment functions  
* WeatherModeling_202006_v002 runs XGB classification for all 2016 locales  
  
### Expansions week of 17-AUG-2020  
* WeatherModeling_202006_v002 integrates one vs all XGB classification for 2016 locales  
* WeatherModeling_202006_v002 expands one vs all XGB classification for 2016 locales  
* WeatherModeling_202006_v002 integrates one vs one XGB classification for 2016 locales  
* WeatherModeling_202006_v002 integrates archetype XGB classification for 2016 locales  
* WeatherModeling_202006_v002 expands archetype XGB classification for 2016 locales  
* Coronavirus_Statistics_v001 explores data from COVID Tracking Project  
* Coronavirus_Statistics_v001 explores state-level segments for coronavirus  
  
### Expansions week of 24-AUG-2020  
* Coronavirus_Statistics_v001 creates functions for coronavirus exploration  
* Coronavirus_Statistics_v001 creates functions for state-level coronavirus segments  
* Coronavirus_Statistics_v001 integrates functions for state-level coronavirus segments  
* Coronavirus_Statistics_v001 adds kmeans as a segmentation option  
* Coronavirus_Statistics_v001 adds option to place multiple charts on one page  
* Coronavirus_Statistics_v001 explores hospitalization data  
* Coronavirus_Statistics_v001 analyzes hospitalization vs. time by segment  
  
### Expansions week of 31-AUG-2020  
* Coronavirus_Statistics_v001 includes functions for data download and initial checks  
* Coronavirus_Statistics_v001 includes functions for cross-metric comparisons by segment  
* Coronavirus_Statistics_v001 explores test data  
* Coronavirus_Statistics_v001 explores cumulative disease burden  
* Coronavirus_Statistics_v001 loads county-level data  
* Coronavirus_Statistics_v001 explores county-level data in New England and NY/NJ  
* Coronavirus_Statistics_v001 explores county-level data in southern hot-spots  
  
### Expansions week of 07-SEP-2020  
* Coronavirus_Statistics_v001 explores county-level disease evolution in southern hot-spots  
* Coronavirus_Statistics_v001 explores county-level disease burden by state cluster  
* Coronavirus_Statistics_v001 creates county-level segments  
* Coronavirus_Statistics_v001 updates assessClusters() to handle counties in all plots  
* Coronavirus_Statistics_v001 updates assessClusters() to be more modular  
* Coronavirus_Statistics_v001 explores various numbers of centers for kmeans  
* Coronavirus_Statistics_v001 explores disease evolution by county segment by state  
  
### Expansions week of 14-SEP-2020  
* Coronavirus_Statistics_v001 includes cumulative deaths by county segment by state  
* Coronavirus_Statistics_v001 reorders factors for more consistent colors and legends  
* Coronavirus_Statistics_v001 reorders factors prior to assessClusters()  
* Coronavirus_Statistics_v001 includes functional form for reading and converting data  
* Coronavirus_Statistics_v001 reads in more recent data  
* Coronavirus_Statistics_v001 explores relationships in trends for cases and deaths  
* Coronavirus_Statistics_v001 explores lag for cases and deaths in early pandemic  
  
### Expansions week of 21-SEP-2020  
* Coronavirus_Statistics_v001 converts lag exploration to functional form  
* Coronavirus_Statistics_v001 explores relationships between cases and deaths by county  
* Coronavirus_Statistics_v001 explores lags for early pandemic states  
* Coronavirus_Statistics_v001 explores lags for late pandemic states  
* Coronavirus_Statistics_v001 explores CDC 2015-2020 all-cause US death data  
* Coronavirus_Statistics_v001 explores age 65-74 cohort in CDC 2015-2020 all-cause US death data  
* Coronavirus_Statistics_v001 includes functional form for cohort exploration  
  
### Expansions week of 28-SEP-2020  
* Coronavirus_Statistics_v001 explores 2020 excess deaths by segment and nationally  
* Coronavirus_Statistics_v001 explores all-cause deaths vs. coronavirus deaths by segment  
* Coronavirus_Statistics_v001 explores all-cause deaths vs. coronavirus deaths by state  
* Coronavirus_Statistics_v001 explores all-cause deaths deaths by age cohort  
* Coronavirus_Statistics_v002 created to contain main components from _v001  
* Coronavirus_Statistics_v002 contains code for COVID Tracking Project data  
* Coronavirus_Statistics_v002 begins adding code for USA Facts data  
* Coronavirus_Statistics_v002 continues adding code for USA Facts data  
  
### Expansions week of 05-OCT-2020  
* Coronavirus_Statistics_v002 includes all code for USA Facts data  
* Coronavirus_Statistics_v002 begins adding code for CDC all-cause deaths data  
* Coronavirus_Statistics_v002 continues adding code for CDC all-cause deaths data  
* Coronavirus_Statistics_v002 includes all code for CDC all-cause deaths data  
* Coronavirus_Statistics_v002 fixes plot labels for CDC all-cause deaths data  
* Coronavirus_Statistics_Functions_v002.R created to hold functions for Coronavirus_Statistics_v002  
* Coronavirus_Statistics_v002 functions sourced from Coronavirus_Statistics_Functions_v002.R  
* Coronavirus_Statistics_v002 created state-level segments with new COVID Tracking Project data  
  
### Expansions week of 12-OCT-2020  
* Coronavirus_Statistics_v002 consolidates code for reading and processing USA Facts data  
* Coronavirus_Statistics_v002 consolidates code for downloading updated USA Facts data  
* Coronavirus_Statistics_v002 consolidates code for reading and processing CDC all-cause deaths data  
* Coronavirus_Statistics_v002 consolidates code for downloading updated CDC all-cause deaths data  
* Coronavirus_Statistics_Functions_v002.R holds updated functions for Coronavirus_Statistics_v002  
* Coronavirus_Statistics_v002 downloads new data and creates new segments  
* Coronavirus_Statistics_v002 downloads new data and applies against existing segments  
* Coronavirus_Statistics_v002 adds rules-based segmentation option  
  
### Expansions week of 19-OCT-2020  
* Coronavirus_Statistics_v002 explores other segmentation options  
* Coronavirus_Statistics_v002 further explores county-level segmentation  
* Coronavirus_Statistics_v002 explores county-level segmentation using cases  
* Coronavirus_Statistics_v002 further explores county-level segmentation using cases  
* Coronavirus_Statistics_v002 updates clustering functions for additional control options  
* Coronavirus_Statistics_v002 explores counties with heavy disease in early October  
* Coronavirus_Statistics_v002 includes most recent data  
  
### Expansions week of 26-OCT-2020  
* Coronavirus_Statistics_Functions_Shared_v003 contains functions shared across routines  
* Coronavirus_Statistics_Functions_CTP_v003 contains functions for state-level data from COVID Tracking Project  
* Coronavirus_Statistics_CTP_v003 created as a stand-alone for running state-level analysis of COVID Tracking Project data  
* Coronavirus_Statistics_Functions_USAF_v003 contains functions for county-level data from USA Facts  
* Coronavirus_Statistics_USAF_v003 created as a stand-alone for running county-level analysis of USA Facts data  
* Coronavirus_Statistics_Functions_CDC_v003 contains functions for all-cause deaths data from CDC  
* Coronavirus_Statistics_CDC_v003 created as a stand-alone for running all-cause death analysis using CDC data  
* Coronavirus_Statistics_State_v003 created to combine state-level data from multiple sources  
* Coronavirus_Statistics_State_v003 assesses deaths by state from multiple sources  
* Coronavirus_Statistics_State_v003 uses a secondary axis for burden methods with different scales  
* Coronavirus_Statistics_State_v003 adds capability to combine states  
  
### Expansions week of 02-NOV-2020  
* Coronavirus_Statistics_State_v003 assesses lag times between metrics  
* Coronavirus_Statistics_State_v003 assesses lag times and ratios over time between metrics  
* Coronavirus_Statistics_State_v003 allows for separate lags by state  
* Coronavirus_Statistics_State_v003 explores relationship between hospitalizations and deaths  
* Coronavirus_Statistics_State_v003 includes function for correlations by window and lag in vectors  
* Coronavirus_Statistics_State_v003 includes functional form for lag correlations  
* Coronavirus_Statistics_State_v003 includes functional form for lead correlations  
  
### Expansions week of 09-NOV-2020  
* Coronavirus_Statistics_DataUpdate_v003 created to download and integrate latest data  
* Coronavirus_Statistics_DataUpdate_v003 downloads latest USA Facts county-level data  
* Coronavirus_Statistics_DataUpdate_v003 downloads latest CDC all-cause deaths data  
* Coronavirus_Statistics_DataUpdate_v003 updates deaths per day comparison across data sources  
* Coronavirus_Statistics_DataUpdate_v003 updates integrated state-level database  
* Coronavirus_Statistics_DataUpdate_v003 updates state metrics on two axes  
* Coronavirus_Statistics_DataUpdate_v003 updates single lag time analysis  
  
### Expansions week of 16-NOV-2020  
* Coronavirus_Statistics_DataUpdate_v003 updates estimated CFR  
* Coronavirus_Statistics_DataUpdate_v003 updates lag and lead estimations  
* Coronavirus_Statistics_DataUpdate_v003 begins functional form for cross-metric comparisons  
* Coronavirus_Statistics_DataUpdate_v003 expands functional form for cross-metric comparisons  
* Coronavirus_Statistics_DataUpdate_v003 explores lags on other metrics  
* Coronavirus_Statistics_DataUpdate_v003 refreshed to most recent data  
* Coronavirus_Statistics_DataUpdate_v003 run for Plains states experiencing heavy current burden  
  
### Expansions week of 23-NOV-2020  
* Coronavirus_Statistics_DataUpdate_v003 adjusts for low-n and high-ratio outliers  
* Coronavirus_Statistics_DataUpdate_v003 updates summaries by state cluster  
* Coronavirus_Statistics_DataUpdate_v003 updates summaries for NY/NJ/CT/MA  
* Coronavirus_Statistics_DataUpdate_v003 updates summaries for southern states  
* Coronavirus_Statistics_DataUpdate_v003 updates summaries for earlier impacted northern states  
* Coronavirus_Statistics_DataUpdate_v003 updates summaries for large cohort of later impacted states  
* Coronavirus_Statistics_DataUpdate_v003 updates summaries for outlier cohort of lower impacted states  
  
### Expansions week of 30-NOV-2020  
* Coronavirus_Statistics_DataUpdate_v003 updates summaries for select states  
* Coronavirus_Statistics_DataUpdate_v003 updates COVID Tracking Project data and creates new state-level segments  
* Coronavirus_Statistics_DataUpdate_v003 assesses movement in state segments  
* Coronavirus_Statistics_DataUpdate_v003 assesses burden by state segment overlap  
* Coronavirus_Statistics_DataUpdate_v003 updates USA Facts data and creates new county-level segments  
* Coronavirus_Statistics_DataUpdate_v003 assesses movement in county segments  
* Coronavirus_Statistics_DataUpdate_v003 updates CDC all-cause deaths data  
  
### Expansions week of 07-DEC-2020  
* Coronavirus_Statistics_DataUpdate_v003 explores restatements over time in CDC all-cause deaths data  
* Coronavirus_Statistics_DataUpdate_v003 restatement function returns a list if requested  
* Coronavirus_Statistics_DataUpdate_v003 restatement function run on different time periods  
* Coronavirus_Statistics_DataUpdate_v003 restatement function can accept processed file as input  
* Coronavirus_Statistics_DataUpdate_v003 explores restatement percentage by week in early-September data  
* Coronavirus_Statistics_DataUpdate_v003 converts restatement percentage by week to functional form  
* Coronavirus_Statistics_DataUpdate_v003 explores restatement percentage by week in late-September data  
  
### Expansions week of 14-DEC-2020  
* Coronavirus_Statistics_DataUpdate_v003 updates CDC all-cause deaths data  
* Coronavirus_Statistics_DataUpdate_v003 updates COVID Tracking Project data and uses existing state-level segments  
* Coronavirus_Statistics_DataUpdate_v003 updates USA Facts data and uses existing county-level segments  
* Coronavirus_Statistics_DataUpdate_v003 compares state-level totals in USA Facts and COVID Tracking Project  
* Coronavirus_Statistics_DataUpdate_v003 compares IA, NY, VA totals in USA Facts and COVID Tracking Project  
* Coronavirus_Statistics_DataUpdate_v003 explores disparity in reported NY deaths  
* Coronavirus_Statistics_DataUpdate_v003 compares state-level absolute value deltas in USA Facts and COVID Tracking Project  
  
### Expansions week of 21-DEC-2020  
* Coronavirus_Statistics_DataUpdate_v003 explores Virginia disconnects  
* Coronavirus_Statistics_DataUpdate_v003 explores county name mismatches for deaths in USA Facts  
* Coronavirus_Statistics_DataUpdate_v003 explores county name mismatches for cases in USA Facts  
* Coronavirus_Statistics_DataUpdate_v003 updates readUSAFacts() to address county name mismatches  
* Coronavirus_Statistics_DataUpdate_v003 updates COVID Tracking Project data and uses existing state-level segments  
* Coronavirus_Statistics_DataUpdate_v003 updates USA Facts data and uses existing county-level segments  
* Coronavirus_Statistics_DataUpdate_v003 updates CDC all-cause deaths summary  
  
### Expansions week of 28-DEC-2020  
* Coronavirus_Statistics_DataUpdate_v003 explores alignment of raw and processed USA Facts metrics by state  
* Coronavirus_Statistics_DataUpdate_v003 explores alignment of metrics by state in USA Facts and COVID Tracking Project  
* Coronavirus_Statistics_DataUpdate_v003 continues exploring alignment of metrics by state in USA Facts and COVID Tracking Project  
* Coronavirus_Statistics_DataUpdate_v003 explores subset of states in USA Facts and COVID Tracking Project  
* Coronavirus_Statistics_DataUpdate_v003 explores census divisions as segments with COVID Tracking Project  
* Coronavirus_Statistics_DataUpdate_v003 updates COVID Tracking Project data and uses existing state-level segments  
* Coronavirus_Statistics_DataUpdate_v003 updates USA Facts data and uses existing county-level segments  
  
### Expansions week of 04-JAN-2021  
* Coronavirus_Statistics_DataUpdate_v003 explores anomalous declines in USA Facts metrics  
* Coronavirus_Statistics_DataUpdate_v003 updates USA Facts data and uses existing county-level segments  
* Coronavirus_Statistics_DataUpdate_v003 updates CDC all-cause deaths summary  
* Coronavirus_Statistics_State_v003 integrates latest data  
* Coronavirus_Statistics_State_v003 runs latest data for Northeast  
* Coronavirus_Statistics_State_v003 runs createAndAlignCurves() for Northeast  
* Coronavirus_Statistics_State_v003 runs createAndAlignCurves() for select Great Lakes states  
  
### Expansions week of 11-JAN-2021  
* Coronavirus_Statistics_State_v003 runs createAndAlignCurves() for select Upper Midwest states  
* Coronavirus_Statistics_State_v003 runs createAndAlignCurves() for select southern states  
* Coronavirus_Statistics_State_v003 estimates CFR for select northeast states  
* Coronavirus_Statistics_State_v003 estimates CFR for select midwest states  
* Coronavirus_Statistics_State_v003 estimates CFR for select north central states  
* Coronavirus_Statistics_State_v003 estimates CFR for select southern states  
* Coronavirus_Statistics_State_v003 estimates hospital-death lag for select northeast states  
  
### Expansions week of 18-JAN-2021  
* Coronavirus_Statistics_State_v003 estimates hospital-death lag for select midwest states  
* Coronavirus_Statistics_State_v003 estimates hospital-death lag for select north central states  
* Coronavirus_Statistics_State_v003 estimates hospital-death lag for select southern states  
* Coronavirus_Statistics_CTP_v004 created to begin updating code for 2021 data  
* Coronavirus_Statistics_CTP_v004 updates downloading and initial reading code  
* Coronavirus_Statistics_CTP_v004 updates readCOVIDbyState() for cleaner log output  
* Coronavirus_Statistics_CTP_v004 updates clusterStates() for yyyy-mm  
  
### Expansions week of 25-JAN-2021  
* Coronavirus_Statistics_CTP_v004 updates assessClusters()  
* Coronavirus_Statistics_CTP_v004 updates remaining functions  
* Coronavirus_Statistics_CTP_v004 run with latest data  
* Coronavirus_Statistics_CTP_v004 updates readRunCOVIDTrackingProject() to integrate all updated functions  
* Coronavirus_Statistics_CTP_v004 diagnostics run by census region  
* Coronavirus_Statistics_CTP_v004 includes consolidated metrics by census region  
* Coronavirus_Statistics_CTP_v004 includes cumulative metrics by census region  
  
### Expansions week of 01-FEB-2021  
* Coronavirus_Statistics_CTP_v004 integrates diagnostics and metrics as a stand-alone  
* Coronavirus_Statistics_CTP_v004 run with data through January 2021  
* Coronavirus_Statistics_CTP_v004 run by census region through January 2021  
* Coronavirus_Statistics_CTP_v004 run by census division through January 2021  
* Coronavirus_Statistics_CTP_v004 run by population density through January 2021  
* Coronavirus_Statistics_CTP_v004 highlights specific states in daily and cumulative plots  
* Coronavirus_Statistics_CTP_v004 run for large states and states with high disease burden  
  
### Expansions week of 08-FEB-2021  
* Coronavirus_Statistics_CTP_v004 begins curve alignment function  
* Coronavirus_Statistics_CTP_v004 adds best scalar and RMSE to curve alignment function  
* Coronavirus_Statistics_CTP_v004 updates plot labels  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for FL  
* Coronavirus_Statistics_CTP_v004 runs deaths for ND vs MI  
* Coronavirus_Statistics_CTP_v004 runs deaths for WI vs MI  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for WI  
  
### Expansions week of 15-FEB-2021  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for FL  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for OH  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for IA  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for IL  
* Coronavirus_Statistics_CTP_v004 runs total hospitalizations vs deaths for IL  
* Coronavirus_Statistics_CTP_v004 adds intercept yes/no option to curve-aligning process  
* Coronavirus_Statistics_CTP_v004 adds state aggregation capability for curve-aligning process  
  
### Expansions week of 22-FEB-2021  
* Coronavirus_Statistics_CTP_v004 runs total hospitalizations vs deaths for US  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for primary midwest cluster  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for primary southern cluster  
* Coronavirus_Statistics_CTP_v004 runs deaths for midwest vs southern clusters  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for second wave of primary northeastern cluster  
* Coronavirus_Statistics_CTP_v004 runs cases vs deaths for first wave of primary northeastern cluster  
* Coronavirus_Statistics_CTP_v004 runs hospitalization vs deaths for first wave of primary northeastern cluster  
  
### Expansions week of 01-MAR-2021  
* Coronavirus_Statistics_CTP_v004 runs hospitalization vs deaths for second wave of primary northeastern cluster  
* Coronavirus_Statistics_CTP_v004 data updated through 28-FEB-2021  
* Coronavirus_Statistics_CTP_v004 updates cases vs deaths for second wave of primary northeastern cluster  
* Coronavirus_Statistics_CTP_v004 updates cases vs deaths for current wave of US  
* Coronavirus_Statistics_CTP_v004 updates hospitalizations vs deaths for current wave of US  
* Coronavirus_Statistics_CTP_v004 updates cases vs hospitalizations for current wave of US  
* Coronavirus_Statistics_CTP_v004 updates cases vs deaths for primary midwestern cluster  
  
### Expansions week of 08-MAR-2021  
* Coronavirus_Statistics_CTP_v004 investigates impact of OH/IN death spikes in primary midwestern cluster  
* Coronavirus_Statistics_CTP_v004 explores states with spikes in deaths data  
* Coronavirus_Statistics_CTP_v004 updates total hospitalization vs deaths for primary midwestern cluster excluding OH/IN  
* Coronavirus_Statistics_CTP_v004 updates cases vs total hospitalized for primary midwestern cluster excluding OH/IN  
* Coronavirus_Statistics_CTP_v004 investigates alignment of estimated and actual deaths  
* Coronavirus_Statistics_CTP_v004 plots consolidated cluster-level metrics  
* Coronavirus_Statistics_CTP_v004 plots RMSE and R-squared by lag/lead and cluster  
  
### Expansions week of 15-MAR-2021  
* Coronavirus_Statistics_CTP_v004 plots best fit deaths~cases parameters by cluster  
* Coronavirus_Statistics_CTP_v004 plots best fit deaths~cases alignment by cluster  
* Coronavirus_Statistics_CTP_v004 uses more efficient call for plotting list of ggplot objects  
* Coronavirus_Statistics_CTP_v004 adjusts legend locations prior to grid.arrange  
* Coronavirus_Statistics_CTP_v004 controls figure size with fig.height and fig.width  
* Coronavirus_Statistics_CTP_v004 adjusts plot titles with lapply  
* Coronavirus_Statistics_CTP_v004 uses facet_wrap with ggplot data from inside the lists  
  
### Expansions week of 22-MAR-2021  
* Coronavirus_Statistics_CTP_v004 adds key statistics to the facetted plots  
* Coronavirus_Statistics_CTP_v004 plots consolidated cluster-level metrics for lagged total hospitalizations ~ cases  
* Coronavirus_Statistics_CTP_v004 plots best fit parameters for lagged total hospitalizations ~ cases  
* Coronavirus_Statistics_CTP_v004 plots best fit hospitalized~cases alignment by cluster  
* Coronavirus_Statistics_CTP_v004 uses facet_wrap with ggplot data from inside the lists  
* Coronavirus_Statistics_CTP_v004 plots consolidated cluster-level metrics for lagged deaths ~ total hospitalizations  
* Coronavirus_Statistics_CTP_v004 plots best fit parameters for lagged deaths ~ total hospitalizations  
  
### Expansions week of 29-MAR-2021  
* Coronavirus_Statistics_CTP_v004 plots best fit deaths-hospitalized alignment by cluster  
* Coronavirus_Statistics_CTP_v004 uses facet_wrap with ggplot data from inside the lists  
* Coronavirus_Statistics_CTP_v004 converts first component of curve alignment to functional form  
* Coronavirus_Statistics_CTP_v004 converts remaining components of curve alignment to functional form  
* Coronavirus_Statistics_CTP_v004 downloads final CTP data (through 07-MAR-21)  
* Coronavirus_Statistics_CTP_v004 chains curve alignment functions for one-step call  
* Coronavirus_Statistics_CTP_v004 runs chained function in reverse direction (cases ~ hosp)  
  
### Expansions week of 05-APR-2021  
* Coronavirus_Statistics_CTP_v004 compares forward and reverse curve alignments  
* Coronavirus_Statistics_CTP_v004 runs chained alignment process for deaths ~ hosp  
* Coronavirus_Statistics_CTP_v004 runs reverse chained alignment process for hosp ~ deaths  
* Coronavirus_Statistics_CTP_v004 runs reverse chained alignment process for cases ~ deaths  
* Coronavirus_Statistics_CTP_v004 uses best-fit models to project hospitalizations and deaths  
* Coronavirus_Statistics_CTP_v004 compares projected vs. actual  
* Coronavirus_Statistics_CTP_v004 downloads CDC case and death by state data  
  
### Expansions week of 12-APR-2021  
* Coronavirus_Statistics_CTP_v004 compares 'total' and 'new' fields for cases and deaths  
* Coronavirus_Statistics_CTP_v004 cleans and formats CDC case and death by state data  
* Coronavirus_Statistics_CTP_v004 compares CDC and CTP  
* Coronavirus_CDC_Daily_v001 created to use CDC daily data in place of CTP  
* Coronavirus_CDC_Daily_v001 cleans and pivots CDC daily data  
* Coronavirus_CDC_Daily_v001 combines NYC and NYS as NY  
* Coronavirus_CDC_Daily_v001 calculates cumulative totals as of 2021-03-07 (end date of CTP)  
  
### Expansions week of 19-APR-2021  
* Coronavirus_CDC_Daily_v001 compares CTP and CDC cumulative totals as of 2021-03-07 (end date of CTP)  
* Coronavirus_CDC_Daily_v001 compares shape of the curve in CDC and CTP  
* Coronavirus_CDC_Daily_v001 plots states with largest differences in shape of the curve  
* Coronavirus_CDC_Daily_v001 creates per capita CDC daily data  
* Coronavirus_CDC_Daily_v001 adapts clustering code for CDC daily data  
* Coronavirus_CDC_Daily_v001 adapts assessClusters() for CDC daily data  
* Coronavirus_CDC_Daily_v001 adapts plotConsolidatedMetrics() for CDC daily data  
  
### Expansions week of 26-APR-2021  
* Coronavirus_CDC_Daily_v001 plots cumulative and consolidated data  
* Coronavirus_CDC_Daily_v001 updates steps 1-4 of readRunCOVIDTrackingProject  
* Coronavirus_CDC_Daily_v001 updates readRunCOVIDTrackingProject to run on CDC daily data  
* Coronavirus_CDC_Daily_v001 ensures readRunCOVIDTrackingProject back-compatibility with CTP  
* Coronavirus_CDC_Daily_v001 downloads and reads CDC daily hospital data  
* Coronavirus_CDC_Daily_v001 integrates and reports using CDC daily hospital data  
* Coronavirus_CDC_Daily_v001 includes small function for data download, read, and initial QC  
  
### Expansions week of 03-MAY-2021  
* Coronavirus_CDC_Daily_v001 downloads latest deaths and cases data  
* Coronavirus_CDC_Daily_v001 adds function for plotting differences in raw and reference file  
* Coronavirus_CDC_Daily_v001 adds function for text summary of differences in raw and reference file  
* Coronavirus_CDC_Daily_v001 runs readQCRawFile() on previously downloaded cases and death data  
* Coronavirus_CDC_Daily_v001 runs readQCRawFile() on previously downloaded hospital data  
* Coronavirus_CDC_Daily_v001 runs readQCRawFile() to downloaded and read latest hospital data  
* Coronavirus_CDC_Daily_v001 updates keyAggMapper to be more modular  
  
### Expansions week of 10-MAY-2021  
* Coronavirus_CDC_Daily_v001 includes print to log capability  
* Coronavirus_CDC_Daily_v001 includes processRawFile()  
* Coronavirus_CDC_Daily_v001 merges data and creates per capita metrics  
* Coronavirus_CDC_Daily_v001 increases argument flexibility for createPerCapita()  
* Coronavirus_CDC_Daily_v001 updates steps 2-4 of readRunCDCDaily()  
* Coronavirus_CDC_Daily_v001 creates clusters using integrated data  
* Coronavirus_CDC_Daily_v001 includes functions to create unweighted aggregates  
  
### Expansions week of 17-MAY-2021  
* Coronavirus_CDC_Daily_v001 includes functions to create weighted means  
* Coronavirus_CDC_Daily_v001 includes additional aggregation capabilities  
* Coronavirus_CDC_Daily_v001 creates aggregation plots  
* Coronavirus_CDC_Daily_v001 creates state cluster dashboard page  
* Coronavirus_CDC_Daily_v001 parameters drive dashboard page  
* Coronavirus_CDC_Daily_v001 includes function clustersToFrame()  
* Coronavirus_CDC_Daily_v001 includes detailed summary plots  
  
### Expansions week of 24-MAY-2021  
* Coronavirus_CDC_Daily_v001 includes growth plots  
* Coronavirus_CDC_Daily_v001 further parameterizes plot creation  
* Coronavirus_CDC_Daily_v001 integrates all functions  
* Coronavirus_CDC_Daily_v001 concluded, preparation to split off functions and move to _v002  
* Generic_Added_Utility_Functions_202105_v001.R contains functions for data wrangling and analysis  
* Coronavirus_CDC_Daily_Functions_v001.R contains functions for working with CDC daily data  
* Coronavirus_CDC_Daily_Default_Mappings_v002.R contains mapping parameters for Coronavirus_CDC_Daily_v002  
* Coronavirus_CDC_Daily_v002 created to use functions and parameters in .R files  
* Coronavirus_Statistics_USAF_v004 created for updated processing of USA Facts data  
  
### Expansions week of 31-MAY-2021  
* Coronavirus_Statistics_USAF_v004 includes function for county-level clustering  
* Coronavirus_Statistics_USAF_v004 converts county-level clustering output to vector  
* Coronavirus_Statistics_USAF_v004 includes functions to read and QC raw USAF files  
* Coronavirus_Statistics_USAF_v004 function readRunUSAFacts() includes all elements except assessment  
* Coronavirus_Statistics_USAF_v004 includes functions to assess clusters  
* Coronavirus_Statistics_USAF_v004 function diagnoseClusters() is updated  
* Coronavirus_Statistics_USAF_v004 function readRunUSAFacts() is updated  
  
### Expansions week of 07-JUN-2021  
* Coronavirus_USAF_Functions_v001 contains functions for working with USAF data  
* Coronavirus_USAF_Default_Mappings_v001.R contains mapping parameters for working with USAF data  
* Coronavirus_Statistics_USAF_v005 created to use functions and parameters in .R files  
* Coronavirus_Statistics_USAF_v005 downloads and processes new data  
* Coronavirus_Statistics_USAF_v005 creates segments using new data  
* Coronavirus_Statistics_USAF_v005 updates plots to manage small counties  
* Coronavirus_Statistics_USAF_v005 explores hierarchical clustering for shape of curve  
* Coronavirus_Statistics_USAF_v005 plots differences in hierarchical clusters  
  
### Expansions week of 14-JUN-2021  
* Coronavirus_Statistics_USAF_v005 creates maps by shape of the curve  
* Coronavirus_Statistics_USAF_v005 creates maps by county for time to burden thresholds  
* Coronavirus_Statistics_USAF_v005 maps smaller counties to 'most similar' cluster  
* Coronavirus_Statistics_USAF_v005 plots total deaths per million vs shape clusters  
* Coronavirus_Statistics_USAF_v005 creates county clusters based on dpm7 by date  
* Coronavirus_Statistics_USAF_v005 assigns each smaller county to closest cluster  
* Coronavirus_Statistics_USAF_v005 further splits subset of county clusters  
  
### Expansions week of 21-JUN-2021  
* Coronavirus_Statistics_USAF_v005 integrates county clusters for populations of 100k+  
* Coronavirus_Statistics_USAF_v005 assigns clusters to all counties  
* Coronavirus_Statistics_USAF_v005 downloads latest data and assesses new clusters  
* Coronavirus_Statistics_CDC_v004 created for CDC all-cause excess death analysis  
* Coronavirus_Statistics_CDC_v004 downloads new data  
* Coronavirus_Statistics_CDC_v004 updates function readProcessCDC()  
* Coronavirus_Statistics_CDC_v004 updates function helperKeyStateClusterMetrics()  
  
### Expansions week of 28-JUN-2021  
* Coronavirus_Statistics_CDC_v004 updates function cdcBasicPlots()  
* Coronavirus_Statistics_CDC_v004 updates function cdcCohortAnalysis()  
* Coronavirus_Statistics_CDC_v004 updates function cdcAggregateSummary()  
* Coronavirus_Statistics_CDC_v004 updates function helperKeyStateExcessPlots()  
* Coronavirus_Statistics_CDC_v004 updates function helperKeyAgeExcessPlots()  
* Coronavirus_Statistics_CDC_v004 includes PDF option for cdcAggregateSummary()  
* Coronavirus_Statistics_CDC_v004 integrates functions in readRunCDCAllCause()  
  
### Expansions week of 05-JUL-2021  
* Coronavirus_Statistics_CDC_v004 updates default arguments in readRunCDCAllCause()  
* Coronavirus_Statistics_CDC_v004 includes PDF options for readRunCDCAllCause()  
* Coronavirus_CDC_Excess_Functions_v001.R contains functions and parameters for working with CDC all-cause deaths data   
* Coronavirus_Statistics_CDC_v005 created to use functions and parameters in .R files  
* Coronavirus_CDC_Daily_v002 downloads and processes latest data  
* Coronavirus_Statistics_CDC_v005 downloads and processes latest data  
* Coronavirus_Statistics_CDC_v005 updates readProcessCDC() to better handle supressed data  
  
### Expansions week of 12-JUL-2021  
* Coronavirus_Statistics_CDC_v005 includes plotQCReadProcessCDC() for control total checks  
* Coronavirus_CDC_Daily_v002 downloads vaccine data  
* Coronavirus_CDC_Daily_v002 reads and explores vaccine data  
* Coronavirus_CDC_Daily_v002 explores availability of vaccine by age data  
* Coronavirus_CDC_Daily_v002 adjusts for varying vaccine data availability by age cohort  
* Coronavirus_CDC_Daily_v002 uses readQCRawCDCDaily() to read a vaccine data file  
* Coronavirus_CDC_Daily_v002 uses full functionality of readQCRawCDCDaily() for vaccine data files  
  
### Expansions week of 19-JUL-2021  
* Coronavirus_CDC_Daily_v002 uses processRawFile() for vaccine data files  
* Coronavirus_CDC_Daily_v002 uses createPerCapita() for vaccine data files  
* Coronavirus_CDC_Daily_v002 updates createPerCapita() to keep variables without rolling 7 per-million calculations  
* Coronavirus_CDC_Daily_v002 creates daily metrics and age-group metrics  
* Coronavirus_CDC_Daily_v002 estimates total population and per-capita by state  
* Coronavirus_CDC_Daily_v002 estimates population by age bucket by state  
* Coronavirus_CDC_Daily_v002 estimates evolution of completely vaccinated proportion by age bucket by state  
  
### Expansions week of 26-JUL-2021  
* Coronavirus_CDC_Daily_v002 investigates implied negative spikes in vaccination  
* Coronavirus_CDC_Daily_v002 includes updateByDay() for negative spikes in vaccination  
* Coronavirus_CDC_Daily_v002 continues exploring age-bucket vaccination splits  
* Coronavirus_CDC_Daily_v002 adapts readRunCDCDaily() to read and process vaccines data  
* Coronavirus_CDC_Daily_v002 plots hospitalization and vaccination by state  
* Coronavirus_CDC_Daily_v002 adapts diagnoseClusters() for vaccination by state  
* Coronavirus_CDC_Daily_v002 adapts diagnoseClusters() for additional weighted metrics  
  
### Expansions week of 02-AUG-2021  
* Coronavirus_CDC_Daily_v002 adapts readRunCDCDaily() to integrate cases, deaths, hospitalizations, and vaccines data  
* Coronavirus_CDC_Daily_Functions_v001.R contains updated functions for working with CDC daily data, including vaccines  
* Generic_Added_Utility_Functions_202105_v001.R contains updated functions for data wrangling and analysis  
* Coronavirus_CDC_Daily_Default_Mappings_v002.R contains updated mapping parameters for Coronavirus_CDC_Daily_v003  
* Coronavirus_CDC_Daily_v003 created to run updated functions and mappings that include vaccines data  
* Coronavirus_CDC_Daily_v003 run with latest data  
* Coronavirus_CDC_Daily_v003 explores latest raw hospitalization data  
* Coronavirus_CDC_Daily_v003 explores hospitalization by age  
* Coronavirus_CDC_Daily_v003 explores alignment of cases, hospitalizations, and deaths for a single state  
  
### Expansions week of 09-AUG-2021  
* Coronavirus_CDC_Daily_v003 includes functional form for curve alignment  
* Coronavirus_CDC_Daily_v003 explores correlations by lag for cases and deaths  
* Coronavirus_CDC_Daily_v003 includes functional form for correlations by lag  
* Coronavirus_CDC_Daily_v003 includes functional form for CFR estimates  
* Coronavirus_CDC_Daily_v003 includes single page summary for CFR estimates  
* Coronavirus_Statistics_USAF_v005 downloads latest data and assesses existing clusters  
* Coronavirus_Statistics_USAF_v005 explores records with negative cases  
  
### Expansions week of 16-AUG-2021  
* Coronavirus_Statistics_USAF_v005 explores smoothing for nearby negative and positive spikes  
* Coronavirus_Statistics_USAF_v005 runs smoothing algorithm by county for cases  
* Coronavirus_Statistics_USAF_v005 explores reporting rate by epiweek and state  
* Coronavirus_Statistics_USAF_v005 explores drops and spikes in reporting rate by epiweek and state  
* Coronavirus_Statistics_USAF_v005 compares CDC and USAF state-level totas  
* Coronavirus_CDC_Daily_v003 explores vaccination by age cohort and state  
* Coronavirus_CDC_Daily_v003 explores evolution of vaccination by age cohort and state  
  
### Expansions week of 23-AUG-2021  
* Coronavirus_CDC_Daily_v003 explores evolution of cumulative cases and deaths and state  
* Coronavirus_Statistics_CDC_v005 updated with latest data  
* Coronavirus_Statistics_CDC_v005 downloads deaths by age and location data  
* Coronavirus_Statistics_CDC_v005 starts building checkSubTotals()  
* Coronavirus_Statistics_CDC_v005 explores alignment of subtotals and totals  
* Coronavirus_Statistics_CDC_v005 compares deaths by state to summed CDC daily data  
* Coronavirus_Statistics_CDC_v005 further explores deaths by state vs. CDC daily data  
  
### Expansions week of 30-AUG-2021  
* Coronavirus_Statistics_CDC_v005 explores deaths by age and cause  
* Coronavirus_Statistics_CDC_v005 explores deaths by place and cause  
* Coronavirus_Statistics_CDC_v005 explores age and place of death by cause  
* Coronavirus_Statistics_CDC_v005 explores evolution by month of age and place of death by cause  
* Coronavirus_CDC_Daily_v003 updated with most recent data  
* Coronavirus_CDC_Daily_v003 updates hospitalization and curve alignment analyses  
* Coronavirus_Statistics_USAF_v005 downloads latest data and assesses existing clusters  
  
### Expansions week of 06-SEP-2021  
* Coronavirus_Statistics_USAF_v005 updates source for latest data downloads  
* Coronavirus_Statistics_USAF_v005 adds burden by county plots  
* Coronavirus_Statistics_USAF_v005 maps counties with 0 reported burden in past 28 days  
* Coronavirus_Statistics_USAF_v005 explores select counties in FL, NE, NJ  
* Coronavirus_Statistics_USAF_v005 explores unallocated vs total in FL, NE, NJ  
* Coronavirus_Statistics_USAF_v005 explores proportion unallocated by state, metric, and month  
* Coronavirus_Statistics_CDC_v005 updated with latest data  
  
### Expansions week of 13-SEP-2021  
* Coronavirus_Statistics_CDC_v005 creates plots of deaths by week and age group  
* Coronavirus_Statistics_CDC_v005 includes excess death calculation  
* Coronavirus_Statistics_CDC_v005 plotAgeWeekDeath() updated to return data  
* Coronavirus_Statistics_CDC_v005 calculates excess all-cause death by state, age, and week  
* Coronavirus_Statistics_CDC_v005 adds zeroes for missing state-week-age combinations  
* Coronavirus_Statistics_CDC_v005 plots coronavirus and all-cause deaths by age group and state  
* Coronavirus_Statistics_CDC_v005 plots proportion of coronavirus and all-cause deaths by age group and state  
  
### Expansions week of 20-SEP-2021  
* Coronavirus_Statistics_CDC_v005 plots coronavirus and all-cause deaths by age group and state for 2020 and YTD 2021  
* DataCamp_Insights_v003_d captures Introduction to Statistics in R - Chapter 1 (Summary Statistics)  
* DataCamp_Insights_v003_d captures Introduction to Statistics in R - Chapter 2 (Random Numbers and Probability)  
* DataCamp_Insights_v003_d captures Introduction to Statistics in R - Chapter 3 (More Distributions and Central Limit Theorem)  
* DataCamp_Insights_v003_d captures Introduction to Statistics in R - Chapter 4 (Correlation and Experimental Design)  
* DataCamp_Insights_v003_d captures Introduction to Regression in R - Chapter 1 (Simple Linear Regression) - code  
* DataCamp_Insights_v003_d captures Introduction to Regression in R - Chapter 2 (Predictions and Model Objects) - code  
  
### Expansions week of 27-SEP-2021  
* DataCamp_Insights_v003_d captures Introduction to Regression in R - Chapter 3 (Assessing Model Fit) - code  
* DataCamp_Insights_v003_d captures Introduction to Regression in R - Chapter 4 (Simple Logistic Regression) - code  
* DataCamp_Insights_v003_d captures Handling Missing Data with Imputations in R - Chapter 1 (Problem of Missing Data) - code  
* DataCamp_Insights_v003_d captures Handling Missing Data with Imputations in R - Chapter 2 (Donor Based Imputation) - code  
* DataCamp_Insights_v003_d captures Handling Missing Data with Imputations in R - Chapter 3 (Model Based Imputation) - code  
* DataCamp_Insights_v003_d captures Handling Missing Data with Imputations in R - Chapter 4 (Uncertainty from Imputation) - code  
* DataCamp_Insights_v003_d captures Data Cleaning in R - Chapter 1 (Common Data Problems) - code  
  
### Expansions week of 04-OCT-2021  
* DataCamp_Insights_v003_d captures Data Cleaning in R - Chapter 2 (Categorical and Text Data) - code  
* DataCamp_Insights_v003_d captures Data Cleaning in R - Chapter 3 (Advanced Data Problems) - code  
* DataCamp_Insights_v003_d captures Data Cleaning in R - Chapter 4 (Record Linkage) - code  
* Coronavirus_CDC_Daily_v003 updated with latest data  
* Coronavirus_CDC_Daily_v003 updates state CFR summaries  
* Coronavirus_CDC_Daily_v003 includes functional form for creating hospitalized and burden datasets  
* Coronavirus_CDC_Daily_v003 includes cumulative burden plots  
  
### Expansions week of 11-OCT-2021  
* Coronavirus_CDC_Daily_v003 includes functional form for cumulative burden plots  
* Coronavirus_CDC_Daily_v003 includes cumulative vaccination plots  
* Coronavirus_CDC_Daily_v003 includes functional form for cumulative vaccination plots  
* Coronavirus_CDC_Daily_v003 adds age cohort for cumulative vaccination plots  
* Coronavirus_CDC_Daily_v003 adds age-state-vaccination plots  
* Coronavirus_CDC_Daily_v003 loads population by state-age estimates  
  

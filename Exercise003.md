## Overall objective:  
Extend Exercise001 and Exercise002 by adding core functionality, particularly the inclusion of covariance.  
  
#### *Covariance objectives*  
Suppose we have N trials each of M outcomes that are non-indepdendent (in other words, the N trials are independent, but the M outcomes inside a given trial are not) and want to understand the resulting impact on average and variance due to induced covariance.  
  
More or less, my goal is to replicate something like http://wizardofodds.com/games/video-poker/appendix/2/ using the algorithm shown at http://wizardofodds.com/games/video-poker/methodology/ as a starting point.  This will require learning more about matrices (and probably arrays) in R.  

##### **_25-FEB-2016_**  
Create a routine to assess the value of a 13x4 matrix representing 5-card hands.  The plan is to extend the matrix as follows:  
  
* Compress it since suits do not matter, so keep the "heaviest" suit in the first column  
* Suppose you hold N cards of M originally dealt - assess the relative likelihoods of each final outcome based on that specific hold  
* Calculate the expected value and probability vector for holding a specific N cards of M originally dealt  

##### **_26-FEB-2016_**  
Simple update to calculate the compression possible when suits do not matter (brings down to 13% of original volume).  Next step will be to create the matrices for 5-0-0-0 and 4-1-0-0 and . . . and 2-1-1-1  

##### **_27-FEB-2016_**  
Cut processing time by about 50%, updated to Exercise003_v002.R.  Also put in some initial coding for creating the hand matrices, but do not yet have the full combinatorics working for combining the suits.  Save as Exercise003_v003.R.  

##### **_29-FEB-2016_**  
Fixed the combinatorics issues and validated that hand types for each sub-group match expectations.  Next step will be to save the matrices as a big lookup file and calculate against them.  Save latest as Exercise003_v003.R (overwrote non-working version).  
  
##### **_01-MAR-2016_**  
Updated the code so the matrices all have an EV in column 53.  Optimized code so that even with this step it runs 5% faster than before.  Code is much less of a memory hog, with the largest output matrix (which should cover ~50% of final rows) and related files needing ~75 MB.  Next step will be to aggregate the individual matrices and then learn how to lookup against them.  Saved latest version as Exercise003_v004.R (new working version).  
  
##### **_02-MAR-2016_**  
Updated the code so there is a final combined matrix where each row includes EV (column 53) and hand type (column 54).  Optimized code to be further function-driven for a cleaner work space.  Next step will be to learn how to lookup against an aggregated matrix.  Saved latest version as Exercise003_v004.R (overwrote old version).  
  
##### **_03-MAR-2016_**  
Cleaned the code and identified rough magnitude of time by component.  Significantly reduced this in one area by replacing a for-loop with match(), cutting processing time in this step by >95%.  There are two other locations to investigate for speed improvement.  Next step after that will be to learn how to lookup against an aggregated matrix.  Saved as Exercise003_v005.R (new working version).  
  
##### **_07-MAR-2016_**  
First attempt at permuting the data to a full 52c5 from the truncated dataset.  Permutation code runs very fast but is extremely inefficient to write.  Look in to a better algorithm for this next.  Should also start to save some files and have a few different scripts to optimize run time and memory usage.  Saved as Exercise003_v006.R (new working version).  

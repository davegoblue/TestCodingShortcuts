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

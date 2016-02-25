## Overall objective:  
Extend Exercise001 and Exercise002 by adding core functionality, particularly the inclusion of covariance.  
  
#### *Covariance objectives*  
Suppose a base-outcomes table exists for a single draw.  Suppose that instead of M x N individual draws, we instead have N trials each of M outcomes that are non-indepdendent (in other words, the N trials are independent, but the M outcomes inside a given trial are not).
  
More or less, my goal is to be able to replicate something like http://wizardofodds.com/games/video-poker/appendix/2/ using the algorithm shown at http://wizardofodds.com/games/video-poker/methodology/ as a starting point.  This will require learning more about matrices (and probably arrays) in R.  

##### **_Need #1_**  
Create a new base-modOutcomes that converts from base-outcomes to achieve a specified covariance tolerance while prserving the mean per unit outcome (in other words, p(outcome) x outcomes = p(modOutcomes) x modOutcomes / M)  
  
##### **_25-FEB-2016_**  
Create a routine to assess the value of a 13x4 matrix representing 5-card hands.  The plan is to extend the matrix as follows:  
  
* Compress it since suits do not matter, so keep the "heaviest" suit in the first column  
* Suppose you hold N cards of M originally dealt - assess the relative likelihoods of each final outcome based on that specific hold  
* Calculate the expected value and probability vector for holding a specific N cards of M originally dealt
* 

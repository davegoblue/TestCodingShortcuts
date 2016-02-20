## Overall objective:  
Extend Exercise001 in two ways:  
1.  Simplify / increase efficiency for processing  
2.  for additional functionality (especially covariance)  
  
#### *Simplification objectives*  
The Exercise001 code uses mostly matrices and base.  These are fine unless the data become very large, at which point the code hogs memory and takes a long time.  Come up with approaches to simplify the processing and make it more efficient (keep as few items as possible, leverage dplyr, etc.).  
  
#### *Covariance objectives*  
Suppose a base-outcomes table exists for a single draw.  Suppose that instead of M x N individual draws, we instead have N trials each of M outcomes that are non-indepdendent (in other words, the N trials are independent, but the M outcomes inside a given trial are not).
  
##### **_Need #1_**  
Create a new base-modOutcomes that converts from base-outcomes to achieve a specified covariance tolerance while prserving the mean per unit outcome (in other words, p(outcome) x outcomes = p(modOutcomes) x modOutcomes / M)

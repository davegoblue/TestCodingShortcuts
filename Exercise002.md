## Overall objective:  
Extend the exercise for additional functionality (especially covariance)  

#### *Covariance objectives*  
Suppose a base-outcomes table exists for a single draw.  Suppose that instead of M x N individual draws, we instead have N trials each of M outcomes that are non-indepdendent (in other words, the N trials are independent, but the M outcomes inside a given trial are not).
  
#### *Need #1*  
Create a new base-modOutcomes that converts from base-outcomes to achieve a specified covariance tolerance while prserving the mean per unit outcome (in other words, p(outcome) x outcomes = p(modOutcomes) x modOutcomes / M)

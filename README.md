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
  

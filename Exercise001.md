### Combination or sort, VLOOKUP (inexact), and MATCH


Suppose that I have a results table with probabilities:
* 0.01 Outcome is +10
* 0.02 Outcome is +5
* 0.05 Outcome is +2
* 0.18 Outcome is +1
* 0.24 Outcome is 0
* 0.50 Outcome is -1


Suppose also that I run A trials with B random variables, each continuous between 0 and 1.  I want to map every random number to a specific outcome in the table above, and keep a cumulative outcome assuming the starting point was N0 (so N1 = N0 + Outcome 1). For this given distribution, mu is -0.02 and sigma is 1.54 


I am then interested in the first time that cumulative outcome "does something" (becomes zero/negative, doubles, whatever).  I also want to sort the resulting A trials based on a combination of final cumulative outcome and time it took to "does something".


For simplicity, I will assume the random numbers mapo to the distribution above as follows:

* 0.00 - 0.01 Outcome is +10
* 0.01 - 0.03 Outcome is +5
* 0.03 - 0.08 Outcome is +2
* 0.08 - 0.26 Outcome is +1
* 0.26 - 0.50 Outcome is 0
* 0.50 - 1.00 Outcome is -1


While I will not need to worry about it in the instant case (the random variables will be to many decimal points and never precisely equal to a cut point), it will be good to understand how to treat edge cases (e.g., if I wanted precisely 0.01 to mean +10 rather than +5 and whatnot).

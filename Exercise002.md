## Overall objective:  
Extend Exercise001 by making the code simpler and more efficient where possible.  
  
#### *Simplification objectives*  
The Exercise001 code uses mostly matrices and base.  These are fine unless the data become very large, at which point the code hogs memory and takes a long time.  Come up with approaches to simplify the processing and make it more efficient (keep as few items as possible, leverage dplyr, etc.).  
  
#### *Updated 22-FEB-2016*  
Learned that matrices actually process faster than data frames.  Added some code to keep fewer copies of essentially the same data in multiple matrices, and increased the amount of matrix processing.  Code is 50% faster and uses 30% less space with these edits.  Still need to improve Part D in particular.  
  
#### *Updated 23-FEB-2016*  
Finished the recoding to use primarily matrices.  For 12,000 trials of 5,000 random draws each, processing time and memory needs have changed as follows (RStudio with rm(list=ls()) and CTRL-SHIFT-F10 prior to each run):  
* Exercise001_v007.R: 2.8 GB with 18.7 user and 20.1 elapsed  
* Exercise002_v001.R: 1.4 GB with 12.1 user and 13.6 elapsed  
* Results (mean/median by metric) substantially the same  
  
#### *Updated 24-FEB-2016*  
Reduced storing a database of randoms and a database of converted randoms, and converted the graphing to a function to reduce the number of variables kept in the environment:  
* Exercise002_v002.R: 0.9 GB with 13.5 user and 15.2 elapsed  
* Appears that using the same DB as source/output cuts total size by ~35% but takes ~10% longer to run  
* Results (mean/median by metric) substantially the same  
  
  

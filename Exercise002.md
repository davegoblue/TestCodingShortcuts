## Overall objective:  
Extend Exercise001 by making the code simpler and more efficient where possible.  
  
#### *Simplification objectives*  
The Exercise001 code uses mostly matrices and base.  These are fine unless the data become very large, at which point the code hogs memory and takes a long time.  Come up with approaches to simplify the processing and make it more efficient (keep as few items as possible, leverage dplyr, etc.).  
  
#### *Updated 22-FEB-2016*  
Learned that matrices actually process faster than data frames.  Added some code to keep fewer copies of essentially the same data in multiple matrices, and increased the amount of matrix processing.  Code is 50% faster and uses 30% less space with these edits.  Still need to improve Part D in particular.  


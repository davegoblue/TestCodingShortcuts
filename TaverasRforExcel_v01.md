---
title: "Taveras Examples - Excel for R"
author: "davegoblue"
date: "February 17, 2016"
output: html_document
---

This document captures some useful tips and tricks from John Taveras' book "R for Excel Users".  John has a nice style for comparing Excel with R.  One of my objectives is to get R to "behave more like Excel" in some instances.


### Chapter 4 Cells vs. Vectors  

This chapter contains some useful string functions.  I expand on them a bit with some other searching.
  
####_Standard Excel text functions_  
Standard Excel functions like LEN, SUBSTR, UPPER, and LOWER can be replicated:  

```r
fakeName <- "AbcDe FFGhiJL mfoP"

nchar(fakeName)  ## Number of characters including spaces - 18
```

```
## [1] 18
```

```r
length(fakeName)  ## Not what you want!  It is of length 1 (1 item)
```

```
## [1] 1
```

```r
substr(fakeName,1,3)  ## Start at position 1 and stop at position 3 (left(fakeName,3))
```

```
## [1] "Abc"
```

```r
substr(fakeName,nchar(fakeName)-2,nchar(fakeName))  ## Start at len-2 and stop at len (right(fakeName,3))
```

```
## [1] "foP"
```

```r
toupper(fakeName)
```

```
## [1] "ABCDE FFGHIJL MFOP"
```

```r
tolower(fakeName)
```

```
## [1] "abcde ffghijl mfop"
```
  
  
####_String splitting is available_  
Additionally, a string can be split on a key character, with the results then manipulated as needed in one of many different ways:  

```r
strsplit(fakeName," ")  ## Split by " "
```

```
## [[1]]
## [1] "AbcDe"   "FFGhiJL" "mfoP"
```

```r
length(strsplit(fakeName," "))  ## It is still 1 because it is a list
```

```
## [1] 1
```

```r
nchar(strsplit(fakeName," "))  ## It is now 29!  Not totally sure why though . . . 
```

```
## [1] 29
```

```r
strsplit(fakeName," ")[c(1,3)]  ## Prints the full list, with 3 being NULL (list has only 1 item)
```

```
## [[1]]
## [1] "AbcDe"   "FFGhiJL" "mfoP"   
## 
## [[2]]
## NULL
```

```r
strsplit(fakeName," ")[[1]][1:3]  ## Prints each item separately (list of length 3)
```

```
## [1] "AbcDe"   "FFGhiJL" "mfoP"
```

```r
length(strsplit(fakeName," ")[[1]])  ## 3
```

```
## [1] 3
```

```r
length(strsplit(fakeName," ")[[1]][1:3])  ## 3
```

```
## [1] 3
```

```r
nchar(strsplit(fakeName," ")[[1]][1:3])  ## 5 7 4
```

```
## [1] 5 7 4
```

```r
sum(nchar(strsplit(fakeName," ")[[1]][1:3]))  ## 16 (5+7+4)
```

```
## [1] 16
```

```r
strsplit(fakeName," ")[[1]][c(1,3)]  ## Grab the first and third item - "AbcDe" and "mfoP"
```

```
## [1] "AbcDe" "mfoP"
```
  
  
####_Pattern substitution is available:_  
Pattern substitution is available, with sub finding/replacing the first instance with gsub finding/replacing all instances:  

```r
sub(pattern=" ",replacement="_-_",fakeName)  ## Replace just the first space
```

```
## [1] "AbcDe_-_FFGhiJL mfoP"
```

```r
gsub(pattern=" ",replacement="_-_",fakeName)  ## Replace all spaces
```

```
## [1] "AbcDe_-_FFGhiJL_-_mfoP"
```

```r
nchar(gsub(pattern=" ",replacement="_-_",fakeName))  ## Grew from 18->22 due to each space getting 2 bigger
```

```
## [1] 22
```

```r
gsub(pattern=" ",replacement="",fakeName)  ## compressed vector
```

```
## [1] "AbcDeFFGhiJLmfoP"
```

```r
nchar(gsub(pattern=" ",replacement="",fakeName))  ## Shrank from 18->16 by compressing away the fakes
```

```
## [1] 16
```
  
  
####_Pattern identification and location ID is available_  
The regexpr function finds the first match and provides a list with attributes about it:  

```r
regexpr("f",fakeName)  ## Provides first match, length of match, and then useBytes
```

```
## [1] 16
## attr(,"match.length")
## [1] 1
## attr(,"useBytes")
## [1] TRUE
```

```r
regexpr("f",fakeName)[1]  ## shows the location of the first match
```

```
## [1] 16
```

```r
attr(regexpr("f",fakeName),"match.length")  ## Shows the length of match
```

```
## [1] 1
```

```r
attr(regexpr("f",fakeName),"useBytes")  ## Shows the useBytes of match (TRUE/FALSE)
```

```
## [1] TRUE
```

```r
regexpr("f",fakeName,ignore.case=TRUE)  ## finds the F which comes before f
```

```
## [1] 7
## attr(,"match.length")
## [1] 1
## attr(,"useBytes")
## [1] TRUE
```

```r
regexpr("f",fakeName,ignore.case=TRUE)[1] ## shows the location of the first match
```

```
## [1] 7
```

```r
regexpr("2/4","2/4/16")  ## Returns 1 3 TRUE (result is 1 with match.length 3 and useBytes TRUE)
```

```
## [1] 1
## attr(,"match.length")
## [1] 3
## attr(,"useBytes")
## [1] TRUE
```

```r
regexpr("2/4","2/4/16",fixed=TRUE)  ## No difference
```

```
## [1] 1
## attr(,"match.length")
## [1] 3
## attr(,"useBytes")
## [1] TRUE
```
  
  
####_This can be expanded to identify all instances of the pattern_  
The gregexpr does the same thing but returns the lists/attributes for all the matches:  

```r
gregexpr("f",fakeName,ignore.case=TRUE)  ## Finds positions 7, 8, and 16
```

```
## [[1]]
## [1]  7  8 16
## attr(,"match.length")
## [1] 1 1 1
## attr(,"useBytes")
## [1] TRUE
```

```r
length(gregexpr("f",fakeName,ignore.case=TRUE)[[1]]) ## Reports the 3 matches
```

```
## [1] 3
```

```r
gregexpr("f",fakeName,ignore.case=TRUE)[[1]][1] ## Returns 7
```

```
## [1] 7
```

```r
gregexpr("f",fakeName,ignore.case=TRUE)[[1]][2] ## Returns 8
```

```
## [1] 8
```

```r
gregexpr("f",fakeName,ignore.case=TRUE)[[1]][3] ## Returns 16
```

```
## [1] 16
```

```r
attr(gregexpr("f",fakeName,ignore.case=TRUE)[[1]],"match.length") ## Returns 1 1 1
```

```
## [1] 1 1 1
```

```r
attr(gregexpr("f",fakeName,ignore.case=TRUE)[[1]],"match.length")[2] ## Returns 1
```

```
## [1] 1
```

```r
gregexpr("fGh",fakeName,ignore.case=TRUE)  ## Finds just position 8 with length 3
```

```
## [[1]]
## [1] 8
## attr(,"match.length")
## [1] 3
## attr(,"useBytes")
## [1] TRUE
```
  
  
####_Not sure I understand this_  
There is also a regexec command, but I do not yet see where it comes in handy:  

```r
regexec("f",fakeName,ignore.case=TRUE) ## Not so sure, get back 7 and attr(,"match.lenght") 1
```

```
## [[1]]
## [1] 7
## attr(,"match.length")
## [1] 1
```

```r
regexec("fGh",fakeName,ignore.case=TRUE)  ## Not so sure, get back 8 and 3
```

```
## [[1]]
## [1] 8
## attr(,"match.length")
## [1] 3
```
  
  
####_There are a few handy preset variables available in base_  
Further, R contains some handy presets to help with common processing needs:  

```r
letters ## all the lower case letters a-z
```

```
##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
## [18] "r" "s" "t" "u" "v" "w" "x" "y" "z"
```

```r
length(letters)  ## 26
```

```
## [1] 26
```

```r
letters[c(1,4,9,16,25)]  ## a d i p y
```

```
## [1] "a" "d" "i" "p" "y"
```

```r
nchar(letters[c(1,4,9,16,25)])  ## 1 1 1 1 1
```

```
## [1] 1 1 1 1 1
```

```r
LETTERS ## all the upper case letters A-Z
```

```
##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q"
## [18] "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
```

```r
month.abb ## all the 3-character month abbreviations in form Jan
```

```
##  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
## [12] "Dec"
```

```r
month.abb[c(1,10)]  ## "Jan" "Oct"
```

```
## [1] "Jan" "Oct"
```

```r
month.name ## full month names in form February
```

```
##  [1] "January"   "February"  "March"     "April"     "May"      
##  [6] "June"      "July"      "August"    "September" "October"  
## [11] "November"  "December"
```

```r
length(month.name) ## 12
```

```
## [1] 12
```

```r
nchar(month.name) ## 7 8 5 5 3 4 4 6 9 7 8 8
```

```
##  [1] 7 8 5 5 3 4 4 6 9 7 8 8
```

```r
month.name[c(1,13)]  ## "January" NA
```

```
## [1] "January" NA
```

```r
length(month.name[c(1,13)])  ## 2
```

```
## [1] 2
```

```r
month.name[c(0,12)]  ## "December"
```

```
## [1] "December"
```

```r
length(month.name[c(0,12)])  ## 1
```

```
## [1] 1
```

```r
month.name[c(0,13)]  ## NA
```

```
## [1] NA
```

```r
length(month.name[c(0,13)])  ## 1
```

```
## [1] 1
```

```r
pi ## 3.141593 etc.
```

```
## [1] 3.141593
```
  
  
### Chapter 5 Formulae vs. Functions  

I am mostly OK with functions already, but it will be good to have a few key operations documented.  

####_Rounding functions_  
It will be helpful to document how R handles a few key rounding functions:  

```r
## Note that there is no option to pass a digit to ceiling - it is always to nearest 1
ceiling(0.2)  ## 1
```

```
## [1] 1
```

```r
ceiling(0)  ## 0
```

```
## [1] 0
```

```r
ceiling (-0.2) ## 0
```

```
## [1] 0
```

```r
ceiling(10*pi)/10 ## 3.2 (this will round up to the nearest 0.1)
```

```
## [1] 3.2
```

```r
floor(0.2) ## 0
```

```
## [1] 0
```

```r
floor(0) ## 0
```

```
## [1] 0
```

```r
floor(-0.2)  ## -1
```

```
## [1] -1
```

```r
floor(10*pi)/10 ## 3.1 (this will round down to the nearest 0.1)
```

```
## [1] 3.1
```

```r
round(2.6)  ## 3 (default is round to nearest integer)
```

```
## [1] 3
```

```r
round(2.5)  ## 2 (NOTE the goof-ball algorithm where 0.5 always rounds to even)
```

```
## [1] 2
```

```r
round(1.5)  ## 2 (default is round to nearest integer)
```

```
## [1] 2
```

```r
round(-2.5)  ## -2 (NOTE the goof-ball algorithm where 0.5 always rounds to even)
```

```
## [1] -2
```

```r
round(-1.5)  ## -2 (default is round to nearest integer)
```

```
## [1] -2
```

```r
round(pi,4)  ## 3.1416 (the four means four post-decimal)
```

```
## [1] 3.1416
```

```r
signif(pi,4)  ## 3.142 (the four means four total)
```

```
## [1] 3.142
```

```r
round(pi/1000,4)  ## .0031 (still four post-decimal)
```

```
## [1] 0.0031
```

```r
signif(pi/1000,4) ## .003142 (still four total)
```

```
## [1] 0.003142
```

```r
trunc (1.8)  ## 1 (truncate is TOWARDS 0)
```

```
## [1] 1
```

```r
trunc(-1.8)  ## -1 (truncate is TOWARDS 0)
```

```
## [1] -1
```

```r
log(10)  ## default is LN
```

```
## [1] 2.302585
```

```r
log(10, base=10)  ## back to 1
```

```
## [1] 1
```

```r
log10(10)  ## Same as setting log and base=10 but may be faster
```

```
## [1] 1
```
  
  
####_Ranking variables in R_  
It is also handy sometimes to gather the rank of something - see below:  

```r
str(rank)
```

```
## function (x, na.last = TRUE, ties.method = c("average", "first", "random", 
##     "max", "min"))
```

```r
## na.last: default TRUE (NA last) or FALSE (NA first) or NA (NA removed) or "keep" (rank NA)
## ties.method: default "average" (all ties get their average rank) or "random" (ties break randomly)
##              or "min" (typicaly sports ranking, all ties get the best rank)
##              or "max" (all ties get worst rank) or "first" (ties break by order found)

set.seed(0218160926)
x <- c(1,2,2,2,3,4,NA,NA,7,7,9,10)

rank(x)  ## The 2 all get rank 3, the 7 both get rank 7.5 and the NA get rank 11 and 12
```

```
##  [1]  1.0  3.0  3.0  3.0  5.0  6.0 11.0 12.0  7.5  7.5  9.0 10.0
```

```r
rank(x,na.last=TRUE)  ## Same
```

```
##  [1]  1.0  3.0  3.0  3.0  5.0  6.0 11.0 12.0  7.5  7.5  9.0 10.0
```

```r
rank(x,na.last=FALSE)  ## NA promoted to 1 and 2
```

```
##  [1]  3.0  5.0  5.0  5.0  7.0  8.0  1.0  2.0  9.5  9.5 11.0 12.0
```

```r
rank(x,na.last=NA)  ## NA vector shortened as NA discarded from results vector (seems risky . . . )
```

```
##  [1]  1.0  3.0  3.0  3.0  5.0  6.0  7.5  7.5  9.0 10.0
```

```r
rank(x,na.last="keep")  ## Same as above, but vector stays same length with rank as NA
```

```
##  [1]  1.0  3.0  3.0  3.0  5.0  6.0   NA   NA  7.5  7.5  9.0 10.0
```

```r
rank(x,ties.method="average")  ## Same as default
```

```
##  [1]  1.0  3.0  3.0  3.0  5.0  6.0 11.0 12.0  7.5  7.5  9.0 10.0
```

```r
rank(x,ties.method="first")  ## Ties broken so the 2's rank 2 3 4 and the 7's rank 7 8
```

```
##  [1]  1  2  3  4  5  6 11 12  7  8  9 10
```

```r
rank(x,ties.method="random")  ## Ties broken so the 2's rank randomly 2-4 and the 7's rank randomly 7-8
```

```
##  [1]  1  3  4  2  5  6 11 12  8  7  9 10
```

```r
rank(x,ties.method="random")  ## Ranks change with different random numbers
```

```
##  [1]  1  2  4  3  5  6 11 12  8  7  9 10
```

```r
rank(x,ties.method="max")  ## The 2's all rank 4 and the 7's both rank 8
```

```
##  [1]  1  4  4  4  5  6 11 12  8  8  9 10
```

```r
rank(x,ties.method="min")  ## The 2's all rank 2 and the 7's both rank 7
```

```
##  [1]  1  2  2  2  5  6 11 12  7  7  9 10
```

  
  
### Chapter 7: Data Inspection  

Remember the unique() call which is handy in certain cases:  

```r
fakeName <- "AbcDe FFGhiJL mfoP"
unique(strsplit(toupper(fakeName),"")[[1]])  ## A B C D E " " F G H I J K L M O P
```

```
##  [1] "A" "B" "C" "D" "E" " " "F" "G" "H" "I" "J" "L" "M" "O" "P"
```
  
  
Remember also the pairs() function for getting pairwise plots:  

```r
pairs(iris)  ## Default dataset in R
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
  
  
There may be benefit to further studying xtabs (cross tabulation):  

```r
str(xtabs)
```

```
## function (formula = ~., data = parent.frame(), subset, sparse = FALSE, 
##     na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE)
```

```r
xtabs(~ Species + Petal.Width, data=iris) ## counts of Species x Petal.Width
```

```
##             Petal.Width
## Species      0.1 0.2 0.3 0.4 0.5 0.6  1 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8
##   setosa       5  29   7   7   1   1  0   0   0   0   0   0   0   0   0
##   versicolor   0   0   0   0   0   0  7   3   5  13   7  10   3   1   1
##   virginica    0   0   0   0   0   0  0   0   0   0   1   2   1   1  11
##             Petal.Width
## Species      1.9  2 2.1 2.2 2.3 2.4 2.5
##   setosa       0  0   0   0   0   0   0
##   versicolor   0  0   0   0   0   0   0
##   virginica    5  6   6   3   8   3   3
```

```r
xtabs(Petal.Width ~ Species, data=iris) ## Sum of Petal.Width by Species
```

```
## Species
##     setosa versicolor  virginica 
##       12.3       66.3      101.3
```

```r
xtabs(Petal.Width + Petal.Length ~ Species, data=iris) ## Straight sums Width/Length, by Species
```

```
## Species
##     setosa versicolor  virginica 
##       85.4      279.3      378.9
```

```r
xtabs(Petal.Length ~ Species + Petal.Width, data=iris) ## Sum of Petal Length for Species x Petal Width
```

```
##             Petal.Width
## Species       0.1  0.2  0.3  0.4  0.5  0.6    1  1.1  1.2  1.3  1.4  1.5
##   setosa      6.9 41.9 10.0 11.0  1.7  1.6  0.0  0.0  0.0  0.0  0.0  0.0
##   versicolor  0.0  0.0  0.0  0.0  0.0  0.0 25.4 10.7 21.2 54.3 31.5 45.8
##   virginica   0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  5.6 10.1
##             Petal.Width
## Species       1.6  1.7  1.8  1.9    2  2.1  2.2  2.3  2.4  2.5
##   setosa      0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0
##   versicolor 14.3  5.0  4.8  0.0  0.0  0.0  0.0  0.0  0.0  0.0
##   virginica   5.8  4.5 59.2 26.6 33.3 34.7 18.1 45.6 16.3 17.8
```
  
  
### Chapter 8: Column Operations  
  
I am mostly OK with these, but good to have a refresher of ifelse()  

```r
ifelse(iris$Species[c(1:2,100,150)]=="setosa","yes","no")  ## yes yes no no
```

```
## [1] "yes" "yes" "no"  "no"
```
  
  
Quick refresher on some some helpful grep syntax:  

```r
fakeName <- "AbcDe FFGhiJL mfoP"
grep("$",strsplit(gsub(" ","$",fakeName),"")[[1]])  ## Returns 1 2 3 4 . . . $ is a special character
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
```

```r
grep("\\$",strsplit(gsub(" ","$",fakeName),"")[[1]])  ## Returns 6 14 since $ is escaped by \\
```

```
## [1]  6 14
```

```r
grep("\\$|f|F",strsplit(gsub(" ","$",fakeName),"")[[1]])  ## Returns 6 7 8 14 16 since | is OR
```

```
## [1]  6  7  8 14 16
```
  
  
####_Removing unwanted columns_  
Sometimes you do not want a column anymore -- there are several ways to delete it:  

```r
## myFrame$badColumn <- NULL ## Incredibly, thet gets rid of a column!
## myFrame <- myFrame[ , c(-2,-4,-6)]  ## Will remove columns 2, 4, 6
```


### Chapter 10: sqldf and dplyr  
Some examples for running SQL code or dplyr.  
  
####_Running sqldf_  
With sqldf, you basically have access to on-board SQL but without the hassles of loading RMySQL:  

```r
library(sqldf) ## The sqldf library
batting <- read.csv("Lahman/Batting.csv",stringsAsFactors=FALSE) ## The baseball dataset

sqldf("SELECT playerID, yearID, teamID, AB  
       FROM batting                         
       WHERE yearID=2009                    
       ORDER BY AB DESC                     
       LIMIT 10")
```

```
##     playerID yearID teamID  AB
## 1   hillaa01   2009    TOR 682
## 2  rolliji01   2009    PHI 672
## 3  markani01   2009    BAL 642
## 4  suzukic01   2009    SEA 639
## 5   canoro01   2009    NYA 637
## 6  braunry02   2009    MIL 635
## 7  tejadmi01   2009    HOU 635
## 8  jeterde01   2009    NYA 634
## 9  roberbr01   2009    BAL 632
## 10 grandcu01   2009    DET 631
```
  
  
####_Manipulating data with dplyr (basics)_  
The dplyr package is an amazing way to work with data - key verbs:  
* select() select columns  
* mutate() add or reformat columns  
* filter() select rows  
* arrange() sort rows  
* summarize() summarize datasets  
  


```r
library(dplyr)
## Core frame is listed first in each of the calls

## Select just takes the requested columns
head(select(batting,playerID,yearID))  
```

```
##    playerID yearID
## 1 aardsda01   2004
## 2 aardsda01   2006
## 3 aardsda01   2007
## 4 aardsda01   2008
## 5 aardsda01   2009
## 6 aardsda01   2010
```

```r
## Mutate creates new variables based on existing variables
myTemp <- mutate(batting,avg=H/AB,shortID=substr(playerID,1,5))
head(select(myTemp,playerID,shortID,H,AB,avg))
```

```
##    playerID shortID H AB avg
## 1 aardsda01   aards 0  0 NaN
## 2 aardsda01   aards 0  2   0
## 3 aardsda01   aards 0  0 NaN
## 4 aardsda01   aards 0  1   0
## 5 aardsda01   aards 0  0 NaN
## 6 aardsda01   aards 0  0 NaN
```

```r
tail(select(myTemp,playerID,shortID,H,AB,avg))
```

```
##        playerID shortID   H  AB       avg
## 97884 zieglbr01   ziegl   0   2 0.0000000
## 97885 zimmejo02   zimme   8  65 0.1230769
## 97886 zimmery01   zimme 156 568 0.2746479
## 97887  zitoba01   zitob   5  34 0.1470588
## 97888 zobribe01   zobri 168 612 0.2745098
## 97889 zuninmi01   zunin  37 173 0.2138728
```

```r
## Filter subsets the data based on criteria with each , being and (use | for or)
filter(batting,yearID==2010,teamID=="NYA" | teamID=="NYN",AB>600)[,1:5]
```

```
##    playerID yearID stint teamID lgID
## 1  canoro01   2010     1    NYA   AL
## 2 jeterde01   2010     1    NYA   AL
## 3 teixema01   2010     1    NYA   AL
```

```r
## Arrange runs the sorting for you - no need to use the x[order(x)] concept
head(arrange(batting,desc(yearID),teamID,desc(AB)))[,1:5]
```

```
##    playerID yearID stint teamID lgID
## 1 pradoma01   2013     1    ARI   NL
## 2 goldspa01   2013     1    ARI   NL
## 3 parrage01   2013     1    ARI   NL
## 4 polloaj01   2013     1    ARI   NL
## 5 montemi01   2013     1    ARI   NL
## 6 gregodi01   2013     1    ARI   NL
```
  
  
####_Expanding dplyr with group_by()_  
The dplyr functionality can then be expanded with the group_by() call, which creates an internal grouping without actually modifying the data.  See below for example usages:  

```r
library(dplyr)
set.seed(0218160926)

## By-group processing (keep n results from n items)
x <- filter(batting, yearID==2013)  ## take only 2013
x <- group_by(x, teamID)  ## create an internal grouping (no real data change)
x <- arrange(x, teamID, desc(HR))  ## sort by teamID, then descending HR

## Now, the mutate below will be running everything by teamID -- rank, cumulative, cumulative %
x <- mutate(x, rank=rank(desc(HR), ties.method="min"), cum_HR=cumsum(HR), 
            cum_HR_pct = round(cumsum(HR)/sum(HR),2))

x[1:10,c("teamID","playerID","HR","rank","cum_HR","cum_HR_pct")]
```

```
## Source: local data frame [10 x 6]
## Groups: teamID [1]
## 
##    teamID  playerID    HR  rank cum_HR cum_HR_pct
##     (chr)     (chr) (int) (int)  (int)      (dbl)
## 1     ARI goldspa01    36     1     36       0.28
## 2     ARI pradoma01    14     2     50       0.38
## 3     ARI  hillaa01    11     3     61       0.47
## 4     ARI montemi01    11     3     72       0.55
## 5     ARI parrage01    10     5     82       0.63
## 6     ARI chaveer01     9     6     91       0.70
## 7     ARI polloaj01     8     7     99       0.76
## 8     ARI  rossco01     8     7    107       0.82
## 9     ARI gregodi01     7     9    114       0.88
## 10    ARI kubelja01     5    10    119       0.92
```

```r
x[sample(1:nrow(x),10,replace=FALSE),c("teamID","playerID","HR","rank","cum_HR","cum_HR_pct")]
```

```
## Source: local data frame [10 x 6]
## Groups: teamID [8]
## 
##    teamID  playerID    HR  rank cum_HR cum_HR_pct
##     (chr)     (chr) (int) (int)  (int)      (dbl)
## 1     BAL flahery01    10     7    169       0.80
## 2     LAN rodrist02     0    21    138       1.00
## 3     SEA wilheto01     0    21    188       1.00
## 4     LAA troutmi01    27     2     61       0.37
## 5     LAN ramirha01    20     2     42       0.30
## 6     BOS berryqu01     1    15    174       0.98
## 7     PIT harrijo05     3    12    152       0.94
## 8     COL tulowtr01    25     2     51       0.32
## 9     HOU  penaca01     8     5     97       0.66
## 10    LAN garcion01     0    21    138       1.00
```

```r
x <- filter(x, rank <= 5)
x[1:15,c("teamID","playerID","HR","rank","cum_HR","cum_HR_pct")]  ## Top 5 players on first 3 teams
```

```
## Source: local data frame [15 x 6]
## Groups: teamID [3]
## 
##    teamID  playerID    HR  rank cum_HR cum_HR_pct
##     (chr)     (chr) (int) (int)  (int)      (dbl)
## 1     ARI goldspa01    36     1     36       0.28
## 2     ARI pradoma01    14     2     50       0.38
## 3     ARI  hillaa01    11     3     61       0.47
## 4     ARI montemi01    11     3     72       0.55
## 5     ARI parrage01    10     5     82       0.63
## 6     ATL uptonju01    27     1     27       0.15
## 7     ATL freemfr01    23     2     50       0.28
## 8     ATL ugglada01    22     3     72       0.40
## 9     ATL gattiev01    21     4     93       0.51
## 10    ATL mccanbr01    20     5    113       0.62
## 11    BAL davisch02    53     1     53       0.25
## 12    BAL jonesad01    33     2     86       0.41
## 13    BAL hardyjj01    25     3    111       0.52
## 14    BAL wietema01    22     4    133       0.63
## 15    BAL machama01    14     5    147       0.69
```

```r
myTest <- filter(x, rank <= 3)
myTest[1:10,c("teamID","playerID","HR","rank","cum_HR","cum_HR_pct")]  ## keeps tie
```

```
## Source: local data frame [10 x 6]
## Groups: teamID [3]
## 
##    teamID  playerID    HR  rank cum_HR cum_HR_pct
##     (chr)     (chr) (int) (int)  (int)      (dbl)
## 1     ARI goldspa01    36     1     36       0.28
## 2     ARI pradoma01    14     2     50       0.38
## 3     ARI  hillaa01    11     3     61       0.47
## 4     ARI montemi01    11     3     72       0.55
## 5     ATL uptonju01    27     1     27       0.15
## 6     ATL freemfr01    23     2     50       0.28
## 7     ATL ugglada01    22     3     72       0.40
## 8     BAL davisch02    53     1     53       0.25
## 9     BAL jonesad01    33     2     86       0.41
## 10    BAL hardyjj01    25     3    111       0.52
```
  
  
####_Expanding dplyr by skipping new column creation_  
The dplyr code above can be consolidated, avoiding the need to create new variables (maybe harder to read and debug, but with the reward of greater efficiency):  

```r
library(dplyr)

## Sam as previous
x <- filter(batting, yearID==2013)  ## take only 2013
x <- group_by(x, teamID)  ## create an internal grouping (no real data change)
x <- arrange(x, teamID, desc(HR))  ## sort by teamID, then descending HR

## Skip creating columns and updating databases
x <- filter(x, rank(desc(HR), ties.method="min") <= 5)
x[1:15,c("teamID","playerID","yearID","HR")]  ## Top 5 players on first 3 teams
```

```
## Source: local data frame [15 x 4]
## Groups: teamID [3]
## 
##    teamID  playerID yearID    HR
##     (chr)     (chr)  (int) (int)
## 1     ARI goldspa01   2013    36
## 2     ARI pradoma01   2013    14
## 3     ARI  hillaa01   2013    11
## 4     ARI montemi01   2013    11
## 5     ARI parrage01   2013    10
## 6     ATL uptonju01   2013    27
## 7     ATL freemfr01   2013    23
## 8     ATL ugglada01   2013    22
## 9     ATL gattiev01   2013    21
## 10    ATL mccanbr01   2013    20
## 11    BAL davisch02   2013    53
## 12    BAL jonesad01   2013    33
## 13    BAL hardyjj01   2013    25
## 14    BAL wietema01   2013    22
## 15    BAL machama01   2013    14
```
  
  
####_Expanding dplyr by chaining_  
The %>% operator in dplyr lets many of these operations run in order.  For example:  

```r
library(dplyr)

## Old code for reference
x <- filter(batting, yearID==2013)  ## take only 2013
x <- group_by(x, teamID)  ## create an internal grouping (no real data change)
x <- arrange(x, teamID, desc(HR))  ## sort by teamID, then descending HR
x <- mutate(x, rank=rank(desc(HR), ties.method="min"), cum_HR=cumsum(HR), 
            cum_HR_pct = round(cumsum(HR)/sum(HR),2))
x <- filter(x, rank <= 5)
x[1:15,c("teamID","playerID","HR","rank","cum_HR","cum_HR_pct")]  ## Top 5 players on first 3 teams
```

```
## Source: local data frame [15 x 6]
## Groups: teamID [3]
## 
##    teamID  playerID    HR  rank cum_HR cum_HR_pct
##     (chr)     (chr) (int) (int)  (int)      (dbl)
## 1     ARI goldspa01    36     1     36       0.28
## 2     ARI pradoma01    14     2     50       0.38
## 3     ARI  hillaa01    11     3     61       0.47
## 4     ARI montemi01    11     3     72       0.55
## 5     ARI parrage01    10     5     82       0.63
## 6     ATL uptonju01    27     1     27       0.15
## 7     ATL freemfr01    23     2     50       0.28
## 8     ATL ugglada01    22     3     72       0.40
## 9     ATL gattiev01    21     4     93       0.51
## 10    ATL mccanbr01    20     5    113       0.62
## 11    BAL davisch02    53     1     53       0.25
## 12    BAL jonesad01    33     2     86       0.41
## 13    BAL hardyjj01    25     3    111       0.52
## 14    BAL wietema01    22     4    133       0.63
## 15    BAL machama01    14     5    147       0.69
```

```r
## Use chaining to filter, then group_by, then arrange, then mutate, then filter again
y <- filter(batting, yearID==2013) %>%
     group_by(teamID) %>%
     arrange(teamID, desc(HR)) %>%
     mutate(rank=rank(desc(HR),ties.method="min"),cum_HR=cumsum(HR),
            cum_HR_pct=round(cum_HR/sum(HR),2)
            ) %>%
     filter(rank <= 5)
y[1:15,c("teamID","playerID","HR","rank","cum_HR","cum_HR_pct")]  ## Top 5 players on first 3 teams
```

```
## Source: local data frame [15 x 6]
## Groups: teamID [3]
## 
##    teamID  playerID    HR  rank cum_HR cum_HR_pct
##     (chr)     (chr) (int) (int)  (int)      (dbl)
## 1     ARI goldspa01    36     1     36       0.28
## 2     ARI pradoma01    14     2     50       0.38
## 3     ARI  hillaa01    11     3     61       0.47
## 4     ARI montemi01    11     3     72       0.55
## 5     ARI parrage01    10     5     82       0.63
## 6     ATL uptonju01    27     1     27       0.15
## 7     ATL freemfr01    23     2     50       0.28
## 8     ATL ugglada01    22     3     72       0.40
## 9     ATL gattiev01    21     4     93       0.51
## 10    ATL mccanbr01    20     5    113       0.62
## 11    BAL davisch02    53     1     53       0.25
## 12    BAL jonesad01    33     2     86       0.41
## 13    BAL hardyjj01    25     3    111       0.52
## 14    BAL wietema01    22     4    133       0.63
## 15    BAL machama01    14     5    147       0.69
```

```r
identical(x,y)  ## The results are exactly, 100% the same
```

```
## [1] TRUE
```
  
  
### Chapter 11: merge()  
Per Taveras, merge() is more or less the equivalent of VLOOKUP in R.  The advantages seem to be that you can do a multi-column lookup and/or value-gathering.  The downside is that it is always doing a ,FALSE where the lookup needs to exactly match.  My Exercise001 code is designed to allow for ,1 and ,-1 lookup also.  
  

```r
players <- read.csv("Lahman/Master.csv",stringsAsFactors=FALSE) ## The master players data

## merge(x, y, 
##       by=intersect(names(x),names(y)), by.x=by, by.y=by, 
##       all=FALSE, all.x=all, all.y=all
##       sort=TRUE, suffixes=c(".x",".y"), incomparables=NULL)
##
## can declare both dataset columns using by= or single dataset columns using by.x= and by.y=
## notably, the by variables need not match names; by.x=Player_ID, by.y=IDPLAYER
## default is inner join; can override with all.x=TRUE (left) or all.y=TRUE (right) or all=TRUE (outer)
## sort=TRUE sorts the result by the by columns

battingCountry <- merge(batting, players[ ,c("playerID","birthCountry","birthState")])

## Note that dplyr has merge functions also, which may be handy when more advanced programming is needed
```
  
  
### Chapter 12: summarize()  
The summarize() call in dplyr can give you totals by the group_by variables (this no longer has the property that n-inputs keeps n-outputs):  

```r
library(dplyr)

mySummary <- filter(batting, yearID>=2008, !is.na(AB)) %>%
             group_by(teamID, yearID) %>%
             summarize(RBI_sum=sum(RBI),sd_RBI=round(sd(RBI),1),
                       avg=round(sum(H)/sum(AB),3),size=n_distinct(playerID)
                       )

mySummary[1:15,]
```

```
## Source: local data frame [15 x 6]
## Groups: teamID [3]
## 
##    teamID yearID RBI_sum sd_RBI   avg  size
##     (chr)  (int)   (int)  (dbl) (dbl) (int)
## 1     ARI   2008     683   26.0 0.251    41
## 2     ARI   2009     686   24.4 0.253    45
## 3     ARI   2010     691   26.8 0.250    48
## 4     ARI   2011     702   22.7 0.250    51
## 5     ARI   2012     710   25.7 0.259    48
## 6     ARI   2013     647   25.1 0.259    44
## 7     ATL   2008     721   24.9 0.270    49
## 8     ATL   2009     700   24.6 0.263    45
## 9     ATL   2010     699   23.8 0.258    43
## 10    ATL   2011     606   23.8 0.243    45
## 11    ATL   2012     660   27.4 0.247    41
## 12    ATL   2013     656   26.0 0.249    44
## 13    BAL   2008     750   32.7 0.267    36
## 14    BAL   2009     708   28.9 0.268    35
## 15    BAL   2010     577   23.2 0.259    36
```

```r
## Can always use aggregate from base for simpler versions
altApproach <- aggregate(RBI ~ teamID, data=batting[batting$yearID==2008,], FUN=sum)
altApproach[1:15,]
```

```
##    teamID RBI
## 1     ARI 683
## 2     ATL 721
## 3     BAL 750
## 4     BOS 807
## 5     CHA 785
## 6     CHN 811
## 7     CIN 677
## 8     CLE 772
## 9     COL 714
## 10    DET 780
## 11    FLO 741
## 12    HOU 684
## 13    KCA 650
## 14    LAA 721
## 15    LAN 659
```
  
  
### Chapter 13: Reshaping the data  
The data can be reshaped using Wickham's package reshape.  I am not yet sure whether-how this relates to Wickham's tidyr package but it has two examples that are very commonly useful.  
* melt coverts wide data to long  
* dcast converts long data to wide  
  
####_Basic philosophy of melting_  
Melting consists of making two decisions:  
1.  What represents a single object?  These become id.vars  
2.  What columns represent the measures per object?  These become measure.vars  



```r
library(reshape2)

wide_data <- data.frame(player=c(1,2,3),points_Age25=c(263, 279, 172),points_Age28=c(111,117,221),
                        fake=c("Who","needs","these")
                        )

wide_data
```

```
##   player points_Age25 points_Age28  fake
## 1      1          263          111   Who
## 2      2          279          117 needs
## 3      3          172          221 these
```

```r
melt(wide_data,id.vars=c("player"),measure.vars=c("points_Age25","points_Age28"),
     value.name="Points",variable.name="Age")
```

```
##   player          Age Points
## 1      1 points_Age25    263
## 2      2 points_Age25    279
## 3      3 points_Age25    172
## 4      1 points_Age28    111
## 5      2 points_Age28    117
## 6      3 points_Age28    221
```

```r
## Could gsub away the Points_Age in Age and convert to numeric
```
  
  
####_Basic philosophy of dcast_  
To dcast requires making three decisions:  
1.  Which column(s) represent the object?  
2.  Which column(s) do you want to transpose over?  
3.  Which column contains the measure?  
  

```r
library(reshape2)

long_data <- data.frame(player=c(1,1,2,2,3,3),age=c(25,28,25,28,25,28),
                        points=c(263,111,279,117,172,221),fake=c("do","not","want","these","to","stay")
                        )

long_data
```

```
##   player age points  fake
## 1      1  25    263    do
## 2      1  28    111   not
## 3      2  25    279  want
## 4      2  28    117 these
## 5      3  25    172    to
## 6      3  28    221  stay
```

```r
dcast(long_data, player ~ age, value.var="points")
```

```
##   player  25  28
## 1      1 263 111
## 2      2 279 117
## 3      3 172 221
```

```r
long_data_2 <- data.frame(player=c(1,1,2,2,3,3,1,1,2,2,3,3),
                          age=c(25,28,25,28,25,28,25,28,25,28,25,28),
                          points=c(263,111,279,117,172,221,269,113,285,119,178,223),
                          day_night=c(rep("D",6),rep("N",6)),
                          fake = LETTERS[1:12]
                          )

long_data_2
```

```
##    player age points day_night fake
## 1       1  25    263         D    A
## 2       1  28    111         D    B
## 3       2  25    279         D    C
## 4       2  28    117         D    D
## 5       3  25    172         D    E
## 6       3  28    221         D    F
## 7       1  25    269         N    G
## 8       1  28    113         N    H
## 9       2  25    285         N    I
## 10      2  28    119         N    J
## 11      3  25    178         N    K
## 12      3  28    223         N    L
```

```r
dcast(long_data_2,player ~ day_night + age, value.var="points")
```

```
##   player D_25 D_28 N_25 N_28
## 1      1  263  111  269  113
## 2      2  279  117  285  119
## 3      3  172  221  178  223
```
  
  

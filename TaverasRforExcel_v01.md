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
  
  
### Chapter 4 Formulae vs. Functions  

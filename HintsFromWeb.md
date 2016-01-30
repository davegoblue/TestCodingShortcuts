
VLOOKUP with interval (-1 or +1) in R idea from:
http://shashiasrblog.blogspot.com/2014/01/excel-style-vlookup-and-rangelookup-in-r.html

find_interval<-function(data,lookup){
  data$label<-findInterval(x=data$indep1,vec=lookup$min_value)
  data
}
data_interval<-find_interval(data=data_table,lookup_table)


SORT from discussion board:

x <- x[order(x)]


MERGE BY from base R:
https://stat.ethz.ch/R-manual/R-devel/library/base/html/merge.html

## S3 method for class 'data.frame'
merge(x, y, by = intersect(names(x), names(y)),
      by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
      sort = TRUE, suffixes = c(".x",".y"),
      incomparables = NULL, ...)

* by=c(a,b,c) requires that columns a,b,c be in each of frames x, y
* by.x=c(a,b), by.y=c(d,e) will merge on x-a == y-c AND x-b == y-e
* all=FALSE with no arguments for all.x or all.y means inner join (only items matching in x and y)
* all=TRUE with no arguments for all.x or all.y means outer join (keeping everything in x or y, create NA as needed)
* all.x=TRUE will keep everything in x and only the matching elements in y (create NA as needed)
* all.y=TRUE will keep everything in y and only the matching elements in x (create NA as needed)
* sort=TRUE will create output that is sorted by the BY variables
* incomparables=NA will avoid treating NA as a match to NA

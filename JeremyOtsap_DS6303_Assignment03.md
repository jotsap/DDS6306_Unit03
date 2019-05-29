---
title: "Live Session 3"
author: "Jeremy Otsap"
date: "May 21, 2019"
output: 
  html_document:
    keep_md: true

---



## Question 2

A) Loading titanic.csv into data frame


```r
read.csv("https://raw.githubusercontent.com/jotsap/DDS6306_Unit03/master/titanic/titanic.csv", sep = ",", header = T) -> titanic.df
```

B) Output the respective count of females and males aboard the Titanic


```r
#all 'female' rows
titanic.df[titanic.df$Sex == 'female',] -> titanic.female
print("female passengers")
```

```
## [1] "female passengers"
```

```r
nrow(titanic.female) 
```

```
## [1] 314
```

```r
#all 'male' rows
titanic.df[titanic.df$Sex == 'male',] -> titanic.male
print("male passengers")
```

```
## [1] "male passengers"
```

```r
nrow(titanic.male) 
```

```
## [1] 577
```


### Pie Chart of Male vs Female

```r
#Pie Chart male vs femaile
titanic.table <- table(titanic.df$Sex)
titanic.labels <- paste(names(titanic.table), "\n", titanic.table, sep="")
pie(titanic.table, labels = titanic.labels, main = "Pie Chart of Gender") 
```

![](JeremyOtsap_DS6303_Assignment03_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

C) Means of Age, Fare, and Survival


```r
# ignoring NA values
sapply(titanic.df[,c(6,10,2)], mean, na.rm=T) -> titanic.mean
titanic.mean
```

```
##        Age       Fare   Survived 
## 29.6991176 32.2042080  0.3838384
```


## Question 3

A) Creating the Helper function

```r
#loading sleep data set
sleep.df <- read.csv("http://talklab.psy.gla.ac.uk/L1_labs/lab_1/homework/sleep_data_01.csv", sep = ",", header = T)

#function for 
#median of Age
#range of Duration
#mean of RSES
#std dev of RSES

sleephelp <- function(a,b,c,d) {
  aa <- median(a, na.rm = T)
  bb <- range(b, na.rm = T)
  cc <- mean(c, na.rm = T)
  dd <- sd(d, na.rm = T)
  output <- c(
    Age_Median = aa,
    Duration_Range = bb,
    RSES_Mean = cc,
    RSES_StdDev = dd
  )
  return( output)
}

sleephelp(sleep.df$Age,sleep.df$Duration,sleep.df$RSES,sleep.df$RSES)
```

```
##      Age_Median Duration_Range1 Duration_Range2       RSES_Mean 
##       14.000000        4.000000       11.000000       18.114943 
##     RSES_StdDev 
##        6.176522
```


B, C, E) Report Output

```r
#report function
#median of age
#RSES Mean / 5
#RSES Std Dev / 5
#Duration Max - Min

sleepreport <- function(a,b,c) {
  aa <- median(a, na.rm = T)
  bb <- mean(b, na.rm = T) / 5
  cc <- sd(b, na.rm = T) / 5
  dd <- max(c, na.rm = T) - min(c, na.rm = T)
  output <- c(
    MedianAge = aa,
    SelfEsteem = bb,
    SE_SD = cc,
    DurationRange = dd
  )
  outputframe <- data.frame(output)
  return(outputframe)
}
sleepreport(sleep.df$Age,sleep.df$RSES,sleep.df$Duration)
```

```
##                  output
## MedianAge     14.000000
## SelfEsteem     3.622989
## SE_SD          1.235304
## DurationRange  7.000000
```




## Question 4
A) Loading fivethirtyeight package & listing all data sets within package

```r
#install fivethirtyeight packages
library(fivethirtyeight)

# To see a list of all data sets:
data(package = "fivethirtyeight")
```

B) Loading 22nd data set "college_recent_grads" into data frame


```r
#loading 22nd data set "college_recent_grads" from fivethirtyeight package
df <- college_recent_grads
head(df)
```

```
##   rank major_code                                     major major_category
## 1    1       2419                     Petroleum Engineering    Engineering
## 2    2       2416            Mining And Mineral Engineering    Engineering
## 3    3       2415                 Metallurgical Engineering    Engineering
## 4    4       2417 Naval Architecture And Marine Engineering    Engineering
## 5    5       2405                      Chemical Engineering    Engineering
## 6    6       2418                       Nuclear Engineering    Engineering
##   total sample_size   men women sharewomen employed employed_fulltime
## 1  2339          36  2057   282  0.1205643     1976              1849
## 2   756           7   679    77  0.1018519      640               556
## 3   856           3   725   131  0.1530374      648               558
## 4  1258          16  1123   135  0.1073132      758              1069
## 5 32260         289 21239 11021  0.3416305    25694             23170
## 6  2573          17  2200   373  0.1449670     1857              2038
##   employed_parttime employed_fulltime_yearround unemployed
## 1               270                        1207         37
## 2               170                         388         85
## 3               133                         340         16
## 4               150                         692         40
## 5              5180                       16697       1672
## 6               264                        1449        400
##   unemployment_rate p25th median  p75th college_jobs non_college_jobs
## 1        0.01838053 95000 110000 125000         1534              364
## 2        0.11724138 55000  75000  90000          350              257
## 3        0.02409639 50000  73000 105000          456              176
## 4        0.05012531 43000  70000  80000          529              102
## 5        0.06109771 50000  65000  75000        18314             4440
## 6        0.17722641 50000  65000 102000         1142              657
##   low_wage_jobs
## 1           193
## 2            50
## 3             0
## 4             0
## 5           972
## 6           244
```

C) Detailed view of data sets w/in fivethirtyeight package
URL is http://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/

```r
# To see a more detailed list of all data sets, see the package vignette:
vignette("fivethirtyeight", package = "fivethirtyeight") 
```

```
## starting httpd help server ... done
```

```r
# URL is http://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/
```

D) Dimensions of dataframe


```r
#dimensions of df
dim(df)
```

```
## [1] 173  21
```

```r
#column names of df
colnames(df)
```

```
##  [1] "rank"                        "major_code"                 
##  [3] "major"                       "major_category"             
##  [5] "total"                       "sample_size"                
##  [7] "men"                         "women"                      
##  [9] "sharewomen"                  "employed"                   
## [11] "employed_fulltime"           "employed_parttime"          
## [13] "employed_fulltime_yearround" "unemployed"                 
## [15] "unemployment_rate"           "p25th"                      
## [17] "median"                      "p75th"                      
## [19] "college_jobs"                "non_college_jobs"           
## [21] "low_wage_jobs"
```

```r
#alternatively
str(df)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	173 obs. of  21 variables:
##  $ rank                       : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ major_code                 : int  2419 2416 2415 2417 2405 2418 6202 5001 2414 2408 ...
##  $ major                      : chr  "Petroleum Engineering" "Mining And Mineral Engineering" "Metallurgical Engineering" "Naval Architecture And Marine Engineering" ...
##  $ major_category             : chr  "Engineering" "Engineering" "Engineering" "Engineering" ...
##  $ total                      : int  2339 756 856 1258 32260 2573 3777 1792 91227 81527 ...
##  $ sample_size                : int  36 7 3 16 289 17 51 10 1029 631 ...
##  $ men                        : int  2057 679 725 1123 21239 2200 2110 832 80320 65511 ...
##  $ women                      : int  282 77 131 135 11021 373 1667 960 10907 16016 ...
##  $ sharewomen                 : num  0.121 0.102 0.153 0.107 0.342 ...
##  $ employed                   : int  1976 640 648 758 25694 1857 2912 1526 76442 61928 ...
##  $ employed_fulltime          : int  1849 556 558 1069 23170 2038 2924 1085 71298 55450 ...
##  $ employed_parttime          : int  270 170 133 150 5180 264 296 553 13101 12695 ...
##  $ employed_fulltime_yearround: int  1207 388 340 692 16697 1449 2482 827 54639 41413 ...
##  $ unemployed                 : int  37 85 16 40 1672 400 308 33 4650 3895 ...
##  $ unemployment_rate          : num  0.0184 0.1172 0.0241 0.0501 0.0611 ...
##  $ p25th                      : num  95000 55000 50000 43000 50000 50000 53000 31500 48000 45000 ...
##  $ median                     : num  110000 75000 73000 70000 65000 65000 62000 62000 60000 60000 ...
##  $ p75th                      : num  125000 90000 105000 80000 75000 102000 72000 109000 70000 72000 ...
##  $ college_jobs               : int  1534 350 456 529 18314 1142 1768 972 52844 45829 ...
##  $ non_college_jobs           : int  364 257 176 102 4440 657 314 500 16384 10874 ...
##  $ low_wage_jobs              : int  193 50 0 0 972 244 259 220 3253 3170 ...
```



## Question 5

A) Column names and number of colums

```r
# column names
colnames(df)
```

```
##  [1] "rank"                        "major_code"                 
##  [3] "major"                       "major_category"             
##  [5] "total"                       "sample_size"                
##  [7] "men"                         "women"                      
##  [9] "sharewomen"                  "employed"                   
## [11] "employed_fulltime"           "employed_parttime"          
## [13] "employed_fulltime_yearround" "unemployed"                 
## [15] "unemployment_rate"           "p25th"                      
## [17] "median"                      "p75th"                      
## [19] "college_jobs"                "non_college_jobs"           
## [21] "low_wage_jobs"
```

```r
# number of columns
length(colnames(df))
```

```
## [1] 21
```


B) Unique factors for major_category column in df

```r
#much simpler to use count() from dplyr
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
count(df, major_category) -> major_count
major_count
```

```
## # A tibble: 16 x 2
##    major_category                          n
##    <chr>                               <int>
##  1 Agriculture & Natural Resources        10
##  2 Arts                                    8
##  3 Biology & Life Science                 14
##  4 Business                               13
##  5 Communications & Journalism             4
##  6 Computers & Mathematics                11
##  7 Education                              16
##  8 Engineering                            29
##  9 Health                                 12
## 10 Humanities & Liberal Arts              15
## 11 Industrial Arts & Consumer Services     7
## 12 Interdisciplinary                       1
## 13 Law & Public Policy                     5
## 14 Physical Sciences                      10
## 15 Psychology & Social Work                9
## 16 Social Science                          9
```


C) Barplot of Graduates by Major


```r
# uses rainbow() function to generate as many colors as major categories
# then legend will associate those colors to the respective major_category

barplot(
  major_count$n,
  col = rainbow(16), 
  main = "Graduates by Major",
  horiz = T,
  legend.text = major_count$major_category,
  xlab = "Num of Graduates",
  ylab = "Graduate Major"
  )
```

![](JeremyOtsap_DS6303_Assignment03_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


D) Export to CSV without row labels



```r
write.csv(
  df, 
  file = "fivethirtyeight/fivethirtyeight.csv", 
  append = F, 
  sep = ",", 
  row.names = F
  )
```

```
## Warning in write.csv(df, file = "fivethirtyeight/fivethirtyeight.csv",
## append = F, : attempt to set 'append' ignored
```

```
## Warning in write.csv(df, file = "fivethirtyeight/fivethirtyeight.csv",
## append = F, : attempt to set 'sep' ignored
```



## Question 6

Github repo located here:
https://github.com/jotsap/DDS6306_Unit03 


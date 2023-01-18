---
title: "Unit 3 Overall"
author: "Bivin"
date: "4/29/2019"
output: html_document
---

## Missing Values


```r
x = NA
is.na(x)
```

```
## [1] TRUE
```

```r
#Dataframe for the Example
age = c(22,21,NA,24,19,20,23)
yrs_math_ed = c(4,5,NA,2,5,3,5)
names = c("Mary","Martha","Rosy","Kim","Kristen","Amy","Sam")
subject = c("English","Math",NA,"Sociology","Math","Music","Dance")
df = data.frame(Age = age, Years = yrs_math_ed,Name = names, Major = subject)
df
```

```
##   Age Years    Name     Major
## 1  22     4    Mary   English
## 2  21     5  Martha      Math
## 3  NA    NA    Rosy      <NA>
## 4  24     2     Kim Sociology
## 5  19     5 Kristen      Math
## 6  20     3     Amy     Music
## 7  23     5     Sam     Dance
```

```r
is.na(df$Years)
```

```
## [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
```

```r
is.na(df$Name)
```

```
## [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```

```r
is.na(df$Major)
```

```
## [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
```


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   1.0.0 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
df %>% filter(!is.na(Years)) #Filter out rows with missing Years
```

```
##   Age Years    Name     Major
## 1  22     4    Mary   English
## 2  21     5  Martha      Math
## 3  24     2     Kim Sociology
## 4  19     5 Kristen      Math
## 5  20     3     Amy     Music
## 6  23     5     Sam     Dance
```


```r
mpg %>% 
arrange(manufacturer) %>% 
print(n = 30)
```

```
## # A tibble: 234 × 11
##    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
##    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
##  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
##  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
##  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
##  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
##  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
##  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
##  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
##  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
##  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
## 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
## 11 audi         a4 quattro   2    2008     4 auto… 4        19    27 p     comp…
## 12 audi         a4 quattro   2.8  1999     6 auto… 4        15    25 p     comp…
## 13 audi         a4 quattro   2.8  1999     6 manu… 4        17    25 p     comp…
## 14 audi         a4 quattro   3.1  2008     6 auto… 4        17    25 p     comp…
## 15 audi         a4 quattro   3.1  2008     6 manu… 4        15    25 p     comp…
## 16 audi         a6 quattro   2.8  1999     6 auto… 4        15    24 p     mids…
## 17 audi         a6 quattro   3.1  2008     6 auto… 4        17    25 p     mids…
## 18 audi         a6 quattro   4.2  2008     8 auto… 4        16    23 p     mids…
## 19 chevrolet    c1500 sub…   5.3  2008     8 auto… r        14    20 r     suv  
## 20 chevrolet    c1500 sub…   5.3  2008     8 auto… r        11    15 e     suv  
## 21 chevrolet    c1500 sub…   5.3  2008     8 auto… r        14    20 r     suv  
## 22 chevrolet    c1500 sub…   5.7  1999     8 auto… r        13    17 r     suv  
## 23 chevrolet    c1500 sub…   6    2008     8 auto… r        12    17 r     suv  
## 24 chevrolet    corvette     5.7  1999     8 manu… r        16    26 p     2sea…
## 25 chevrolet    corvette     5.7  1999     8 auto… r        15    23 p     2sea…
## 26 chevrolet    corvette     6.2  2008     8 manu… r        16    26 p     2sea…
## 27 chevrolet    corvette     6.2  2008     8 auto… r        15    25 p     2sea…
## 28 chevrolet    corvette     7    2008     8 manu… r        15    24 p     2sea…
## 29 chevrolet    k1500 tah…   5.3  2008     8 auto… 4        14    19 r     suv  
## 30 chevrolet    k1500 tah…   5.3  2008     8 auto… 4        11    14 e     suv  
## # … with 204 more rows
```


```r
mpg %>%
arrange(manufacturer, cty) %>% 
print(n = 30)
```

```
## # A tibble: 234 × 11
##    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
##    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
##  1 audi         a4 quattro   2.8  1999     6 auto… 4        15    25 p     comp…
##  2 audi         a4 quattro   3.1  2008     6 manu… 4        15    25 p     comp…
##  3 audi         a6 quattro   2.8  1999     6 auto… 4        15    24 p     mids…
##  4 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
##  5 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
##  6 audi         a6 quattro   4.2  2008     8 auto… 4        16    23 p     mids…
##  7 audi         a4 quattro   2.8  1999     6 manu… 4        17    25 p     comp…
##  8 audi         a4 quattro   3.1  2008     6 auto… 4        17    25 p     comp…
##  9 audi         a6 quattro   3.1  2008     6 auto… 4        17    25 p     mids…
## 10 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
## 11 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
## 12 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
## 13 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
## 14 audi         a4 quattro   2    2008     4 auto… 4        19    27 p     comp…
## 15 audi         a4           2    2008     4 manu… f        20    31 p     comp…
## 16 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
## 17 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
## 18 audi         a4           2    2008     4 auto… f        21    30 p     comp…
## 19 chevrolet    c1500 sub…   5.3  2008     8 auto… r        11    15 e     suv  
## 20 chevrolet    k1500 tah…   5.3  2008     8 auto… 4        11    14 e     suv  
## 21 chevrolet    k1500 tah…   5.7  1999     8 auto… 4        11    15 r     suv  
## 22 chevrolet    c1500 sub…   6    2008     8 auto… r        12    17 r     suv  
## 23 chevrolet    c1500 sub…   5.7  1999     8 auto… r        13    17 r     suv  
## 24 chevrolet    c1500 sub…   5.3  2008     8 auto… r        14    20 r     suv  
## 25 chevrolet    c1500 sub…   5.3  2008     8 auto… r        14    20 r     suv  
## 26 chevrolet    k1500 tah…   5.3  2008     8 auto… 4        14    19 r     suv  
## 27 chevrolet    k1500 tah…   6.5  1999     8 auto… 4        14    17 d     suv  
## 28 chevrolet    corvette     5.7  1999     8 auto… r        15    23 p     2sea…
## 29 chevrolet    corvette     6.2  2008     8 auto… r        15    25 p     2sea…
## 30 chevrolet    corvette     7    2008     8 manu… r        15    24 p     2sea…
## # … with 204 more rows
```


```r
#Dataframe for the Example
age = c(22,21,NA,24,19,20,23)
yrs_math_ed = c(4,5,NA,2,5,3,5)
names = c("Mary","Martha","Rosy","Kim","Kristen","Amy","Sam")
subject = c("English","Math",NA,"Sociology","Math","Music","Dance")
df = data.frame(Age = age, Years = yrs_math_ed,Name = names, Major = subject)
df
```

```
##   Age Years    Name     Major
## 1  22     4    Mary   English
## 2  21     5  Martha      Math
## 3  NA    NA    Rosy      <NA>
## 4  24     2     Kim Sociology
## 5  19     5 Kristen      Math
## 6  20     3     Amy     Music
## 7  23     5     Sam     Dance
```

```r
#Sort on a column with no NAs
df %>% arrange(df$Name)
```

```
##   Age Years    Name     Major
## 1  20     3     Amy     Music
## 2  24     2     Kim Sociology
## 3  19     5 Kristen      Math
## 4  21     5  Martha      Math
## 5  22     4    Mary   English
## 6  NA    NA    Rosy      <NA>
## 7  23     5     Sam     Dance
```

```r
#Sort on a column with NAs
df %>% arrange(df$Age)
```

```
##   Age Years    Name     Major
## 1  19     5 Kristen      Math
## 2  20     3     Amy     Music
## 3  21     5  Martha      Math
## 4  22     4    Mary   English
## 5  23     5     Sam     Dance
## 6  24     2     Kim Sociology
## 7  NA    NA    Rosy      <NA>
```

```r
# NA end up at the end of the list when sorted on column with NAs.
```

##Select v. Filter

```r
# chooses rows with year < 2000
mpg %>% filter(year < 2000)
```

```
## # A tibble: 117 × 11
##    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
##    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
##  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
##  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
##  3 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
##  4 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
##  5 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
##  6 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
##  7 audi         a4 quattro   2.8  1999     6 auto… 4        15    25 p     comp…
##  8 audi         a4 quattro   2.8  1999     6 manu… 4        17    25 p     comp…
##  9 audi         a6 quattro   2.8  1999     6 auto… 4        15    24 p     mids…
## 10 chevrolet    c1500 sub…   5.7  1999     8 auto… r        13    17 r     suv  
## # … with 107 more rows
```

```r
#chooses the columns class, city, hwy
mpg %>% select(class, cty, hwy)
```

```
## # A tibble: 234 × 3
##    class     cty   hwy
##    <chr>   <int> <int>
##  1 compact    18    29
##  2 compact    21    29
##  3 compact    20    31
##  4 compact    21    30
##  5 compact    16    26
##  6 compact    18    26
##  7 compact    18    27
##  8 compact    18    26
##  9 compact    16    25
## 10 compact    20    28
## # … with 224 more rows
```

### New Package GGally
























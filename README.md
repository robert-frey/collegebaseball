# 

# `collegebaseball`



`collegebaseball` is a package written for R focused on the acquistion and analysis of college baseball data. It
includes functions for scraping stats and schedules in the NCAA (DI, DII, DIII), as well as (in the future) NAIA and Junior College (NJCAA, NWAC, 3C2A)

## Installation

You can install collegebaseball from Github using the following code:

``` r
# using the devtools package:
if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
devtools::install_github(repo = "robert-frey/collegebaseball")
```

## **Functionality**

Below are some examples of what you can (currently) do with the collegebaseball package.

Acquire new team IDs:
``` r
library(collegebaseball)
ncaa_school_id_lookup(team_name = "Tampa", season =  2024)
```

    ## A tibble: 1 × 8
    ## team_id  year team_name conference_id conference     division season_id prev_team_id
    ## <dbl> <dbl> <chr>             <dbl> <chr>             <dbl>     <dbl>        <dbl>
    ## 1  574455  2024 Tampa               919 Sunshine State        2     16580          689


Acquire hitting, pitching, or fielding stats:
``` r
library(collegebaseball)
ncaa_stats(team_id = 574455, year = 2024, type = "fielding")
```

    ## # A tibble: 35 × 26
    ## year team_name conference     division Jersey player_name Yr    Pos      GP    GS    PO     A     E FldPct    CI    PB   SBA   CSB   IDP    TP
    ## <int> <chr>     <chr>             <dbl> <chr>  <chr>       <chr> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1  2024 Tampa     Sunshine State        2 28     Matthew Fo… So    C         6     0    13     0     0  1         0     2     0     0     0     0
    ## 2  2024 Tampa     Sunshine State        2 21     Beckett Wh… Jr    P        10     0     1     2     0  1         0     0     0     1     0     0
    ## 3  2024 Tampa     Sunshine State        2 16     Macalliste… Jr    INF      39    18    38    60     2  0.98      0     0     0     0    11     0
    ## 4  2024 Tampa     Sunshine State        2 12     Alex Canney Jr    P        19    19     3    10     2  0.867     0     0    16     4     0     0
    ## 5  2024 Tampa     Sunshine State        2 14     Santiago G… Sr    C        50    48   275    21     7  0.977     1     2    32    10     1     0
    ## 6  2024 Tampa     Sunshine State        2 30     T.J. Palma  Sr    OF       29     3    12     0     2  0.857     0     0     0     0     0     0
    ## 7  2024 Tampa     Sunshine State        2 36     Carson Caso Jr    P         2     0     0     0     0  0         0     0     0     0     0     0
    ## 8  2024 Tampa     Sunshine State        2 6      Parker Sch… Sr    C         9     0    15     0     0  1         0     1     1     0     0     0
    ## 9  2024 Tampa     Sunshine State        2 9      Anthony Nu… Jr    3B       57    56    38   114    10  0.938     0     0     0     0     8     0
    ## 10  2024 Tampa     Sunshine State        2 10     Nico Salad… Jr    SS       60    54    68   156    13  0.945     0     0     0     0    32     0
    ## # ℹ 25 more rows

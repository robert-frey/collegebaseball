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

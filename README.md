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


Acquire available college baseball games from Baseball Savant
``` r
available_savant_games()
```

     ## #         date game_pk                      away_team                      home_team            venue      league_name pbp_available statcast_available
     ## 1: 2017-02-16  514235      Arkansas-Fort Smith Lions            St. Mary's Rattlers Minute Maid Park College Baseball           Yes                Yes
     ## 2: 2017-02-17  510555            St. Mary's Rattlers          Tarleton State Texans Minute Maid Park College Baseball           Yes                Yes
     ## 3: 2017-02-17  510556 Texas A&M-Kingsville Javelinas           Chico State Wildcats Minute Maid Park College Baseball           Yes                Yes
     ## 4: 2017-02-17  510557      Arkansas-Fort Smith Lions         Central Missouri Mules Minute Maid Park College Baseball           Yes                Yes
     ## 5: 2017-02-18  510558      Arkansas-Fort Smith Lions Texas A&M-Kingsville Javelinas Minute Maid Park College Baseball           Yes                Yes
     ## ---                                                                                                                                                    
     ## 624: 2024-05-25  773295         Wichita State Shockers          East Carolina Pirates BayCare Ballpark College Baseball           Yes                Yes
     ## 625: 2024-05-25  773296          Florida Atlantic Owls              Tulane Green Wave BayCare Ballpark College Baseball           Yes                Yes
     ## 626: 2024-05-25  773297         Wichita State Shockers          East Carolina Pirates BayCare Ballpark College Baseball           Yes                Yes
     ## 627: 2024-05-26  769388               Duke Blue Devils        Florida State Seminoles     Truist Field College Baseball           Yes                Yes
     ## 628: 2024-05-26  773293         Wichita State Shockers              Tulane Green Wave BayCare Ballpark College Baseball           Yes                Yes

Then, you can acquire Statcast data from a game. We'll use the last game from the previous example:
``` r
statcast_pbp_college(773293)
```

      ## #                              play_id inning ab_number cap_index outs batter stand    batter_name pitcher p_throws   pitcher_name team_batting
      ## 1  858e6c60-3a85-4161-9be0-ca666c66fd1d      1         1        33    0 825130     L   Lane Haworth  803068        R Chandler Welch          WSU
      ## 2  32d0423a-6309-44a7-ab43-02c098a1231b      1         1        33    0 825130     L   Lane Haworth  803068        R Chandler Welch          WSU
      ## 3  8e694fef-2421-48e9-87fd-d16869cebef0      1         1        33    0 825130     L   Lane Haworth  803068        R Chandler Welch          WSU
      ## 4  b3b8dbd8-059e-49ff-bda2-6eb97c3e4371      1         1        33    0 825130     L   Lane Haworth  803068        R Chandler Welch          WSU
      ## 5  6e07bcbb-ea2e-4d81-8ff4-40da0b622902      1         1        33    0 825130     L   Lane Haworth  803068        R Chandler Welch          WSU
      ## 6  207d9087-cdcb-4cc3-ae29-4f8203d93389      1         1        33    0 825130     L   Lane Haworth  803068        R Chandler Welch          WSU
      ## 7  55764fb4-4e1a-4deb-8b9f-1c57b3b90abc      1         1        33    0 825130     L   Lane Haworth  803068        R Chandler Welch          WSU
      ## 8  6c599a57-03af-4fe9-a216-d188e0255cfa      1         1        33    0 825130     L   Lane Haworth  803068        R Chandler Welch          WSU
      ## 9  27498b2a-5469-4e7a-ad22-c8a56433c26b      1         2        24    2 825114     L Dayvin Johnson  803068        R Chandler Welch          WSU
      ## 10 a35d0674-285d-4fab-a1d9-8d3648cfeec7      1         3         0    3 700974     R  Jordan Rogers  803068        R Chandler Welch          WSU
      ## 11 8d92ee0c-9b9d-4601-bb70-88853a28e143      1         3         0    3 700974     R  Jordan Rogers  803068        R Chandler Welch          WSU

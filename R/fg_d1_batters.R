#' @title **Scrape Division 1 Batter Stats from FanGraphs**
#' @description This function allows you to scrape all statistics (basic and advanced) of Division 1 college hitters from FanGraphs.com
#' @param season (character) Season for which you want to scrape the data (Available seasons: 2021-present)
#' @param q (character) Whether you want only batters/pitchers that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimum number of plate appearaces/innings pitched, use the number desired. Defaults to "y"
#' @param pos (character) Position of players, defaults to "all". Other positions include: C (catchers), 1B (first baseman), 2B (second baseman), 3B (third baseman), SS (shortstop), OF (outfielder), np (non-pitchers)
#'
#' @rdname fg_d1_batters
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr rename any_of select everything
#'
#' @return A data frame of batter data.
#'
#' | col_name      | types          | description                                                                     |
#' | Season        | integer        | Season (YYYY).                                                                  |
#' | team_name     | character      | Team Name.                                                                      |
#' | Player        | character      | Player Name.                                                                    |
#' | G             | numeric        | Games Played.                                                                   |
#' | AB            | numeric        | At-bats.                                                                        |
#' | PA            | numeric        | Plate Appearances.                                                              |
#' | H             | numeric        | Hits.                                                                           |
#' | 1B            | numeric        | Singles.                                                                        |
#' | 2B            | numeric        | Doubles.                                                                        |
#' | 3B            | numeric        | Triples.                                                                        |
#' | HR            | numeric        | Home Runs.                                                                      |
#' | R             | numeric        | Runs.                                                                           |
#' | RBI           | numeric        | Runs Batted In.                                                                 |
#' | BB            | numeric        | Walks.                                                                          |
#' | IBB           | logical        | Intentional walks.                                                              |
#' | SO            | numeric        | Strikeouts.                                                                     |
#' | HBP           | numeric        | Hit by Pitches.                                                                 |
#' | SF            | numeric        | Sacrifice Flies.                                                                |
#' | SH            | numeric        | Sacrifice Hits.                                                                 |
#' | GDP           | numeric        | Grounded into Double Play.                                                      |
#' | SB            | numeric        | Stolen Bases.                                                                   |
#' | CS            | numeric        | Caught Stealing.                                                                |
#' | AVG           | numeric        | Batting Average.                                                                |
#' | BB_pct        | numeric        | Walk Percentage.                                                                |
#' | K_pct         | numeric        | Strikeout Percentage.                                                           |
#' | BB/K          | numeric        | Walk-to-Strikeout ratio.                                                        |
#' | OBP           | numeric        | On-Base Percentage.                                                             |
#' | SLG           | numeric        | Slugging Percentage.                                                            |
#' | OPS           | numeric        | On-Base Plus Slugging                                                           |
#' | ISO           | numeric        | Isolated Power: https://library.fangraphs.com/offense/iso/                      |
#' | Spd           | numeric        | Speed Score: https://library.fangraphs.com/offense/spd/                         |
#' | BABIP         | numeric        | Batting Average on Balls in Play: https://library.fangraphs.com/offense/babip/  |
#' | wRC           | numeric        | Weighted runs created: https://library.fangraphs.com/offense/wrc/               |
#' | wRAA          | numeric        | Weighted runs above average: https://library.fangraphs.com/offense/wraa/        |
#' | wOBA          | numeric        | Weighted on base average: https://library.fangraphs.com/offense/woba/           |
#' | wRC_plus      | numeric        | Weighted runs created plus: https://library.fangraphs.com/offense/wrc/          |
#' | wBsR          | numeric        | Weighted baserunning: https://library.fangraphs.com/offense/bsr/                |
#' | Age           | integer        | Player Age.                                                                     |
#' | MaxAge        | integer        | Player Max Age.                                                                 |
#' | team_name_abb | character      | Abbreviated Team Name.                                                          |
#' | UPID          | character      | Universal Player ID by Fangraphs.                                               |
#' | UPURL         | character      | Universal Player URL by Fangraphs.                                              |
#' | xMLBAMID      | integer        | Player MLBAMID (if applicable).                                                 |
#' | teamid        | integer        | Team ID.                                                                        |
#'
#'#' @examples
#' \dontrun{
#' if(interactive()){
#'  fg_d1_batters(season = 2026, q = 0)
#'  }
#' }
#'
fg_d1_batters <- function(season = 2026, q = "y", pos = "all") {

  message("Consider supporting FanGraphs with a membership: https://plus.fangraphs.com/product/fangraphs-membership/")

  if (is.null(season) | !(season >= 2021)) {
    cli::cli_abort("FanGraphs has Division 1 college data beginning in 2021. Please select a year 2021 or later.")
  }

  if (is.null(pos) | !(pos %in% c("all","c","1b","2b","3b","ss","of","np"))) {
    cli::cli_abort("Please select a valid position: all, c (catchers), 1b (first baseman), 2b (second baseman), 3b (third baseman), ss (shortstop), of (outfielder), np (non-pitchers)")
  }

  payload <- paste0("https://www.fangraphs.com/api/leaders/college/data?position=",pos,"&type=1&stats=bat&qual=",q,"&seasonstart=",season,"&seasonend=",season,"&team=0&players=0&conference=0&pageitems=100000&pagenum=1&sortdir=default")

  resp <- jsonlite::fromJSON(payload, flatten = T)

  df <- resp$data |>
    dplyr::rename(dplyr::any_of(c(
      "wRC_plus" = "wRC+",
      "BB_pct" = "BB%",
      "K_pct" = "K%",
      "Player" = "PlayerName",
      "team_name_abb" = "Team",
      "team_name" = "FullTeamName"
    ))) |>
    dplyr::select(Season,team_name,Player,G,PA,AB,dplyr::everything())

  return(df)

}

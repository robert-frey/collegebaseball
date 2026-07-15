#' @title **Scrape Division 1 Pitcher Stats from FanGraphs**
#' @description This function allows you to scrape all statistics (basic and advanced) of Division 1 college pitchers from FanGraphs.com
#' @param season (character) Season for which you want to scrape the data (Available seasons: 2021-present)
#' @param q (character) Whether you want only batters/pitchers that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimum number of plate appearaces/innings pitched, use the number desired. Defaults to "y"
#'
#' @return A data frame of pitcher data.
#' | col_name       | types     | description                                                                |
#' | Season         | integer   | Season (YYYY).                                                             |
#' | team_name      | character | Team Name.                                                                 |
#' | Player         | character | Player Name.                                                               |
#' | W              | numeric   | Wins.                                                                      |
#' | L              | numeric   | Losses.                                                                    |
#' | ERA            | numeric   | Earned Run Average.                                                        |
#' | G              | numeric   | Games Pitched.                                                             |
#' | GS             | numeric   | Games Started.                                                             |
#' | CG             | numeric   | Complete Games.                                                            |
#' | SHO            | numeric   | Shutouts.                                                                  |
#' | SV             | numeric   | Saves.                                                                     |
#' | IP             | numeric   | Innings Pitched.                                                           |
#' | TBF            | numeric   | Total Batters Faced.                                                       |
#' | H              | numeric   | Hits.                                                                      |
#' | R              | numeric   | Runs.                                                                      |
#' | ER             | numeric   | Earned Runs.                                                               |
#' | HR             | numeric   | Home Runs Allowed.                                                         |
#' | BB             | numeric   | Walks.                                                                     |
#' | HBP            | numeric   | Hit By Pitches.                                                            |
#' | WP             | numeric   | Wild Pitches.                                                              |
#' | BK             | numeric   | Balks.                                                                     |
#' | SO             | numeric   | Strikeouts.                                                                |
#' | K_per_nine     | numeric   | Strikeouts per nine innings.                                               |
#' | BB_per_nine    | numeric   | Walks per nine innings.                                                    |
#' | KBB            | numeric   | Strikeout-to-walk-ratio.                                                   |
#' | HR_per_nine    | numeric   | Home Runs per nine innings.                                                |
#' | K_pct          | numeric   | Strikeout percentage.                                                      |
#' | BB_pct         | numeric   | Walk percentage.                                                           |
#' | AVG            | numeric   | Batting Average Against.                                                   |
#' | WHIP           | numeric   | Walks and Hits per inning pitched.                                         |
#' | BABIP          | numeric   | Batting Average Against on balls in play.                                  |
#' | LOB_pct        | numeric   | Left on Base percentage.                                                   |
#' | FIP            | numeric   | Fielding Independent Pitching: https://library.fangraphs.com/pitching/fip/ |
#' | ERA_minus_FIP  | numeric   | Earned Run Average minus Fielding Independent Pitching.                    |
#' | K_minus_BB_pct | numeric   | Strikeout percentage minus Walk percentage.                                |
#' | Age            | integer   | Player Age.                                                                |
#' | MaxAge         | integer   | Player Max Age.                                                            |
#' | team_name_abb  | character | Abbreviated Team Name.                                                     |
#' | UPID           | character | Universal Player ID by Fangraphs.                                          |
#' | xMLBAMID       | integer   | Player MLBAMID (if applicable).                                            |
#' | UPURL          | character | Universal Player URL by Fangraphs.                                         |
#' | teamid         | integer   | Team ID.                                                                   |
#'
#' @rdname fg_d1_pitchers
#' @export
#' @importFrom cli cli_abort
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr rename any_of select everything
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fg_d1_pitchers(season = 2026, q = 0)
#'  }
#' }
fg_d1_pitchers <- function(season = 2026, q = "y") {

  message("Consider supporting FanGraphs with a membership: https://plus.fangraphs.com/product/fangraphs-membership/")

  if (is.null(season) | !(season >= 2021)) {
    cli::cli_abort("FanGraphs has Division 1 college data beginning in 2021. Please select a year 2021 or later.")
  }

  payload <- paste0("https://www.fangraphs.com/api/leaders/college/data?position=&type=1&stats=pit&qual=",q,"&seasonstart=",season,"&seasonend=",season,"&team=0&players=0&conference=0&pageitems=10000&pagenum=1&sortdir=default")

  resp <- jsonlite::fromJSON(payload, flatten = T)

  df <- resp$data |>
    dplyr::rename(dplyr::any_of(c(
      "SHO" = "ShO",
      "HR_per_pine"="HR/9",
      "K_per_nine"="K/9",
      "BB_per_nine"="BB/9",
      "KBB" = "K/BB",
      "BB_pct" = "BB%",
      "K_pct" = "K%",
      "LOB_pct" = "LOB%",
      "ERA_minus_FIP" = "E-F",
      "K_minus_BB_pct" = "K-BB%",
      "Player" = "PlayerName",
      "team_name_abb" = "Team",
      "team_name" = "FullTeamName"
    ))) |>
    dplyr::select(Season,team_name,Player,dplyr::everything())

  return(df)

}

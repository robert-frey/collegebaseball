#' Title
#' @title **Get Available College/Summer League Games in Baseball Savant**
#' @param year Year as in season (format: YYYY). Years go from 2017-present
#' @param level The level to select from. Available levels: College Baseball, MLB Draft League, Appalachian League,
#' Cape Cod Baseball League (NO STATCAST DATA AVAILABLE FOR Cape Cod)
#'
#' @return A data frame containing the game date, the game pk, away team, home team, venue, league name, play by play data available,
#' statcast data available
#'
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest
#'
#' @export
#'
#' @examples \donttest{
#'   try(available_savant_games(level = "College Baseball"))
#'   try(available_savant_games(level = "MLB Draft League"))
#'   try(available_savant_games(level = "Appalachian League"))
#'   try(available_savant_games(level = "Cape Cod Baseball League"))
#' }


available_savant_games <- function(year, level = "College Baseball") {

  endpoint <- paste0("http://statsapi.mlb.com/api/v1/schedule?language=en&sportId=22&season=",year)

  league_endpoint <- paste0("http://statsapi.mlb.com/api/v1/teams?language=en&sportId=22&season=",year)

  resp <- jsonlite::fromJSON(endpoint, flatten = TRUE)

  lg_resp <- jsonlite::fromJSON(league_endpoint, flatten = TRUE)

  games <- resp$dates |>
    tidyr::unnest('games') |>
    as.data.frame()

  lg <- lg_resp$teams |>
    dplyr::select(away_team=name,league_name=league.name)

  df <- games |>
    dplyr::select(date,game_pk=gamePk,away_team=teams.away.team.name,home_team=teams.home.team.name,venue=venue.name)

  df <- dplyr::left_join(df,lg,by="away_team")

  df <- dplyr::filter(df,league_name == level)

  return(df)
}

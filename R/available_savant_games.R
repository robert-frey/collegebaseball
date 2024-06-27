#' Title
#' @title **Get Available College/Summer League Games in Baseball Savant**
#' @param level The level to select from. Available levels: College Baseball, MLB Draft League, Appalachian League,
#' Cape Cod Baseball League (NO STATCAST DATA AVAILABLE FOR Cape Cod)
#'
#' @return A data frame containing the game date, the game pk, away team, home team, venue, league name, play by play data available,
#' statcast data available
#'
#' @importFrom dplyr filter
#' @import baseballr
#'
#' @export
#'
#' @examples \donttest{
#'   try(available_savant_games(level = "College Baseball"))
#'   try(available_savant_games(level = "MLB Draft League"))
#'   try(available_savant_games(level = "Appalachian League"))
#'   try(available_savant_games(level = "Cape Cod Baseball League"))
#' }


available_savant_games <- function(level = "College Baseball") {

  df <- baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/amateur_savant_games.rds")

  df <- df |> dplyr::filter(league_name == level)

  return(df)
}

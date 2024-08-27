#' Title
#' @title **Get NCAA Baseball Teams**
#' @param years A year or range of years to acquire data (2013 is the earliest)
#' @param divisions One of three (or all 3 options) divisions: 1,2, and/or 3
#'
#' @return A data frame containing each team's new id, the year, the team_name, the conference + conference_id, and season + season_id
#'
#' @import dplyr
#'
#' @export
#' @examples \donttest{
#'   try(ncaa_teams(years = c(2013:2024), divisions = c(1,2,3)))
#'   try(ncaa_teams(years = 2024, divisions = 1))
#' }
ncaa_teams <- function(years = 2024, divisions = c(1,2,3)) {

  df <- baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/ncaa_team_lookup.rds")

  df <- df |> dplyr::filter(year %in% years, division %in% divisions)

  df

}

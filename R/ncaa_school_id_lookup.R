#'
#' @title **School ID Lookup for NCAA (I, II, III) Schools**
#' @param team_name The name of the team you'd like to lookup (Ex: Tennessee)
#' @param season The season you'd like to look up in (YYYY) format. Available years 2010-2024
#'
#' @return A tibble containing the teams that match the team name and these 8 columns:
#' team_id, year, team_name, conference_id, conference, division, season_id, prev_team_id (the previous team id before the site change)
#'
#' @import baseballr
#' @import dplyr
#' @export
#'
#'  @examples \donttest{
#'   try(ncaa_school_id_lookup("Tamp"))
#' }
#'
ncaa_school_id_lookup <- function(team_name = NULL, season = NULL) {


  if (is.null(team_name)) {
    cli::cli_abort("Enter valid ncaa school")
  }

  if (is.null(season) || season < 2013) {
    x <- cli::cli_abort("Enter a year between 2013-2024")
  }


  teams <- tibble::tibble(baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/ncaa_team_lookup.rds"))
  x <- teams |>
    dplyr::filter(grepl({{team_name}}, .data$team_name),
                  year == season)
  return(x)
}

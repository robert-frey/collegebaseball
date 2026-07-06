#' @title **Get Play-By-Play Data for NCAA Baseball Games**
#' @param game_id The id for the game's play-by-play data. This can be
#'  found using the ncaa_schedule function
#' @import rvest
#' @import dplyr
#' @return A data frame with play-by-play data for an individual game.
#' @export
#'
#' @examples \donttest{
#'   try(ncaa_pbp(game_id = 6582141))
#' }
ncaa_pbp <- function(game_id) {

  message("Scraping college baseball play-by-play data. Please be patient...")


  url <- paste0("https://stats.ncaa.org/contests/", game_id, "/play_by_play")

  pbp_payload <- rvest::read_html_live(url)

  table_list <- (pbp_payload |>
                   rvest::html_elements("table"))[-c(1,2,3)] |>
    rvest::html_table()

  date_slug <- pbp_payload |>
    rvest::html_elements("tr:nth-child(4) .grey_text") |>
    rvest::html_text(trim=T)

  loc_slug <- pbp_payload |>
    rvest::html_elements("tr:nth-child(5) .grey_text") |>
    rvest::html_text(trim=T)

  att <- pbp_payload |>
    rvest::html_elements("tr:nth-child(6) .grey_text") |>
    rvest::html_text(trim=T)

  add_inning_column <- function(df, inning) {
    df$inning <- inning
    return(df)
  }

  mapped_table <- lapply(seq_along(table_list), function(i) add_inning_column(table_list[[i]], i))

  mapped_table <- mapped_table |> dplyr::bind_rows()

  mapped_table <- mapped_table |>
    dplyr::mutate(away_team = names(mapped_table)[1],
                  home_team = names(mapped_table)[3]) |>
    dplyr::rename(away_des = 1,
                  home_des = 3) |>
    dplyr::mutate(game_id = as.numeric(gsub("\\D", "", url)),
                  date = substr(date_slug, start = 1, stop = 10),
                  location = loc_slug,
                  attendance = as.numeric(gsub(".*Attendance:","",att)),
                  bat_team = ifelse(away_des == "",home_team,away_team),
                  pitch_team = ifelse(away_des == "",away_team,home_team),
                  description = ifelse(away_des == "",home_des,away_des),
                  inning_top_bot = ifelse(away_des == "","bot","top"),
                  away_score = gsub("-.*","",Score),
                  home_score = gsub(".*-","",Score)) |>
    dplyr::filter(!grepl("LOB:",description))

  mapped_table <- mapped_table |>
    dplyr::select(
      game_id,
      date,
      inning,
      inning_top_bot,
      description,
      away_team,
      away_score,
      home_team,
      home_score,
      bat_team,
      pitch_team,
      location,
      attendance
    )

  return(mapped_table)
}

#' Title
#' @title **Get NCAA Baseball Game Logs**
#' @param player_id A player's unique id. Can be found using the
#' ncaa_roster function.
#' @param year The year of interest.
#' @param type The kind of statistics you want to return. Current options
#' are 'batting' or 'pitching'.
#' @param span The span of time; can either be 'game' for game logs in a season, or 'career' which
#' returns seasonal stats for a player's career.
#'
#' @return A data frame containing player and school information
#' as well as game by game statistics
#' @importFrom tidyr everything, replace_na
#' @import dplyr
#' @import rvest
#'
#' @export
#' @examples \donttest{
#'   try(ncaa_game_logs(player_id = 8279872, year = 2024, type = "pitching", span = "game"))
#'   try(ncaa_game_logs(player_id = 8279872, year = 2024, type = "pitching", span = "career"))
#'   try(ncaa_game_logs(player_id = 8897479, year = 2025, type = "batting", span = "game"))
#'   try(ncaa_game_logs(player_id = 8897479, year = 2025, type = "batting", span = "career"))
#' }
ncaa_game_logs <- function(player_id,year,type = "batting", span = 'game') {

  message("Scraping college baseball player logs. Please be patient...")

  if (is.null(player_id)) {
    cli::cli_abort("Enter valid player_id")
  }
  if (is.null(type) | !(type %in% c("batting","pitching", "fielding"))) {
    cli::cli_abort("Enter valid type: 'batting', 'pitching'")
  }
  if (is.null(span) | !(span %in% c("game","career"))) {
    cli::cli_abort("Enter valid span: 'game', 'career'")
  }


  season_ids <-  baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/ncaa_season_ids.rds") |>
    dplyr::filter(.data$season == year)
  year_id <- season_ids |>
    dplyr::filter(.data$season == year) |>
    dplyr::select("id")
  batting_id <- season_ids |>
    dplyr::filter(.data$season == year) |>
    dplyr::pull("batting_id")
  pitching_id <- season_ids |>
    dplyr::filter(.data$season == year) |>
    dplyr::pull("pitching_id")
  fielding_id <- season_ids |>
    dplyr::filter(.data$season == year) |>
    dplyr::mutate(fielding_id = pitching_id + 1) |>
    dplyr::pull("fielding_id")


      if (type == "batting" & span == "game") {

        batting_url <- paste0("http://stats.ncaa.org/players/",player_id,"&year_stat_category_id=",batting_id)

        batting_payload <- rvest::read_html_live(batting_url)

        Sys.sleep(1)

        player_name <- (batting_payload |>
                           rvest::html_elements("#player_id") |>
                          rvest::html_elements(xpath = "//option[@selected]") |>
                          rvest::html_text())[1]
        player_name <- gsub("#.*","",player_name)
        last <- trimws(gsub(",.*","",player_name))
        first <- trimws(gsub(".*,","",player_name))
        player_name <- paste(first, last)

        payload_df <- ((batting_payload |>
                          rvest::html_elements("table"))[[5]] |>
                         rvest::html_table() |>
                         dplyr::tibble())

        payload_df <- dplyr::filter(payload_df, nchar(Date)>=10)

        payload_df <- payload_df |> dplyr::mutate(dplyr::across(4:ncol(payload_df), ~ gsub("/","",.)))

        if ('OPP DP' %in% colnames(payload_df) == TRUE) {
          payload_df <- payload_df |>
            dplyr::rename("DP" = "OPP DP")
        }

        batting_cols <- c("Date", "Opponent", "Result","GP",
                          "R", "AB", "H", "2B", "3B", "TB", "HR", "RBI",
                          "BB", "HBP", "SF", "SH", "K", "DP", "CS", "Picked",
                          "SB", "IBB", "RBI2out")

        payload_df <- payload_df |>
          dplyr::select(batting_cols)

        cols_to_num <- c("GP","R", "AB", "H", "2B", "3B", "TB", "HR", "RBI",
                         "BB", "HBP", "SF", "SH", "K", "DP", "CS", "Picked",
                         "SB", "IBB", "RBI2out")
        suppressWarnings(
          payload_df <- payload_df |>
            dplyr::mutate_at(cols_to_num, as.numeric)
        )
        payload_df <- payload_df |>
          dplyr::filter(GP == 1) |>
          dplyr::mutate(
            player_id = player_id,
            player_name = player_name,
            Year = year) |>
          dplyr::select("player_id", "player_name", tidyr::everything())

      } else if (type == "pitching" & span == "game") {

        pitching_url <- paste0("http://stats.ncaa.org/players/",player_id,"&year_stat_category_id=",pitching_id)

        pitching_payload <- rvest::read_html_live(pitching_url)

        Sys.sleep(1)

        player_name <- (pitching_payload |>
                           rvest::html_elements("#player_id") |>
                          rvest::html_elements(xpath = "//option[@selected]") |>
                          rvest::html_text())[1]

        player_name <- gsub("#.*","",player_name)
        last <- trimws(gsub(",.*","",player_name))
        first <- trimws(gsub(".*,","",player_name))
        player_name <- paste(first, last)

        payload_df <- ((pitching_payload |>
                          rvest::html_elements("table"))[[5]] |>
                         rvest::html_table() |>
                         dplyr::tibble())

        pitch_cols <- c("Date", "Opponent", "Result",
                        "App", "GS", "IP", "CG", "H", "R", "ER", "BB",
                        "SO", "SHO", "BF", "P-OAB", "2B-A", "3B-A", "Bk", "HR-A",
                        "WP", "HB", "IBB","Inh Run","Inh Run Score",
                        "SHA","SFA","Pitches","GO","FO","W","L","SV","OrdAppeared","KL","pickoffs")

        payload_df <- payload_df |>
          dplyr::select(pitch_cols)

        payload_df <- dplyr::filter(payload_df, nchar(Date)==10, App == 1)

        payload_df <- payload_df |> dplyr::mutate(dplyr::across(4:ncol(payload_df), ~ gsub("/","",.)))

        cols_to_num <- c(
          "App", "GS", "IP", "CG", "H", "R", "ER", "BB",
          "SO", "SHO", "BF", "P-OAB", "2B-A", "3B-A", "Bk", "HR-A",
          "WP", "HB", "IBB","Inh Run","Inh Run Score",
          "SHA","SFA","Pitches","GO","FO","W","L","SV","OrdAppeared","KL","pickoffs")


        suppressWarnings(
          payload_df <- payload_df |>
            dplyr::mutate_at(cols_to_num, as.numeric)
        )

        payload_df <- payload_df |>
          dplyr::mutate(
            player_id = player_id,
            player_name = player_name,
            Year = year) |>
          dplyr::select("player_id", "player_name", tidyr::everything())

      } else if (type == "batting" & span == "career") {

        batting_url <- paste0("http://stats.ncaa.org/players/",player_id,"&year_stat_category_id=",batting_id)

        batting_payload <- rvest::read_html_live(batting_url)

        Sys.sleep(1)

          payload_df <- ((batting_payload |>
                            rvest::html_elements("table"))[[2]] |>
                           rvest::html_table() |>
                           dplyr::tibble())

          if ('OPP DP' %in% colnames(payload_df) == TRUE) {

            payload_df <- payload_df |>
              dplyr::rename("DP" = "OPP DP")
          }

          payload_df <- payload_df |>
            dplyr::select(
              "Year",
              "Team",
              "G",
              "BA",
              "OBP"="OBPct",
              "SLG"="SlgPct",
              "R",
              "AB",
              "H",
              "2B",
              "3B",
              "TB",
              "HR",
              "RBI",
              "BB",
              "HBP",
              "SF",
              "SH",
              "K",
              "DP",
              "CS",
              "Picked",
              "SB",
              "IBB",
              "RBI2out")

          payload_df <- payload_df |>
            dplyr::mutate(
              AB = gsub(",","",AB),
              player_id = player_id,
              player_name = player_name) |>
            dplyr::select("Year", "player_id", "player_name", tidyr::everything())

          suppressWarnings(
            payload_df <- payload_df |>
              dplyr::filter(Year != "") |>
              dplyr::mutate_at(5:ncol(payload_df), as.numeric)
          )


        } else {

          pitching_url <- paste0("http://stats.ncaa.org/players/",player_id,"&year_stat_category_id=",pitching_id)

          pitching_payload <- rvest::read_html_live(pitching_url)

          Sys.sleep(1)

          payload_df <- ((pitching_payload |>
                            rvest::html_elements('table'))[[2]] |>
                           rvest::html_table() |>
                           dplyr::tibble())

          player_name <- (pitching_payload |>
                            rvest::html_elements("#player_id") |>
                            rvest::html_elements(xpath = "//option[@selected]") |>
                            rvest::html_text())[1]

          player_name <- gsub("#.*","",player_name)
          last <- trimws(gsub(",.*","",player_name))
          first <- trimws(gsub(".*,","",player_name))
          player_name <- paste(first, last)


          payload_df <- payload_df |>
            dplyr::select(
              "Year",
              "Team",
              "App",
              "GS",
              "ERA",
              "IP",
              "CG",
              "H",
              "R",
              "ER",
              "BB",
              "SO",
              "SHO",
              "BF",
              "P-OAB",
              "2B-A",
              "3B-A",
              "Bk",
              "HR-A",
              "WP",
              "HB",
              "IBB",
              "Inh Run",
              "Inh Run Score",
              "SHA",
              "SFA",
              "Pitches",
              "GO",
              "FO",
              "W",
              "L",
              "SV",
              "KL",
              tidyr::everything())


          payload_df <- payload_df |>
            dplyr::mutate(
              Pitches = gsub(",","",Pitches),
              player_id = player_id,
              player_name = player_name) |>
            dplyr::select("Year", "player_id", "player_name", tidyr::everything())

          suppressWarnings(
          payload_df <- payload_df |>
            dplyr::filter(Year != "") |>
                dplyr::mutate_at(5:ncol(payload_df), as.numeric)
            )

        }

      payload_df <- payload_df |>
        dplyr::mutate_if(is.numeric, ~tidyr::replace_na(.,0))

  return(payload_df)

}

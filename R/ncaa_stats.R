#' Title
#' @title **Scrape NCAA baseball Team Player Stats (Division I, II, and III)**
#' @description This function allows the user to obtain batting, pitching, or fielding statistics for any school affiliated with the NCAA at the division I, II, or III levels. The function acquires data from the NCAA's website (stats.ncaa.org) and returns a tibble.
#' @param team_id The numerical ID that the NCAA website uses to identify a team
#' @param year The season for which data should be returned, in the form of "YYYY". Years currently available: 2013-2024.
#' @param type A string indicating whether to return "batting" or "pitching" statistics
#' @param situation Select which type of stat you'd like to pull:
#' @return A tibble with variables dependent upon whether you select batting, pitching, and fielding
#' For batting:
#' For pitching:
#'
#' @import dplyr
#' @import rvest
#' @import tidyr
#' @import baseballr
#'
#'
#' @export
#'
#' @examples
#' \donttest{
#'   try(ncaa_team_player_stats(team_id = 574224, year = 2024, type = "batting"))
#' }
ncaa_stats <- function(team_id, year = 2024, type = 'batting', situation = "all") {

  if (is.null(team_id)) {
    cli::cli_abort("Enter valid team_id")
  }
  if (is.null(year)) {
    cli::cli_abort("Enter valid year as a number (YYYY)")
  }
  if (is.null(type) || !(type %in% c("batting","pitching", "fielding"))) {
    cli::cli_abort("Enter valid type: 'batting', 'pitching','fielding'")
  }

  if (year < 2013) {
    stop('you must provide a year that is greater than or equal to 2013')
  }

  if (type == "batting") {

  type_id <- baseballr::load_ncaa_baseball_season_ids() |>
    dplyr::filter(.data$season == year) |>
    dplyr::select("batting_id")
  url <- paste0("http://stats.ncaa.org/teams/",team_id,"/season_to_date_stats?year_stat_category_id=",type_id)

  payload <- url |>
    xml2::read_html()

  data_read <- payload

  data <- (data_read |>
             rvest::html_elements("table"))[[1]] |>
    rvest::html_table()

  df <- tibble::tibble(data)
  df$year <- year
  df$team_id <- team_id

  df <- df |> dplyr::mutate(Player = ifelse(Player == "",NA_character_,Player)) |>
    tidyr::fill(Player)

  if (!"RBI2out" %in% names(df)) {
    df$RBI2out <- NA
  }

  if('OPP DP' %in% names(df) == TRUE) {
    df <- df |>
      dplyr::rename(DP = "OPP DP")
  }

  if (!"B/T" %in% names(df)) {
    df$`B/T` <- NA
  }

  if (!"Ht" %in% names(df)) {
    df$Ht <- NA
  }

  if (!"IBB" %in% names(df)) {
    df$`IBB` <- NA
  }

  if (situation == "all") {
    suppressWarnings({
    df <- dplyr::mutate(df,`#` = as.numeric(`#`))
    df <- dplyr::filter(df,!is.na(`#`))
    }
    )
  } else {
    suppressWarnings({
    df <- dplyr::filter(df, `#` == situation)
    df <- dplyr::mutate(df,situation = `#`)
    }
    )
  }

  df <- df |>
    dplyr::left_join(baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/ncaa_team_lookup.rds"), by = c("team_id" = "team_id", "year" = "year"))
  df <- df |>
    dplyr::select("year", "team_name", "conference", "division", tidyr::everything())

  df <- df |> dplyr::select(
    "year","team_name","conference","division","Jersey"=`#`,"Player",
    "Yr","Pos",
    "GP","GS","BA","OBPct","SlgPct","R","AB",
    "H","2B","3B","TB","HR","RBI","BB","HBP","SF","SH",
    "K","DP","CS","Picked","SB","IBB","RBI2out","Ht","B/T","team_id","conference_id")

  character_cols <- c("year", "team_name", "conference", "Jersey", "Player",
                      "Yr", "Pos")

  numeric_cols <- c("division", "GP", "GS", "BA", "OBPct", "SlgPct", "R", "AB",
                    "H", "2B", "3B", "TB", "HR", "RBI", "BB", "HBP", "SF", "SH",
                    "K", "DP", "CS", "Picked", "SB","IBB","RBI2out", "team_id", "conference_id")

  suppressWarnings(
    df <- df |>
      dplyr::mutate_at(dplyr::vars(character_cols), function(x){as.character(x)})
  )
  suppressWarnings(
    df <- df |>
      dplyr::mutate_at(dplyr::vars(numeric_cols), function(x){as.numeric(as.character(x))})
  )

  } else if (type == "pitching") {

    type_id <- baseballr::load_ncaa_baseball_season_ids() |>
      dplyr::filter(.data$season == year) |>
      dplyr::select("pitching_id")
    url <- paste0("http://stats.ncaa.org/teams/",team_id,"/season_to_date_stats?year_stat_category_id=",type_id)

    payload <- url |>
      xml2::read_html()

    data_read <- payload

    data <- (data_read |>
               rvest::html_elements("table"))[[1]] |>
      rvest::html_table()

    df <- tibble::tibble(data)
    df$year <- year
    df$team_id <- team_id

    if (!"B/T" %in% names(df)) {
      df$`B/T` <- NA
    }

    if (!"Ht" %in% names(df)) {
      df$Ht <- NA
    }

    if (situation == "all") {
      suppressWarnings({
        df <- dplyr::mutate(df,`#` = as.numeric(`#`))
        df <- dplyr::filter(df,!is.na(`#`))
      }
      )
    } else {
      suppressWarnings({
        df <- dplyr::filter(df, `#` == situation)
        df <- dplyr::mutate(df,situation = `#`)
      }
      )
    }

    df <- df |>
      dplyr::left_join(baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/ncaa_team_lookup.rds"), by = c("team_id" = "team_id", "year" = "year"))
    df <- df |>
      dplyr::select("year", "team_name", "conference", "division", tidyr::everything())

    df <- df |> dplyr::select(
      "year","team_name","conference","division","Jersey"="#","Player",
      "Yr","Pos","App","GS","ERA","IP","H","R",
      "ER","BB","SO","SHO","BF","P-OAB",
      "2B-A","3B-A","Bk","HR-A","WP","HB",
      "IBB","Inh Run","Inh Run Score",
      "SHA","SFA","Pitches","GO","FO","W","L",
      "SV","KL","Ht","B/T","team_id","conference_id")

    character_cols <- c("year", "team_name", "conference", "Jersey", "Player",
                        "Yr", "Pos","Ht","B/T")

    numeric_cols <- c("division",  "App", "GS", "ERA", "IP", "H", "R", "ER",
                      "BB", "SO", "SHO", "BF", "P-OAB", "2B-A", "3B-A", "Bk", "HR-A",
                      "WP", "HB", "IBB", "Inh Run", "Inh Run Score", "SHA", "SFA",
                      "Pitches", "GO", "FO", "W", "L", "SV", "KL", "team_id", "conference_id")

    suppressWarnings(
      df <- df |>
        dplyr::mutate_at(dplyr::vars(character_cols), function(x){as.character(x)})
    )
    suppressWarnings(
      df <- df |>
        dplyr::mutate_at(dplyr::vars(numeric_cols), function(x){as.numeric(as.character(x))})
    )

  } else {

    type_id <- baseballr::load_ncaa_baseball_season_ids() |>
      dplyr::filter(.data$season == year) |>
      dplyr::mutate(fielding_id = pitching_id + 1) |>
      dplyr::select("fielding_id")
    url <- paste0("http://stats.ncaa.org/teams/",team_id,"/season_to_date_stats?year_stat_category_id=",type_id)

    payload <- url |>
      xml2::read_html()

    data_read <- payload

    data <- (data_read |>
               rvest::html_elements("table"))[[1]] |>
      rvest::html_table()

    df <- tibble::tibble(data)
    df$year <- year
    df$team_id <- team_id

    if (!"B/T" %in% names(df)) {
      df$`B/T` <- NA
    }

    if (!"Ht" %in% names(df)) {
      df$Ht <- NA
    }

    suppressWarnings({
      df <- dplyr::mutate(df,`#` = as.numeric(`#`))
      df <- dplyr::filter(df,!is.na(`#`))
    }
    )

    df <- df |>
      dplyr::left_join(baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/ncaa_team_lookup.rds"), by = c("team_id" = "team_id", "year" = "year"))
    df <- df |>
      dplyr::select("year", "team_name", "conference", "division", tidyr::everything())

    df <- df |> dplyr::select(
      "year","team_name","conference","division","Jersey"="#","Player",
      "Yr","Pos","GP","GS","PO","A","E","FldPct",
      "CI","PB","SBA","CSB","IDP","TP",
      "Ht","B/T","team_id","conference_id")

    character_cols <- c("year", "team_name", "conference", "Jersey", "Player",
                        "Yr", "Pos","Ht","B/T")

    numeric_cols <- c("division",  "GP", "GS", "PO", "A", "E",
                      "FldPct", "CI", "PB", "SBA", "CSB", "IDP", "TP",
                      "team_id","conference_id")

    suppressWarnings(
      df <- df |>
        dplyr::mutate_at(dplyr::vars(character_cols), function(x){as.character(x)})
    )
    suppressWarnings(
      df <- df |>
        dplyr::mutate_at(dplyr::vars(numeric_cols), function(x){as.numeric(as.character(x))})
    )
  }

  player_url <- data_read |>
    rvest::html_elements('#stat_grid a') |>
    rvest::html_attr('href') |>
    as.data.frame() |>
    dplyr::rename("player_url" = 1) |>
    dplyr::mutate(player_url = paste0('http://stats.ncaa.org', .data$player_url))

  player_names_join <- data_read |>
    rvest::html_elements('#stat_grid a') |>
    rvest::html_text() |>
    as.data.frame() |>
    dplyr::rename("player_names_join" = 1)

  player_id <-
    gsub("http://stats.ncaa.org/players/","",gsub("\\?.*","",player_url$player_url)) |>
    as.data.frame() |>
    dplyr::rename("player_id" = 1)

  player_url_comb <- dplyr::bind_cols(player_names_join, player_id, player_url)

  df <- df |>
    dplyr::left_join(player_url_comb, by = c('Player' = 'player_names_join'))
  df <- df |>
    dplyr::rename("player_name" = "Player")

  df <- df |>
    dplyr::mutate_at(dplyr::vars(player_url), as.character) |>
    dplyr::mutate_at(c("conference_id", "player_id", "year"), as.integer) |>
    dplyr::mutate_if(is.numeric, ~tidyr::replace_na(.,0))

  return(df)


}

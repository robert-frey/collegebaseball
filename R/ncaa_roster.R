ncaa_roster <- function(team_id = NULL, year, ...){
  if (is.null(team_id)) {
    cli::cli_abort("Enter valid team_id")
  }
  if (is.null(year)) {
    cli::cli_abort("Enter valid year between 2010-2024 as a number (YYYY)")
  }

  ncaa_teams_lookup <- baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/ncaa_team_lookup.rds")

  school_info <- ncaa_teams_lookup |>
    dplyr::filter(.data$team_id == {{team_id}} & .data$year == {{year}}) |>
    dplyr::distinct()

  url <- paste0("https://stats.ncaa.org/teams/", team_id, "/roster")

      payload <- url |>
        xml2::read_html()

      data_read <- payload

      payload1 <- (data_read |>
                     rvest::html_elements("table"))[[1]] |>
        rvest::html_elements("tr")

      table <- (data_read |>
                  rvest::html_elements("table"))[[1]] |>
                  rvest::html_table(trim=T)

      roster <- table

      extractor <- function(x){
        data.frame(url_slug = ifelse(
          is.null(
            (x |>
               rvest::html_elements("td"))[4] |>
              rvest::html_element("a")),
          NA_character_,
          (x |>
             rvest::html_elements("td"))[4] |>
            rvest::html_element("a")  |>
            rvest::html_attr("href")
        ))
      }
      url_slug <- lapply(payload1, extractor) |>
        dplyr::bind_rows() |> tail(-1)

      roster <- table |>
        dplyr::bind_cols(url_slug)

      if (!"Height" %in% names(roster)) {
        roster$Height <- NA
      }

      if (!"Hometown" %in% names(roster)) {
        roster$Hometown <- NA
      }

      if (!"High School" %in% names(roster)) {
        roster$`High School` <- NA
      }

      if (!"Bats" %in% names(roster)) {
        roster$Bats <- NA
      }

      if (!"Throws" %in% names(roster)) {
        roster$Throws <- NA
      }

       roster <- roster |>
         dplyr::mutate(
           season =  {{year}},
           player_id = gsub(".*\\/players\\/", "", .data$url_slug),
           player_url = ifelse(is.na(.data$player_id), NA, paste0("https://stats.ncaa.org", .data$url_slug))) |>
        dplyr::select(
          "player_name" = "Name",
          "class" = "Class",
          "position" = "Position",
          "games_played" = "GP",
          "games_started" = "GS",
          "number" = "#",
          "height" = "Height",
          "bats" = "Bats",
          "throws" = "Throws",
          "hometown" = "Hometown",
          "high_school" = "High School",
          "player_id",
          "player_url")
      school_info <- school_info |>
        dplyr::slice(rep(1:n(), each = nrow(roster)))
      #
      roster <- roster |>
        dplyr::bind_cols(school_info)

  return(roster)
}

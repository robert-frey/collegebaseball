#' Title
#' @title **Get Schedule and Results for NCAA Baseball Teams**
#' @param team_id The team's new unique NCAA id
#' @param year The season i.e. use 2024 for the 2023-2024 season, etc.
#'
#'
#' @return A data frame that returns a teams schedule info along with both the home and away team ids, conference, and division, as well as
#' the result of the game, innings, game_info_url, and a contest_id
#' @export
#'
#' @examples
#'
#'  try(ncaa_schedule_info(team_id = 574226, year = 2024))
#'
ncaa_schedule <- function(team_id = NULL, year = NULL){

  if (is.null(team_id)) {
    cli::cli_abort("Enter valid team_id")
  }
  if (is.null(year)) {
    cli::cli_abort("Enter valid year as a number (YYYY)")
  }
  season_ids <- baseballr::load_ncaa_baseball_season_ids()
  year2 <- year
  id <- subset(season_ids, season_ids$season == year, select = id)
  ncaa_baseball_teams <- baseballr:::rds_from_url("https://raw.githubusercontent.com/robert-frey/college-baseball/main/ncaa_team_lookup.rds")
  school_info <- ncaa_baseball_teams |>
    dplyr::filter(.data$team_id == {{team_id}}, as.integer(.data$year) == as.integer(year2)) |>
    dplyr::distinct() |>
    dplyr::first()

  url <- paste0("https://stats.ncaa.org/teams/",team_id)

  payload <- url |> xml2::read_html()

  data_read <- payload

  sched <- (data_read |>
             rvest::html_elements("table"))[[1]] |>
    rvest::html_table()

  sched <- dplyr::select(sched,Date=1,Opponent=2,Result=3)

  sched <- dplyr::filter(sched,Result != "Result", Result != "Schedule/Results",
                         Date != "",
                         !(Result %in% c("Canceled","Ppd")))

  game_extractor <- function(x){
    data.frame(slug =
      (x |>
         rvest::html_nodes("td+ td .skipMask")) |>
        rvest::html_attr("href")
    )
  }
  slugs <- game_extractor(data_read)


  sched <- dplyr::bind_cols(sched, slugs)

  sched <- sched |>
    dplyr::mutate(
      Date = substr(Date,1,10),
      game_info_url = ifelse(!is.na(slug), paste0("https://stats.ncaa.org", slug), NA_character_))

  suppressWarnings(
    sched <- sched |>
      dplyr::mutate(
        game_result = stringr::str_extract(.data$Result, "\\w+"),
        Result = stringr::str_remove(.data$Result, "\\w+"),
        Innings = stringr::str_extract(.data$Result, "\\(\\d+\\)"),
        Innings = stringr::str_extract(.data$Innings, "\\d+"),
        Score = stringr::str_trim(stringr::str_remove(.data$Result, "\\(\\d+\\)"))
      ) |>
      dplyr::select(-"Result")
  )

  sched <- school_info |>
    dplyr::bind_cols(sched)

  sched <- sched |>
    dplyr::mutate(
      away = stringr::str_extract(string = .data$Opponent,"^@"),
      Opponent = stringr::str_trim(stringr::str_remove(.data$Opponent,"^@")),
      game_number = dplyr::row_number()
    )

  sched <- sched |>
    tidyr::separate("Opponent", sep = "@", into = c("OpponentName","NeutralSite"), fill = "right")

  sched <- sched |>
    dplyr::group_by(.data$game_number) |>
    dplyr::mutate(
      home_team = dplyr::case_when(
        !is.na(.data$NeutralSite) ~ sort(c(.data$team_name, .data$OpponentName))[[1]],
        .data$away == "@" ~ .data$OpponentName,
        TRUE ~ .data$team_name),
      away_team = dplyr::case_when(
        !is.na(.data$NeutralSite) ~ sort(c(.data$team_name, .data$OpponentName))[[2]],
        .data$away == "@" ~ .data$team_name,
        TRUE ~ .data$OpponentName),
      OpponentName = stringr::str_trim(.data$OpponentName),
      NeutralSite = stringr::str_trim(.data$NeutralSite)) |>
    dplyr::ungroup() |>
    dplyr::select(-"away") |>
    dplyr::left_join(ncaa_baseball_teams |>
                       dplyr::filter(as.integer(.data$year) == as.integer(year2)) |>
                       dplyr::distinct() |>
                       dplyr::select(
                         "OpponentName" = "team_name",
                         "OpponentTeamId" = "team_id",
                         "OpponentConference" = "conference",
                         "OpponentConferenceId" = "conference_id",
                         "OpponentDivision" = "division"), by = c("OpponentName" = "OpponentName")) |>
    janitor::clean_names()
  suppressWarnings(
    sched <- sched |>
      tidyr::separate("score", sep = "-", into = c("team_score", "opponent_score")) |>
      dplyr::mutate(
        team_score = as.integer(stringr::str_trim(.data$team_score)),
        opponent_score = as.integer(stringr::str_trim(.data$opponent_score))
      )
  )
  sched <- sched |>
    dplyr::mutate(
      team_id = team_id,
      home_team_id = ifelse(.data$home_team == .data$team_name, .data$team_id, .data$opponent_team_id),
      home_team_score = ifelse(.data$home_team == .data$team_name, .data$team_score, .data$opponent_score),
      home_team_conference = ifelse(.data$home_team == .data$team_name, .data$conference, .data$opponent_conference),
      home_team_conference_id = ifelse(.data$home_team == .data$team_name, .data$conference_id, .data$opponent_conference_id),
      home_team_division = ifelse(.data$home_team == .data$team_name, .data$division, .data$opponent_division),

      away_team_id = ifelse(.data$home_team == .data$team_name, .data$opponent_team_id, .data$team_id),
      away_team_score = ifelse(.data$home_team == .data$team_name, .data$opponent_score, .data$team_score),
      away_team_conference = ifelse(.data$home_team == .data$team_name, .data$opponent_conference, .data$conference),
      away_team_conference_id = ifelse(.data$home_team == .data$team_name, .data$opponent_conference_id, .data$conference_id),
      away_team_division = ifelse(.data$home_team == .data$team_name, .data$opponent_division, .data$division),
      contest_id = as.integer(stringr::str_extract(.data$game_info_url, "\\d+"))) |>
    dplyr::select(
      "year",
      "season_id",
      "team_id",
      "date",
      "home_team",
      "home_team_id",
      "home_team_score",
      "home_team_conference",
      "home_team_conference_id",
      "home_team_division",
      "away_team",
      "away_team_id",
      "away_team_score",
      "away_team_conference",
      "away_team_conference_id",
      "away_team_division",
      "neutral_site",
      "game_result",
      "innings",
      "slug",
      "game_info_url",
      "contest_id"
    )


  return(sched)
}


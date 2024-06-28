#' @title **Acquire Statcast or raw play-by-play data from collegiate/summer league games**
#' @param game_pk The game pk acquired from running the available_savant_games function
#'
#' @return A data frame containing the following columns:
#'
#' colname  type
#' play_id: character
#' inning: integer
#' ab_number: integer
#' cap_index: integer
#' outs: integer
#' batter: integer
#' stand: character
#' batter_name: character
#' pitcher: integer
#' p_throws: character
#' pitcher_name: character
#' team_batting: character
#' team_fielding: character
#' team_batting_id: integer
#' team_fielding_id: integer
#' result: character
#' des: character
#' events: character
#' strikes: integer
#' balls: integer
#' pre_strikes: integer
#' pre_balls: integer
#' call: character
#' call_name: character
#' description: character
#' result_code: character
#' pitch_call: character
#' is_strike_swinging: logical
#' balls_and_strikes: character
#' sz_top: numeric
#' sz_bot: numeric
#' pfxZWithGravity: logical
#' pfxZWithGravityNice: logical
#' pfxZDirection: character
#' pfxXWithGravity: logical
#' pfxXNoAbs: numeric
#' pfxXDirection: character
#' breakX: logical
#' breakZ: logical
#' inducedBreakZ: logical
#' is_bip_out: character
#' pitch_number: integer
#' player_total_pitches: integer
#' player_total_pitches_pitch_types: integer
#' game_total_pitches: integer
#' rowId: character
#' game_pk: character
#' player_name: character
#' hc_x: numeric
#' hc_x_ft: numeric
#' hc_y: numeric
#' hc_y_ft: numeric
#' runnerOn1B: logical
#' runnerOn2B: logical
#' runnerOn3B: logical
#'
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows arranage
#' @importFrom glue glue
#'
#' @export
#' @examples \donttest{
#'   try(statcast_pbp_college(769400))
#' }
statcast_pbp_college <- function(game_pk) {

df <- jsonlite::fromJSON(glue::glue("https://baseballsavant.mlb.com/gf?game_pk=",game_pk), flatten = T)

df <- dplyr::bind_rows(df[['team_home']],df[['team_away']]) |>
  dplyr::arrange(inning,ab_number,pitch_number)

return(df)

}
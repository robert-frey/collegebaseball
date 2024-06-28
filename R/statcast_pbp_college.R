#' @title **Acquire Statcast or raw play-by-play data from collegiate/summer league games**
#' @param game_pk The game pk acquired from running the available_savant_games function
#'
#' @return A data frame containing the following columns:
#'
#' colname  type
#' play_id: character
#' inning: numeric
#' ab_number: numeric
#' cap_index: numeric
#' outs: numeric
#' batter: numeric
#' stand: character
#' batter_name: character
#' pitcher: numeric
#' p_throws: character
#' pitcher_name: character
#' team_batting: character
#' team_fielding: character
#' team_batting_id: numeric
#' team_fielding_id: numeric
#' result: character
#' des: character
#' events: character
#' strikes: numeric
#' balls: numeric
#' pre_strikes: numeric
#' pre_balls: numeric
#' call: character
#' call_name: character
#' description: character
#' result_code: character
#' pitch_call: character
#' is_strike_swinging: logical
#' balls_and_strikes: character
#' sz_top: numeric
#' sz_bot: numeric
#' extension: numeric
#' plateTime: numeric
#' zone: numeric
#' spin_rate: numeric
#' px: numeric
#' pz: numeric
#' x0: numeric
#' y0: numeric
#' z0: numeric
#' ax: numeric
#' ay: numeric
#' az: numeric
#' vx0: numeric
#' vy0: numeric
#' vz0: numeric
#' pfxX: numeric
#' pfxZ: numeric
#' pfxZWithGravity: numeric
#' pfxZWithGravityNice: numeric
#' pfxZDirection: character
#' pfxXWithGravity: numeric
#' pfxXNoAbs: numeric
#' pfxXDirection: character
#' breakX: numeric
#' breakZ: numeric
#' inducedBreakZ: numeric
#' is_bip_out: character
#' pitch_number: numeric
#' player_total_pitches: numeric
#' player_total_pitches_pitch_types: numeric
#' game_total_pitches: numeric
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
#' @importFrom dplyr bind_rows arrange mutate_at vars
#' @importFrom glue glue
#'
#' @export
#' @examples \donttest{
#'   try(statcast_pbp_college(769400))
#' }
statcast_pbp_college <- function(game_pk) {

df <- jsonlite::fromJSON(glue::glue("https://baseballsavant.mlb.com/gf?game_pk=",game_pk), flatten = T)

numeric_cols <- c("inning","ab_number","cap_index","outs","batter","pitcher","team_batting_id","team_fielding_id",
                  "strikes","balls","pre_strikes","pre_balls","start_speed","end_speed","sz_top","sz_bot",
                  "extension","plateTime","zone","spin_rate","px","pz","x0","y0","z0","ax","ay","az","vx0","vy0","vz0",
                  "pfxX","pfxZ","pfxZWithGravity","pfxXWithGravity","pfxZWithGravityNice","pfxXNoAbs","breakX","breakZ",
                  "inducedBreakZ","pitch_number","player_total_pitches","player_total_pitches_pitch_types","game_total_pitches",
                  "game_pk","hit_speed_round","hit_speed","hit_distance","hit_angle","is_barrel","hc_x","hc_x_ft","hc_y","hc_y_ft")

suppressWarnings(
    home_df <- df[['team_home']] |>
      dplyr::mutate_at(dplyr::vars(!!numeric_cols), function(x){as.character(x)})
  )
suppressWarnings(
  home_df <- df[['team_home']] |>
    dplyr::mutate_at(dplyr::vars(numeric_cols), function(x){as.numeric(as.character(x))})
)

suppressWarnings(
  away_df <- df[['team_away']] |>
    dplyr::mutate_at(dplyr::vars(!!numeric_cols), function(x){as.character(x)})
)
suppressWarnings(
  away_df <- df[['team_away']] |>
    dplyr::mutate_at(dplyr::vars(numeric_cols), function(x){as.numeric(as.character(x))})
)

df <-  dplyr::bind_rows(home_df, away_df) |>
  dplyr::arrange(inning,ab_number,pitch_number)

return(df)

}

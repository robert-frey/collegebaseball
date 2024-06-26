% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncaa_school_id_lookup.R
\name{ncaa_school_id_lookup}
\alias{ncaa_school_id_lookup}
\title{**School ID Lookup for NCAA (I, II, III) Schools**}
\usage{
ncaa_school_id_lookup(team_name = NULL, season = NULL)
}
\arguments{
\item{team_name}{The name of the team you'd like to lookup (Ex: Tennessee)}

\item{season}{The season you'd like to look up in (YYYY) format. Available years 2010-2024}
}
\value{
A tibble containing the teams that match the team name and these 8 columns:
team_id, year, team_name, conference_id, conference, division, season_id, prev_team_id (the previous team id before the site change)
}
\description{
**School ID Lookup for NCAA (I, II, III) Schools**
}

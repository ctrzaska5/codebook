library(tidyverse)
library(readr)
mlb_teams[mlb_teams == ""] <- "N/A"
mlb_teams[is.na(mlb_teams)] <- "N/A"
mlb_teams[mlb_teams == 'N/A'] <- NA

mlb_teams<- mlb_teams|>
  mutate(
    mlb_teams, winning_per = wins/games_played
  )

mlb_teams <- mutate(mlb_teams, winning_percentage = wins/games_played)

mlb_teams <- mlb_teams %>%
  mutate_at(vars(hits, walks, batters_hit_by_pitch, at_bats), as.numeric)

mlb_teams <- mlb_teams |>
  rowwise() |>
  mutate(on_base_per = sum(c(hits, walks, batters_hit_by_pitch), na.rm = TRUE) / at_bats)

avg_obp <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_obp = mean(on_base_per))

mlb_teams <- merge(mlb_teams, avg_home_obp, by = "year", suffixes = c("", "_avg"))

mlb_teams <- mlb_teams |>
  mutate(obp_status = ifelse(on_base_per > avg_obp, "Over", "Under"))

mlb_teams$obp_status <- factor(mlb_teams$obp_status, levels = c("Under", "Over"))

mlb_teams$obp_status <- factor(mlb_teams$obp_status, levels = c("Under", "Over"))

mlb_teams <- mlb_teams |>
  mutate(slugging_perc = ((singles * 1) + (doubles * 2) + (triples * 3) + (homeruns * 4)) / at_bats)

mlb_teams <- mlb_teams %>%
  mutate(singles = (hits - (doubles + triples + homeruns)))

mlb_teams <- mlb_teams|>
  mutate(ops = on_base_per + slugging_perc)

mlb_teams <- mlb_teams|>
  mutate(batting_avg = hits/at_bats)

avg_ops <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_ops = mean(ops))

mlb_teams <- merge(mlb_teams, avg_ops, by = "year", suffixes = c("", "_avg"))

mlb_teams <- mlb_teams %>%
  mutate(ops_status = ifelse(ops > avg_ops, "Over", "Under"))

mlb_teams$ops_status <- factor(mlb_teams$ops_status, levels = c("Under", "Over"))

mlb_teams$league_id <- factor(mlb_teams$league_id, levels = c("NL", "AL"))

mlb_teams$division_id <- factor(mlb_teams$division_id, levels = c("C","E","W"))

mlb_teams <- 
  mlb_teams |>
  mutate_at(vars(games_played, 
                 wins, 
                 losses,
                 runs_scored,
                 at_bats,
                 strikeouts_by_batters,
                 stolen_bases,
                 caught_stealing,
                 sacrifice_flies,
                 hits,
                 singles,
                 walks,
                 batters_hit_by_pitch,
                 ), as.numeric)

mlb_teams$division_winner <- factor(mlb_teams$division_winner, levels = c("Y", "N"))

mlb_teams$wild_card_winner <- factor(mlb_teams$wild_card_winner, levels = c("Y", "N"))

mlb_teams$leauge_winner <- factor(mlb_teams$league_winner, levels = c("Y", "N"))

mlb_teams$world_series_winner <- factor(mlb_teams$world_series_winner, levels = c("Y", "N"))

mlb_teams <- mlb_teams %>%
  mutate_at(vars(winning_per), as.numeric)

avg_slugging <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_slugging = mean(slugging_perc))

mlb_teams <- merge(mlb_teams, avg_slugging, by = "year", suffixes = c("", "_avg"))

mlb_teams <- mlb_teams |>
  mutate(slug_status = ifelse(slugging_perc > avg_slugging, "Over", "Under"))

mlb_teams$slug_status <- factor(mlb_teams$slug_status, levels = c("Under", "Over"))

avg_ba <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_ba = mean(batting_avg))

mlb_teams <- merge(mlb_teams, avg_ba, by = "year", suffixes = c("", "_avg"))

mlb_teams <- mlb_teams |>
  mutate(ba_status = ifelse(batting_avg > avg_ba, "Over", "Under"))

mlb_teams$ba_status <- factor(mlb_teams$ba_status, levels = c("Under", "Over"))

avg_obp <- 
  mlb_teams |>
  group_by(year) |>
  summarize(avg_obp = mean(on_base_per))

mlb_teams <- merge(mlb_teams, avg_obp, by = "year", suffixes = c("", "_avg"))

mlb_teams <- mlb_teams |>
  mutate(obp_status = ifelse(on_base_per > avg_obp, "Over", "Under"))

mlb_teams$obp_status <- factor(mlb_teams$obp_status, levels = c("Under", "Over"))

mlb_teams <- select(mlb_teams, -fielding_percentage,
                    -opponents_runs_scored,
                    -earned_runs_allowed,
                    -earned_run_average,
                    -complete_games,
                    -shutouts,
                    -saves,
                    -outs_pitches,
                    -hits_allowed,
                    -walks_allowed,
                    -homeruns_allowed,
                    -strikeouts_by_pitchers,
                    -double_plays,
                    -errors)
mlb_teams <-
  mlb_teams|>
  relocate(
    team_name,
    .before = league_id)

mlb_teams <-
  mlb_teams|>
  relocate(
    ball_park,
    home_attendance,
    .before = league_id)

mlb_teams <-
  mlb_teams|>
  relocate(
    winning_per,
    .before = division_winner)

mlb_teams <- select(mlb_teams, -home_games)

mlb_teams <- select(mlb_teams, -home_attendance)

mlb_teams <-
  mlb_teams|>
  relocate(
    wild_card_winner,
    .before = division_winner)

mlb_teams <-
  mlb_teams|>
  relocate(
    singles,
    .before = doubles)

mlb_teams <- mlb_teams %>%
  mutate_at(vars(doubles, triples, homeruns, year), as.numeric)

mlb_teams$year <- as.character(mlb_teams$year)

mlb_teams$league_winner <- factor(mlb_teams$league_winner, levels = c("Y", "N"))

mlb_teams <-
  mlb_teams|>
  relocate(
    batting_avg,
    .before = avg_ba)

mlb_teams <-
  mlb_teams|>
  relocate(
    ops,
    .before = avg_ops)

mlb_teams <-
  mlb_teams|>
  relocate(
    slugging_perc,
    .before = avg_slugging)

mlb_teams <-
  mlb_teams|>
  relocate(
    on_base_per,
    .before = avg_obp)

mlb_teams <- select(mlb_teams, -leauge_winner)

write.csv(mlb_teams, "mlb_teams_final.csv", row.names = FALSE)

save(mlb_teams, file = "mlb_teams_final.RData")

mlb_teams <- select(mlb_teams, -Avg_HomeRuns)

mlb_teams <-
  mlb_teams|>
  relocate(
    on_base_per,
    avg_obp,
    obp_status,
    slugging_perc,
    avg_slugging,
    slug_status,
    .before = ops)

mlb_teams <- select(mlb_teams, -leauge_winner)


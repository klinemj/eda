#Setup
library(dplyr)
library(nflfastR)
library(ggplot2)
library(sqldf)

#Load data
bets_data = nflreadr::load_schedules(2014:2024) %>%
  select(game_id, week, home_team, away_team, home_score, away_score,
         spread_line, result, home_coach, away_coach, home_qb_name, away_qb_name)%>%
  arrange(game_id)

#break out home vs. away, there will a record for each team for each game
home = bets_data %>%
  mutate(team = home_team) %>%
  mutate(opponent = away_team) %>%
  mutate(score = home_score) %>%
  mutate(opp_score = away_score) %>%
  mutate(points_favored = spread_line) %>%
  mutate(spread_differential = result-spread_line) %>%
  mutate(coach = home_coach) %>%
  mutate(opp_coach = away_coach) %>%
  mutate(qb = home_qb_name) %>%
  mutate(opp_qb = away_qb_name) %>%
  select(game_id, week, team, opponent, points_favored, score, opp_score, result,
         spread_differential,coach, opp_coach, qb, opp_qb
         )
away = bets_data %>%
  mutate(team = away_team) %>%
  mutate(opponent = home_team) %>%
  mutate(score = away_score) %>%
  mutate(opp_score = home_score) %>%
  mutate(result = result*-1) %>%
  mutate(points_favored = spread_line*-1) %>%
  mutate(spread_differential = result+spread_line) %>%
  mutate(coach = away_coach) %>%
  mutate(opp_coach = home_coach) %>%
  mutate(qb = away_qb_name) %>%
  mutate(opp_qb = home_qb_name) %>%
  select(game_id, week, team, opponent, points_favored, score, opp_score, result,
         spread_differential,coach, opp_coach, qb, opp_qb
  )
#Combine data
games = rbind(home,away)

test = games %>%
  group_by(coach) %>%
  mutate(games = count(n=n()))%>%
  mutate(avg_spread_differential = mean(spread_differential, na.rm=TRUE)) %>%
  select(coach, games,avg_spread_differential) %>%
  distinct()
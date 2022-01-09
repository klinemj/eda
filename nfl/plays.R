#Purpose of this code is to if plays regresses to points scored and over/unders
#Date: 12/18/2021
library(sqldf)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(nflfastR)
library(gmodels)
library(openxlsx)

#Load data
seasons <- 2010:2021
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

#Only get regular season games
#Pass and runs
#WP >5%
pbp1 = pbp %>% filter(season_type=="REG", play_type !='no_play', play_type !='kickoff', play_type !='extra_point', wp>0.05) %>%
  mutate(week = substr(game_id,6,7)) %>%
  mutate(season_str = as.character(season))


#Get number of plays and other metrics for every season, game, down, quarter, team
off_stats=sqldf("select season_str, week, game_id, down, qtr, posteam, count(play_id)as plays, sum(rush_attempt)as runs,
      sum(pass_attempt)as passes, sum(passing_yards)as passing_yds, sum(rushing_yards)as rushing_yds,
      sum(yards_gained)as yds, sum(success)as successful_plays,
      sum(case when rush=1 then success else 0 end)as rush_success,
      sum(case when pass=1 then success else 0 end)as pass_success,
      sum(case when rush=1 and rushing_yards>=10 then 1 else 0 end)as xpl_runs,
      sum(case when pass=1 and passing_yards>=15 then 1 else 0 end)as xpl_passes
      from pbp1
      where down != 'NA'
      group by season_str, week, game_id, down, qtr, posteam 
      order by season_str, week, game_id, down, qtr, posteam ")

#Get points scored per quarter and total score at the end of each quarter
points = sqldf("select season, week, game_id, qtr, posteam, 
               sum(case when touchdown=1 then 6 else 0 end)as td_pts,
               sum(case when extra_point_result ='good' then 1 else 0 end) as extra_pts,
               sum(case when two_point_conv_result ='success' then 2 else 0 end) as two_pts,
               sum(case when field_goal_result ='made' then 3 else 0 end)as fg_pts,
               sum(case when safety=1 then 2 else 0 end)as safety_pts,
               sum(case when touchdown=1 then 6 else 0 end)+
               sum(case when extra_point_result ='good' then 1 else 0 end)+
               sum(case when two_point_conv_result ='success' then 2 else 0 end)+
               sum(case when field_goal_result ='made' then 3 else 0 end)+
               sum(case when safety=1 then 2 else 0 end)as pts_scored
               from pbp
               where posteam != 'NA' and defteam != 'NA'
               group by season, week, game_id, qtr, posteam
               order by season, week, game_id, qtr, posteam")

#Add score to main data set
off_stats1=sqldf("select a.*, b.pts_scored
                 from off_stats a left join points b on a.season_str=b.season and
                 a.week=b.week and a.game_id=b.game_id and a.qtr=b.qtr and a.posteam=b.posteam")

#Save data
wb <- loadWorkbook("C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\eda\\off_stats1.xlsx")
writeData(wb, sheet = "RAW", x = off_stats1)
saveWorkbook(wb, "C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\eda\\off_stats3.xlsx")
openXL("C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\eda\\off_stats3.xlsx")


#Get summary stats
season_sum = sqldf("select season_str, game_id, posteam, sum(plays)as total_plays, sum(runs)as total_runs,
                   sum(rush_success)as total_rush_success, sum(xpl_runs)as total_xpl_runs, sum(rushing_yds)as total_rushing_yds,
                   sum(passes)as total_passes, sum(pass_success)as total_pass_success, sum(xpl_passes)as total_xpl_passes, sum(passing_yds)as total_passing_yds,
                   sum(successful_plays)as total_successful_plays, sum(yds)as total_yds
                   from off_stats1
                   group by season_str, game_id, posteam
                   order by season_str, game_id, posteam")
#Export
avg_plays = sqldf("select season_str, avg(total_plays)as avg_plays, sum(total_runs)/sum(total_plays)as avg_run_pct,
                  sum(total_rush_success)/sum(total_runs)as avg_rush_success, sum(total_xpl_runs)/sum(total_runs)as avg_xpl_run_rate,
                  avg(total_rushing_yds)as avg_rushing_yds, sum(total_rushing_yds)/sum(total_runs)as avg_ypr,
                  sum(total_passes)/sum(total_plays)as avg_pass_pct, sum(total_pass_success)/sum(total_passes)as avg_pass_success,
                  sum(total_xpl_passes)/sum(total_passes)as avg_xpl_pass_rate, avg(total_passing_yds)as avg_passing_yds,
                  sum(total_passing_yds)/sum(total_passes)as avg_ypa, sum(total_yds)/sum(total_plays)as avg_ypp,
                  sum(total_successful_plays)/sum(total_plays)as avg_success_rate,
                  avg(total_yds)as avg_yds
                  from season_sum
                  group by season_str
                  order by season_str")
#Export
avg_team_plays = sqldf("select season_str, posteam,avg(total_plays)as avg_plays, sum(total_runs)/sum(total_plays)as avg_run_pct,
                  sum(total_rush_success)/sum(total_runs)as avg_rush_success, sum(total_xpl_runs)/sum(total_runs)as avg_xpl_run_rate,
                  avg(total_rushing_yds)as avg_rushing_yds, sum(total_rushing_yds)/sum(total_runs)as avg_ypr,
                  sum(total_passes)/sum(total_plays)as avg_pass_pct, sum(total_pass_success)/sum(total_passes)as avg_pass_success,
                  sum(total_xpl_passes)/sum(total_passes)as avg_xpl_pass_rate, avg(total_passing_yds)as avg_passing_yds,
                  sum(total_passing_yds)/sum(total_passes)as avg_ypa, sum(total_yds)/sum(total_plays)as avg_ypp,
                  sum(total_successful_plays)/sum(total_plays)as avg_success_rate,
                  avg(total_yds)as avg_yds
                  from season_sum
                  group by season_str, posteam
                  order by season_str, posteam")

quarter_sum = sqldf("select season_str, game_id, posteam, qtr, sum(plays)as total_plays, sum(runs)as total_runs,
                   sum(rush_success)as total_rush_success, sum(xpl_runs)as total_xpl_runs, sum(rushing_yds)as total_rushing_yds,
                   sum(passes)as total_passes, sum(pass_success)as total_pass_success, sum(xpl_passes)as total_xpl_passes, sum(passing_yds)as total_passing_yds,
                   sum(successful_plays)as total_successful_plays, sum(yds)as total_yds
                   from off_stats1
                   group by season_str, game_id, posteam, qtr
                   order by season_str, game_id, posteam, qtr")
#Export
avg_qtr_plays = sqldf("select season_str, qtr, avg(total_plays)as avg_plays, sum(total_runs)/sum(total_plays)as avg_run_pct,
                  sum(total_rush_success)/sum(total_runs)as avg_rush_success, sum(total_xpl_runs)/sum(total_runs)as avg_xpl_run_rate,
                  avg(total_rushing_yds)as avg_rushing_yds, sum(total_rushing_yds)/sum(total_runs)as avg_ypr,
                  sum(total_passes)/sum(total_plays)as avg_pass_pct, sum(total_pass_success)/sum(total_passes)as avg_pass_success,
                  sum(total_xpl_passes)/sum(total_passes)as avg_xpl_pass_rate, avg(total_passing_yds)as avg_passing_yds,
                  sum(total_passing_yds)/sum(total_passes)as avg_ypa, sum(total_yds)/sum(total_plays)as avg_ypp,
                  sum(total_successful_plays)/sum(total_plays)as avg_success_rate,
                  avg(total_yds)as avg_yds
                  from quarter_sum
                  group by season_str, qtr
                  order by season_str, qtr")
#Export
avg_team_qtr_plays = sqldf("select season_str, qtr, posteam,avg(total_plays)as avg_plays, sum(total_runs)/sum(total_plays)as avg_run_pct,
                       sum(total_rush_success)/sum(total_runs)as avg_rush_success, sum(total_xpl_runs)/sum(total_runs)as avg_xpl_run_rate,
                       avg(total_rushing_yds)as avg_rushing_yds, sum(total_rushing_yds)/sum(total_runs)as avg_ypr,
                       sum(total_passes)/sum(total_plays)as avg_pass_pct, sum(total_pass_success)/sum(total_passes)as avg_pass_success,
                       sum(total_xpl_passes)/sum(total_passes)as avg_xpl_pass_rate, avg(total_passing_yds)as avg_passing_yds,
                       sum(total_passing_yds)/sum(total_passes)as avg_ypa, sum(total_yds)/sum(total_plays)as avg_ypp,
                       sum(total_successful_plays)/sum(total_plays)as avg_success_rate,
                       avg(total_yds)as avg_yds
                       from quarter_sum
                       group by season_str, qtr, posteam
                       order by season_str, qtr, posteam")

down_sum = sqldf("select season_str, game_id, posteam, down, sum(plays)as total_plays, sum(runs)as total_runs,
                   sum(rush_success)as total_rush_success, sum(xpl_runs)as total_xpl_runs, sum(rushing_yds)as total_rushing_yds,
                    sum(passes)as total_passes, sum(pass_success)as total_pass_success, sum(xpl_passes)as total_xpl_passes, sum(passing_yds)as total_passing_yds,
                    sum(successful_plays)as total_successful_plays, sum(yds)as total_yds
                    from off_stats1
                    group by season_str, game_id, posteam, down
                    order by season_str, game_id, posteam, down")
#Export
avg_down_plays = sqldf("select season_str, down, avg(total_plays)as avg_plays, sum(total_runs)/sum(total_plays)as avg_run_pct,
                  sum(total_rush_success)/sum(total_runs)as avg_rush_success, sum(total_xpl_runs)/sum(total_runs)as avg_xpl_run_rate,
                  avg(total_rushing_yds)as avg_rushing_yds, sum(total_rushing_yds)/sum(total_runs)as avg_ypr,
                  sum(total_passes)/sum(total_plays)as avg_pass_pct, sum(total_pass_success)/sum(total_passes)as avg_pass_success,
                  sum(total_xpl_passes)/sum(total_passes)as avg_xpl_pass_rate, avg(total_passing_yds)as avg_passing_yds,
                  sum(total_passing_yds)/sum(total_passes)as avg_ypa, sum(total_yds)/sum(total_plays)as avg_ypp,
                  sum(total_successful_plays)/sum(total_plays)as avg_success_rate,
                  avg(total_yds)as avg_yds
                  from down_sum
                  group by season_str, down
                  order by season_str, down")
#Export
avg_team_down_plays = sqldf("select season_str, down, posteam,avg(total_plays)as avg_plays, sum(total_runs)/sum(total_plays)as avg_run_pct,
                       sum(total_rush_success)/sum(total_runs)as avg_rush_success, sum(total_xpl_runs)/sum(total_runs)as avg_xpl_run_rate,
                       avg(total_rushing_yds)as avg_rushing_yds, sum(total_rushing_yds)/sum(total_runs)as avg_ypr,
                       sum(total_passes)/sum(total_plays)as avg_pass_pct, sum(total_pass_success)/sum(total_passes)as avg_pass_success,
                       sum(total_xpl_passes)/sum(total_passes)as avg_xpl_pass_rate, avg(total_passing_yds)as avg_passing_yds,
                       sum(total_passing_yds)/sum(total_passes)as avg_ypa, sum(total_yds)/sum(total_plays)as avg_ypp,
                       sum(total_successful_plays)/sum(total_plays)as avg_success_rate,
                       avg(total_yds)as avg_yds
                       from down_sum
                       group by season_str, down, posteam
                       order by season_str, down, posteam")

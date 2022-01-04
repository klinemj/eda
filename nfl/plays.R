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


#Get number of plays and other metrics for every season, game, quarter, team
off_stats=sqldf("select season_str, week, game_id, qtr, posteam, count(play_id)as plays, sum(rush_attempt)as runs,
      sum(pass_attempt)as passes, sum(passing_yards)as passing_yds, sum(rushing_yards)as rushing_yds,
      sum(yards_gained)as yds, sum(success)as successful_plays,
      sum(case when rush=1 then success else 0 end)as rush_success,
      sum(case when pass=1 then success else 0 end)as pass_success,
      sum(case when rush=1 and rushing_yards>=10 then 1 else 0 end)as xpl_runs,
      sum(case when pass=1 and passing_yards>=15 then 1 else 0 end)as xpl_passes
      from pbp1
      group by season_str, week, game_id, qtr, posteam 
      order by season_str, week, game_id, qtr, posteam ")

#Get points scored per quarter and total score at the end of each quarter, if the game went over
points=sqldf("select season, week, game_id, qtr, posteam, posteam_score_post,
            posteam_score_post+defteam_score_post as qtr_total, total,total_line, 
            case when total > total_line then 1 else 0 end as over
            from pbp
            where posteam != 'NA' and defteam != 'NA'
            group by  season, week, game_id, qtr, posteam, defteam
            having MAX(play_id) =play_id
            order by  season, week, game_id, qtr, posteam, defteam")


#Add score to main data set
off_stats1=sqldf("select a.*, b.posteam_score_post as points
                 from off_stats a left join points b on a.season_str=b.season and
                 a.week=b.week and a.game_id=b.game_id and a.qtr=b.qtr and a.posteam=b.posteam")


wb <- loadWorkbook("C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\eda\\template.xlsx")
writeData(wb, sheet = "RAW", x = off_stats1)
saveWorkbook(wb, "C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\eda\\populated_template.xlsx")
openXL("C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\eda\\populated_template.xlsx")


write.xlsx(
  off_stats1,
  file="C:\\R Projects\\Kaggle\\nfl-scores-and-betting-data\\nflbets\\eda\\off_stats.xlsx",
  sheetName = "RAW",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE
)

#Get points scored per quarter and total score at the end of each quarter, if the game went over
points=sqldf("select season, game_id, qtr, posteam, defteam, posteam_score_post,defteam_score_post,
            posteam_score_post+defteam_score_post as qtr_total, total,total_line, 
            case when total > total_line then 1 else 0 end as over
            from pbp
            where posteam != 'NA' and defteam != 'NA'
            group by  season, game_id, qtr, posteam, defteam
            having MAX(play_id) =play_id
            order by  season, game_id, qtr, posteam, defteam")

#Get overall sec_per_play
#Get number of plays for every season, game, team
plays_all=sqldf("select season_str,posteam, count(play_id)as plays
            from pbp1
            group by season_str,posteam 
            order by season_str,posteam ")

sum_time_all=sqldf("select season_str,posteam, sum(drive_sec_of_possesion)as tot_sec_of_possession
               from time
               group by season_str, posteam
               order by season_str, posteam")

#Bring in plays
time_plays_all = sqldf("select a.season_str, a.posteam, a.tot_sec_of_possession, b.plays,
                   round(a.tot_sec_of_possession/b.plays,2) as sec_per_play
                   from sum_time_all a left join plays_all b on a.season_str=b.season_str and 
                   a.posteam=b.posteam
                   group by a.season_str,a.posteam
                   order by a.season_str,a.posteam")
#Add sec_per_play
time_plays_points = sqldf("select a.*, b.plays, b.tot_sec_of_possession, b.sec_per_play
                          from points a left join time_plays b on a.game_id=b.game_id and a.qtr=b.qtr and 
                          a.posteam=b.posteam")

test= pbp %>% filter(game_id=='2021_01_GB_NO')
write.csv(time_plays_points,"C:/R Projects/Kaggle/nfl-scores-and-betting-data/nflbets/eda/sec_per_play.csv")
write.csv(time_plays_all,"C:/R Projects/Kaggle/nfl-scores-and-betting-data/nflbets/eda/sec_per_play_all.csv")

#Get over under for every game
ou = sqldf("select distinct season, game_id, posteam, posteam_score_post,total, total_line, over
              from points
              where qtr=4")
over = sqldf("select season, posteam,avg(posteam_score_post)as avg_pts_scored, avg(total) as avg_total, avg(total_line) as avg_total_line,
              sum(cast(over as decimal)) overs,
              count(game_id)as games
              from ou
              group by season, posteam
              order by season, posteam")
over1 = over %>% mutate(over_pct =overs/games)

#Add to sec per play
pace_overs = sqldf("select a.*, b.avg_pts_scored,b.avg_total, b.avg_total_line, b.overs, b.games, b.over_pct
                  from time_plays_all a left join over1 b on a.season_str=b.season and a.posteam=b.posteam")
write.csv(pace_overs,"C:/R Projects/Kaggle/nfl-scores-and-betting-data/nflbets/eda/sec_per_play_all.csv")


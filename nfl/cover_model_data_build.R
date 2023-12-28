#Setup
library(dplyr)
library(nflfastR)
library(ggplot2)
library(sqldf)

##################################################################################
#DATA MANIPULATION SECTION
#Run queries, clean data, create fields for potential use in a model
##################################################################################

#Load data
data = load_pbp(2018:2023)
bets_data = nflreadr::load_schedules(2018:2023) %>% 
          select(game_id, roof, away_qb_name, home_qb_name, away_coach, home_coach, 
           referee, home_rest, away_rest, home_moneyline, 
           away_moneyline, away_spread_odds, home_spread_odds, total_line,
           over_odds, under_odds)

#Get basic facts from a game
games = data %>% 
  filter(season_type =="REG") %>%
  filter(posteam != '') %>%
  rename(team = posteam) %>%
  mutate(game_day = weekdays(as.Date(game_date))) %>%
  mutate(home = ifelse(posteam_type=="home",1,0)) %>%
  mutate(win = case_when(home==1 & home_score>away_score ~1,
                         home==0 & away_score>home_score ~1,
                         .default=0)) %>%
  mutate(favorite = case_when(home==1 & spread_line>0 ~ 1,
                              home==0 & spread_line<0 ~ 1,
                              .default = 0)) %>%
  mutate(pts_favored = case_when(favorite==1 ~ abs(spread_line),
                                 favorite==0 ~ 0 -(abs(spread_line)),
                                 .default = 0)) %>%
  mutate(pts_scored = case_when(home==1 ~ home_score,
                                home==0 ~ away_score,
                                .default = 0)) %>%
  mutate(pts_allowed = case_when(home==1 ~ away_score,
                                 home==0 ~ home_score,
                                 .default = 0)) %>%
  mutate(score_diff = case_when(home==1 ~ home_score-away_score,
                                home==0 ~ away_score-home_score)) %>%
  mutate(cover = case_when(home==1 & result>spread_line ~1,
                           home==0 & result<spread_line ~1,
                           .default = 0)) %>%
  mutate(big_win = case_when(home==1 & result >=20 ~1,
                             home==0 & result <=-20 ~1,
                             .default = 0)) %>%
  mutate(big_loss = case_when(home==1 & result <=-20 ~1,
                              home==0 & result >=20 ~1,
                              .default = 0)) %>%
  mutate(one_score_game = ifelse(abs(result)<=8,1,0)) %>%
  mutate(run_or_pass = ifelse(play_type %in% c('pass','run'),1,0)) %>%
  
  #Aggregates
  filter(run_or_pass==1)%>%
  filter(down >=1) %>%
  group_by(game_id,team) %>%
  mutate(yards_gained = ifelse(run_or_pass==1,sum(yards_gained),0)) %>%
  mutate(off_plays = sum(run_or_pass)) %>%
  mutate(first_downs = sum(ifelse(down==1,1,0))) %>%
  mutate(third_downs = sum(ifelse(down==3,1,0))) %>%
  mutate(fourth_down_att = sum(ifelse(down==4,1,0))) %>%
  mutate(fourth_down_conv = sum(ifelse(down==4,fourth_down_converted,0))) %>%
  mutate(sacks_allowed = sum(sack)) %>%
  mutate(turnovers = sum(interception, fumble_lost)) %>%
  ungroup() %>%
  #select statement
  select(game_id, season, week, game_day, surface, team, div_game, defteam, 
         home, pts_scored, pts_allowed, score_diff, spread_line, favorite, pts_favored, 
         cover, win, big_win, big_loss, 
         one_score_game, off_plays,
         yards_gained, first_downs, third_downs, fourth_down_att, 
         fourth_down_conv, sacks_allowed, turnovers) %>%
  distinct()

#Some stats need to be created at the play level

#redzone
redzone_raw = data %>% 
  filter(season_type =="REG") %>%
  filter(posteam != '') %>%
  filter(drive != '') %>%
  rename(team = posteam) %>%
  select(game_id,season,team, td_team , drive, touchdown ,drive_inside20) %>%
  mutate(touchdown = case_when(is.na(touchdown)~0,
                               td_team==team ~ touchdown,
                               .default = 0)) %>%
  distinct() %>%
  group_by(game_id, team)%>%
  mutate(red_zone_drives = sum(drive_inside20)) %>%
  mutate(red_zone_tds = sum(ifelse(drive_inside20==1,touchdown, 0))) %>%
  select(game_id,season,team, red_zone_drives, red_zone_tds) %>%
  distinct()



#Early down pass/run attempts neutral game script
early_down = data %>%
  filter(season_type =="REG") %>%
  filter(posteam != '') %>%
  filter(play_type %in% c('pass','run')) %>%
  #Get plays with atleast 30% wp and less than 70%
  filter(vegas_wp >=0.3 & vegas_wp <=0.7) %>%
  filter(down >=1 & down <=2) %>%
  rename(team = posteam) %>%
  group_by(game_id, team) %>%
  mutate(early_down_passes_neutral = sum(pass)) %>%
  mutate(early_down_runs_neutral = sum(rush)) %>%
  mutate(total_early_downs_neutral = early_down_passes_neutral+ early_down_runs_neutral) %>%
  mutate(early_down_pass_pct_neutral = early_down_passes_neutral/total_early_downs_neutral) %>%
  mutate(early_down_run_pct_neutral = early_down_runs_neutral/total_early_downs_neutral) %>% 
  select(game_id, team, early_down_passes_neutral, early_down_runs_neutral, total_early_downs_neutral, 
         early_down_pass_pct_neutral, early_down_run_pct_neutral) %>%
  distinct()

###############################################################################
#JOINS SECTION
#Start joining data together, use games as base table
###############################################################################
main_join = sqldf('select a.*,
                          b.roof,
                          b.total_line,
                          b.over_odds,
                          b.under_odds,
                     case when a.home=1 then b.home_qb_name
                          when a.home=0 then b.away_qb_name end as qb_name,
                     case when a.home=1 then b.home_coach
                          when a.home=0 then b.away_coach end as head_coach,
                     case when a.home=1 then b.home_rest
                          when a.home=0 then b.away_rest end as rest,
                     case when a.home=1 then b.home_moneyline
                          when a.home=0 then b.away_moneyline end as moneyline,
                     case when a.home=1 then b.home_spread_odds
                          when a.home=0 then b.away_spread_odds end as spread_odds,
                     c.red_zone_drives,
                     c.red_zone_tds,
                     d.early_down_passes_neutral,
                     d.early_down_runs_neutral,
                     d.total_early_downs_neutral
              from games a
              left join bets_data b
              on a.game_id=b.game_id
              left join redzone_raw c 
              on a.game_id = c.game_id and a.team = c.team
              left join early_down d 
              on a.game_id = d.game_id and a.team = d.team
              order by a.season, a.team, a.week')

###############################################################################
#GET MEDIANS AND MODES SECTION
#Machine learning models can't handle NAs or missings, calculate median or modes
#based off season.
#Add new field to the file below!
###############################################################################
source("C:/R Projects/Git/eda/nfl/cover_model_avgs.r")

###############################################################################
#IMPUTE MEDIANS AND MODES SECTION
#Machine learning models can't handle NAs or missings, calculate median or modes.
#Join main join to these and use medians or mode if missing
###############################################################################

join_impute = sqldf('select a.game_id,
                            a.season,
                            a.week,
                            a.game_day,
                            case when a.surface is null then b.surface_mode
                                 when a.surface = "" then b.surface_mode
                                 else a.surface end as surface,
                            a.roof,
                            a.qb_name,
                            a.head_coach,
                            a.team,
                            a.div_game,
                            a.defteam,
                            a.home,
                            a.pts_scored,
                            a.pts_allowed,
                            a.total_line,
                            a.over_odds,
                            a.under_odds,
                            a.score_diff,
                            a.spread_line,
                            a.pts_favored,
                            a.favorite,
                            a.cover,
                            a.win,
                            a.big_win,
                            a.big_loss,
                            a.one_score_game,
                            a.off_plays,
                            a.yards_gained,
                            a.first_downs,
                            a.third_downs,
                            a.fourth_down_att,
                            a.fourth_down_conv,
                            a.sacks_allowed,
                            a.turnovers,
                            a.rest,
                            a.moneyline,
                            a.spread_odds,
                            a.red_zone_drives,
                            a.red_zone_tds,
                            case when a.early_down_passes_neutral is null then c.early_down_passes_neutral_median
                                 when a.early_down_passes_neutral = "" then c.early_down_passes_neutral_median
                                 else a.early_down_passes_neutral end as early_down_passes_neutral,
                            case when a.early_down_runs_neutral is null then d.early_down_runs_neutral_median
                                 when a.early_down_runs_neutral = "" then d.early_down_runs_neutral_median
                                 else a.early_down_runs_neutral end as early_down_runs_neutral,
                            case when a.total_early_downs_neutral is null then e.total_early_downs_neutral_median
                                 when a.total_early_downs_neutral = "" then e.total_early_downs_neutral_median
                                 else a.total_early_downs_neutral end as total_early_downs_neutral
                     from main_join a
                     left join mode_surface b
                     on a.season=b.season
                     left join med_early_down_passes_neutral c on
                     a.season=c.season
                     left join med_early_down_runs_neutral d on
                     a.season = d.season
                     left join med_total_early_downs_neutral e on
                     a.season = e.season')

###############################################################################
#CALCULATE HISTORIC FIELDS SECTION
#Need to calculate stats from prior weeks for prediction
###############################################################################
aggregates = sqldf('select *,
                        -- Last week section
                         lag(week, 1) 
                         OVER(ORDER BY season, team, week ASC) as last_week,
                         case when week = 1 then 0
                              when week != 1 and ((week-(lag(week, 1) 
                         OVER(ORDER BY season, team, week ASC))) > 1) then 1
                              else 0 end as last_week_bye,
                         lag(pts_scored, 1) OVER(
                         ORDER BY season, team, week ASC) AS last_gm_pts_scored,
                         lag(pts_allowed, 1) OVER(
                         ORDER BY season, team, week ASC) AS last_gm_pts_allowed,
                         lag(spread_line, 1) OVER(
                         ORDER BY season, team, week ASC) AS last_gm_spread_line,
                         lag(cover, 1) OVER(
                         ORDER BY season, team, week ASC) AS last_gm_cover,
                         lag(win, 1) OVER(
                         ORDER BY season, team, week ASC) AS last_gm_win,
                         lag(big_win, 1) OVER(
                         ORDER BY season, team, week ASC) AS last_gm_big_win,
                         lag(big_loss, 1) OVER(
                         ORDER BY season, team, week ASC) AS last_gm_big_loss,
                         lag(one_score_game, 1) OVER(
                         ORDER BY season, team, week ASC) AS last_gm_one_score,
                      -- Averages section
                         avg(score_diff) OVER(
                         ORDER BY season, team, week ASC) AS avg_score_diff,
                         avg(score_diff) OVER(
                         ORDER BY season, team, week ASC
                         ROWS BETWEEN 3 PRECEDING AND 1 PRECEDING) AS last_3_avg_score_diff,
                         avg(score_diff) OVER(
                         ORDER BY season, team, week ASC
                         ROWS BETWEEN 5 PRECEDING AND 1 PRECEDING) AS last_5_avg_score_diff,
                         avg(pts_scored) OVER(
                         ORDER BY season, team, week ASC) AS avg_pts_scored,
                         avg(pts_scored) OVER(
                         ORDER BY season, team, week ASC
                         ROWS BETWEEN 3 PRECEDING AND 1 PRECEDING) AS last_3_avg_pts_scored,
                         avg(pts_scored) OVER(
                         ORDER BY season, team, week ASC
                         ROWS BETWEEN 5 PRECEDING AND 1 PRECEDING) AS last_5_avg_pts_scored,
                         avg(pts_allowed) OVER(
                         ORDER BY season, team, week ASC) AS avg_pts_allowed,
                         avg(pts_allowed) OVER(
                         ORDER BY season, team, week ASC
                         ROWS BETWEEN 3 PRECEDING AND 1 PRECEDING) AS last_3_avg_pts_allowed,
                         avg(pts_allowed) OVER(
                         ORDER BY season, team, week ASC
                         ROWS BETWEEN 5 PRECEDING AND 1 PRECEDING) AS last_5_avg_pts_allowed,
                         avg(pts_favored) OVER(
                         ORDER BY season, team, week ASC) AS avg_pts_favored,
                         avg(pts_favored) OVER(
                         ORDER BY season, team, week ASC
                         ROWS BETWEEN 3 PRECEDING AND 1 PRECEDING) AS last_3_avg_pts_favored,
                         avg(pts_favored) OVER(
                         ORDER BY season, team, week ASC
                         ROWS BETWEEN 5 PRECEDING AND 1 PRECEDING) AS last_5_avg_pts_favored
                    from join_impute')
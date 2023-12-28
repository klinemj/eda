#Function for mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#off_plays
med_off_plays = main_join %>% 
  group_by(season) %>% 
  summarise(off_plays_median = median(off_plays, na.rm = TRUE))
#yards_gained
med_yards_gained = main_join %>% 
  group_by(season) %>% 
  summarise(yards_gained_median = median(yards_gained, na.rm = TRUE))

#first_downs
med_first_downs = main_join %>% 
  group_by(season) %>% 
  summarise(first_downs_median = median(first_downs, na.rm = TRUE))

#third_downs
med_third_downs = main_join %>% 
  group_by(season) %>% 
  summarise(third_downs_median = median(third_downs, na.rm = TRUE))

#fourth_down_att
med_fourth_down_att = main_join %>% 
  group_by(season) %>% 
  summarise(fourth_down_att_median = median(fourth_down_att, na.rm = TRUE))

#fourth_down_conv
med_fourth_down_conv = main_join %>% 
  group_by(season) %>% 
  summarise(fourth_down_conv_median = median(fourth_down_conv, na.rm = TRUE))

#sacks_allowed
med_sacks_allowed = main_join %>% 
  group_by(season) %>% 
  summarise(sacks_allowed_median = median(sacks_allowed, na.rm = TRUE))

#turnovers
med_turnovers = main_join %>% 
  group_by(season) %>% 
  summarise(turnovers_median = median(turnovers, na.rm = TRUE))

#rest
med_rest = main_join %>% 
  group_by(season) %>% 
  summarise(rest_median = median(rest, na.rm = TRUE))

#moneyline
med_moneyline = main_join %>% 
  group_by(season) %>% 
  summarise(moneyline_median = median(moneyline, na.rm = TRUE))

#spread_odds
med_spread_odds = main_join %>% 
  group_by(season) %>% 
  summarise(spread_odds_median = median(spread_odds, na.rm = TRUE))

#red_zone_drives
med_red_zone_drives = main_join %>% 
  group_by(season) %>% 
  summarise(red_zone_drives_median = median(red_zone_drives, na.rm = TRUE))

#red_zone_tds
med_red_zone_tds = main_join %>% 
  group_by(season) %>% 
  summarise(red_zone_tds_median = median(red_zone_tds, na.rm = TRUE))

#early_down_passes_neutral
med_early_down_passes_neutral = main_join %>% 
  group_by(season) %>% 
  summarise(early_down_passes_neutral_median = median(early_down_passes_neutral, na.rm = TRUE))

#early_down_runs_neutral
med_early_down_runs_neutral = main_join %>% 
  group_by(season) %>% 
  summarise(early_down_runs_neutral_median = median(early_down_runs_neutral, na.rm = TRUE))

#total_early_downs_neutral
med_total_early_downs_neutral = main_join %>% 
  group_by(season) %>% 
  summarise(total_early_downs_neutral_median = median(total_early_downs_neutral, na.rm = TRUE))

#surface
mode_surface = main_join %>% 
  group_by(season) %>% 
  summarise(surface_mode = mode(surface))
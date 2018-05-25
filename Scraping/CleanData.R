library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

load('../Data/allMatchStats.RData')

## bind all stats tables into a large table
detailedStats = lapply(allMatchStats, function(match) {
  date = match$date
  team1 = match$team1
  team2 = match$team2
  players1 = match$playerStats$team1$Player
  players2 = match$playerStats$team2$Player
  match$detailedStats %>% mutate(Date = mdy(date)) %>%
    mutate(Team = if_else(Player %in% players1, team1, team2),
           Opponent = if_else(Player %in% players1, team2, team1),
           Score = if_else(Player %in% players1, paste0(match$score, collapse = '-'),
                           paste0(rev(match$score), collapse = '-'))) %>%
    select(-Impact)
}) %>% bind_rows()

detailedStats = detailedStats %>%
  mutate_at(vars(`FWin%`, PTK, UOOF, FK, FD), funs(str_replace(., '%', '') %>% as.numeric())) %>%
  group_by(Team) %>% mutate(Match = dense_rank(Date)) %>%
  ungroup() %>%
  mutate(Rating = as.numeric(Rating), Time = hms(Time) %>% as.numeric() / 60)

save(detailedStats, file = '../Data/detailedStats.RData')

## aggregate player stats over all match, stratified by player and by hero
## playerStats = detailedStats %>%
##   mutate(Time = as.numeric(Time) / 60) %>%
##   group_by(Team, Player, Hero) %>%
##   summarise(Time = sum(Time), )

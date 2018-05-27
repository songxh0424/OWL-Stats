library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

load('../Data/allMatchStats.RData')

finals = c('2625', '3141', '2624', '2510', '2509', '2454', '2817')
idx = which(names(allMatchStats) %in% finals)

## bind all stats tables into a large table
detailedStats = lapply(allMatchStats[-idx], function(match) {
  date = match$date
  team1 = match$team1
  team2 = match$team2
  players1 = match$playerStats$team1$Player
  players2 = match$playerStats$team2$Player
  match$detailedStats %>% mutate(Date = mdy(date)) %>%
    mutate(Team = if_else(Player %in% players1, team1, team2),
           Opponent = if_else(Player %in% players1, team2, team1),
           Score = if_else(Player %in% players1, paste0(match$score, collapse = '-'),
                           paste0(rev(match$score), collapse = '-')),
           Result = if_else(str_sub(Score, end = 1) > str_sub(Score, start = -1), 'Win', 'Lose')) %>%
    select(-Impact)
}) %>% bind_rows()

detailedStats = detailedStats %>%
  mutate_at(vars(`FWin%`, PTK, UOOF, FK, FD), funs(str_replace(., '%', '') %>% as.numeric())) %>%
  group_by(Team) %>% mutate(Match = dense_rank(Date)) %>%
  ungroup() %>%
  mutate(Rating = as.numeric(Rating), Time = hms(Time) %>% as.numeric() / 60)

save(detailedStats, file = '../Data/detailedStats.RData')

playerStats = lapply(allMatchStats[-idx], function(match) {
  date = match$date

  df1 = match$playerStats$team1 %>%
    mutate(Team = match$team1, Opponent = match$team2) %>%
    select(-`Rating?`)
  df2 = match$playerStats$team2 %>%
    mutate(Team = match$team2, Opponent = match$team1) %>%
    select(-`Rating?`)
  df1 %>% bind_rows(df2) %>% mutate(Date = mdy(date))
}) %>% bind_rows()

playerStats = detailedStats %>% group_by(Team, Date, Player) %>%
  summarise(Time = sum(Time), Score = first(Score), Result = first(Result)) %>%
  right_join(playerStats, by = c('Team', 'Date', 'Player'))

save(playerStats, file = '../Data/playerStats.RData')

library(RCurl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

load('../Data/detailedStats.RData')

players = unique(detailedStats$Player)
playedHeroes = lapply(players, function(p) {
  df = filter(detailedStats, Player == p) %>% group_by(Hero) %>%
    summarise(Time = sum(Time)) %>% arrange(desc(Time)) %>%
    filter(Time > 10)
  return(df$Hero)
})
names(playedHeroes) = players

heroes = unique(detailedStats$Hero)
teams = unique(detailedStats$Team)
teamColors = c('olive', 'red', 'purple', 'black', 'yellow', 'aqua', 'teal',
               'maroon', 'orange', 'green', 'blue', 'light-blue')
names(teamColors) = teams

photoURLs = lapply(players, function(p) {
  url1 = sprintf('https://www.winstonslab.com/pics/players/owl_%s.png', str_to_lower(p))
  url2 = sprintf('https://www.winstonslab.com/pics/players/%s.png', str_to_lower(p))
  photoURL = ifelse(url.exists(url1), url1, url2)
  if(!url.exists(photoURL)) photoURL = 'emptyPortrait.png'
  return(photoURL)
})
names(photoURLs) = players

save(players, playedHeroes, heroes, teams, teamColors, photoURLs, file = '../Data/savedObjects.RData')

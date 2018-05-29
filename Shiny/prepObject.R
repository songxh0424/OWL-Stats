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
    filter(Time > 30)
  return(df$Hero)
})
names(playedHeroes) = players

heroes = unique(detailedStats$Hero)
teams = unique(detailedStats$Team)
teamColors = c('olive', 'red', 'purple', 'black', 'yellow', 'aqua', 'teal',
               'maroon', 'orange', 'green', 'blue', 'light-blue')
names(teamColors) = teams
teamTrueColors = c('#2A7230', '#FC4C01', '#381360', '#000000', '#AA8A00', '#0071CD',
                   '#59CBE8', '#AF272F', '#FF9E1B', '#97D700', '#0F57EA', '#174B97')
names(teamTrueColors) = teams

photoURLs = lapply(players, function(p) {
  url1 = sprintf('https://www.winstonslab.com/pics/players/owl_%s.png', str_to_lower(p))
  url2 = sprintf('https://www.winstonslab.com/pics/players/%s.png', str_to_lower(p))
  url3 = sprintf('https://www.winstonslab.com/pics/players/%s1.png', str_to_lower(p))
  photoURL = ifelse(url.exists(url3), url3, ifelse(url.exists(url1), url1, url2))
  if(!url.exists(photoURL)) photoURL = 'emptyPortrait.png'
  return(photoURL)
})
names(photoURLs) = players

save(players, playedHeroes, heroes, teams, teamColors, teamTrueColors,
     photoURLs, file = '../Data/savedObjects.RData')

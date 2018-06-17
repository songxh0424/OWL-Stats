library(RCurl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

load('../Data/detailedStats.RData')
load('../Data/heroStats.RData')

## players, teams, heroes
players = unique(detailedStats$Player)
playedHeroes = lapply(players, function(p) {
  df = filter(detailedStats, Player == p) %>% group_by(Hero) %>%
    summarise(`Time(min.)` = sum(`Time(min.)`)) %>% arrange(desc(`Time(min.)`)) %>%
    filter(`Time(min.)` > 30)
  return(df$Hero)
})
names(playedHeroes) = players

heroes = unique(detailedStats$Hero)
teams = unique(detailedStats$Team)

## team colors
teamColors = c('olive', 'red', 'purple', 'black', 'yellow', 'aqua', 'teal',
               'maroon', 'orange', 'green', 'blue', 'light-blue')
names(teamColors) = teams
teamTrueColors = c('#2A7230', '#FC4C01', '#381360', '#000000', '#AA8A00', '#0071CD',
                   '#59CBE8', '#AF272F', '#FF9E1B', '#97D700', '#0F57EA', '#174B97')
names(teamTrueColors) = teams

## player photos
photoURLs = lapply(players, function(p) {
  url1 = sprintf('https://www.winstonslab.com/pics/players/owl_%s.png', str_to_lower(p))
  url2 = sprintf('https://www.winstonslab.com/pics/players/%s.png', str_to_lower(p))
  url3 = sprintf('https://www.winstonslab.com/pics/players/%s1.png', str_to_lower(p))
  url4 = sprintf('https://www.winstonslab.com/pics/players/%s.jpg', str_to_lower(p))
  photoURL = ifelse(url.exists(url3), url3, ifelse(url.exists(url1), url1, ifelse(url.exists(url2), url2, url4)))
  if(!url.exists(photoURL)) photoURL = 'emptyPortrait.png'
  return(photoURL)
})
names(photoURLs) = players

## all players most played heroes
top3Heroes = lapply(players, function(p) {
  df = heroStats[[1]] %>% filter(Player == p, Hero != 'All Heroes') %>%
    select(Hero, `Hero Usage`) %>% arrange(desc(`Hero Usage`))
  df = df[1:3, ] %>% mutate(`Hero Usage` = `Hero Usage` %>% paste0('%'))
  return(df)
}) 
names(top3Heroes) = players

save(players, playedHeroes, heroes, teams, teamColors, teamTrueColors,
     photoURLs, top3Heroes, file = '../Data/savedObjects.RData')

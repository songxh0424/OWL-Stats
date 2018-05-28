library(reticulate)
library(jsonlite)
library(dplyr)
library(progress)

source_python('./MatchStats.py')

matchIDs = scrapeMatchID() %>% rev()

pd2R = function(df.pd) {
  df = fromJSON(df.pd$to_json(orient = 'records'))
  return(df)
}

allMatchStats = list()
pb = progress_bar$new(format = "  scraping [:bar] :percent in :elapsed",
                      total = length(matchIDs), clear = FALSE, width = 80)
for(id in matchIDs) {
  ## print(paste('scraping match:', id))
  pb$tick()
  matchStats = scrapeMatchStats(id)
  matchStats$playerStats$team1 = pd2R(matchStats$playerStats$team1)
  matchStats$playerStats$team2 = pd2R(matchStats$playerStats$team2)
  matchStats$detailedStats = pd2R(matchStats$detailedStats)[, 1:16]
  allMatchStats[[id]] = matchStats
}

save(allMatchStats, file = '../Data/allMatchStats.RData')

heroStats = scrapeHeroStats()

save(heroStats, file = '../Data/heroStatsRaw.RData')

library(reticulate)
library(jsonlite)
library(dplyr)
library(progress)

source_python('./MatchStats.py')

load('../Data/matchIDs.RData')
load('../Data/allMatchStats.RData')
## matchIDs.old = matchIDs
## matchIDs = scrapeMatchID() %>% rev()
## matchIDs.new = setdiff(matchIDs, matchIDs.old)
matchIDs.new = as.character(2632:2667)

pd2R = function(df.pd) {
  df = fromJSON(df.pd$to_json(orient = 'records'))
  return(df)
}

pb = progress_bar$new(format = "  scraping [:bar] :percent in :elapsed",
                      total = length(matchIDs.new), clear = FALSE, width = 80)
for(id in matchIDs.new) {
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

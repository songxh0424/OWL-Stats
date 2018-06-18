dbSidebar = dashboardSidebar(
  useShinyjs(),
  width = 250,
  sidebarMenu(
    convertMenuItem(
      menuItem(
        selected = TRUE, startExpanded = T,
        'Player Stats', tabName = 'players', icon = icon('list'),
        selectizeInput('player', label = 'Player', choices = players, selected = 'Saebyeolbe',
                       options = list(maxOptions = 5, placeholder = 'Input a player\'s name')),
        uiOutput('playedHero'),
        selectInput('player_stage', label = 'Stage', choices = c('All Stages', paste('Stage', 1:4))),
        br()
      ),
      'players'
    ),
    convertMenuItem(
      menuItem(
        selected = FALSE, startExpanded = F,
        'Heroe Stats', tabName = 'heroes', icon = icon('list'),
        selectizeInput('hero', label = 'Hero', choices = heroes, selected = 'Widowmaker',
                       options = list(maxOptions = 5, placeholder = 'Input a player\'s name')),
        selectInput('hero_stat', label = 'Statistic', choices = names(heroStats[[1]])[-(1:3)],
                    selected = 'Kills per 10 min'),
        selectInput('hero_stage', label = 'Stage', choices = c('All Stages', paste('Stage', 1:4))),
        br()
      ),
      'heroes'
    ),
    ## menuItem('League Overview', tabName = 'league', icon = icon('list')),
    tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')))
  )
)

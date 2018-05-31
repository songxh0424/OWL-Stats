dbSidebar = dashboardSidebar(
  useShinyjs(),
  width = 250,
  sidebarMenu(
    convertMenuItem(
      menuItem(
        selected = TRUE, startExpanded = T,
        'Player Stats', tabName = 'players', icon = icon('bar-chart-o'),
        selectizeInput('player', label = 'Player', choices = players, selected = 'Saebyeolbe',
                       options = list(maxOptions = 5, placeholder = 'Input a player\'s name')),
        uiOutput('playedHero'),
        br()
      ),
      'players'
    ),
    convertMenuItem(
      menuItem(
        selected = FALSE, startExpanded = F,
        'Heroe Stats', tabName = 'heroes', icon = icon('bar-chart'),
        selectizeInput('hero', label = 'Hero', choices = heroes, selected = 'Widowmaker',
                       options = list(maxOptions = 5, placeholder = 'Input a player\'s name')),
        selectInput('hero_stat', label = 'Stat', choices = names(heroStats)[-(1:3)],
                    selected = 'Kills per 10 min'),
        br()
      ),
      'heroes'
    ),
    menuItem('Fun Facts', tabName = 'facts', icon = icon('info-circle')),
    tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')))
  )
)

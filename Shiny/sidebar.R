dbSidebar = dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem('Players Stats', tabName = 'players', icon = icon('bar-chart-o'),
             selectizeInput('search_player', label = 'Player', choices = players, selected = 'Saebyeolbe',
                            options = list(maxOptions = 5, placeholder = 'Input a player\'s name')),
             uiOutput('select_played_hero'),
             br()
             ),
    menuItem('Heroes Stats', tabName = 'heroes', icon = icon('bar-chart'),
             selectizeInput('search_hero', label = 'Hero', choices = heroes, selected = 'Widowmaker',
                            options = list(maxOptions = 5, placeholder = 'Input a player\'s name')),
             br()
             ),
    menuItem('Fun Facts', tabName = 'facts', icon = icon('info-circle')),
    tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')))
  )
)

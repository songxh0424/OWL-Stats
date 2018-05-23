dbSidebar = dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem('Players Stats', tabName = 'players', icon = icon('bar-chart-o')),
    menuItem('Heroes Stats', tabName = 'heroes', icon = icon('bar-chart')),
    menuItem('Fun Facts', tabName = 'facts', icon = icon('info-circle')),
    ## selectizeInput(inputId = 'search_actor', label = NULL, choices = unique(acts$Actor), selected = 'Leonardo DiCaprio',
    ##                options = list(maxOptions = 5, placeholder = 'Input an actor\'s name')),
    tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')))
  )
)

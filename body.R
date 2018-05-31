dbBody = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  # use this in case want to set all menuItems collapsed at startup
  ## tags$head(
  ##   tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "none");})'))
  ## ),
  tabItems(
    tabItem(
      tabName = 'players',
      fluidRow(
        column(
          width = 10, offset = 1, align = 'center',
          uiOutput('playerCard'),
          box(title = 'Stats Progression over Season 1', solidHeader = T, width = NULL, collapsible = T,
              status = 'success', plotlyOutput('scatter_player', width = '85%', height = 600))
        )
      )
    ),
    tabItem(
      tabName = 'heroes',
      fluidRow(
        column(
          width = 10, offset = 1, align = 'center',
          box(title = 'Players Ranked by Hero Stats', solidHeader = T, width = NULL, collapsible = T,
              status = 'success', plotlyOutput('bar_heroes', width = '90%', height = 600)),
          box(title = 'Players Consistency by Hero', solidHeader = T, width = NULL, collapsible = T,
              status = 'success', plotlyOutput('box_heroes', width = '90%', height = 600))
        )
      )
    )
  )
)

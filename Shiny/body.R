dbBody = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  fluidRow(
    column(
      width = 10, offset = 1, align = 'center',
      uiOutput('playerCard'),
      box(title = 'Scatter Plot', solidHeader = T, width = NULL, collapsible = T,
          status = 'success', plotlyOutput('scatter_player', height = 600, width = '85%'))
    )
  )
)

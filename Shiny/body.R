dbBody = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  fluidRow(
    column(
      width = 12, offset = 0, align = 'center',
      box(title = 'Scatter Plot', solidHeader = T, width = NULL, collapsible = T,
          status = 'success', plotlyOutput('scatter_player', height = 600, width = '85%'))
    )
  )
)

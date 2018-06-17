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
          uiOutput('playerCard') %>% withSpinner(),
          box(title = 'Stats Progression over Season 1', solidHeader = T, width = NULL,
              collapsible = T, status = 'success',
              plotlyOutput('scatter_player', width = '85%', height = 600) %>% withSpinner()),
          box(title = 'Hero Usage during Season 1', solidHeader = T, width = NULL,
              collapsible = T, status = 'success',
              plotlyOutput('bar_heroUsage', width = '85%', height = 600) %>% withSpinner()),
          box(title = 'Comparison with Other Players', solidHeader = T, width = NULL,
              collapsible = T, status = 'success',
              dropdown(
                textInput('time_thres5', label = "Player's Minimum Time Played", value = 30),
                uiOutput('selectize_player2'),
                style = "unite", icon = icon("gear"), status = "primary", width = "300px"
              ),
              box(title = 'Comparison with League Average', status = 'primary',
                  formattableOutput('table_compare_avg', height = 400) %>% withSpinner()),
              box(title = 'Comparison with Another Player', status = 'primary',
                  formattableOutput('table_compare', height = 400) %>% withSpinner()))
        )
      )
    ),
    tabItem(
      tabName = 'heroes',
      fluidRow(
        column(
          width = 10, offset = 1, align = 'center',
          box(title = 'Players Ranked by Hero Stats', solidHeader = T, width = NULL,
              collapsible = T, status = 'success',
              dropdown(
                textInput('time_thres2', label = 'Minimum Time Played (min.)', value = 30),
                pickerInput("picker_teams1",  label = "Select Teams to Include", choices = teams, 
                            options = list(`actions-box` = TRUE, size = 12, `selected-text-format` = "count > 2"), 
                            selected = teams, multiple = TRUE),
                actionButton('confirm2', label = 'Confirm Changes', width = '150px', status = 'primary'),
                style = "unite", icon = icon("gear"), status = "primary", width = "300px"
              ),
              plotlyOutput('bar_heroes', width = '90%', height = 600) %>% withSpinner()),
          box(title = 'Players Consistency by Hero', solidHeader = T, width = NULL,
              collapsible = T, status = 'success',
              dropdown(
                textInput('time_thres3', label = 'Minimum Total Time Played (min.)', value = 30),
                textInput('time_thres4', label = 'Minimum Match Time Played (min.)', value = 3),
                pickerInput("picker_teams2",  label = "Select Teams to Include", choices = teams, 
                            options = list(`actions-box` = TRUE, size = 12, `selected-text-format` = "count > 2"), 
                            selected = teams, multiple = TRUE),
                radioGroupButtons('toggle_violin', label = "Choose Plot Type",
                                  choices = c('Box Plot', 'Violin Plot'), status = "primary",
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))),
                actionButton('confirm3', label = 'Confirm Changes', width = '150px', status = 'primary'),
                style = "unite", icon = icon("gear"), status = "primary", width = "300px"
              ),
              plotlyOutput('box_heroes', width = '90%', height = 600) %>% withSpinner())
        )
      )
    )
  )
)

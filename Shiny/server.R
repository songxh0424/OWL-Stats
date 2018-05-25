function(input, output, session) {
  output$playedHero = renderUI(
    selectInput('playedHero', label = 'Hero', choices = playedHeroes[[input$player]])
  )

  df = reactive({
    detailedStats %>%
      filter(Player == input$player, Hero == input$playedHero) %>%
      mutate(`K/D` = `K/10` / `D/10`) %>%
      gather(key = Var, value = Stats, `FWin%`, `K/10`, `D/10`, `K/D`) %>%
      mutate(Var = factor(Var, levels = c('FWin%', 'K/10', 'D/10', 'K/D')))
  })

  output$playerCard = renderUI({
    team = unique(df()$Team) %>% last()
    box(
      title = NULL, background = teamColors[[team]],
      solidHeader = F, width = NULL,
      fluidRow(
        column(
          width = 5, align = 'left',
          img(src = photoURLs[[input$player]], width = 300)
        ),
        column(
          width = 4, align = 'left',
          h2(input$player),
          p()
        )
      )
    )
  })

  output$scatter_player = renderPlotly({
    p = df() %>% ggplot(aes(x = Match, y = Stats)) +
      geom_line(color = '#386cb0') + geom_point(aes(text = Opponent, score = Score), color = "#386cb0") +
      facet_grid(Var ~ ., scales = 'free_y') +
      ylab('')
    layout(ggplotly(p), margin = list(l = 50))
  })
}

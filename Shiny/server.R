function(input, output, session) {
  output$select_played_hero = renderUI(
    selectInput('select_played_hero', label = 'Hero', choices = playedHeroes[[input$search_player]])
  )

  df = reactive({
    detailedStats %>%
      filter(Player == input$search_player, Hero == input$select_played_hero) %>%
      mutate(`K/D` = `K/10` / `D/10`) %>%
      gather(key = Var, value = Stats, `FWin%`, `K/10`, `D/10`, `K/D`) %>%
      mutate(Var = factor(Var, levels = c('FWin%', 'K/10', 'D/10', 'K/D')))
  })
  output$scatter_player = renderPlotly({
    p = df() %>% ggplot(aes(x = Match, y = Stats)) +
      geom_line(color = '#386cb0') + geom_point(aes(text = Opponent, score = Score), color = "#386cb0") +
      facet_grid(Var ~ ., scales = 'free_y') +
      ylab('')
    layout(ggplotly(p), margin = list(l = 50))
  })
}

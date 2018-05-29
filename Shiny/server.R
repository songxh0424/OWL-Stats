function(input, output, session) {
  observeEvent('players' %in% input$tabs, {
    shinyjs::hide(selector = '#sidebarItemExpanded > ul > li:nth-child(2) > ul')
  })

  output$playedHero = renderUI(
    selectInput('playedHero', label = 'Hero', choices = c('All Heroes', playedHeroes[[input$player]]))
  )

  df = reactive({
    matches = data.frame(Match = 1:max(detailedStats$Match))
    out = detailedStats %>%
      filter(Player == input$player, Hero == input$playedHero, Time >= 3) %>%
      mutate(`K/D` = `K/10` / `D/10`) 
    out %>% right_join(matches, by = 'Match') %>%
      replace_na(list(Player = input$player, `FWin%` = 0, `K/10` = 0, `D/10` = 0,
                      `K/D` = 0, Result = 'Not played', Team = last(out$Team))) %>%
      mutate(Result = factor(Result, levels = c('Win', 'Lose', 'Not played'))) %>%
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
    p = df() %>% ggplot(aes(x = Match, y = Stats)) + geom_line(color = '#386cb0') +
      geom_point(aes(text = Opponent, score = Score, time = Time, color = Result), size = 1) +
      facet_grid(Var ~ ., scales = 'free_y') +
      scale_color_manual(values = c('green', 'red', 'grey')) +
      ylab('')
    p = plot_custom(p, legend.pos = 'bottom', color = FALSE) + theme(legend.title = element_blank())
    layout(ggplotly(p, tooltip = c('text', 'score', 'time', 'x', 'y')),
           margin = list(l = 50), legend = list(orientation = 'h', x = 0.3, y = 1.1))
  })

  output$bar_heroes = renderPlotly({
    dat = heroStats %>% filter(Hero == input$hero, `Time(min.)` > 30) %>%
      select(Player, Team, input$hero_stat)
    dat = arrange(dat, dat[[input$hero_stat]]) %>%
      mutate(Player = factor(Player, levels = Player))
    p = dat %>% ggplot(aes_string('Player', paste0('`', input$hero_stat, '`'))) +
      geom_col(aes(fill = Team)) + coord_flip()
    p = plot_custom(p, color = FALSE) +
      theme(legend.position = 'none') + xlab('') +
      scale_fill_manual(values = teamTrueColors, breaks = teams)
    ggplotly(p, tooltip = c('x', 'fill', 'y'))
  })
}

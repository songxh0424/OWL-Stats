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
      mutate(Var = factor(Var, levels = c('FWin%', 'K/10', 'D/10', 'K/D'))) %>%
      mutate(Time = round(Time, 2), Stats = round(Stats, 2))
  })

  output$playerCard = renderUI({
    team = unique(df()$Team) %>% last()
    top3 = top3Heroes[[input$player]]
    highlights = heroStats %>% filter(Player == input$player, Hero == 'All Heroes')
    box(
      title = NULL, background = teamColors[[team]],
      solidHeader = F, width = NULL,
      fluidRow(
        column(width = 12, align = 'center', h1(input$player))
      ),
      fluidRow(
        column(
          width = 4, align = 'left',
          img(src = photoURLs[[input$player]], width = 320, height = 320)
        ),
        column(
          width = 4, align = 'left', br(),
          valueBox(top3[1, 2], top3[1, 1], icon = icon('bar-chart'), width = 12),
          valueBox(top3[2, 2], top3[2, 1], icon = icon('bar-chart'), width = 12),
          valueBox(top3[3, 2], top3[3, 1], icon = icon('bar-chart'), width = 12)
        ),
        column(
          width = 4, align = 'left', br(),
          valueBox(highlights$`Win Rate` %>% paste0('%'), 'Win Rate', icon = icon('trophy'), width = 12),
          valueBox(highlights$`Kills per 10 min`, 'Kills per 10 min', icon = icon('user-times'), width = 12),
          valueBox(highlights$`Deaths per 10 min`, 'Deaths per 10 min', icon = icon('bed'), width = 12)
        )
      )
    )
  })

  output$scatter_player = renderPlotly({
    p = df() %>% ggplot(aes(x = Match, y = Stats)) + geom_line(color = '#386cb0') +
      geom_point(aes(text = Opponent, score = Score, time = Time, color = Result), size = 1) +
      facet_grid(Var ~ ., scales = 'free_y') +
      scale_color_manual(values = c('green', 'red', 'grey'), breaks = c('Win', 'Lose', 'Not played')) +
      geom_vline(xintercept = c(10.5, 20.5, 30.5), linetype = 'dashed', color = 'black', size = 0.1) +
      ylab('')
    p = plot_custom(p, legend.pos = 'bottom', color = FALSE) +
      theme(legend.title = element_blank(), panel.grid.major.x = element_blank()) +
      ggtitle(input$playedHero)
    layout(ggplotly(p, tooltip = c('text', 'score', 'time', 'x', 'y')),
           margin = list(l = 50), legend = list(orientation = 'h', x = 0.3, y = 1.07))
  })

  output$bar_heroes = renderPlotly({
    dat = heroStats %>% filter(Hero == input$hero, `Time(min.)` > 30) %>%
      mutate(`Time(min.)` = round(`Time(min.)`, 2)) %>%
      select(Player, Team, input$hero_stat)
    dat = arrange(dat, dat[[input$hero_stat]]) %>%
      mutate(Player = factor(Player, levels = Player))
    p = dat %>% ggplot(aes_string('Player', paste0('`', input$hero_stat, '`'))) +
      geom_col(aes(fill = Team), width = 0.8) + coord_flip()
    p = plot_custom(p, color = FALSE) +
      theme(legend.position = 'none') + xlab('') + ggtitle(input$hero) + 
      scale_fill_manual(values = teamTrueColors, breaks = teams)
    ggplotly(p, tooltip = c('x', 'fill', 'y'))
  })
}

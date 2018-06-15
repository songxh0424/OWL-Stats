function(input, output, session) {
  observeEvent('players' %in% input$tabs, {
    shinyjs::hide(selector = '#sidebarItemExpanded > ul > li:nth-child(2) > ul')
  })

  output$playedHero = renderUI(
    selectInput('playedHero', label = 'Hero', choices = playedHeroes[[input$player]],
                selected = 'All Heroes')
  )

  df = reactive({
    stage = switch(input$player_stage, 'All Stages' = 1:4, as.numeric(str_sub(input$player_stage, start = -1)))
    matches = data.frame(Match = 1:max(detailedStats$Match)) %>%
      filter(ceiling(Match / 10) %in% stage)
    out = detailedStats %>%
      filter(Player == input$player, Hero == input$playedHero, `Time(min.)` >= 3, Stage %in% stage) %>%
      mutate(`Kills/Deaths` = `Kills per 10 min` / `Deaths per 10 min`) 
    out %>% right_join(matches, by = 'Match') %>%
      replace_na(list(Player = input$player, `Fight Win Rate` = 0, `Kills per 10 min` = 0, `Deaths per 10 min` = 0,
                      `Kills/Deaths` = 0, Result = 'Not played', Team = last(out$Team))) %>%
      mutate(Result = factor(Result, levels = c('Win', 'Lose', 'Not played'))) %>%
      gather(key = Var, value = Stats, `Fight Win Rate`, `Kills per 10 min`, `Deaths per 10 min`, `Kills/Deaths`) %>%
      mutate(Var = factor(Var, levels = c('Fight Win Rate', 'Kills per 10 min', 'Deaths per 10 min', 'Kills/Deaths'))) %>%
      mutate(`Time(min.)` = round(`Time(min.)`, 2), Stats = round(Stats, 2))
  })

  output$playerCard = renderUI({
    team = unique(filter(detailedStats, Player == input$player)$Team) %>% last()
    top3 = top3Heroes[[input$player]]
    highlights = heroStats[[1]] %>% filter(Player == input$player, Hero == 'All Heroes')
    box(
      title = NULL, background = teamColors[[team]],
      solidHeader = F, width = NULL,
      fluidRow(
        column(width = 12, align = 'center', h1(em(input$player)))
      ),
      fluidRow(
        column(
          width = 4, align = 'left',
          img(src = photoURLs[[input$player]], width = 320, height = 320)
        ),
        column(
          width = 4, align = 'left', br(),
          valueBox(top3[1, 2], top3[1, 1], icon = icon('bar-chart'), width = 12, color = 'fuchsia'),
          valueBox(top3[2, 2], top3[2, 1], icon = icon('bar-chart'), width = 12, color = 'fuchsia'),
          valueBox(top3[3, 2], top3[3, 1], icon = icon('bar-chart'), width = 12, color = 'fuchsia')
        ),
        column(
          width = 4, align = 'left', br(),
          valueBox(highlights$`Win Rate` %>% paste0('%'), 'Win Rate', icon = icon('trophy'), width = 12, color = 'lime'),
          valueBox(highlights$`Kills per 10 min`, 'Kills per 10 min', icon = icon('user-times'), width = 12, color = 'lime'),
          valueBox(highlights$`Deaths per 10 min`, 'Deaths per 10 min', icon = icon('bed'), width = 12, color = 'lime')
        )
      )
    )
  })

  output$scatter_player = renderPlotly({
    p = df() %>% ggplot(aes(x = Match, y = Stats)) + geom_line(color = '#386cb0') +
      geom_point(aes(text = Opponent, score = Score, time = `Time(min.)`, color = Result), size = 1) +
      facet_grid(Var ~ ., scales = 'free_y') +
      scale_color_manual(values = c('green', 'red', 'grey'), breaks = c('Win', 'Lose', 'Not played')) +
      ylab('')
    if(input$player_stage == 'All Stages')
      p = p + geom_vline(xintercept = c(10.5, 20.5, 30.5), linetype = 'dashed', color = 'black', size = 0.1)
    p = plot_custom(p, legend.pos = 'bottom', color = FALSE) +
      theme(legend.title = element_blank(), panel.grid.major.x = element_blank()) +
      ggtitle(paste(input$playedHero, '-', input$player_stage))
    layout(ggplotly(p, tooltip = c('text', 'score', 'time', 'x', 'y'), height = 600),
           margin = list(l = 50), legend = list(orientation = 'h', x = 0.3, y = 1.07))
  })

  output$bar_heroUsage = renderPlotly({
    dat = heroUsage %>% filter(Player == input$player, Stage == input$player_stage) %>%
      mutate(Hero = factor(Hero, levels = rev(Hero)))
    p = dat %>% ggplot(aes(Hero, Usage)) + geom_col(aes(z = `Time(min.)`), width = 0.7) +
      coord_flip() + ylab('% of Time Played') +
      ggtitle(sprintf('Hero Usage - %s', input$player_stage))
    p = plot_custom(p, color = FALSE)
    ggplotly(p, tooltip = c('x', 'y', 'z'))
  })

  output$bar_heroes = renderPlotly({
    dat = heroStats[[input$hero_stage]] %>% filter(Hero == input$hero, `Time(min.)` > 30) %>%
      mutate(`Time(min.)` = round(`Time(min.)`, 2)) %>%
      select(Player, Team, input$hero_stat, `Time(min.)`)
    dat = arrange(dat, dat[[input$hero_stat]]) %>%
      mutate(Player = factor(Player, levels = Player))
    p = dat %>% ggplot(aes_string('Player', paste0('`', input$hero_stat, '`'), z = '`Time(min.)`')) +
      geom_col(aes(fill = Team), width = 0.7) + coord_flip()
    p = plot_custom(p, color = FALSE) +
      theme(legend.position = 'right') + xlab('') + ggtitle(input$hero) + 
      scale_fill_manual(values = teamTrueColors, breaks = teams)
    ggplotly(p)
  })

  output$box_heroes = renderPlotly({
    validPlayers = heroStats[[input$hero_stage]] %>% filter(Hero == input$hero, `Time(min.)` > 30) %>%
      select(Player, input$hero_stat)
    validPlayers = arrange(validPlayers, validPlayers[[input$hero_stat]]) %>%
      select(Player) %>% unlist()
    dat = detailedStats %>% filter(Hero == input$hero, Player %in% validPlayers, `Time(min.)` > 3) %>%
      mutate(Player = factor(Player, levels = validPlayers)) %>%
      select(Player, Team, input$hero_stat)
    p = dat %>% ggplot(aes_string('Player', paste0('`', input$hero_stat, '`'), color = 'Team')) +
      geom_boxplot(width =0.8) + coord_flip()
    p = plot_custom(p, color = TRUE) +
      theme(legend.position = 'right') + xlab('') + ggtitle(input$hero) + 
      scale_color_manual(values = teamTrueColors, breaks = teams)
    ggplotly(p)
  })
}

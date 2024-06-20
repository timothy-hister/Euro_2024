is_local = Sys.getenv('SHINY_PORT') == ""
params = list(import_round_1 = F, import_round_2 = F, import_games = F, scrape = T, authenticate = !is_local)

pacman::p_load(tidyverse, gt, ggiraph, reactable, RColorBrewer, shiny, htmltools, bslib, shinyWidgets, shinymanager, shinycssloaders, rvest, shinyjs)

`%,%` = function(a,b) paste0(a,b)
`%,,%` = function(a,b) paste(a,b)

source(here::here() %,% "/functions.R", local = T)
source(here::here() %,% "/calculations.R", local = T)
source(here::here() %,% "/ui.R", local = T)

server = function(input, output, session) {
  shinyjs::hide(id = "teams")
  shinyjs::hide(id = "locations")

  #if (!is_local) nav_remove(id = "navbar", target = "Prints")
  #nav_select(id = "navbar", selected = "Welcome")

  if (params$authenticate) {
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )

    output$auth_output <- renderPrint({
      reactiveValuesToList(res_auth)
    })
  }

  observeEvent(input$navbar, if (input$navbar == "Fun Graph") show("graph_player") else hide("graph_player"))

  prev_game = reactive(last_games_of_day %>% filter(game_id == req(input$as_of_game)) %>% pull(prev_game_id))

  last_rank = reactive(if (req(input$as_of_game) == 0) {
    select(players, player_id) %>% mutate(last_rank = 1L)
    } else {
      standings %>%
        filter(game_id == prev_game()) %>%
        select(player_id, last_rank = rank) # work????
    }
  )


  ## STANDINGS

  standings_tbl1 = reactive(
    suppressMessages(
    standings %>%
        inner_join(players) %>%
        filter(name %in% input$players) %>%
        left_join(games) %>%
        filter(game_id == input$as_of_game) %>%
        group_by(player_id) %>%
        arrange(game_id) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        arrange(rank) %>%
        left_join(last_rank()) %>%
        mutate(last_rank = case_when(is.na(last_rank) ~ 1L, T ~ last_rank)) %>%
        mutate(rank_change = last_rank - rank) %>%
        select(player_id, rank, rank_change, nickname, total_points, last_rank, max_points) %>%
        rename(name = nickname)
  ))

  #output$standings_tbl1 = renderTable(standings_tbl1())

  t1 = reactive(reactable(standings_tbl1(), columns = t1_cols(standings_tbl1()), searchable = TRUE, highlight = TRUE, onClick = 'expand', defaultPageSize = nrow(players), rowStyle = list(cursor = "pointer"), defaultExpanded = F, details = function(index) inner_tables_list[[standings_tbl1()$player_id[index]]]))

  output$standings = renderReactable(t1())


  ## FUN GRAPH

  gg1 = reactive(
    suppressMessages(
    players %>%
      filter(name %in% input$players) %>%
      inner_join(points) %>%
      filter(game_id <= input$as_of_game) %>%
      #mutate(name = factor(name, ordered = T)) %>%
      ggplot(aes(x = game_id, y = total_points, color = name, alpha=.5)) +
      #geom_line_interactive(aes(tooltip = nickname, data_id = name)) +
      #geom_smooth_interactive(aes(tooltip = nickname, data_id = name)) +
      geom_line() +
      geom_point() +
      ggthemes::theme_clean() +
      labs(x = "Game #", y = "Total Points", color = NULL, alpha = NULL, linewidth = NULL) +
      guides(alpha = 'none') +
      scale_x_continuous(breaks = scores$game_id) +
      scale_color_viridis_d(option = 'rocket') +
      theme(legend.position = ifelse(length(input$players) <= 5, 'right', 'none')) +
      geom_line(data = players %>% filter(name == input$graph_player) %>% inner_join(points) %>% filter(game_id <= input$as_of_game), linewidth=5, color='black')
  ))

  gg = reactive(girafe(ggobj = gg1(), options = list(opts_hover(css = "stroke: black; stroke-width: 5px;"), opts_hover_inv(css = "opacity:0.1;"))))

  output$graph = renderGirafe(gg())


  ## GAMES

  output$games_tbl = renderReactable(
    suppressMessages(
    games %>%
      left_join(scores) %>%
      relocate(is_played, 1) %>%
      left_join(points) %>%
      group_by(round, game_id) %>%
      summarise(
        total_points = sum(points),
        avg_points = mean(points),
        perc_got_points = mean(points > 0)
      ) %>%
      ungroup() %>%
      right_join(games) %>%
      left_join(scores) %>%
      mutate(game = case_when(is.na(team_1) ~ NA_character_, T ~ team_1 %,,% "-" %,,% team_2)) %>%
      mutate(result = case_when(!is_played ~ NA_character_, round == 1 ~ score_1 %,% " - " %,% score_2, T ~ result)) %>%
      select(is_played, round, points_available, game_id, date, location, game,  result, total_points, avg_points, perc_got_points)) %>%

      reactable(
        columns = list(
          is_played = colDef(show = F),
          total_points = colDef(name = "Total # of points received"),
          avg_points = colDef(name = "Average # of points received", format = colFormat(digits = 2)),
          perc_got_points = colDef(name = "% of players who got >=1 points", format = colFormat(percent = T, digits = 0)),
          game = colDef(na = "", cell = function(value) {
            div(style = "display: flex; align-items: center;",
                print_flag(word(value, 1)),
                if (!is.na(value)) div("V", style = "fontWeight: 600; margin: 0 10px;"),
                print_flag(word(value, 3))
            )
          }, minWidth = 150)
        ),
        rowStyle = function(index) if (!games$is_played[index]) list(background = "rgba(0, 0, 0, 0.05)")
    )
  )

  #updateReactable("games_tbl", data = cars)

  ## WELCOME

  output$welcome = renderUI({
    suppressMessages({
    lucrative_game = points %>% group_by(game_id) %>% summarise(points = sum(points)) %>% arrange(desc(points)) %>% slice_head(n=1) %>% inner_join(games)
    lucrative_team = bind_rows(points %>% inner_join(games) %>% select(team = team_1, points), points %>% inner_join(games) %>% select(team = team_2, points)) %>% group_by(team) %>% summarise(points = sum(points)) %>% arrange(desc(points))


    vbs = list(
      value_box(title = "Number of Games Played", value = nrow(filter(games, is_played)), theme = "bg-gradient-purple-pink"),
      value_box(title = "Current Leader(s)", value = standings %>% filter(game_id == last_game) %>% filter(rank == 1) %>% inner_join(players) %>% pull(name) %>% paste(collapse=", "), p("With an impressive" %,,% (standings %>% filter(game_id == last_game) %>% filter(rank == 1) %>% inner_join(players) %>% pull(total_points)) %,,% "points"), theme = "bg-gradient-green-teal"),
      value_box(title = "Current 'Most Room For Improvement(s)'", value = standings %>% filter(game_id == last_game) %>% filter(rank == standings %>% filter(game_id == last_game) %>% pull(rank) %>% max()) %>% inner_join(players) %>% pull(name) %>% paste(collapse=", "), p("With a great-work-but-you-can-do-better!" %,,% (standings %>% filter(game_id == last_game) %>% filter(rank == standings %>% filter(game_id == last_game) %>% pull(rank) %>% max()) %>% inner_join(players) %>% pull(total_points) %>% unique()) %,,% "points"), theme = "bg-gradient-purple-red"),
      value_box(title = "Average # of Points Received Per Game", value = round(mean(points$points), 2), theme = "bg-gradient-cyan-purple"),
      value_box(title = "Most Lucrative Game So Far", value = lucrative_game$team_1 %,,% "versus" %,,% lucrative_game$team_2, p(lucrative_game$points %,,% "points received"), theme = "bg-gradient-blue-orange"),
      value_box(title = "Most Lucrative Team(s)", value = lucrative_team %>% filter(points == max(lucrative_team$points)) %>% pull(team) %>% paste(collapse = ", "), p(max(lucrative_team$points) %,,% "points received"), theme = "bg-gradient-orange-pink")
    )

    div(
      h3("Good" %,,% ifelse(hour(now()) < 12, 'morning', 'afternoon') %,,% "sports fans!"),
      br(),
      br(),
      layout_column_wrap(width = 1/3, !!!vbs, fill = F)
    )
    })})


  # flip to correct page in tables
  # observeEvent(input$games_tbl, {
  #   req(input$games_tbl)
  #   print('hello')
  #   updateReactable("games_tbl", page=ceiling(last_game / 10))
  # })

    # walk(players$player_id, function(id) updateReactable("#it_" %,% id, page=ceiling(last_game / 10)))

  updateReactable("games_tbl", page=ceiling(last_game / 10))

}


shinyApp(ui, server)
is_for_shinyapps = F

params = list(import_round_1 = F, import_round_2 = F, import_games = F, scores_round_2_url = NULL)
params = if (is_for_shinyapps) append(params, list(scores_round_1_url = "https://raw.githubusercontent.com/timothy-hister/Euro_2024/main/scores/round_1_scores.csv", authenticate = T)) else append(params, list(scores_round_1_url = "scores/round_1_scores.csv", authenticate = F, sample_players = NULL))

pacman::p_load(tidyverse, gt, ggiraph, reactable, RColorBrewer, shiny, htmltools, bslib, shinyWidgets, shinymanager, shinycssloaders, rvest)

`%,%` = function(a,b) paste0(a,b)
`%,,%` = function(a,b) paste(a,b)

source(here::here() %,% "/functions.R", local = T)
source(here::here() %,% "/calculations.R", local = T)
source(here::here() %,% "/ui.R", local = T)

server = function(input, output, session) {
  if (params$authenticate) {
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )

    output$auth_output <- renderPrint({
      reactiveValuesToList(res_auth)
    })
  }

  observe(shinyjs::hide(id = "teams"))
  observe(shinyjs::hide(id = "locations"))

  prev_game = reactive(last_games_of_day %>% filter(game_id == req(input$as_of_game)) %>% pull(prev_game_id))

  last_rank = reactive(if (req(input$as_of_game) == 0) {
    select(players, player_id) %>% mutate(last_rank = 1L)
    } else {
      standings %>%
        filter(game_id == prev_game()) %>%
        select(player_id, last_rank = rank) # work????
    }
  )

  observe(print(last_rank()))
  #observe(print(input$as_of_game))

  standings_tbl1 = reactive(
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
  )

  observe(print(standings_tbl1()))

  t1 = reactive(reactable(standings_tbl1(), columns = t1_cols(standings_tbl1()), searchable = TRUE, highlight = TRUE, onClick = 'expand', rowStyle = list(cursor = "pointer"), defaultExpanded = F, details = function(index) inner_tables_list[[standings_tbl1()$player_id[index]]]))

  output$standings_tbl = renderReactable(t1())

  gg1 = reactive(
    players %>%
      inner_join(players %>% filter(name %in% input$players)) %>%
      inner_join(points) %>%
      filter(game_id <= input$as_of_game) %>%
      #mutate(name = factor(name, ordered = T)) %>%
      ggplot(aes(x = game_id, y = total_points, color = name)) +
      geom_line_interactive(aes(tooltip = nickname, data_id = name)) +
      #geom_smooth_interactive(aes(tooltip = nickname, data_id = name)) +
      geom_point() +
      ggthemes::theme_clean() +
      labs(x = "Game #", y = "Total Points", color = NULL) +
      scale_x_continuous(breaks = scores$game_id) +
      scale_color_viridis_d(option = 'rocket') +
      theme(legend.position = 'none')
  )

  gg = reactive(girafe(ggobj = gg1(), options = list(opts_hover(css = "stroke: black; stroke-width: 5px;"), opts_hover_inv(css = "opacity:0.1;"))))

  output$graph = renderGirafe(gg())

  # output$players = renderUI({
  #   if (length(input$players) == 0) return()
  #   accordions = map(input$players, function(player) {
  #     #df = make_inner_tbl1(players %>% filter(name == player))
  #     df = players %>%
  #       filter(name == player) %>%
  #       inner_join(preds) %>%
  #       inner_join(games) %>%
  #       left_join(scores) %>%
  #       left_join(points) %>%
  #       filter(game_id <= last_game)
  #
  #     vbs = list(
  #       #value_box(title = "Rank", tail(df$rank, 1), theme='purple'),
  #       value_box(title = "Rank", 1L, theme='purple', full_screen = TRUE, fill = TRUE, height = NULL),
  #       value_box(title = "Rank", 1L, theme='teal', full_screen = TRUE, fill = TRUE, height = NULL)
  #       #b2 = value_box(title = "Average Rank", mean(df$rank))
  #     )
  #
  #     accordion_panel(title = player, layout_columns(!!!vbs))
  #   })
  #
  #   accordion(accordions)
  # })




  # txt = "<h3>Good" %,,% ifelse(hour(now()) < 12, 'morning', 'afternoon') %,,% "sports fans!</h3><br><br>"
  # txt = txt %,% "Since last time," %,,% (games %>% filter(is_played, game_id > prev_game) %>% inner_join(scores) %>% nrow()) %,,% "games have been played.<br>"
  # txt = txt %,% (games %>%
  #     filter(is_played, game_id > prev_game) %>%
  #     inner_join(scores) %>%
  #     mutate(blurb = "In game" %,,% game_id %,% "," %,,% team_1 %,,% "played" %,,% team_2 %,% ", and" %,,% ifelse(result == "tie", "the result was a tie!", "the winner was " %,% result %,% "!")) %>%
  #     pull(blurb) %>%
  #     paste(collapse = "<br>"))
  #
  # txt = txt %,% "<br><br>" %,% (games %>%
  #   filter(!is_played) %>%
  #   arrange(game_id) %>%
  #   slice_head(n = 3) %>%
  #   ungroup() %>%
  #   inner_join(preds) %>%
  #   group_by(player_id) %>%
  #   inner_join(players) %>%
  #   select(round, game_id, date, location, team_1, team_2, name, pred_winner) %>%
  #   arrange(round, game_id, pred_winner) %>%
  #   group_by(round, game_id, date, location, team_1, team_2, pred_winner) %>%
  #   summarise(
  #     n_players = n_distinct(name),
  #     players = paste(name, collapse = ", ")) %>%
  #   ungroup() %>%
  #   mutate(players = str_replace(players, ",(?=[^,]+$)", " and")) %>%
  #   mutate(game_blurb = ifelse(game_id != lag(game_id) | row_number() == 1, "<br><br>Game #" %,% game_id %,,% "will be played on" %,,% date %,,% "in" %,,% location %,,% "between **" %,% team_1 %,% "** and **" %,% team_2 %,% "**. ", "")) %>%
  #   mutate(pred_blurb = n_players %,,% "people picked" %,,% pred_winner %,% ":" %,,% players %,% ".") %>%
  #   mutate(blurb = game_blurb %,% pred_blurb) %>%
  #   pull(blurb) %>%
  #   paste(collapse = "") %>%
  #   str_remove("^<br><br>"))
  #
  #
  # output$welcome = renderText(txt)




}

shinyApp(ui, server)
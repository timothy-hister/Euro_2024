pacman::p_load(tidyverse, gt, ggiraph, reactable, RColorBrewer, shiny, htmltools, bslib, shinyWidgets, shinymanager, googlesheets4)

`%,%` = function(a,b) paste0(a,b)
`%,,%` = function(a,b) paste(a,b)

params = list(import_round_1 = F, import_round_2 = F, import_games = F, import_scores = F, google_scores = T)

source(here::here() %,% "/tribbles.R", local = T)
source(here::here() %,% "/imports.R", local = T)
source(here::here() %,% "/ui.R", local = T)

#ui = secure_app(ui)

server = function(input, output, session) {

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  observe(if (!is.integer(input$as_of_game)) updateNumericInputIcon(session = session, inputId = "prev_as_of_game", value = last_game))

  standings_tbl1 = reactive(
    standings %>%
      inner_join(players) %>%
      inner_join(games) %>%
      filter(name %in% input$players) %>%
      filter(any(team_1 %in% input$teams, team_2 %in% input$teams)) %>%
      filter(location %in% input$locations) %>%
      filter(game_id == input$as_of_game) %>%
      group_by(player_id) %>%
      arrange(game_id) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      arrange(rank) %>%
      left_join(points %>%
          filter(game_id == prev_game) %>%
          select(points, player_id, last_rank = rank)
      ) %>%
      mutate(last_rank = case_when(is.na(last_rank) ~ 1L, T ~ last_rank)) %>%
      mutate(rank_change = last_rank - rank) %>%
      select(player_id, rank, rank_change, nickname, total_points, last_rank, max_points) %>%
      rename(name = nickname)
  )

  #observe(print(standings_tbl1()))

  t1 = reactive(
    standings_tbl1() %>%
      select(-last_rank) %>%
      reactable(
        columns = list(
          player_id = colDef(show = F),
          rank = colDef(
            header = "",
            width = 50,
            cell = function(value, index) {
              arrow = standings_tbl1()$rank_change[index]
              image = if (arrow == 0) icon("arrow-right") else if (arrow > 0) icon("arrow-up") else icon("arrow-down")
              color = if_else(arrow > 0, "#008000", if_else(arrow == 0, "orange", "#e00000"))
              div(
                div(value, style = list(float = "left", fontWeight = 600)),
                div(image, style = list(fontWeight = 600, color=color))
              )
            }
          ),
          rank_change = colDef(show = F),
          total_points = colDef(
            style = function(value) {
              if (length(unique(standings_tbl1()$total_points)) == 1) return(list(fontWeight = 600))
              normalized = (value - min(standings_tbl1()$total_points)) / (max(standings_tbl1()$total_points) - min(standings_tbl1()$total_points))
              color = PuOr_pal(normalized)
              list(background = color, fontWeight = 600, color = 'white')
            }
          )
        ),
        searchable = TRUE, highlight = TRUE, onClick = 'expand', rowStyle = list(cursor = "pointer"), defaultExpanded = F,
        details = function(index) {
          tbl = players %>% filter(player_id == standings_tbl1()$player_id[index])
          inner_tbl1 = suppressMessages(make_inner_tbl1(tbl))
        }
    )
  )

  output$standings_tbl = renderReactable(t1())

  gg1 = reactive(
    players %>%
      inner_join(players %>% filter(name %in% input$players)) %>%
      inner_join(points) %>%
      filter(game_id <= input$as_of_game) %>%
      ggplot(aes(x = game_id, y = total_points, color = name)) +
      geom_line_interactive(aes(tooltip = nickname, data_id = name)) +
      geom_point() +
      ggthemes::theme_clean() +
      labs(x = "Game #", y = "Total Points", color = NULL) +
      scale_x_continuous(breaks = scores$game_id) +
      scale_color_brewer(palette = 'PuOr')
  )

  gg = reactive(girafe(ggobj = gg1(), options = list(opts_hover(css = "stroke: black; stroke-width: 5px;"), opts_hover_inv(css = "opacity:0.1;"))))

  output$graph = renderGirafe(gg())


  txt = "<h3>Good" %,,% ifelse(hour(now()) < 12, 'morning', 'afternoon') %,,% "sports fans!</h3><br><br>"
  txt = txt %,% "Since last time," %,,% (games %>% filter(is_played, game_id > prev_game) %>% inner_join(scores) %>% nrow()) %,,% "games have been played.<br>"
  txt = txt %,% (games %>%
      filter(is_played, game_id > prev_game) %>%
      inner_join(scores) %>%
      mutate(blurb = "In game" %,,% game_id %,% "," %,,% team_1 %,,% "played" %,,% team_2 %,% ", and" %,,% ifelse(result == "tie", "the result was a tie!", "the winner was " %,% result %,% "!")) %>%
      pull(blurb) %>%
      paste(collapse = "<br>"))

  txt = txt %,% "<br><br>" %,% (games %>%
    filter(!is_played) %>%
    arrange(game_id) %>%
    slice_head(n = 3) %>%
    ungroup() %>%
    inner_join(preds) %>%
    group_by(player_id) %>%
    inner_join(players) %>%
    select(round, game_id, date, location, team_1, team_2, name, pred_winner) %>%
    arrange(round, game_id, pred_winner) %>%
    group_by(round, game_id, date, location, team_1, team_2, pred_winner) %>%
    summarise(
      n_players = n_distinct(name),
      players = paste(name, collapse = ", ")) %>%
    ungroup() %>%
    mutate(players = str_replace(players, ",(?=[^,]+$)", " and")) %>%
    mutate(game_blurb = ifelse(game_id != lag(game_id) | row_number() == 1, "<br><br>Game #" %,% game_id %,,% "will be played on" %,,% date %,,% "in" %,,% location %,,% "between **" %,% team_1 %,% "** and **" %,% team_2 %,% "**. ", "")) %>%
    mutate(pred_blurb = n_players %,,% "people picked" %,,% pred_winner %,% ":" %,,% players %,% ".") %>%
    mutate(blurb = game_blurb %,% pred_blurb) %>%
    pull(blurb) %>%
    paste(collapse = "") %>%
    str_remove("^<br><br>"))


  output$welcome = renderText(txt)




}

shinyApp(ui, server)

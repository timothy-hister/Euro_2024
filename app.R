is_local = Sys.getenv('SHINY_PORT') == ""
params = list(import_round_1 = F, import_round_2 = F, import_games = F, scrape = T, authenticate = !is_local)

pacman::p_load(tidyverse, gt, ggiraph, reactable, RColorBrewer, shiny, htmltools, bslib, shinyWidgets, shinymanager, shinycssloaders, rvest, shinyjs, shinyalert)

`%,%` = function(a,b) paste0(a,b)
`%,,%` = function(a,b) paste(a,b)
pa = function(x, n=10000) if (methods::is(x, "tbl_df")) print(x, width=Inf, n=n) else rlang::with_options(max.print = n, print(x))

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

  shinyjs::hide(id = "teams")
  shinyjs::hide(id = "locations")

  observeEvent(input$navbar, if (input$navbar == "Fun Graph") shinyjs::show("graph_player") else shinyjs::hide("graph_player"))
  observeEvent(input$navbar, if (input$navbar == "Games") shinyjs::hide(c("players", "as_of_game")) else shinyjs::show(c("players", "as_of_game")))

  ## SCORES
  navbar_clicked = reactiveVal(0)
  new_score_updated = reactiveVal(0)
  observeEvent(input$navbar, navbar_clicked(navbar_clicked() + 1))

  new_scores = eventReactive(navbar_clicked(), {
    if (!params$scrape) return(NULL)
    print("new scores?")
    new_scores = get_new_scores() %>% anti_join(games)
    if (nrow(new_scores) == 0) return(NULL)
    new_score_updated(new_score_updated() + 1)
    return(new_scores)
  })

  observeEvent(new_score_updated(), {
    if (new_score_updated() == 0) return()
    print("scores updated")
    if (is_local) {
      tryCatch({
        write_csv2(select(games, round, game_id, points_available, date, location, team_1, team_2, score_1, score_2), "results/games.csv")
        repo = git2r::repository()
        git2r::add(repo, "results/games.csv")
        if (length(git2r::status()$staged) != 0) {
          git2r::commit(repo, "Updating scores")
          system("git push")
        }
      }, error=function(e) message(e))
    }
  })

  ## POINTS

  # points = reactive(
  #   games %>%
  #     inner_join(scores(), by = join_by(round, game_id, team_1, team_2)) %>%
  #     inner_join(preds, by = join_by(round, game_id)) %>%
  #     arrange(player_id, round, game_id) %>%
  #     rowwise() %>%
  #     mutate(points = calc_points(round, score_1, score_2, pred_score_1, pred_score_2, points_available)) %>%
  #     group_by(player_id) %>%
  #     mutate(total_points = cumsum(points)) %>%
  #     ungroup() %>%
  #     group_by(game_id) %>%
  #     mutate(rank = dense_rank(desc(total_points))) %>%
  #     ungroup() %>%
  #     select(player_id, round, game_id, points, total_points, rank)
  # )

  ## LAST RANK

  prev_game = reactive(last_games_of_day %>% filter(game_id == req(input$as_of_game)) %>% pull(prev_game_id))

  last_rank = reactive(if (req(input$as_of_game) == 0) {
    select(players, player_id) %>% mutate(last_rank = 1L)
  } else {
    standings %>%
      filter(game_id == prev_game()) %>%
      select(player_id, last_rank = rank) # work????
  })


  ## STANDINGS TABLE

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

  t1 = reactive(reactable(standings_tbl1(), columns = t1_cols(standings_tbl1()), searchable = TRUE, highlight = TRUE, onClick = 'expand', defaultPageSize = nrow(players), rowStyle = list(cursor = "pointer"), defaultExpanded = F, details = function(index) inner_tables[[standings_tbl1()[[index, 'player_id']]]]))

  output$standings = renderReactable(t1())


  ## FUN GRAPH

  gg1 = reactive(
    suppressMessages(
    players %>%
      filter(name %in% input$players) %>%
      inner_join(points) %>%
      filter(game_id <= input$as_of_game) %>%
      ggplot(aes(x = game_id, y = total_points, color = name, alpha=.5)) +
      geom_line() +
      geom_point() +
      ggthemes::theme_clean() +
      labs(x = "Game #", y = "Total Points", color = NULL, alpha = NULL, linewidth = NULL) +
      guides(alpha = 'none') +
      scale_x_continuous(breaks = 1:input$as_of_game) +
      scale_color_viridis_d(option = 'rocket') +
      geom_line(data = players %>% filter(name == input$graph_player) %>% inner_join(points) %>% filter(game_id <= input$as_of_game), linewidth=5, color='black')
  ))

  gg = reactive(girafe(ggobj = gg1(), options = list(opts_hover(css = "stroke: black; stroke-width: 5px;"), opts_hover_inv(css = "opacity:0.1;"))))

  output$graph = renderGirafe(gg())


  ## GAMES

  output$games_tbl = renderReactable(games_tbl)


  ## WELCOME

  output$welcome = renderUI({
    suppressMessages({

    vbs = list(
      value_box(title = "Number of Games Played", value = sum(games$is_played), theme = "bg-gradient-purple-pink"),
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


}


shinyApp(ui, server)
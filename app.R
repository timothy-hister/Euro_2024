is_local = Sys.getenv('SHINY_PORT') == ""
pacman::p_load(tidyverse, ggiraph, reactable, RColorBrewer, shiny, htmltools, bslib, shinyWidgets, shinymanager, shinycssloaders, rvest, shinyjs, shinyalert)

`%,%` = function(a,b) paste0(a,b)
`%,,%` = function(a,b) paste(a,b)
pa = function(x, n=10000) if (methods::is(x, "tbl_df")) print(x, width=Inf, n=n) else rlang::with_options(max.print = n, print(x))

source(here::here() %,% "/functions.R", local = T)
source(here::here() %,% "/calculations.R", local = T)
source(here::here() %,% "/ui.R", local = T)
if (!is_local) ui = secure_app(ui)

server = function(input, output, session) {

  if (!is_local) {
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
  observeEvent(input$navbar, if (input$navbar == "Fun Graph") shinyjs::show("graph_y") else shinyjs::hide("graph_y"))
  observeEvent(input$navbar, if (input$navbar == "Games") shinyjs::hide(c("players", "as_of_game")) else shinyjs::show(c("players", "as_of_game")))


  #
  # ## SCORES
  # navbar_clicked = reactiveVal(0)
  # new_score_updated = reactiveVal(0)
  # observeEvent(input$navbar, navbar_clicked(navbar_clicked() + 1))
  #
  # new_scores = eventReactive(navbar_clicked(), {
  #   if (!params$scrape) return(NULL)
  #   print("new scores?")
  #   new_scores = get_new_scores() %>% anti_join(games)
  #   if (nrow(new_scores) == 0) return(NULL)
  #   new_score_updated(new_score_updated() + 1)
  #   return(new_scores)
  # })
  #
  # observeEvent(new_score_updated(), {
  #   if (new_score_updated() == 0) return()
  #   print("scores updated")
  #   games = bind_rows(games, new_scores)
  #   if (is_local) {
  #     tryCatch({
  #       write_csv2(select(games, round, game_id, points_available, date, location, team_1, team_2, score_1, score_2), "results/games.csv")
  #       repo = git2r::repository()
  #       git2r::add(repo, "results/games.csv")
  #       if (length(git2r::status()$staged) != 0) {
  #         git2r::commit(repo, "Updating scores")
  #         system("git push")
  #       }
  #     }, error=function(e) message(e))
  #   }
  # })

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

  t1 = reactive(reactable(standings_tbl1(), columns = list(
    name = colDef(cell = function(value, index) {
      if (!round_2_ready) return(value)
      flag = preds %>%
        filter(player_id == standings_tbl1()$player_id[index]) |>
        filter(game_id == max(games$game_id)) %>%
        pull(pred_winner) %>%
        print_flag()
      div(style = "display: flex;",
          div(flag, style="margin-right: 10px;"),
          print_flag(value)
      )
      }),
      player_id = colDef(show = F),
      last_rank = colDef(show = F),
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
      total_points = colDef(header = "total points", style = function(value) {
        if (length(unique(standings_tbl1()$total_points)) == 1) return(list(fontWeight = 600))
        normalized = (value - min(standings_tbl1()$total_points)) / (max(standings_tbl1()$total_points) - min(standings_tbl1()$total_points))
        color = PuOr_pal(normalized)
        list(background = color, fontWeight = 600, color = 'white')
      }
      ),
      max_points = colDef(header = "maximum possible points")
    ), searchable = TRUE, highlight = TRUE, onClick = 'expand', defaultPageSize = nrow(players), rowStyle = list(cursor = "pointer"), defaultExpanded = F, details = function(index) inner_tables[[standings_tbl1()[[index, 'player_id']]]]))

  output$standings = renderReactable(t1())


  ## FUN GRAPH

  gg1 = reactive(
    suppressMessages(
    players %>%
      filter(name %in% input$players) %>%
      inner_join(points) %>%
      filter(game_id <= input$as_of_game) %>%
      ggplot(aes(x = game_id, y = .data[[input$graph_y]], color = name, alpha=.5)) +
      geom_line() +
      geom_point() +
      ggthemes::theme_clean() +
      labs(x = "Game #", y = case_match(input$graph_y, "total_points" ~ "Total Points", "rank" ~ "rank", "points" ~ "points"), color = NULL, alpha = NULL, linewidth = NULL) +
      guides(alpha = 'none') +
      theme(legend.position = 'none') +
      #scale_x_continuous(breaks = 1:input$as_of_game) +
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


  output$team_graph = renderGirafe({
    g = points %>%
      inner_join(games) %>%
      filter(is_played) %>%
      filter(game_id <= input$as_of_game) %>%
      inner_join(filter(players, name %in% input$players)) %>%
      select(team_1, team_2, points) %>%
      pivot_longer(cols = c("team_1", "team_2")) %>%
      rename(team = value) %>%
      group_by(team) %>%
      summarise(total_points = sum(points)) %>%
      ungroup() %>%
      arrange(total_points) %>%
      mutate(team = fct_inorder(team)) %>%
      ggplot(aes(y=team, x=total_points, fill=team)) +
      geom_col() +
      ggthemes::theme_clean() +
      scale_fill_viridis_d() +
      guides(fill = 'none')

    girafe(ggobj = g)
  })

}


shinyApp(ui, server)
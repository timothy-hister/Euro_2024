game_cell = function(obj1, obj2) {
  function(value, index) {
    div(style = "display: flex; justify-content: space-between;",
        print_flag(obj1[index]),
        div("V", style = "fontWeight: 600; margin: 0"),
        print_flag(obj2[index])
    )
  }
}

PuOr_pal <- function(x) {
  palette <- brewer.pal(11, "PuOr")
  ramp <- colorRamp(palette, space = "rgb")
  colors <- ramp(x)
  return(rgb(colors, maxColorValue = 255))
}

print_flag = function(value) {
  if (is.na(value)) image = shiny::icon("question", alt = "?", style = "max-width: 100%; height: auto; display: block; margin: 0 auto;") else if (value == "tie") image = icon("arrows-left-right", alt = "tie", "max-width: 100%; height: auto; display: block; margin: 0 auto;") else {
    if (!value %in% countries$country) return(div(value))
    code = filter(countries, country == value) %>% pull(code) %>% tolower()
    if (!fs::file_exists("www/" %,% code %,% ".svg")) {
      flag_url = "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/" %,% code %,% ".svg"
      download.file(flag_url, destfile = "www/" %,% code %,% ".svg")
    }
    #image = img(src = knitr::image_uri(flag_url), height = "24px", alt = flag)
    image = img(src = code %,% ".svg", alt=value, style = "max-width: 100%; height: auto; display: block; margin: 0 auto;")
  }
  div(image, style = "flex: 0 0 auto; width: 30px; max-width: 30px; text-align: center")
}

calc_points = function(round, score_1, score_2, prediction_1, prediction_2, points_available) {
  case_when(round == 1 ~
    case_when(score_1 == prediction_1 && score_2 == prediction_2 ~ 3L,
              score_1 - score_2 == prediction_1 - prediction_2 ~ 2L,
              sign(score_1 - score_2) == sign(prediction_1 - prediction_2) ~ 1L,
              T ~ 0L),
    # round 2
    T ~ 0L
  )
}

make_inner_tbl1 = function(id) {
  suppressMessages({
  player_table = filter(players, player_id == id) %>%
    inner_join(preds) %>%
    inner_join(games) %>%
    left_join(points) %>%
    # mutate(game = case_when(is.na(team_1) ~ NA_character_, T ~ team_1 %,,% "-" %,,% team_2)) %>%
    # mutate(pred_game = case_when(is.na(pred_team_1) ~ NA_character_, T ~ pred_team_1 %,,% "-" %,,% pred_team_2)) %>%
    # mutate(pred_result = case_when(round == 1 ~ pred_score_1 %,% " - " %,% pred_score_2, T ~ pred_winner)) %>%
    # mutate(result = case_when(!is_played ~ NA_character_, round == 1 ~ score_1 %,% " - " %,% score_2, T ~ NA_character_)) %>%
    mutate(game = NA, pred_game = NA, pred_result = NA, result = NA) %>%
    select(is_played, round, points_available, game_id, date, location, game, pred_game, team_1, team_2, score_1, score_2, pred_score_1, pred_score_2, pred_team_1, pred_team_2, result, pred_result, pred_winner, points, total_points, rank)

  reactable(player_table, outlined = TRUE, highlight = TRUE, searchable = TRUE, fullWidth = FALSE, columns = list(
    is_played = colDef(show = F),
    team_1 = colDef(show = F),
    team_2 = colDef(show = F),
    score_1 = colDef(show = F),
    score_2 = colDef(show = F),
    pred_team_1 = colDef(show = F),
    pred_team_2 = colDef(show = F),
    pred_score_1 = colDef(show = F),
    pred_score_2 = colDef(show = F),
    pred_winner = colDef(show = F),
    game_id = colDef(header = "game #"),
    points_available = colDef(header = "points available"),
    location = colDef(minWidth = 200),
    game = colDef(cell = game_cell(player_table$team_1, player_table$team_2)),
    pred_game = colDef(header = "predicted game", show = round_2_ready, cell = function(value, index) {
      if (player_table$round[index] == 1) return("")
      if (is.na(player_table$pred_winner[index])) return("")
      game_cell(player_table$pred_team_1, player_table$pred_team_2)
    }),
    result = colDef(cell = function(value, index) if (player_table$is_played[index]) player_table$score_1[index] %,,% "-" %,,% player_table$score_2[index] else ""),
    pred_result = colDef(header = "predicted result", cell = function(value, index) {
      #if (!player_table$is_played[index]) return("")
      if (player_table$round[index] == 1) player_table$pred_score_1[index] %,,% "-" %,,% player_table$pred_score_2[index] else print_flag(player_table$pred_winner[index])
    }),
    points = colDef(cell = function(value, index) ifelse(player_table$is_played[index], value, "")),
    total_points = colDef(header = "total points", cell = function(value, index) ifelse(player_table$is_played[index], value, "")),
    rank = colDef(cell = function(value, index) ifelse(player_table$is_played[index], value, ""))
  ), rowStyle = function(index) if (!player_table$is_played[index]) list(background = "rgba(0, 0, 0, 0.05)")
)
  })
}

make_tbl2 = function(index) {
  calculated_players %>%
    ggplot(aes(x=game_id, y = total_points)) +
    geom_line(aes(color = name)) +
    geom_point(aes(color = name)) +
    ggthemes::theme_clean() +
    labs(x = "Match #", y = "Total Points", color = NULL) +
    scale_x_continuous(breaks = scores$game_id) +
    scale_color_brewer(palette = 'PiYG') +
    geom_line(data = filter(points, player_id == standings_tbl$player_id[index]), linewidth = 2) +
    geom_point(data = filter(points, player_id == standings_tbl$player_id[index]), size = 3) +
    theme(legend.position = 'bottom') +
    ggtitle("How You Compare With Your Colleagues")
}

t1_cols = function(tbl) list(
  player_id = colDef(show = F),
  last_rank = colDef(show = F),
  rank = colDef(
    header = "",
    width = 50,
    cell = function(value, index) {
      arrow = tbl$rank_change[index]
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
    if (length(unique(tbl$total_points)) == 1) return(list(fontWeight = 600))
    normalized = (value - min(tbl$total_points)) / (max(tbl$total_points) - min(tbl$total_points))
    color = PuOr_pal(normalized)
    list(background = color, fontWeight = 600, color = 'white')
    }
  ),
  max_points = colDef(header = "maximum possible points")
)

scrape_site = function(url) {
  li = read_html(url) %>%
    html_elements("span") %>%
    html_text2() %>%
    keep(~str_ends(., "(?i)at full time"))
    #keep(~str_detect(., "(?i).+\\d , .+\\d.*"))

  if (length(li) == 0) return()

  li %>%
    str_remove(",") %>%
    str_squish() %>%
    map(function(s) {
      teams = str_split(s, "\\d")[[1]]
      scores = str_extract_all(s, "\\d")[[1]] %>% as.integer()
      tibble(team_1 = teams[1], team_2 = teams[2], score_1 = scores[1], score_2 = scores[2])
    }) %>%
    bind_rows() %>%
    mutate(across(1:2, str_squish)) %>%
    mutate(across(1:2, ~case_match(., "Turkey" ~ "Türkiye", "Czech Republic" ~ "Czechia", .default = .)))
}

scrape_scores = function() {
  new_scores = scrape_site("https://www.bbc.com/sport/football/european-championship/scores-fixtures/2024-06")
  old_scores = scrape_site("https://www.bbc.com/sport/football/european-championship/scores-fixtures/2024-06?filter=results")
  all_scores = bind_rows(new_scores, old_scores) %>% unique()
  all_scores = bind_rows(all_scores,
    all_scores %>%
      select(2, 1, 4, 3) %>%
      set_names(c("team_1", "team_2", "score_1", "score_2"))
  )
  games %>%
    filter(date <= today() + 1) %>%
    left_join(all_scores) %>%
    na.omit() %>%
    select(round, game_id, points_available, date, location, team_1, team_2, score_1, score_2)
}

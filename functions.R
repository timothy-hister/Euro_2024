PuOr_pal <- function(x) {
  # Define the PuOr color palette with two colors
  palette <- brewer.pal(11, "PuOr")
  # Generate the color ramp between the two PuOr colors
  ramp <- colorRamp(palette, space = "rgb")
  # Generate the colors based on the input values using the PuOr color ramp
  colors <- ramp(x)
  return(rgb(colors, maxColorValue = 255))
}

print_flag = function(value) {
  if (is.na(value)) return("")
  if (value == "tie") return(div(icon("arrows-left-right"), height="24px", alt="tie"))
  if (!value %in% countries$country) return(div(value))

  code = filter(countries, country == value) %>% pull(code) %>% tolower()
  if (!fs::file_exists("www/" %,% code %,% ".svg")) {
    flag_url = "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/" %,% code %,% ".svg"
    download.file(flag_url, destfile = "www/" %,% code %,% ".svg")
  }
  #image = img(src = knitr::image_uri(flag_url), height = "24px", alt = flag)
  image = img(src = code %,% ".svg", height = "24px", alt=value)
  div(image)
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
  player_table = filter(players, player_id == id) %>%
    inner_join(preds) %>%
    inner_join(games) %>%
    left_join(scores) %>%
    left_join(points) %>%
    mutate(game = case_when(is.na(team_1) ~ NA_character_, T ~ team_1 %,,% "-" %,,% team_2)) %>%
    mutate(pred_game = case_when(is.na(pred_team_1) ~ NA_character_, T ~ pred_team_1 %,,% "-" %,,% pred_team_2)) %>%
    mutate(pred_result = case_when(round == 1 ~ pred_score_1 %,% " - " %,% pred_score_2, T ~ pred_winner)) %>%
    mutate(result = case_when(!is_played ~ NA_character_, round == 1 ~ score_1 %,% " - " %,% score_2, T ~ result)) %>%
    select(round, points_available, game_id, date, location, game, pred_game, pred_result, result,  points, total_points, rank)

  reactable(player_table,
    outlined = TRUE, highlight = TRUE, searchable = TRUE, fullWidth = FALSE, columns = list(
      game_id = colDef(header = "game #"),
      location = colDef(minWidth = 200),
      game = colDef(na = "", cell = function(value) {
        div(style = "display: flex; align-items: center;",
            print_flag(word(value, 1)),
            if (!is.na(value)) div("V", style = "fontWeight: 600; margin: 0 10px;"),
            print_flag(word(value, 3))
        )
      }, minWidth = 150),
      pred_game = colDef(na = "", show = last_round > 1, cell = function(value) {
        div(style = "display: flex; align-items: center;",
            print_flag(word(value, 1)),
            if (!is.na(value)) div("V", style = "fontWeight: 600; margin: 0 10px;"),
            print_flag(word(value, 2))
        )
      }, minWidth = 150),
      pred_result = colDef(header = "predicted result", na = "", cell = function(value, index) if (player_table$round[index] == 1) value else print_flag(value)),
      result = colDef(na = "", cell = function(value, index) if (is.na(value)) "" else if (player_table$round[index] == 1) value else print_flag(value)),
      points_available = colDef(header = "points available"),
      total_points = colDef(header = "total points")
    )
  )
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


gemini <- function(prompt) {
  library(httr)

  model_query <- "gemini-pro:generateContent"

  response <- httr::POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = Sys.getenv("GEMINI_API_KEY")),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )
      ),
      generationConfig = list(
        temperature = 0.5,
        maxOutputTokens = 1024
      )
    )
  )

  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))

  return(outputs)
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
  read_html(url) %>%
    html_elements("span") %>%
    html_text2() %>%
    #keep(~str_ends(., "(?i)at full time")) %>%
    keep(~str_detect(., "(?i).+\\d , .+\\d.*")) %>%
    str_remove(",") %>%
    str_squish() %>%
    map(function(s) {
      teams = str_split(s, "\\d")[[1]]
      scores = str_extract_all(s, "\\d")[[1]] %>% as.integer()
      tibble(team_1 = teams[1], team_2 = teams[2], score_1 = scores[1], score_2 = scores[2])
    }) %>%
    bind_rows() %>%
    mutate(across(1:2, str_squish)) %>%
    mutate(across(1:2, ~case_match(., "Turkey" ~ "Türkiye", "Czech Republic" ~ "Czechia", .default = .))) %>%
    mutate(result = case_when(score_1 > score_2 ~ team_1, score_1 == score_2 ~ "tie", T ~ team_2))
}

get_new_scores = function() {
  new_scores = scrape_site("https://www.bbc.com/sport/football/european-championship/scores-fixtures/2024-06")
  old_scores = scrape_site("https://www.bbc.com/sport/football/european-championship/scores-fixtures/2024-06?filter=results")
  all_scores = bind_rows(new_scores, old_scores) %>% unique()
  all_scores = bind_rows(all_scores,
    all_scores %>%
      select(2, 1, 4, 3, 5) %>%
      set_names(c("team_1", "team_2", "score_2", "score_1", "result"))
  )
  games %>%
    filter(!is_played) %>%
    inner_join(all_scores) %>%
    select(round, game_id, team_1, team_2, score_1, score_2, result)
}
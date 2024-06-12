players = readxl::read_excel(here::here() %,% "/inputs/players.xlsx") %>%
  mutate(player_id = as.integer(player_id))

countries = read.csv2(here::here() %,% "/inputs/country.csv", header = T, sep = ",", ) %>%
  as_tibble() %>%
  select(1, 2) %>%
  set_names(c("country", "code")) %>%
  mutate(code = str_extract(code, "[A-Z]+")) %>%
  bind_rows(
    tibble_row(country = "Scotland", code = "gb-sct"),
    tibble_row(country = "England", code = "gb-eng"),
    tibble_row(country = "Türkiye", code = "TR")
  )

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
  flag_url = "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/" %,% code %,% ".svg"
  #image = img(src = knitr::image_uri(flag_url), height = "24px", alt = flag)
  image = img(src = flag_url, height = "24px", alt=value)
  #d = div(image, style = "margin: 0px 10px;")
  d = div(image)
  return(d)
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

make_inner_tbl1 = function(tbl) {
  player_table = tbl %>%
    inner_join(preds) %>%
    inner_join(games) %>%
    left_join(scores) %>%
    left_join(points) %>%
    mutate(game = case_when(is.na(team_1) ~ NA_character_, T ~ team_1 %,,% "-" %,,% team_2)) %>%
    mutate(pred_game = case_when(is.na(pred_team_1) ~ NA_character_, T ~ pred_team_1 %,,% "-" %,,% pred_team_2)) %>%
    mutate(pred_result = case_when(round == 1 ~ pred_score_1 %,% " - " %,% pred_score_2, T ~ pred_winner)) %>%
    mutate(result = case_when(!is_played ~ NA_character_, round == 1 ~ score_1 %,% " - " %,% score_2, T ~ result)) %>%
    select(round, game_id, date, location, game, pred_game, pred_result, result, points_available, points, total_points, rank)

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

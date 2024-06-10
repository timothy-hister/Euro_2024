#
# ```{r standings table}
# standings_tbl %>%
#   gt() %>%
#   cols_hide(columns = c(last_rank, rank_change)) %>%
#   cols_add(rank_symbol = case_when(rank_change == 0 ~ "arrow-right", rank_change > 0 ~ "arrow-up", T ~ "arrow-down"), .after = 2) %>%
#   cols_label(rank = "", rank_symbol = "", name = "") %>%
#   tab_header(md("**Standings as of " %,% format(today(), "%B %d, %Y") %,% "**")) %>%
#   data_color(
#     columns = 'total_points',
#     domain = range(standings$total_points),
#     direction = 'row',
#     palette = 'PuOr'
#   ) %>%
#   fmt_icon(
#     columns = rank_symbol,
#     #fill_color = list("arrow-up" = "green", "arrow-down" = "red", "arrow-right" = "grey")
#   )
# ```
#
# # Players
#
# ```{r players}
# players %>%
#   inner_join(points) %>%
#   mutate(across(c(team1, team2, prediction, result), ~ countries$code[match(., countries$country)])) %>%
#   na.omit() %>%
#   select(nickname, game_id, date, location, team1, team2, prediction, result, points, total_points, rank) %>%
#   gt(rowname_col = "nickname") %>%
#   fmt_flag(columns = c(team1, team2, prediction, result), height = "40px", use_title = T) %>%
#   opt_interactive(use_filters = T)
# ```

players = readxl::read_excel("inputs/players.xlsx") %>%
  mutate(player_id = as.integer(player_id))

countries = read.csv2("inputs/country.csv", header = T, sep = ",", ) %>%
  as_tibble() %>%
  select(1, 2) %>%
  set_names(c("country", "code")) %>%
  mutate(code = str_extract(code, "[A-Z]+"))

assess_points = function(prediction, result, game_id) {
  #if (is.na(result)) return(NA_integer_)
  # no need to distinguish R1 and beyond
  ifelse(prediction == result, points_available, 0L)
}

PuOr_pal <- function(x) {
  library()

  # Define the PuOr color palette with two colors
  palette <- brewer.pal(11, "PuOr")

  # Generate the color ramp between the two PuOr colors
  ramp <- colorRamp(palette, space = "rgb")

  # Generate the colors based on the input values using the PuOr color ramp
  colors <- ramp(x)

  return(rgb(colors, maxColorValue = 255))
}


print_flag = function(value) {
  #if (value == "tie") return(div("tie"))
  if (is.na(value)) return("")
  if (!value %in% countries$country) return(div(value))
  code = filter(countries, country == value) %>% pull(code) %>% tolower()
  flag_url = "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/" %,% code %,% ".svg"
  #image = img(src = knitr::image_uri(flag_url), height = "24px", alt = flag)
  image = img(src = flag_url, height = "24px", alt=value)
  d = div(image, style = "margin: 0px 10px;")
  d = div(image)
  return(d)
}












make_tbl1 = function(index) {
  player_table = players %>%
    filter(player_id == standings_tbl$player_id[index]) %>%
    inner_join(predictions) %>%
    inner_join(games) %>%
    left_join(scores) %>%
    left_join(points) %>%
    mutate(match = NA) %>%
    select(round, game_id, date, location, match, prediction, result, points, total_points, rank, team1, team2)

  reactable(player_table,
            outlined = TRUE, highlight = TRUE, fullWidth = FALSE, columns = list(
              team1 = colDef(show = F),
              team2 = colDef(show = F),
              match = colDef(na = "", cell = function(value, index) {

                div(style = "display: flex; align-items: center;",
                    print_flag(player_table$team1[index]),
                    if (!is.na(player_table$team1[index])) div("V", style = "fontWeight: 600; margin: 0 10px;"),
                    print_flag(player_table$team2[index])
                )
              },
              minWidth = 150,
              ),
              prediction = colDef(na = "", cell = function(value) print_flag(value)),
              result = colDef(na = "", cell = function(value) print_flag(value))
            )
  )
}

calc_points = function(r, x, y, pa) ifelse(r == 1, ifelse(x == y, pa, 0L), 0L)


make_tbl2 = function(index) {
  calculated_players %>%
    ggplot(aes(x=game_id, y = total_points)) +
    geom_line(aes(color = name)) +
    geom_point(aes(color = name)) +
    ggthemes::theme_clean() +
    labs(x = "Match #", y = "Total Points", color = NULL) +
    scale_x_continuous(breaks = round_1_scores$game_id) +
    scale_color_brewer(palette = 'PiYG') +
    geom_line(data = filter(points, player_id == index), linewidth = 2) +
    geom_point(data = filter(points, player_id == index), size = 3) +
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

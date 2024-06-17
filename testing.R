bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
    tippy::tippy(value, tooltip, ...))
}

bar_style <- function(width = 1, fill = "#e6e6e6", height = "80%", align = c("left", "right"), color = NULL) {
  align <- match.arg(align)
  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else {
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = color
  )
}

df = games %>%
  left_join(scores) %>%
  left_join(
    games %>%
      inner_join(preds) %>%
      inner_join(players) %>%
      mutate(pred_team = case_when(pred_winner == team_1 ~ "Team_1", pred_winner == team_2 ~ "Team_2", T ~ "tie")) %>%
      group_by(round, game_id, date, location, pred_team) %>%
      summarise(
        n_players = n_distinct(name),
        players = paste(name, collapse = ", ")) %>%
      mutate(players = str_replace(players, ",(?=[^,]+$)", " and")) %>%
      ungroup() %>%
      select(round, game_id, date, location, pred_team, n_players, players)
  ) %>%
  pivot_wider(names_from = pred_team, values_from = c("n_players", "players")) %>%
  arrange(game_id) %>%
  mutate(game = case_when(is.na(team_1) ~ NA_character_, T ~ team_1 %,,% "-" %,,% team_2)) %>%
  mutate(result = case_when(!is_played ~ NA_character_, round == 1 ~ score_1 %,% " - " %,% score_2, T ~ result)) %>%
  select(is_played, round, points_available, game_id, date, location, game, team_1, team_2, starts_with(c("n_", "players"))) %>%
  rowwise() %>%
  mutate(across(c("n_players_Team_1", "n_players_Team_2", "n_players_tie"), ~case_when(is.na(.) && round == 1 ~ 0L, T ~ .))) %>%
#  mutate(across(c("players_Team_1", "players_Team_2", "players_tie"), ~case_when(is.na(.) && round == 1 ~ "", T ~ .))) %>%
  ungroup()

reactable(df,
  columns = list(
    players_Team_1 = colDef(show = F),
    players_Team_2 = colDef(show = F),
    players_tie = colDef(show = F),
    game = colDef(show = F),
    team_1 = colDef(show = T),
    team_2 = colDef(show = F),
    n_players_Team_1 = colDef(align = 'right', cell = function(value, index) {
      width = paste0(value / nrow(players) * 100, "%")
      bar_chart(with_tooltip(df$team_1[index] %,,% "(" %,% df$n_players_Team_1[index] %,% ") ", df$players_Team_1[index]), width = width, fill = "#fc5185", background = "#e1e1e1")
      }),
    n_players_Team_2 = colDef(align = 'left', cell = function(value, index) {
        width = paste0(value / max(df$n_players_Team_2, na.rm = T) * 100, "%")
        bar_chart(with_tooltip(df$team_2[index], df$players_Team_2[index]), width = width, fill = "#fc5185", background = "#e1e1e1")
      })
    # n_players_Team_1 = colDef(align = 'left', cell = function(value, index) with_tooltip(value, df$players_Team_1[index]), style = function(value) bar_style(width = value / max(df$n_players_Team_1, na.rm=T), fill = "#2c5e77", color = "#fff"))
  )
)

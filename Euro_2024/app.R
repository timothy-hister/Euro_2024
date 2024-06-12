params = list(import_round_1 = T, import_round_2 = F, save_last_game = F)

pacman::p_load(tidyverse, hsiaR, gt, ggiraph, reactable, RColorBrewer, shiny, htmltools, bslib)
source(here::here() %,% "/tribbles.R")

games_round_1 = readxl::read_xlsx(here::here() %,% "/inputs/scores/round_1_scores.xlsx", range = "A8:G43", col_names = F) %>%
  set_names(c("game_id", "date", "location", "team_1", "score_1", "score_2", "team_2")) %>%
  mutate(game_id = as.integer(game_id)) %>%
  mutate(date = ymd("2024-06" %,% word(date, 2))) %>%
  mutate(across(starts_with("score"), as.integer))

games_round_2 = readxl::read_xlsx(here::here() %,% "/inputs/scores/round_2_scores.xlsx") %>% as.matrix() %>% unname()
games_round_2 = map(37:52, function(i) {
  w = which(games_round_2 == i, arr.ind = T)
  if (length(w) == 0) return()
  m = games_round_2[w[1]:(w[1]+4), w[2]:(w[2]+1)]
  #tibble(game_id = m[1,1], date = m[1,2], team_1 = m[2,1], team_2 = m[3,1], pred1 = m[2,2], pred2 = m[3,2], location = m[4,1])
  tibble(game_id = m[1,1], date = m[1,2], team_1 = ifelse(m[2,1] == 0, NA, m[2,1]), team_2 = ifelse(m[3,1] == 0, NA, m[3,1]), location = m[4,1])
}) %>%
  bind_rows() %>%
  mutate(game_id = as.integer(game_id)) %>%
  mutate(date = ymd("2024-" %,% word(date, 2) %,% "-" %,% word(date,3)))

games = bind_rows(games_round_1, games_round_2)
games$round = c(rep(1L, 36L), rep(2L, 8L), rep(3L, 4L), rep(4L, 2L), 5L)
games$points_available = map_int(games$round, ~switch(., 3L, 4L, 6L, 8L, 10L))

games = select(games, round, game_id, points_available, date, location, team_1, team_2) %>%
  arrange(round, game_id)

stopifnot(max(games_round_1$game_id) + 1 == min(games_round_2$game_id))
rm(games_round_1, games_round_2)


if (params$import_round_1) {
  round_1_preds = map(fs::dir_info(here::here() %,% "/inputs/Round_1")$path, function(wb) {
    player_id = str_extract(wb, here::here() %,% "/(inputs/Round_1/)(\\d*)", group=2) %>% as.integer()

    readxl::read_xlsx(wb, range = "E8:F43", col_names = F) %>%
      set_names(c("pred_score_1", "pred_score_2")) %>%
      mutate(across(everything(), as.integer)) %>%
      mutate(game_id = row_number()) %>%
      mutate(player_id = player_id) %>%
      inner_join(games) %>%
      mutate(pred_winner = case_when(pred_score_1 > pred_score_2 ~ team_1, pred_score_1 < pred_score_2 ~ team_2, T ~ "tie")) %>%
      select(round, game_id, player_id, starts_with("pred"))
  }) %>%
    bind_rows()

  saveRDS(round_1_preds, here::here() %,% "/results/round_1_preds.Rds")
} else round_1_preds = readRDS(here::here() %,% "/results/round_1_preds.Rds")

if (params$import_round_2) {
  round_2_preds = map(fs::dir_info(here::here() %,% "/inputs/Round_1")$path, function(wb) {
    x = readxl::read_xlsx(wb)
    player_id = str_extract(wb, here::here() %,% "/(inputs/Round_1/)(\\d*)", group=2) %>% as.integer()
    x = x[6:41, c(1, 2, 3, 4, 7, 10)]
    names(x) = c("game_id", "date", "location", "team_1", "pred", "team_2")
    x = x %>% mutate(game_id = as.integer(game_id), player_id = player_id, .before="game_id")
  }) %>%
    bind_rows()

  saveRDS(round_2_preds, "results/round_2_preds.Rds")
} else if (fs::file_exists("results/round_2_preds.Rds")) round_2_preds = readRDS("round_2_preds.Rds") else round_2_preds = games %>%
  filter(round > 1) %>%
  cross_join(players) %>%
  select(round, player_id, game_id) %>%
  mutate(pred_team_1 = NA_character_, pred_team_2 = NA_character_, pred_winner = NA_character_)

preds = bind_rows(round_1_preds, round_2_preds) %>%
  arrange(player_id, round, game_id)
stopifnot(nrow(preds) == nrow(games) * nrow(players))
rm(round_1_preds, round_2_preds)


round_1_scores = readxl::read_xlsx(here::here() %,% "/inputs/scores/round_1_scores.xlsx", range = "E8:F43", col_names = F)

if (nrow(round_1_scores) > 0) round_1_scores = round_1_scores %>%
  set_names(c("score_1", "score_2")) %>%
  mutate(across(everything(), as.integer)) %>%
  mutate(game_id = row_number(), .before=1) %>%
  na.omit() %>%
  inner_join(games) %>%
  mutate(result = case_when(score_1 > score_2 ~ team_1, score_1 == score_2 ~ "tie", T ~ team_2)) %>%
  select(round, game_id, team_1, team_2, score_1, score_2, result)

round_2_scores = if (fs::file_exists(here::here() %,% "/inputs/scores/round_2_scores.xlsx")) readxl::read_xlsx(here::here() %,% "/inputs/scores/round_2_scores.xlsx") else NULL
round_2_scores = NULL # for now

scores = if (is.null(round_2_scores)) round_1_scores else ~bind_rows(round_1_scores, round_2_scores)

rm(round_1_scores, round_2_scores)

last_game = max(scores$game_id)
prev_last_game = if (fs::file_exists("results/last_game.Rds")) read_rds("results/last_game.Rds") else 0L
if (params$save_last_game) write_rds(last_game, "results/last_game.Rds")

games = games %>% mutate(is_played = game_id <= last_game)
last_round = games %>% filter(is_played) %>% tail(1) %>% pull(round)

if (last_round == 1) points = games %>%
  inner_join(scores) %>%
  inner_join(preds) %>%
  arrange(player_id, round, game_id) %>%
  rowwise() %>%
  mutate(points = calc_points(round, score_1, score_2, pred_score_1, pred_score_2, points_available)) %>%
  group_by(player_id) %>%
  mutate(total_points = cumsum(points)) %>%
  ungroup() %>%
  group_by(game_id) %>%
  mutate(rank = dense_rank(desc(total_points))) %>%
  ungroup() %>%
  select(player_id, round, game_id, points, total_points, rank)

if (last_round == 1) {
  max_points_left = games %>%
    inner_join(preds) %>%
    group_by(player_id) %>%
    arrange(player_id, desc(game_id)) %>%
    mutate(max_points_left = cumsum(points_available) - points_available) %>%
    ungroup() %>%
    select(player_id, round, game_id, max_points_left) %>%
    arrange(player_id, game_id)
}


standings = points %>%
  na.omit() %>%
  select(player_id, round, game_id, total_points, rank) %>%
  arrange(game_id, rank) %>%
  inner_join(max_points_left) %>%
  mutate(max_points = total_points + max_points_left) %>%
  select(game_id, player_id, rank, total_points, max_points)


standings_tbl = standings %>%
  group_by(player_id) %>%
  arrange(game_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(player_id, total_points, rank, max_points) %>%
  arrange(rank) %>%
  left_join(
    points %>%
      filter(game_id == last_game) %>%
      select(player_id, last_rank = rank)
  ) %>%
  mutate(last_rank = case_when(is.na(last_rank) ~ 1L, T ~ last_rank)) %>%
  mutate(rank_change = last_rank - rank) %>%
  inner_join(players) %>%
  select(player_id, rank, rank_change, nickname, total_points, last_rank, max_points) %>%
  rename(name = nickname)

calculated_players = bind_rows(
  games %>%
    inner_join(scores) %>%
    mutate(pred_score_1 = 0L, pred_score_2 = 0L) %>%
    arrange(round, game_id) %>%
    rowwise() %>%
    mutate(points = calc_points(round, score_1, score_2, pred_score_1, pred_score_2, points_available)) %>%
    select(round, game_id, points) %>%
    ungroup() %>%
    mutate(total_points = cumsum(points)) %>%
    mutate(name = "0-0 Guy"),
  games %>%
    inner_join(scores) %>%
    mutate(pred_score_1 = 1L, pred_score_2 = 1L) %>%
    arrange(round, game_id) %>%
    rowwise() %>%
    mutate(points = calc_points(round, score_1, score_2, pred_score_1, pred_score_2, points_available)) %>%
    select(round, game_id, points) %>%
    ungroup() %>%
    mutate(total_points = cumsum(points)) %>%
    mutate(name = "1-1 Guy"),
  points %>%
    filter(player_id == standings_tbl[[1,1]]) %>%
    mutate(name = "The Best"),
  points %>%
    filter(player_id == standings_tbl[[nrow(standings_tbl),1]]) %>%
    mutate(name = "The Worst"),
  points %>%
    group_by(round, game_id) %>%
    summarise(points = mean(points)) %>%
    ungroup() %>%
    mutate(total_points = cumsum(points)) %>%
    mutate(name = "The Average")
) %>%
  select(round, game_id, name, total_points)

t1 = standings_tbl %>%
  select(-last_rank) %>%
  reactable(
    columns = list(
      player_id = colDef(show = F),
      rank = colDef(
        header = "",
        width = 50,
        cell = function(value, index) {
          arrow = standings_tbl$rank_change[index]
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
          if (length(unique(standings_tbl$total_points)) == 1) return(list(fontWeight = 600))
          normalized = (value - min(standings_tbl$total_points)) / (max(standings_tbl$total_points) - min(standings_tbl$total_points))
          color = PuOr_pal(normalized)
          list(background = color, fontWeight = 600, color = 'white')
        }
      )
    ),
    searchable = TRUE, highlight = TRUE, onClick = 'expand', rowStyle = list(cursor = "pointer"), defaultExpanded = F
  )


ui <- page_sidebar(
  title = "Penguins dashboard",
  sidebar = numericInput("g", "i", 6),
  navset_card_underline(
    title = "Histograms by species",
    nav_panel("Bill Length", reactableOutput("tbl1")),
    nav_panel("Bill Depth", plotOutput("bill_depth")),
    nav_panel("Body Mass", plotOutput("body_mass"))
  )
)

server = function(session, input, output) {
  output$tbl1 = renderReactable(t1)
}

shinyApp(ui, server)

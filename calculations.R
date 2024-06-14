players = readxl::read_excel(here::here() %,% "/inputs/players.xlsx") %>%
  mutate(player_id = as.integer(player_id))
stopifnot(sum(is.na(players)) == 0)

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



if (params$import_games) {
  games_round_1 = readr::read_csv(params$scores_round_1_url, skip = 5) %>%
    select(1, 2, 3, 4, 7) %>%
    na.omit() %>%
    set_names(c("game_id", "date", "location", "team_1", "team_2")) %>%
    mutate(game_id = as.integer(game_id)) %>%
    mutate(date = ymd("2024-06" %,% word(date, 2)))

  if (is.null(params$scores_round_2_url)) games_round_2 = tibble(game_id = 37:51)

  # games_round_2 = readxl::read_xlsx(here::here() %,% "/inputs/scores/round_2_scores.xlsx") %>% as.matrix() %>% unname()
  # games_round_2 = map(37:52, function(i) {
  #   w = which(games_round_2 == i, arr.ind = T)
  #   if (length(w) == 0) return()
  #   m = games_round_2[w[1]:(w[1]+4), w[2]:(w[2]+1)]
  #   #tibble(game_id = m[1,1], date = m[1,2], team_1 = m[2,1], team_2 = m[3,1], pred1 = m[2,2], pred2 = m[3,2], location = m[4,1])
  #   tibble(game_id = m[1,1], date = m[1,2], team_1 = ifelse(m[2,1] == 0, NA, m[2,1]), team_2 = ifelse(m[3,1] == 0, NA, m[3,1]), location = m[4,1])
  # }) %>%
  #   bind_rows() %>%
  #   mutate(game_id = as.integer(game_id)) %>%
  #   mutate(date = ymd("2024-" %,% word(date, 2) %,% "-" %,% word(date,3)))

  games = bind_rows(games_round_1, games_round_2)
  games$round = c(rep(1L, 36L), rep(2L, 8L), rep(3L, 4L), rep(4L, 2L), 5L)
  games$points_available = map_int(games$round, ~switch(., 3L, 4L, 6L, 8L, 10L))
  games = select(games, round, game_id, points_available, date, location, team_1, team_2) %>%
    arrange(round, game_id)

  stopifnot(max(games_round_1$game_id) + 1 == min(games_round_2$game_id))
  rm(games_round_1, games_round_2)

  saveRDS(games, here::here() %,% "/results/games.Rds")
} else games = readRDS(here::here() %,% "/results/games.Rds")

all_teams = c(games$team_1, games$team_2) %>% unique() %>% sort()
all_locations = sort(unique(games$location))

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

# round_2_preds = map(fs::dir_info(here::here() %,% "/inputs/Round_1")$path, function(wb) {
#   x = readxl::read_xlsx(wb)
#   player_id = str_extract(wb, here::here() %,% "/(inputs/Round_1/)(\\d*)", group=2) %>% as.integer()
#   x = x[6:41, c(1, 2, 3, 4, 7, 10)]
#   names(x) = c("game_id", "date", "location", "team_1", "pred", "team_2")
#   x = x %>% mutate(game_id = as.integer(game_id), player_id = player_id, .before="game_id")
# }) %>%
#   bind_rows()
round_2_preds = games %>%
  filter(round > 1) %>%
  cross_join(players) %>%
  select(round, player_id, game_id) %>%
  mutate(pred_team_1 = NA_character_, pred_team_2 = NA_character_, pred_winner = NA_character_)

preds = bind_rows(round_1_preds, round_2_preds) %>%
  arrange(player_id, round, game_id)

stopifnot(preds |> count(player_id) |> pull(n) |> unique() == nrow(games))
rm(round_1_preds, round_2_preds)

round_1_scores = readr::read_csv(params$scores_round_1_url, skip = 5) %>%
  select(5, 6) %>%
  set_names(c("score_1", "score_2")) %>%
  mutate(across(everything(), as.integer)) %>%
  mutate(game_id = row_number(), .before=1) %>%
  na.omit() %>%
  inner_join(games) %>%
  mutate(result = case_when(score_1 > score_2 ~ team_1, score_1 == score_2 ~ "tie", T ~ team_2)) %>%
  select(round, game_id, team_1, team_2, score_1, score_2, result)

  #saveRDS(round_1_scores, here::here() %,% "/results/round_1_scores.Rds")

  # round_2_scores = if (fs::file_exists(here::here() %,% "/inputs/scores/round_2_scores.xlsx")) readxl::read_xlsx(here::here() %,% "/inputs/scores/round_2_scores.xlsx") else NULL
  round_2_scores = NULL # for now
# } else {
#   round_1_scores = readRDS(here::here() %,% "/results/round_1_scores.Rds")
#   round_2_scores = NULL
# }

scores = if (is.null(round_2_scores)) round_1_scores else ~bind_rows(round_1_scores, round_2_scores)
rm(round_1_scores, round_2_scores)

last_game = if (nrow(scores) > 0) max(scores$game_id) else 0L
games = games %>% mutate(is_played = game_id <= last_game)
last_round = if (nrow(scores) > 0) games %>% filter(is_played) %>% tail(1) %>% pull(round) else 0L

last_games_of_day = c(0, games %>% group_by(date) %>% slice_tail(n=1) %>% pull(game_id))
last_games_of_day = games %>% rowwise() %>% mutate(prev_game_id = as.integer(max(last_games_of_day[last_games_of_day < game_id]))) %>% ungroup() %>% select(game_id, prev_game_id)


# we can make these reactive????
if (last_round %in% 0:1) points = games %>%
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

if (last_round %in% 0:1) {
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
  inner_join(players) %>%
  inner_join(games) %>%
  arrange(game_id) %>%
  inner_join(max_points_left) %>%
  mutate(max_points = total_points + max_points_left) %>%
  select(game_id, player_id, rank, total_points, max_points)

standings = bind_rows(
  players %>%
    select(player_id) %>%
    mutate(game_id = 0L, .before=1) %>%
    mutate(rank = 1L, total_points = 0L, max_points = sum(games$points_available)),
  standings
)

inner_tables_list = map(players$player_id, function(player) suppressMessages(make_inner_tbl1(player)))
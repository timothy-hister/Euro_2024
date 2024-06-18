## PLAYERS

players = readxl::read_excel(here::here() %,% "/inputs/players.xlsx") %>%
  mutate(player_id = as.integer(player_id))
stopifnot(sum(is.na(players)) == 0)

## COUNTRIES

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

## SCORES

scores = if (is_for_shinyapps) read.csv2("https://raw.githubusercontent.com/timothy-hister/Euro_2024/main/results/scores.csv") else read.csv2(here::here() %,% "/results/scores.csv")
scores = scores %>% as_tibble() %>% unique()
last_game = if (nrow(scores) > 0) max(scores$game_id) else 0L
last_round = if (nrow(scores) > 0) max(scores$round) else 0L

## GAMES

games = read.csv2(here::here() %,% "/inputs/games.csv") %>%
  as_tibble() %>%
  unique() %>%
  mutate(is_played = game_id <= last_game)

all_teams = c(games$team_1, games$team_2) %>% unique() %>% sort()
all_locations = sort(unique(games$location))

last_games_of_day = c(0, games %>% group_by(date) %>% slice_tail(n=1) %>% pull(game_id))
last_games_of_day = games %>% rowwise() %>% mutate(prev_game_id = as.integer(max(last_games_of_day[last_games_of_day < game_id]))) %>% ungroup() %>% select(game_id, prev_game_id)

## SCRAPE NEW SCORES

if (params$scrape) {
  played_games_wo_scores = games %>% filter(!is_played) %>% filter(date <= today() + 1)
  if (nrow(played_games_wo_scores) > 0) {
    new_scores = get_new_scores()
    if (nrow(new_scores) > 0) {
      scores = bind_rows(scores, new_scores) %>% na.omit()
      write_csv2(scores, "results/scores.csv")
      tryCatch({
        repo = git2r::repository()
        git2r::add(repo, "results/scores.csv")
        git2r::commit(repo, "Updating scores")
        system("git push")
      }, error=function(e) message(e))
      last_game = if (nrow(scores) > 0) max(scores$game_id) else 0L
      last_round = if (nrow(scores) > 0) max(scores$round) else 0L
      games = games %>% mutate(is_played = game_id <= last_game)
    }
  }
}

## PREDICTIONS

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
  if (sum(is.na(round_1_preds) > 0)) warning("There are NAs in Round 1 preds")
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
  if (sum(is.na(round_2_preds) > 0)) warning("There are NAs in Round 2 preds")
} else {
  round_2_preds = games %>%
  filter(round > 1) %>%
  cross_join(players) %>%
  select(round, player_id, game_id) %>%
  mutate(pred_team_1 = NA_character_, pred_team_2 = NA_character_, pred_winner = NA_character_)
}

preds = bind_rows(round_1_preds, round_2_preds) %>%
  arrange(player_id, round, game_id)
rm(round_1_preds, round_2_preds)

## POINTS

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

## MAX POINTS LEFT

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


## STANDINGS

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

## STANDINGS INNER TABLE

inner_tables_list = map(players$player_id, function(player) suppressMessages(make_inner_tbl1(player)))

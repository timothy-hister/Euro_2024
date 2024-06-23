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

## PREDICTIONS

preds = bind_rows(readRDS(here::here() %,% "/results/round_1_preds.Rds"), readRDS(here::here() %,% "/results/round_2_preds.Rds")) %>%
  arrange(player_id, round, game_id) %>%
  select(round, game_id, player_id, pred_score_1, pred_score_2, pred_team_1, pred_team_2, pred_winner, pred_loser, pred_tie)

## GAMES

read.csv2("results/games.csv") %>% as_tibble()

games = source(here::here() %,% "/import_games.R", local = T)$value
last_games_of_day = c(0, games %>% group_by(date) %>% slice_tail(n=1) %>% pull(game_id))
last_games_of_day = games %>% rowwise() %>% mutate(prev_game_id = as.integer(max(last_games_of_day[last_games_of_day < game_id]))) %>% ungroup() %>% select(game_id, prev_game_id)

all_teams = c(games$team_1, games$team_2) %>% unique() %>% sort()
all_locations = sort(unique(games$location))

## SCORES

#scores_old = tryCatch(read.csv2("https://raw.githubusercontent.com/timothy-hister/Euro_2024/main/results/scores.csv"), error = function(e) read.csv2(here::here() %,% "/results/scores.csv")) %>%
scores_old = read.csv2(here::here() %,% "/results/scores.csv") %>%
  as_tibble() %>%
  unique() %>%
  mutate(winner = case_when(score_1 > score_2 ~ team_1, score_1 < score_2 ~ team_2, T ~ NA_character_)) %>%
  mutate(loser = case_when(score_1 > score_2 ~ team_2, score_1 < score_2 ~ team_1, T ~ NA_character_)) %>%
  mutate(is_tie = case_when(score_1 == score_2 ~ T, T ~ F))

points_old = games %>%
  select(-team_1, -team_2) %>%
#  filter(round > 1)%>%
  inner_join(scores_old, by = join_by(round, game_id)) %>%
  inner_join(preds, by = join_by(round, game_id)) %>%
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

t1 = games %>%
  filter(round > 1) %>%
  select(round, game_id) %>%
  crossing(all_teams) %>%
  rename(team = all_teams)


alive_games = preds %>%
  filter(round > 1) %>%
  anti_join(
    scores_old %>%
      filter(round > 1) %>%
      select(game_id, loser), by = join_by(game_id <= game_id, pred_winner == loser)
  ) %>%
  select(round, game_id, player_id)


t2 = games %>%
  filter(round > 1) %>%
  pivot_longer(cols = c("team_1", "team_2")) %>%
  select(round, game_id, team = value) %>%
  na.omit()

teams_alive = sqldf::sqldf("select t1.round, t1.game_id, t1.team, case when t2.team is NULL then 0 else 1 end as is_alive from t1 left join t2 on t1.round <= t2.round and t1.game_id <= t2.game_id and t1.team = t2.team") %>%
  as_tibble() %>%
  mutate(is_alive = as.logical(is_alive))

rm(t1, t2)

# max points left???
games %>%
  inner_join(preds, by = join_by(round, game_id)) %>%
  left_join(teams_alive, by=join_by(game_id, round, pred_winner == team)) %>%
  arrange(player_id, game_id) %>%
  select(round, game_id, player_id, pred_winner, is_alive, points_available) %>%
  mutate(is_alive = case_when(round == 1L ~ T, T ~ is_alive)) %>%
  mutate(points_available = case_when(is_alive ~ points_available, T ~ 0L)) %>%
  group_by(player_id) %>%
  arrange(player_id, desc(game_id)) %>%
  mutate(max_points_left = cumsum(points_available) - points_available) %>%
  ungroup() %>%
  select(player_id, round, game_id, max_points_left) %>%
  arrange(player_id, game_id)

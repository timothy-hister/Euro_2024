###### STATICS

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
  arrange(player_id, round, game_id)





### REACTIVES













## POINTS

if (last_round %in% 0:1) points = games %>%
  inner_join(scores, by = join_by(round, game_id, team_1, team_2)) %>%
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

## MAX POINTS LEFT

if (last_round %in% 0:1) {
  max_points_left = games %>%
    inner_join(preds, by = join_by(round, game_id)) %>%
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
  inner_join(players, by = join_by(player_id)) %>%
  inner_join(games, by = join_by(round, game_id)) %>%
  arrange(game_id) %>%
  inner_join(max_points_left, by = join_by(player_id, round, game_id)) %>%
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

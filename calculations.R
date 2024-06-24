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

## GAMES

games = if (!is_local) read.csv2("https://raw.githubusercontent.com/timothy-hister/Euro_2024/main/results/games.csv") else read.csv2("results/games.csv")

games = games %>%
  as_tibble() %>%
  select(round, game_id, points_available, date, location, team_1, team_2, score_1, score_2) %>%
  mutate(winner = case_when(is.na(score_1) ~ NA_character_, score_1 > score_2 ~ team_1, score_1 < score_2 ~ team_2, T ~ "tie")) %>%
  mutate(loser = case_when(is.na(score_1) ~ NA_character_, score_1 > score_2 ~ team_2, score_1 < score_2 ~ team_1, T ~ "tie")) %>%
  mutate(is_played = !is.na(score_1))
  # mutate(winner = case_when(score_1 > score_2 ~ team_1, score_1 < score_2 ~ team_2, T ~ NA_character_)) %>%
  # mutate(loser = case_when(score_1 > score_2 ~ team_2, score_1 < score_2 ~ team_1, T ~ NA_character_)) %>%
  # mutate(is_tie = case_when(score_1 == score_2 ~ T, T ~ F))

last_games_of_day = c(0, games %>% group_by(date) %>% slice_tail(n=1) %>% pull(game_id))
last_games_of_day = games %>% rowwise() %>% mutate(prev_game_id = as.integer(max(last_games_of_day[last_games_of_day < game_id]))) %>% ungroup() %>% select(game_id, prev_game_id)

all_teams = c(games$team_1, games$team_2) %>% unique() %>% sort()
all_locations = sort(unique(games$location))

last_game = if (all(!games$is_played)) 0L else filter(games, is_played)$game_id %>% max()
last_round = if (all(!games$is_played)) 0L else filter(games, is_played)$round %>% max()

round_2_ready = unname(fs::file_exists(here::here() %,% "/results/round_2_preds.Rds"))

## PREDICTIONS

preds = if (round_2_ready) bind_rows(readRDS(here::here() %,% "/results/round_1_preds.Rds"), readRDS(here::here() %,% "/results/round_2_preds.Rds")) else readRDS(here::here() %,% "/results/round_1_preds.Rds") %>% mutate(pred_team_1 = NA_character_, pred_team_2 = NA_character_)

preds = games %>%
  left_join(preds) %>%
  arrange(player_id, round, game_id) %>%
  select(round, game_id, player_id, pred_score_1, pred_score_2, pred_team_1, pred_team_2, pred_winner, pred_loser)

## POINTS

points = games %>%
  select(-team_1, -team_2) %>%
#  filter(round > 1)%>%
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

max_points_left = games %>%
  inner_join(preds, by = join_by(round, game_id)) %>%
  group_by(player_id) %>%
  arrange(player_id, desc(game_id)) %>%
  mutate(max_points_left = cumsum(points_available) - points_available) %>%
  ungroup() %>%
  select(player_id, round, game_id, max_points_left) %>%
  arrange(player_id, game_id)

## STANDINGS

standings = bind_rows(
  players %>%
    select(player_id) %>%
    mutate(game_id = 0L, .before=1) %>%
    mutate(rank = 1L, total_points = 0L, max_points = sum(games$points_available)),
  points %>%
    na.omit() %>%
    inner_join(players, by = join_by(player_id)) %>%
    inner_join(games, by = join_by(round, game_id)) %>%
    arrange(game_id) %>%
    inner_join(max_points_left, by = join_by(player_id, round, game_id)) %>%
    mutate(max_points = total_points + max_points_left) %>%
    select(game_id, player_id, rank, total_points, max_points)
)

## INNER TABLE

inner_tables = map(players$player_id, ~make_inner_tbl1(.))

# t1 = games %>%
#   filter(round > 1) %>%
#   select(round, game_id) %>%
#   crossing(all_teams) %>%
#   rename(team = all_teams)
#
#
# alive_games = preds %>%
#   filter(round > 1) %>%
#   anti_join(
#     scores_old %>%
#       filter(round > 1) %>%
#       select(game_id, loser), by = join_by(game_id <= game_id, pred_winner == loser)
#   ) %>%
#   select(round, game_id, player_id)
#
#
# t2 = games %>%
#   filter(round > 1) %>%
#   pivot_longer(cols = c("team_1", "team_2")) %>%
#   select(round, game_id, team = value) %>%
#   na.omit()
#
# teams_alive = sqldf::sqldf("select t1.round, t1.game_id, t1.team, case when t2.team is NULL then 0 else 1 end as is_alive from t1 left join t2 on t1.round <= t2.round and t1.game_id <= t2.game_id and t1.team = t2.team") %>%
#   as_tibble() %>%
#   mutate(is_alive = as.logical(is_alive))
#
# rm(t1, t2)
#
# # max points left???
# games %>%
#   inner_join(preds, by = join_by(round, game_id)) %>%
#   left_join(teams_alive, by=join_by(game_id, round, pred_winner == team)) %>%
#   arrange(player_id, game_id) %>%
#   select(round, game_id, player_id, pred_winner, is_alive, points_available) %>%
#   mutate(is_alive = case_when(round == 1L ~ T, T ~ is_alive)) %>%
#   mutate(points_available = case_when(is_alive ~ points_available, T ~ 0L)) %>%
#   group_by(player_id) %>%
#   arrange(player_id, desc(game_id)) %>%
#   mutate(max_points_left = cumsum(points_available) - points_available) %>%
#   ungroup() %>%
#   select(player_id, round, game_id, max_points_left) %>%
#   arrange(player_id, game_id)




games_tbl = games %>%
  left_join(points) %>%
  group_by(round, game_id) %>%
  summarise(
    total_points = sum(points),
    avg_points = mean(points),
    perc_got_points = mean(points > 0)
  ) %>%
  ungroup() %>%
  right_join(games) %>%
  mutate(game = NA, result = NA) %>%
  select(is_played, round, points_available, game_id, date, location, game, result, total_points, avg_points, perc_got_points) %>%

  reactable(
    columns = list(
      is_played = colDef(show = F),
      total_points = colDef(name = "Total # of points received", cell = function(value, index) if(games$is_played[index]) value),
      avg_points = colDef(name = "Average # of points received", cell = function(value, index) if(games$is_played[index]) round(value, 2)),
      perc_got_points = colDef(name = "% of players who got >=1 points", cell = function(value, index) if(games$is_played[index]) round(value * 100, 0) %,% "%"),
      game = colDef(cell = function(value, index) {
        div(style = "display: flex; justify-content: space-between;",
            print_flag(games$team_1[index]),
            div("V", style = "fontWeight: 600; margin: 0"),
            print_flag(games$team_2[index])
        )
      }),
      result = colDef(cell = function(value, index) if (games$is_played[index]) games$score_1[index] %,,% "-" %,,% games$score_2[index] else "")
    ),
    rowStyle = function(index) if (!games$is_played[index]) list(background = "rgba(0, 0, 0, 0.05)")
  )

lucrative_game = points %>% group_by(game_id) %>% summarise(points = sum(points)) %>% arrange(desc(points)) %>% slice_head(n=1) %>% inner_join(games)
lucrative_team = bind_rows(points %>% inner_join(games) %>% select(team = team_1, points), points %>% inner_join(games) %>% select(team = team_2, points)) %>% group_by(team) %>% summarise(points = sum(points)) %>% arrange(desc(points))


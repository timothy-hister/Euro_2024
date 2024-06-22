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

## GAMES

games = readr::read_csv2(here::here() %,% "/inputs/games.csv") %>%
  as_tibble() %>%
  unique() %>%
  mutate(round = as.integer(round)) %>%
  mutate(game_id = as.integer(game_id)) %>%
  mutate(points_available = as.integer(points_available))

last_games_of_day = c(0, games %>% group_by(date) %>% slice_tail(n=1) %>% pull(game_id))
last_games_of_day = games %>% rowwise() %>% mutate(prev_game_id = as.integer(max(last_games_of_day[last_games_of_day < game_id]))) %>% ungroup() %>% select(game_id, prev_game_id)

all_teams = c(games$team_1, games$team_2) %>% unique() %>% sort()
all_locations = sort(unique(games$location))

## SCORES

scores_old = tryCatch(read.csv2("https://raw.githubusercontent.com/timothy-hister/Euro_2024/main/results/scores.csv"), error = function(e) read.csv2(here::here() %,% "/results/scores.csv")) %>%
  as_tibble() %>%
  unique()

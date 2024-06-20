round_1_preds = map(fs::dir_info(here::here() %,% "/predictions/Round_1")$path, function(wb) {
  player_id = str_extract(wb, here::here() %,% "/(predictions/Round_1/)(\\d*)", group=2) %>% as.integer()
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

if (sum(is.na(round_1_preds) > 0)) warning("There are" %,,% sum(is.na(round_1_preds)) %,,% "NAs in Round 1 preds")
saveRDS(round_1_preds, here::here() %,% "/results/round_1_preds.Rds")

if (last_round > 1) round_2_preds = map(fs::dir_info(here::here() %,% "/predictions/Round_2")$path, function(wb) {
    x = readxl::read_xlsx(wb)
    player_id = str_extract(wb, here::here() %,% "/(predictions/Round_2/)(\\d*)", group=2) %>% as.integer()
    x = x[6:41, c(1, 2, 3, 4, 7, 10)]
    names(x) = c("game_id", "date", "location", "team_1", "pred", "team_2")
    x = x %>% mutate(game_id = as.integer(game_id), player_id = player_id, .before="game_id")
  }) %>%
    bind_rows()
  if (sum(is.na(round_2_preds) > 0)) warning("There are NAs in Round 2 preds")

if (last_round == 1) round_2_preds = games %>%
  filter(round > 1) %>%
  cross_join(players) %>%
  select(round, player_id, game_id) %>%
  mutate(pred_team_1 = NA_character_, pred_team_2 = NA_character_, pred_winner = NA_character_)

if (sum(is.na(round_2_preds) > 0)) warning("There are" %,,% sum(is.na(round_2_preds)) %,,% "NAs in Round 2 preds")
saveRDS(round_2_preds, here::here() %,% "/results/round_2_preds.Rds")

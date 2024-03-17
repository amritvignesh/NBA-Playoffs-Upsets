library(nbastatR)
library(hoopR)
library(dplyr)
library(tidyverse)
library(caret)
library(xgboost)
library(vip)
library(gt)
library(gtExtras)
library(rvest)
library(nnet)

seasons <- c(2007:2019, 2021:2023) # no 2020 due to weird format

total <- data.frame()

teams <- nba_teams() %>%
  select(id = team_id, name = display_name, abbr = abbreviation)

for (szn in seasons) {
  og <- nba_commonplayoffseries(
    league_id = "00",
    season = year_to_season(szn - 1),
    series_id = "",
  ) %>% .[[1]] %>% mutate(season = szn, round = (as.numeric(SERIES_ID) %% 100 - as.numeric(SERIES_ID) %% 10)/10)
  indiv <- og %>% group_by(SERIES_ID) %>% summarize(season = first(season), round = first(round), high_id = first(HOME_TEAM_ID), low_id = first(VISITOR_TEAM_ID))
  indiv <- left_join(indiv, teams, by = c("high_id"="id")) %>% mutate(high_name = name, high_abbr = abbr) %>% select(-name, -abbr)
  indiv <- left_join(indiv, teams, by = c("low_id"="id")) %>% mutate(low_name = name, low_abbr = abbr) %>% select(-name, -abbr)
  indiv <- indiv %>% mutate(high_seed = ifelse(row_number() %in% c(1,5), 1, ifelse(row_number() %in% c(2,6), 2, ifelse(row_number() %in% c(3,7), 3, ifelse(row_number() %in% c(4,8), 4, NA)))), low_seed = 9 - high_seed)
  teams_szn <- rbind(indiv %>% filter(!is.na(high_seed)) %>% select(id = high_id, seed = high_seed), indiv %>% filter(!is.na(low_seed)) %>% select(id = low_id, seed = low_seed))
  indiv <- left_join(indiv, teams_szn, by = c("high_id"="id")) %>% mutate(high_seed = seed) %>% select(-seed)
  indiv <- left_join(indiv, teams_szn, by = c("low_id"="id")) %>% mutate(low_seed = seed) %>% select(-seed)
  
  standings <- nba_leaguestandings(
    league_id = "00",
    season = year_to_season(szn - 1),
    season_type = "Regular Season",
    season_year = "",
  ) %>% .[[1]] %>% mutate(l10_pct = as.numeric(sub("-.*", "", L10))/10) %>% mutate(WinPCT = as.numeric(WinPCT), PointsPG = as.numeric(PointsPG), OppPointsPG = as.numeric(OppPointsPG)) %>% select(id = TeamID, pct = WinPCT, l10_pct, ppg = PointsPG, oppg = OppPointsPG)
  
  indiv <- left_join(indiv, standings, by = c("high_id"="id")) %>% mutate(high_pct = pct, high_l10_pct = l10_pct, high_ppg = ppg, high_oppg = oppg) %>% select(-pct, -l10_pct, -ppg, -oppg)
  indiv <- left_join(indiv, standings, by = c("low_id"="id")) %>% mutate(low_pct = pct, low_l10_pct = l10_pct, low_ppg = ppg, low_oppg = oppg) %>% select(-pct, -l10_pct, -ppg, -oppg)
  
  indiv <- indiv %>% mutate(diff_pct = high_pct - low_pct, diff_l10_pct = high_l10_pct - low_l10_pct, diff_ppg = high_ppg - low_ppg, diff_oppg = high_oppg - low_oppg) %>% select(-high_pct, -low_pct, -high_l10_pct, -low_l10_pct, -high_ppg, -low_ppg, -high_oppg, -low_oppg)
  
  games <- og %>% group_by(SERIES_ID) %>% summarize(games = n()) %>% mutate(winner_games = 4, loser_games = games - winner_games)
  
  indiv <- left_join(indiv, games, by = "SERIES_ID")
  total <- rbind(total, indiv)
  
  Sys.sleep(3)
}

first_round <- total %>% filter(round == 1)
conf_sf <- total %>% filter(round == 2)
conf_f <- total %>% filter(round == 3)
finals <- total %>% filter(round == 4)

first_round <- first_round %>%
  group_by(season) %>%
  mutate(high_win = ifelse(high_id %in% conf_sf$high_id[which(conf_sf$season == season)] | high_id %in% conf_sf$low_id[which(conf_sf$season == season)], 1, 0)) %>%
  ungroup()

conf_sf <- conf_sf %>%
  group_by(season) %>%
  mutate(high_win = ifelse(high_id %in% conf_f$high_id[which(conf_f$season == season)] | high_id %in% conf_f$low_id[which(conf_f$season == season)], 1, 0)) %>%
  ungroup()

conf_f <- conf_f %>%
  group_by(season) %>%
  mutate(high_win = ifelse(high_id %in% finals$high_id[which(finals$season == season)] | high_id %in% finals$low_id[which(finals$season == season)], 1, 0)) %>%
  ungroup()

champ_page <- read_html("https://www.basketball-reference.com/playoffs/")

champ_table <- champ_page %>%
  html_element("#champions_index") %>%
  html_table() 

champions <- data.frame(champ_table[,1], champ_table[,3])

champions$Var.1 <- as.numeric(champions$Var.1)

finals <- finals %>%
  left_join(champions, by = c("season"="Var.1")) %>%
  mutate(high_win = ifelse(high_name == Finals, 1, 0))

first_round <- first_round %>% mutate(high_games = ifelse(high_win == 1, winner_games, loser_games))

factor_data_first_round <- first_round %>%
  mutate(high_seed = as.factor(high_seed)) %>%
  select(high_seed)

dummy_first_round <- dummyVars(" ~ .", data = factor_data_first_round)
new_factor_first_round <- data.frame(predict(dummy_first_round, newdata = factor_data_first_round))

first_round <- cbind(first_round, new_factor_first_round) %>% select(-high_seed)

xgboost_train_first_round <- first_round %>%
  filter(season <= 2018)

xgboost_test_first_round <- first_round %>%
  filter(season > 2018)

xgboost_train_first_round[,19] <- 0.25 * xgboost_train_first_round[,19]
xgboost_test_first_round[,19] <- 0.25 * xgboost_test_first_round[,19]

labels_train_first_round <- as.matrix(xgboost_train_first_round[,19])
xgboost_trainfinal_first_round <- as.matrix(xgboost_train_first_round[, c(11:14,20:23)])
xgboost_testfinal_first_round <- as.matrix(xgboost_test_first_round[, c(11:14,20:23)])

first_round_model <- xgboost(data = xgboost_trainfinal_first_round, label = labels_train_first_round, nrounds = 100, objective = "reg:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

first_round_predictions <- as.data.frame(
  matrix(predict(first_round_model, as.matrix(first_round[,c(11:14,20:23)])) * 4)
)

first_round_test <- cbind(first_round, first_round_predictions) %>%
  filter(season > 2018) %>%
  mutate(pred_games = round(V1)) %>%
  select(-V1)

first_round_opp <- first_round %>%
  mutate(winner = ifelse(high_win == 1, high_id, low_id)) %>%
  select(season, winner, opp_games = loser_games)

# starting with the conf_sf, high does not necessarily mean its going to have a higher seed depending on how the playoff database listed them

conf_sf <- left_join(conf_sf, first_round_opp, by = c("season", "high_id"="winner")) %>% mutate(high_opp_games = opp_games) %>% select(-opp_games) 
conf_sf <- left_join(conf_sf, first_round_opp, by = c("season", "low_id"="winner")) %>% mutate(low_opp_games = opp_games) %>% select(-opp_games) 

conf_sf <- conf_sf %>% mutate(high_games = ifelse(high_win == 1, winner_games, loser_games))

factor_data_conf_sf <- conf_sf %>%
  mutate(high_seed = as.factor(high_seed), low_seed = as.factor(low_seed), high_opp_games = as.factor(high_opp_games), low_opp_games = as.factor(low_opp_games)) %>%
  select(high_seed, low_seed, high_opp_games, low_opp_games)

dummy_conf_sf <- dummyVars(" ~ .", data = factor_data_conf_sf)
new_factor_conf_sf <- data.frame(predict(dummy_conf_sf, newdata = factor_data_conf_sf))

conf_sf <- cbind(conf_sf, new_factor_conf_sf) %>% select(-high_seed, -low_seed, -high_opp_games, -low_opp_games)

xgboost_train_conf_sf <- conf_sf %>%
  filter(season <= 2018)

xgboost_test_conf_sf <- conf_sf %>%
  filter(season > 2018)

xgboost_train_conf_sf[,18] <- 0.25 * xgboost_train_conf_sf[,18]
xgboost_test_conf_sf[,18] <- 0.25 * xgboost_test_conf_sf[,18]

labels_train_conf_sf <- as.matrix(xgboost_train_conf_sf[,18])
xgboost_trainfinal_conf_sf <- as.matrix(xgboost_train_conf_sf[, c(10,12:13,19:38)])
xgboost_testfinal_conf_sf <- as.matrix(xgboost_test_conf_sf[, c(10,12:13,19:38)])

conf_sf_model <- xgboost(data = xgboost_trainfinal_conf_sf, label = labels_train_conf_sf, nrounds = 100, objective = "reg:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

conf_sf_predictions <- as.data.frame(
  matrix(predict(conf_sf_model, as.matrix(conf_sf[,c(10,12:13,19:38)])) * 4)
)

conf_sf_test <- cbind(conf_sf, conf_sf_predictions) %>%
  filter(season > 2018) %>%
  mutate(pred_games = round(V1)) %>%
  select(-V1)

conf_sf_opp <- conf_sf %>%
  mutate(winner = ifelse(high_win == 1, high_id, low_id)) %>%
  select(season, winner, opp_games = loser_games)

conf_f <- left_join(conf_f, conf_sf_opp, by = c("season", "high_id"="winner")) %>% mutate(high_opp_games = opp_games) %>% select(-opp_games) 
conf_f <- left_join(conf_f, conf_sf_opp, by = c("season", "low_id"="winner")) %>% mutate(low_opp_games = opp_games) %>% select(-opp_games) 

conf_f <- conf_f %>% mutate(high_games = ifelse(high_win == 1, winner_games, loser_games))

factor_data_conf_f <- conf_f %>%
  mutate(high_seed = as.factor(high_seed), low_seed = as.factor(low_seed), high_opp_games = as.factor(high_opp_games), low_opp_games = as.factor(low_opp_games)) %>%
  select(high_seed, low_seed, high_opp_games, low_opp_games)

dummy_conf_f <- dummyVars(" ~ .", data = factor_data_conf_f)
new_factor_conf_f <- data.frame(predict(dummy_conf_f, newdata = factor_data_conf_f))

conf_f <- cbind(conf_f, new_factor_conf_f) %>% select(-high_seed, -low_seed, -high_opp_games, -low_opp_games)

xgboost_train_conf_f <- conf_f %>%
  filter(season <= 2018)

xgboost_test_conf_f <- conf_f %>%
  filter(season > 2018)

xgboost_train_conf_f[,18] <- 0.25 * xgboost_train_conf_f[,18]
xgboost_test_conf_f[,18] <- 0.25 * xgboost_test_conf_f[,18]

labels_train_conf_f <- as.matrix(xgboost_train_conf_f[,18])
xgboost_trainfinal_conf_f <- as.matrix(xgboost_train_conf_f[, c(10,12:13,19:35)])
xgboost_testfinal_conf_f <- as.matrix(xgboost_test_conf_f[, c(10,12:13,19:35)])

conf_f_model <- xgboost(data = xgboost_trainfinal_conf_f, label = labels_train_conf_f, nrounds = 100, objective = "reg:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

conf_f_predictions <- as.data.frame(
  matrix(predict(conf_f_model, as.matrix(conf_f[,c(10,12:13,19:35)])) * 4)
)

conf_f_test <- cbind(conf_f, conf_f_predictions) %>%
  filter(season > 2018) %>%
  mutate(pred_games = round(V1)) %>%
  select(-V1)

conf_f_opp <- conf_f %>%
  mutate(winner = ifelse(high_win == 1, high_id, low_id)) %>%
  select(season, winner, opp_games = loser_games)

# for the finals, high and low are still used as words but not necessarily true - its now more of the team with home advantage and team without

finals <- left_join(finals, conf_f_opp, by = c("season", "high_id"="winner")) %>% mutate(high_opp_games = opp_games) %>% select(-opp_games) 
finals <- left_join(finals, conf_f_opp, by = c("season", "low_id"="winner")) %>% mutate(low_opp_games = opp_games) %>% select(-opp_games) 

finals <- finals %>% mutate(high_games = ifelse(high_win == 1, winner_games, loser_games))

finals <- finals %>% mutate(high_seed = as.factor(high_seed), low_seed = as.factor(low_seed), high_opp_games = as.factor(high_opp_games), low_opp_games = as.factor(low_opp_games))

multinom_train_finals <- finals %>%
  filter(season <= 2018)

multinom_test_finals <- finals %>%
  filter(season > 2018)

multinom_train_finals$high_games <- factor(multinom_train_finals$high_games)

finals_model <- multinom(high_games ~ diff_pct + diff_ppg + diff_oppg + high_seed + low_seed + high_opp_games + low_opp_games, data = multinom_train_finals)

finals_predictions <- data.frame(as.numeric(predict(finals_model, newdata = finals, type = "class")))

names(finals_predictions) <- "V1"

finals_test <- cbind(finals, finals_predictions) %>%
  filter(season > 2018) %>%
  mutate(pred_games = round(V1)) %>%
  select(-V1)

logos <- espn_nba_teams() %>%
  select(team = abbreviation, logo)

upsets_first_round <- first_round_test %>%
  filter((high_win == 0 & pred_games == 4) | (high_win == 1 & pred_games < 4)) %>%
  mutate(situation = ifelse(high_games == 4, "Higher Seed Won, Lower Seed Predicted to Win", "Lower Seed Won, Higher Seed Predicted to Win")) %>%
  group_by(situation) %>%
  select(season, high_abbr, low_abbr, high_games, pred_games) %>%
  left_join(logos, by = c("high_abbr"="team")) %>% 
  mutate(high_abbr = logo) %>%
  select(-logo) %>%
  left_join(logos, by = c("low_abbr"="team")) %>% 
  mutate(low_abbr = logo) %>%
  select(-logo) %>%
  arrange(situation)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>hoopR</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

playoffs_pic <- "https://content.sportslogos.net/logos/6/981/full/2111__nba_playoffs-primary-2018.png"

upsets_1 <- nrow(upsets_first_round)
total_1 <- nrow(first_round_test)
percentage_1 <- upsets_1/total_1 * 100

gt_first_round <- upsets_first_round %>% gt() %>%
  gt_img_rows(columns = high_abbr, height = 50) %>%
  gt_img_rows(columns = low_abbr, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(season, high_abbr, low_abbr, high_games, pred_games)
  ) %>%
  gt_hulk_col_numeric(high_games) %>%
  gt_hulk_col_numeric(pred_games) %>%
  cols_label(
    season = md("**Season**"),
    high_abbr = md("**Higher Seed Team**"),
    low_abbr = md("**Lower Seed Team**"),
    high_games = md("**Actual Games Won**"),
    pred_games = md("**Predicted Games Won**")
  ) %>%
  tab_header(
    title = add_text_img(url = playoffs_pic, " FIRST ROUND UPSETS", left = FALSE, height = 30),
    subtitle = md(paste0("*Displayed For **2019 - 2023 (Excluding 2020)** | **", upsets_1, " of ", total_1, " (", percentage_1, "%)** Designated as Upsets*"))
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(season, high_games, pred_games)
    )
  ) %>%
  tab_options(
    row_group.background.color = "black",
    row_group.font.weight = "bold",
    row_group.text_transform = "capitalize"
  ) %>%
  tab_style(
    style = cell_text(align = "center", style = "italic"),
    locations = cells_row_groups(everything()))

upsets_conf_sf <- conf_sf_test %>%
  filter((high_win == 0 & pred_games == 4) | (high_win == 1 & pred_games < 4)) %>%
  mutate(situation = ifelse(high_games == 4, "Higher Seed Won, Lower Seed Predicted to Win", "Lower Seed Won, Higher Seed Predicted to Win")) %>%
  group_by(situation) %>%
  select(season, high_abbr, low_abbr, high_games, pred_games) %>%
  left_join(logos, by = c("high_abbr"="team")) %>% 
  mutate(high_abbr = logo) %>%
  select(-logo) %>%
  left_join(logos, by = c("low_abbr"="team")) %>% 
  mutate(low_abbr = logo) %>%
  select(-logo) %>%
  arrange(situation)

upsets_2 <- nrow(upsets_conf_sf)
total_2 <- nrow(conf_sf_test)
percentage_2 <- upsets_2/total_2 * 100

gt_conf_sf <- upsets_conf_sf %>% gt() %>%
  gt_img_rows(columns = high_abbr, height = 50) %>%
  gt_img_rows(columns = low_abbr, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(season, high_abbr, low_abbr, high_games, pred_games)
  ) %>%
  gt_hulk_col_numeric(high_games) %>%
  gt_hulk_col_numeric(pred_games) %>%
  cols_label(
    season = md("**Season**"),
    high_abbr = md("**Higher Seed Team**"),
    low_abbr = md("**Lower Seed Team**"),
    high_games = md("**Actual Games Won**"),
    pred_games = md("**Predicted Games Won**")
  ) %>%
  tab_header(
    title = add_text_img(url = playoffs_pic, " CONFERENCE SEMIFINALS UPSETS", left = FALSE, height = 30),
    subtitle = md(paste0("*Displayed For **2019 - 2023 (Excluding 2020)** | **", upsets_2, " of ", total_2, " (", percentage_2, "%)** Designated as Upsets*"))
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(season, high_games, pred_games)
    )
  ) %>%
  tab_options(
    row_group.background.color = "black",
    row_group.font.weight = "bold",
    row_group.text_transform = "capitalize"
  ) %>%
  tab_style(
    style = cell_text(align = "center", style = "italic"),
    locations = cells_row_groups(everything()))

upsets_conf_f <- conf_f_test %>%
  filter((high_win == 0 & pred_games == 4) | (high_win == 1 & pred_games < 4)) %>%
  mutate(situation = ifelse(high_games == 4, "Higher Seed Won, Lower Seed Predicted to Win", "Lower Seed Won, Higher Seed Predicted to Win")) %>%
  group_by(situation) %>%
  select(season, high_abbr, low_abbr, high_games, pred_games) %>%
  left_join(logos, by = c("high_abbr"="team")) %>% 
  mutate(high_abbr = logo) %>%
  select(-logo) %>%
  left_join(logos, by = c("low_abbr"="team")) %>% 
  mutate(low_abbr = logo) %>%
  select(-logo) %>%
  arrange(situation)

upsets_3 <- nrow(upsets_conf_f)
total_3 <- nrow(conf_f_test)
percentage_3 <- upsets_3/total_3 * 100

gt_conf_f <- upsets_conf_f %>% gt() %>%
  gt_img_rows(columns = high_abbr, height = 50) %>%
  gt_img_rows(columns = low_abbr, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(season, high_abbr, low_abbr, high_games, pred_games)
  ) %>%
  gt_hulk_col_numeric(high_games) %>%
  gt_hulk_col_numeric(pred_games) %>%
  cols_label(
    season = md("**Season**"),
    high_abbr = md("**Higher Seed Team**"),
    low_abbr = md("**Lower Seed Team**"),
    high_games = md("**Actual Games Won**"),
    pred_games = md("**Predicted Games Won**")
  ) %>%
  tab_header(
    title = add_text_img(url = playoffs_pic, " CONFERENCE FINALS UPSETS", left = FALSE, height = 30),
    subtitle = md(paste0("*Displayed For **2019 - 2023 (Excluding 2020)** | **", upsets_3, " of ", total_3, " (", percentage_3, "%)** Designated as Upsets*"))  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(season, high_games, pred_games)
    )
  ) %>%
  tab_options(
    row_group.background.color = "black",
    row_group.font.weight = "bold",
    row_group.text_transform = "capitalize"
  ) %>%
  tab_style(
    style = cell_text(align = "center", style = "italic"),
    locations = cells_row_groups(everything()))

upsets_finals <- finals_test %>%
  filter((high_win == 0 & pred_games == 4) | (high_win == 1 & pred_games < 4)) %>%
  mutate(situation = ifelse(high_games == 4, "Home Advantage Team Won, Home Disadvantage Team Predicted to Win", "Home Disadvantage Team Won, Home Advantage Team Predicted to Win")) %>%
  group_by(situation) %>%
  select(season, high_abbr, low_abbr, high_games, pred_games) %>%
  left_join(logos, by = c("high_abbr"="team")) %>% 
  mutate(high_abbr = logo) %>%
  select(-logo) %>%
  left_join(logos, by = c("low_abbr"="team")) %>% 
  mutate(low_abbr = logo) %>%
  select(-logo) %>%
  arrange(situation)

upsets_4 <- nrow(upsets_finals)
total_4 <- nrow(finals_test)
percentage_4 <- upsets_4/total_4 * 100

gt_finals <- upsets_finals %>% gt() %>%
  gt_img_rows(columns = high_abbr, height = 50) %>%
  gt_img_rows(columns = low_abbr, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(season, high_abbr, low_abbr, high_games, pred_games)
  ) %>%
  gt_hulk_col_numeric(high_games) %>%
  gt_hulk_col_numeric(pred_games) %>%
  cols_label(
    season = md("**Season**"),
    high_abbr = md("**Home Advantage Team**"),
    low_abbr = md("**Home Disadvantage Team**"),
    high_games = md("**Actual Games Won**"),
    pred_games = md("**Predicted Games Won**")
  ) %>%
  tab_header(
    title = add_text_img(url = playoffs_pic, " FINALS UPSETS", left = FALSE, height = 30),
    subtitle = md(paste0("*Displayed For **2019 - 2023 (Excluding 2020)** | **", upsets_4, " of ", total_4, " (", percentage_4, "%)** Designated as Upsets*"))
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(season, high_games, pred_games)
    )
  ) %>%
  tab_options(
    row_group.background.color = "black",
    row_group.font.weight = "bold",
    row_group.text_transform = "capitalize"
  ) %>%
  tab_style(
    style = cell_text(align = "center", style = "italic"),
    locations = cells_row_groups(everything()))
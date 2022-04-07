

library(nflplotR)
library(ggplot2)
library(nflreadr)


pbp <- nflreadr::load_pbp(2020) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

offense <- pbp %>%
  dplyr::group_by(team = posteam) %>%
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))

defense <- pbp %>%
  dplyr::group_by(team = defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))

combined <- offense %>%
  dplyr::inner_join(defense, by = "team")

qbs <- pbp %>%
  dplyr::filter(pass == 1 | rush == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.rm = TRUE),
    yards_gained = sum(yards_gained,na.rm = TRUE),
    air_yards = sum(air_yards,na.rm = TRUE),
    yards_after_catch = sum(yards_after_catch,na.rm = TRUE)
  ) %>%
  dplyr::filter(plays > 200)






qbs2 <- pbp %>%
  dplyr::filter(rush == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(rusher_id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.rm = TRUE),
    yards_gained = sum(yards_gained,na.rm = TRUE),
    air_yards = sum(air_yards,na.rm = TRUE),
    yards_after_catch = sum(yards_after_catch,na.rm = TRUE)
  ) %>%
  dplyr::filter(plays > 100)






qbs3 <- pbp %>% filter(incomplete_pass == 0) %>% 
  dplyr::filter(pass == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(receiver_id) %>%
  dplyr::summarise(
    name = dplyr::first(fantasy_player_name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.rm = TRUE),
    yards_gained = sum(yards_gained,na.rm = TRUE),
    air_yards = sum(air_yards,na.rm = TRUE),
    yards_after_catch = sum(yards_after_catch,na.rm = TRUE)
  ) %>%
  dplyr::filter(plays > 50)

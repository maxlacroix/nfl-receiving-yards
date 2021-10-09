games_so_far <- nflfastR::fast_scraper_schedules(2021) %>%
  dplyr::pull(game_id)

data_play_by_play_2021 <- nflfastR::load_pbp(2021) %>% 
  dplyr::filter(season_type == "REG") 


data_group_par_receveur <- data_play_by_play_2021 %>% 
  dplyr::filter(pass == 1) %>% 
  group_by(receiver_player_id, receiver, week, posteam, defteam) %>% 
  summarise(yards = sum(yards_gained)) %>%
  ungroup() %>% 
  dplyr::filter(!is.na(receiver_player_id))


top_4_receivers_lagged <- data_group_par_receveur %>% 
  group_by(receiver) %>% 
  mutate(yards_cumu = cummean(yards)) %>% 
  group_by(posteam, week) %>% 
  arrange(desc(yards_cumu)) %>% 
  mutate(rang = row_number()) %>%
  ungroup() %>% 
#  dplyr::filter(rang <= 3) %>% 
  arrange(posteam, week, rang) %>% 
  group_by(receiver, posteam) %>% 
  mutate(rang_before = dplyr::lag(rang, 1),
         yards_cumu_before = dplyr::lag(yards_cumu,1)) %>% 
  ungroup() %>% 
  select(receiver_player_id,
         receiver,
         week,
         posteam,
         defteam,
         yards,
         yards_cumu_before,
         yards_cumu,
         rang_before)


estimates_week_4 <- top_4_receivers_lagged %>% 
  filter(week %in% c(2,3)) %>% 
  group_by(defteam, week) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(yards ~ - 1 + yards_cumu_before, data = .x) %>% 
                       tidy)) %>% 
  unnest(c(model,
         data)) %>% 
  group_by(defteam) %>%
  summarise(average_estimate = mean(estimate)) 


matchups_week_4 <- nflfastR::fast_scraper_schedules(2021) %>% 
  filter(week == 4) %>% 
  select(posteam = away_team,
         defteam_week_4 = home_team) %>% 
  bind_rows(
    nflfastR::fast_scraper_schedules(2021) %>% 
      filter(week == 4) %>% 
      select(posteam = home_team,
             defteam_week_4 = away_team)
  )



estimates_week_4_for_players <- top_4_receivers_lagged %>% 
  filter(week == 3) %>% 
  left_join(matchups_week_4, by = "posteam") %>% 
  select(receiver,
         posteam,
         yards_cumu,
         defteam_week_4) %>% 
  left_join(estimates_week_4, by = c("defteam_week_4" = "defteam")) %>% 
  mutate(final_estimate = yards_cumu * average_estimate) %>% 
  arrange(desc(final_estimate))


top_4_receivers_lagged %>% 
  filter(week == 4) %>% 
  left_join(estimates_week_4_for_players %>% 
              select(-yards_cumu)) %>% 
  ggplot(aes(x = final_estimate,
             y = yards)) + 
  geom_point() +
  geom_label(aes(label = receiver)) +
  geom_abline(slope = 1) +
  theme_bw()
  

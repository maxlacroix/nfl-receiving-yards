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
  dplyr::filter(rang_before <= 5) %>% 
  select(receiver_player_id,
         receiver,
         week,
         posteam,
         defteam,
         yards,
         yards_cumu_before,
         rang_before)


predicted_week_3 <- top_4_receivers_lagged %>% 
  filter(week == 2) %>% 
  group_by(defteam, week) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(yards ~ - 1 + yards_cumu_before, data = .x) %>% 
                       tidy)) %>% 
  unnest(c(model,
         data)) %>% 
  mutate(predicted_yards = estimate*yards_cumu_before) 


predicted_week_3%>% 
  ggplot(aes(x = predicted_yards,
             y = yards)) +
  geom_point() +
  geom_abline(slope = 1, col = "red") +
  theme_bw() 

predicted_week_3 %>% 
  ungroup() %>% 
  summarise(MAE(predicted_yards,yards))


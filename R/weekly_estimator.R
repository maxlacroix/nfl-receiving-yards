week_predictor <- function(week_to_predict,
                           year_to_predict){
  
  
  games_so_far <- nflfastR::fast_scraper_schedules(year_to_predict) %>%
    dplyr::pull(game_id)
  
  data_play_by_play_2021 <- nflfastR::load_pbp(year_to_predict) %>% 
    dplyr::filter(season_type == "REG") 
  
  
  data_group_par_receveur <- data_play_by_play_2021 %>% 
    dplyr::filter(pass == 1) %>% 
    group_by(receiver_player_id, receiver, week, posteam, defteam) %>% 
    summarise(yards = sum(yards_gained), .groups = "keep") %>%
    ungroup() %>% 
    dplyr::filter(!is.na(receiver_player_id))
  
  
  receivers_lagged <- data_group_par_receveur %>% 
    group_by(receiver) %>% 
    mutate(yards_cumu = cummean(yards)) %>% 
    group_by(posteam, week) %>% 
    arrange(desc(yards_cumu)) %>% 
    mutate(rang = row_number()) %>%
    ungroup() %>% 
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
  
  
  estimates_week_prior <- receivers_lagged %>% 
    dplyr::filter(week %in% c(2:(week_to_predict-1))) %>% 
    group_by(defteam, week) %>% 
    nest() %>% 
    mutate(model = map(data, ~lm(yards ~ - 1 + yards_cumu_before, data = .x) %>% 
                         tidy)) %>% 
    unnest(c(model,
             data)) %>% 
    group_by(defteam) %>%
    summarise(average_estimate = mean(estimate),
              average_std_error = mean(std.error)) 
  
  
  matchups_week_to_predict <- nflfastR::fast_scraper_schedules(year_to_predict) %>% 
    dplyr::filter(week == week_to_predict) %>% 
    select(posteam = away_team,
           defteam_week = home_team) %>% 
    bind_rows(
      nflfastR::fast_scraper_schedules(year_to_predict) %>% 
        dplyr::filter(week == week_to_predict) %>% 
        select(posteam = home_team,
               defteam_week = away_team)
    )
  
  
  
  estimates_week_4_for_players <- receivers_lagged %>% 
    dplyr::filter(week == week_to_predict - 1) %>% 
    left_join(matchups_week_to_predict, by = "posteam") %>% 
    select(receiver,
           posteam,
           yards_cumu,
           defteam_week) %>% 
    left_join(estimates_week_prior, by = c("defteam_week" = "defteam")) %>% 
    mutate(final_estimate = yards_cumu * average_estimate,
           min_estimate = yards_cumu * (average_estimate - (1.96 * average_std_error)),
           max_estimate = yards_cumu * (average_estimate + (1.96 * average_std_error))) %>% 
    arrange(desc(final_estimate))
  
  estimates_week_4_for_players
  
  
}

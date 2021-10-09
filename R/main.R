source("R/weekly_estimator.R")

estimates_week_5 <- week_predictor(week_to_predict = 5,
                                   year_to_predict = 2021) 


estimates_week_5 %>% 
  dplyr::filter(defteam_week == "NYJ" | posteam == "NYJ")

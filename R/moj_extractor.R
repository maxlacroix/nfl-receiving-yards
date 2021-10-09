url <- paste0("https://content.mojp-sgdigital-jel.com/content-service/api/v1/q",
              "/resulted-event-list?drilldownTagIds=55&startTimeFrom=2021-09-01",
              "T04%3A00%3A00Z&startTimeTo=2021-10-01T03%3A59%3A59Z")


table_nfl <- fromJSON(url) %>% 
  pluck("data") %>% 
  pluck("eventResults") %>% 
  filter(!str_detect(name, "DNU")) # truc bizarre dans le data
  

tic()

table_nfl_results <- table_nfl %>%
  mutate(url_2 = paste0("https://content.mojp-sgdigital-jel.com/content-service/",
                        "api/v1/q/resulted-events?eventIds=",
                        id,
                        "&includeChildMarkets=true&includeRace=true",
                        "&includeRunners=true")) %>% 
  group_by(id, url_2) %>% 
  nest() %>% 
  mutate(extract = list(fromJSON(url_2) %>% 
                          pluck("data") %>% 
                          pluck("eventResults") %>% 
                          pluck("markets") %>%
                          as.data.frame() %>% 
                          filter(str_detect(name, "Total Reception Yard"))))


toc()


url_test <- "https://content.mojp-sgdigital-jel.com/content-service/api/v1/q/resulted-events?eventIds=268478&includeChildMarkets=true&includeRace=true&includeRunners=true"


data_1 <- fromJSON(url_test)

xd <- data_1 %>% 
  pluck("data") %>% 
  pluck("eventResults") %>% 
  pluck("markets") %>%
  as.data.frame() %>% 
  filter(str_detect(name, "Total Reception Yard")) %>% 
  select(name_player = name,
         outcomes,
         handicapValue) %>% 
  unnest("outcomes") %>% 
  unnest(prices)

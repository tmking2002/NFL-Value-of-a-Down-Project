library(tidyverse)
library(nflfastR)
library(gt)
library(gtExtras)

# Load NFL pbp for this season

source("Conversion Rates.R")

pbp <- load_pbp()

penalties <- pbp %>%
  filter(str_detect(tolower(desc), "penalty"))

conversion_rates <- data.frame(expand.grid(1:4, 1:99, 1:99)) %>% 
  `names<-`(c("down", "ydstogo", "yardline_100"))

conversion_rates$pred <- predict(conversion_model, conversion_rates, type = "response")
  
# extract player, type of penalty, yards, and accepted/declined
holding_info <- penalties %>%
  drop_na(down) %>% 
  filter(!str_detect(desc,"(?i)punt|field goal|FUMBLES")) %>% 
  filter(!(str_detect(desc, "Penalty") & str_detect(desc, "PENALTY")) & str_detect(desc,"Holding")) %>% # Remove offsetting penalties
  mutate(penalty_team = str_extract(desc, "(?i)(?<=penalty on\\s)\\w+"),
         penalty_yards = as.integer(str_extract(desc, "(?<=, )[0-9]+(?= yards)")),
         penalty_accepted = !str_detect(tolower(desc), "declined"),
         penalty_type = ifelse(is.na(penalty_type), 
                               ifelse(str_detect(desc,"Offensive"), "Offensive Holding", "Defensive Holding"), penalty_type),
         penalty_yards = ifelse(posteam == penalty_team, -penalty_yards, penalty_yards),
         penalty_yards = ifelse(is.na(penalty_yards), 
                                case_when(penalty_type == "Offensive Holding" & yardline_100 >= 80 ~ floor((100 - yardline_100) / 2),
                                          penalty_type == "Defensive Holding" & yardline_100 <= 20 ~ floor(yardline_100 / 2),
                                          penalty_type == "Offensive Holding" ~ -10,
                                          penalty_type == "Defensive Holding" ~ 10), penalty_yards),
         decline_down = case_when(yards_gained >= ydstogo ~ 1,
                                  TRUE ~ down + 1),
         decline_ydstogo = case_when(yards_gained >= ydstogo ~ 10,
                                     TRUE ~ ydstogo - yards_gained),
         decline_yardline_100 = yardline_100 - yards_gained,
         accept_down = case_when(penalty_yards >= ydstogo ~ 1,
                                 TRUE ~ down),
         accept_ydstogo = case_when(penalty_yards >= ydstogo ~ 10,
                                    TRUE ~ ydstogo - penalty_yards),
         accept_yardline_100 = yardline_100 - penalty_yards,
         decision_team = ifelse(penalty_type == "Offensive Holding", "Defense", "Offense")) %>% 
  select(game_id, play_id, desc, down, ydstogo, yardline_100, yards_gained, penalty_yards, penalty_accepted, decision_team, 
         decline_down, decline_ydstogo, decline_yardline_100, accept_down, accept_ydstogo, accept_yardline_100)


expected_values <- holding_info %>% 
  merge(conversion_rates, by.x = c("decline_down", "decline_ydstogo", "decline_yardline_100"),
                         by.y = c("down", "ydstogo", "yardline_100")) %>% 
  rename(decline_pred = pred) %>% 
  merge(conversion_rates, by.x = c("accept_down", "accept_ydstogo", "accept_yardline_100"),
        by.y = c("down", "ydstogo", "yardline_100")) %>% 
  rename(accept_pred = pred) %>% 
  select(game_id, play_id, desc, penalty_accepted, decision_team, down, ydstogo, yardline_100,
         decline_down, decline_ydstogo, decline_yardline_100, decline_pred, 
         accept_down, accept_ydstogo, accept_yardline_100, accept_pred) %>% 
  mutate(correct_call = case_when(penalty_accepted == T & (accept_pred > decline_pred) & decision_team == "Offense" ~ T,
                                  penalty_accepted == T & (accept_pred < decline_pred) & decision_team == "Offense" ~ F,
                                  penalty_accepted == T & (accept_pred > decline_pred) & decision_team == "Defense" ~ F,
                                  penalty_accepted == T & (accept_pred < decline_pred) & decision_team == "Defense" ~ T,
                                  penalty_accepted == F & (accept_pred > decline_pred) & decision_team == "Offense" ~ F,
                                  penalty_accepted == F & (accept_pred < decline_pred) & decision_team == "Offense" ~ T,
                                  penalty_accepted == F & (accept_pred > decline_pred) & decision_team == "Defense" ~ T,
                                  penalty_accepted == F & (accept_pred < decline_pred) & decision_team == "Defense" ~ F),
         diff = abs(accept_pred - decline_pred))


expected_values %>% 
  group_by(decision_team) %>% 
  summarise(accuracy = mean(correct_call))

expected_values %>% 
  group_by(down) %>% 
  summarise(accuracy = mean(correct_call))

write_csv(expected_values, "holding_penalties_2022.csv")

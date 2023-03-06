library(tidyverse)
library(nflfastR)
library(gt)
library(gtExtras)

# Load NFL pbp for this season

pbp <- load_pbp()

penalties <- pbp %>%
  filter(str_detect(tolower(desc), "penalty"))

expected_points <- pbp %>% 
  group_by(down, ydstogo, yardline_100) %>% 
  summarise(ep = mean(ep))

# extract player, type of penalty, yards, and accepted/declined
holding_info <- penalties %>%
  drop_na(down) %>% 
  filter(!str_detect(desc,"(?i)punt|field goal")) %>% 
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
  merge(expected_points, by.x = c("decline_down", "decline_ydstogo", "decline_yardline_100"),
                         by.y = c("down", "ydstogo", "yardline_100")) %>% 
  rename(decline_ep = ep) %>% 
  merge(expected_points, by.x = c("accept_down", "accept_ydstogo", "accept_yardline_100"),
        by.y = c("down", "ydstogo", "yardline_100")) %>% 
  rename(accept_ep = ep) %>% 
  select(game_id, play_id, penalty_accepted, decision_team, down, ydstogo, yardline_100,
         decline_down, decline_ydstogo, decline_yardline_100, decline_ep, 
         accept_down, accept_ydstogo, accept_yardline_100, accept_ep) %>% 
  mutate(correct_call = case_when(penalty_accepted == T & (accept_ep > decline_ep) & decision_team == "Offense" ~ T,
                                  penalty_accepted == T & (accept_ep < decline_ep) & decision_team == "Offense" ~ F,
                                  penalty_accepted == T & (accept_ep > decline_ep) & decision_team == "Defense" ~ F,
                                  penalty_accepted == T & (accept_ep < decline_ep) & decision_team == "Defense" ~ T,
                                  penalty_accepted == F & (accept_ep > decline_ep) & decision_team == "Offense" ~ F,
                                  penalty_accepted == F & (accept_ep < decline_ep) & decision_team == "Offense" ~ T,
                                  penalty_accepted == F & (accept_ep > decline_ep) & decision_team == "Defense" ~ T,
                                  penalty_accepted == F & (accept_ep < decline_ep) & decision_team == "Defense" ~ F),
         diff = abs(accept_ep - decline_ep))


write_csv(penalties_info, "2022 Penalties.csv")

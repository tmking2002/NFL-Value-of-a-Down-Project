library(tidyverse)
library(nflfastR)
library(gt)
library(gtExtras)

# Load NFL pbp for this season

pbp <- load_pbp() 

# Find avg pass, rush, and total epa for each team

team_stats_raw <- pbp %>% 
  group_by(game_id, home_team, away_team) %>% 
  summarise(home_rush_epa = last(total_home_rush_epa),
            home_pass_epa = last(total_home_pass_epa),
            home_epa = last(total_home_epa),
            away_rush_epa = last(total_away_rush_epa),
            away_pass_epa = last(total_away_pass_epa),
            away_epa = last(total_away_epa)) %>% 
  ungroup()

team_stats <- rbind(team_stats_raw[c(2,4:6)] %>% `names<-`(c("team","rush_epa","pass_epa","epa")), 
                    team_stats_raw[c(3,7:9)] %>% `names<-`(c("team","rush_epa","pass_epa","epa"))) %>% 
  group_by(team) %>% 
  summarise(avg_rush_epa = mean(rush_epa),
            avg_pass_epa = mean(pass_epa),
            avg_epa = mean(epa)) %>% 
  ungroup()

pbp_upd <- pbp  %>% 
  drop_na(down, series_success) %>% 
  merge(team_stats, by.x = "posteam", by.y = "team")

set.seed(100)

sample <- sample(c(TRUE, FALSE), nrow(pbp_upd), replace=TRUE, prob=c(0.7,0.3))
train  <- pbp_upd[sample, ]
test   <- pbp_upd[!sample, ]

# Creates basic conversion model using just down, yards to go, and yards to endzone

conversion_model <- glm(series_success ~ down + ydstogo + yardline_100, data = train, family = binomial)

# Creates model that accounts for offensive prowess of team

conversion_model_bystats <- glm(series_success ~ down + ydstogo + yardline_100 + avg_epa, data = train, family = binomial)


# Test effectiveness of model 1

test_1 <- predict(conversion_model, test, type = "response")
test$predict <- test_1 > .5
test$correct <- test$series_success == test$predict
accuracy_1 <- mean(test$correct)


# Test effectiveness of model 2

test_2 <- predict(conversion_model_bystats, test, type = "response")
test$predict <- test_2 > .5
test$correct <- test$series_success == test$predict
accuracy_2 <- mean(test$correct)

# The first model seems to be the most accurate model

# A function that predicts the likelihood of conversion using one of the two model based on whether
# a team is included in the arguments

find_conversion_rate <- function(down, ydstogo, yardline_100){
return(predict(conversion_model, data.frame(down = down,
                                            ydstogo = ydstogo,
                                            yardline_100 = yardline_100)))

  
}

# Finds frequency of conversion by every combination of down, yards to go, and yards to endzone

conversion_rates <- pbp_upd %>% 
  group_by(down, ydstogo, yardline_100) %>% 
  summarise(conversion = mean(series_success),
            count = n())

# Example: 75 yards to go, defense gets a TFL for x yards and the offense commits a 10 yd penalty

# At what x value should the defense accept/decline the penalty?

# This number is the model's prediction of a team converting on first and 20 from their own 15

first_and_20_conversion_model <- find_conversion_rate(1,20,85)

# This number is the actual percentage of times during this season that a team converted on first and 20 from their own 15

first_and_20_conversion_actual <- conversion_rates %>% 
  filter(down == 1 & ydstogo == 20 & yardline_100 == 85) %>% 
  pull(conversion)

x = 1:10

# Makes a dataframe to serve as the outline for the table later

table <- data.frame(yd_gain = -x,
                    decline_down = 2,
                    decline_ydstogo = 10 + x,
                    decline_yardline_100 = 75 + x,
                    accept_down = 1,
                    accept_ydstogo = 20,
                    accept_yardline_100 = 85)

# Find conversion probability for each scenario in the table

table$conversion_prob_decline = find_conversion_rate(table[[2]],table[[3]],table[[4]])
table$conversion_prob_accept = find_conversion_rate(table[[5]],table[[6]],table[[7]])

# Create a table in the gt package

table %>% 
  select(yd_gain, decline_down, decline_ydstogo, decline_yardline_100, conversion_prob_decline, 
         accept_down, accept_ydstogo, accept_yardline_100, conversion_prob_accept) %>% 
  gt() %>% 
  fmt_percent(c(5,9), decimals = 1) %>% 
  tab_spanner(label = "Declined Penalty", columns = 2:5) %>% 
  tab_spanner(label = "Accepted Penalty", columns = 6:9) %>% 
  tab_style(
    style = cell_fill(color = "green"),
    locations = cells_body(columns = conversion_prob_decline,
                           rows = conversion_prob_decline < conversion_prob_accept)
  ) %>%
  tab_style(
    style = cell_fill(color = "green"),
    locations = cells_body(columns = conversion_prob_accept,
                           rows = conversion_prob_accept < conversion_prob_decline)
  ) %>% 
  tab_style(
    style = cell_borders(color = "lightgrey", sides = "right"),
    locations = list(cells_body(columns = c(yd_gain, conversion_prob_decline),
                                rows = everything()),
                     cells_column_labels(columns = c(yd_gain, conversion_prob_decline)))
  ) %>% 
  cols_label(yd_gain = "Net Yards",
             decline_down = "Down",
             decline_ydstogo = "To Go",
             decline_yardline_100 = "Yds. to End Zone",
             conversion_prob_decline = "P(Convert)",
             accept_down = "Down",
             accept_ydstogo = "To Go",
             accept_yardline_100 = "Yds. to End Zone",
             conversion_prob_accept = "P(Convert)") %>% 
  tab_header(title = "When should a defense decline a holding penalty after a TFL?",
             subtitle = "Initial Condition: 1st and 10 from own 25 yard line") %>% 
  gt_theme_538() %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px") %>% 
  gtsave("Example Holding Penalty Table.png")

kc_table <- data.frame(yd_gain = -x,
                    decline_down = 2,
                    decline_ydstogo = 10 + x,
                    decline_yardline_100 = 75 + x,
                    accept_down = 1,
                    accept_ydstogo = 20,
                    accept_yardline_100 = 85)

kc_table$conversion_prob_decline = find_conversion_rate(kc_table[[2]],kc_table[[3]],kc_table[[4]],"KC")
kc_table$conversion_prob_accept = find_conversion_rate(kc_table[[5]],kc_table[[6]],kc_table[[7]],"KC")

kc_table %>% 
  select(yd_gain, decline_down, decline_ydstogo, decline_yardline_100, conversion_prob_decline, 
         accept_down, accept_ydstogo, accept_yardline_100, conversion_prob_accept) %>% 
  gt() %>% 
  fmt_percent(c(5,9), decimals = 1) %>% 
  tab_spanner(label = "Declined Penalty", columns = 2:5) %>% 
  tab_spanner(label = "Accepted Penalty", columns = 6:9) %>% 
  tab_style(
    style = cell_fill(color = "green"),
    locations = cells_body(columns = conversion_prob_decline,
                           rows = conversion_prob_decline < conversion_prob_accept)
  ) %>%
  tab_style(
    style = cell_fill(color = "green"),
    locations = cells_body(columns = conversion_prob_accept,
                           rows = conversion_prob_accept < conversion_prob_decline)
  ) %>% 
  tab_style(
    style = cell_borders(color = "lightgrey", sides = "right"),
    locations = list(cells_body(columns = c(yd_gain, conversion_prob_decline),
                                rows = everything()),
                     cells_column_labels(columns = c(yd_gain, conversion_prob_decline)))
  ) %>% 
  cols_label(yd_gain = "Net Yards",
             decline_down = "Down",
             decline_ydstogo = "To Go",
             decline_yardline_100 = "Yds. to End Zone",
             conversion_prob_decline = "P(Convert)",
             accept_down = "Down",
             accept_ydstogo = "To Go",
             accept_yardline_100 = "Yds. to End Zone",
             conversion_prob_accept = "P(Convert)") %>% 
  tab_header(title = "When should a defense decline a holding penalty after a TFL?",
             subtitle = "Initial Condition: 1st and 10 from own 25 yard line... Offensive Team = KC Chiefs") %>% 
  gt_theme_538() %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px") %>% 
  gtsave("Example Holding Penalty Table Chiefs.png")





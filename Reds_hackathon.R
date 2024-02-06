library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
setwd("~/Desktop/Desktop_folders/R_files_Non_SYR/Reds_hackathon/Reds_Hackathon_R_proj")
data <- read.csv("savant_pitch_level.csv")

#Separate data set by year
data_2021 <- data[grepl("^2021", data$game_date), ]
write.csv(data_2021, "data_2021.csv", row.names =  TRUE)
data_2022 <- data[grepl("^2022", data$game_date), ]
write.csv(data_2022, "data_2022.csv", row.names =  TRUE)
data_2023 <- data[grepl("^2023", data$game_date), ]
write.csv(data_2023, "data_2023.csv", row.names =  TRUE)

#Per Game how many batters a pitcher faced
batters_faced_per_game_pitcher_21 <- event_filtered_21 %>% 
  group_by(game_date, player_name) %>% 
  summarise(batters_faced = n())
batters_faced_per_game_pitcher_22 <- event_filtered_22 %>% 
  group_by(game_date, player_name) %>% 
  summarise(batters_faced = n())
batters_faced_per_game_pitcher_23 <- event_filtered_23 %>% 
  group_by(game_date, player_name) %>% 
  summarise(batters_faced = n())

#https://www.baseball-reference.com/leagues/majors/2023-reliever-pitching.shtml
#All the games in which a pitcher had a short relief appearance
short_rp_appearances_21 <- batters_faced_per_game_pitcher_21 %>% 
  filter(batters_faced < 5)
short_rp_appearances_22 <- batters_faced_per_game_pitcher_22 %>% 
  filter(batters_faced < 5)
short_rp_appearances_23 <- batters_faced_per_game_pitcher_23 %>% 
  filter(batters_faced < 5)

#The count per pitcher of short relief appearances
short_rp_21 <- short_rp_appearances_21 %>% 
  group_by(player_name) %>% 
  summarise(total_short_relief_appearances = n())
short_rp_22 <- short_rp_appearances_22 %>% 
  group_by(player_name) %>% 
  summarise(total_short_relief_appearances = n())
short_rp_23 <- short_rp_appearances_23 %>% 
  group_by(player_name) %>% 
  summarise(total_short_relief_appearances = n())


short_rp <- rbind(short_rp_21, short_rp_22, short_rp_23)
write.csv(short_rp, "short_rp.csv", row.names =  TRUE)





#Percentile
percentile_15 <- quantile(short_rp$total_short_relief_appearances, 0.15)
cat("15th percentile:", percentile_15, "\n")
percentile_20 <- quantile(short_rp$total_short_relief_appearances, 0.20)
cat("20th percentile:", percentile_20, "\n")

#Summary Stats
summary(short_rp$total_short_relief_appearances)

#Box and Whisker Plot of short_rp
ggplot(short_rp, aes(total_short_relief_appearances)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box-and-Whisker Plot", y = "total_short_relief_appearances")

#Histogram of short_rp
ggplot(short_rp, aes(total_short_relief_appearances)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Histogram", x = "Total short relief appearances", y = "Frequency")


#create a number that weights their success in high leverage moments with
#how many times they are in those moments (sample size higher should be weighed
#more)
#LEVERAGE ARTICLE: https://www.baseball-reference.com/blog/archives/2458.html
#creating a statistic: success in high leverage moments


#The pitch by pitch data for all short relief appearances in 21,22,23
data_srp_pbp <- data %>%
  arrange(game_date, player_name) %>%  # Sorting the data
  group_by(game_date, player_name) %>%
  filter(n() < 5)

data_srp_pbp_21 <- data_srp_pbp[grepl("^2021", data_srp_pbp$game_date), ]
data_srp_pbp_22 <- data_srp_pbp[grepl("^2022", data_srp_pbp$game_date), ]
data_srp_pbp_23 <- data_srp_pbp[grepl("^2023", data_srp_pbp$game_date), ]

data_srp_pbp$run_diff <- data_srp_pbp$fld_score - data_srp_pbp$bat_score


#Outs, Runners on base, Inning, Run differential, inning
data_srp_pbp$LI <- NA

#Create equation to calculate Leverage Index per pitch




##############################
#Leverage Index Equation (unkown how to calculate the weights per variable)
LI_equation <- function(inning_v, run_diff_v, outs_v, runner_1, runner_2, 
                        runner_3) {
  data_srp_pbp$LI <- inning_v * 0.3 + 1 / run_diff_v * 3 + 1 / outs_v * 3 + 
    runner_1 * 0.1 + runner_2 * 0.4 + runner_3 * 0.5
}

LI_equation(data_srp_pbp$inning, data_srp_pbp$run_diff, data_srp_pbp$outs_when_up, 
            data_srp_pbp$on_1b, data_srp_pbp$on_2b, data_srp_pbp$on_3b)

result <- data_srp_pbp %>% 
  rowwise() %>% 
  mutate(output = pmap_dbl(list(inning, run_diff, outs_when_up, on_1b, 
                                on_2b, on_3b), LI_equation))






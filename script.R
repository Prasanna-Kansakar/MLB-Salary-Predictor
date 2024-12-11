remove(list = ls())

# Installing necessary Packages
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(caret)
library(xgboost)
library(factoextra)
library(tibble)
library(cluster)

# Reading csv file player.csv
player_df <- read.csv('Data/player.csv')
glimpse(player_df)
summary(player_df)

# Reading csv file fielding.csv
fielding_df <- read.csv('Data/fielding.csv')
glimpse(fielding_df)
summary(fielding_df)

# Reading csv file salary.csv
salary_df <- read.csv('Data/salary.csv')
glimpse(salary_df)
summary(salary_df)

# Reading csv file batting.csv
batting_df <- read.csv('Data/batting.csv')
glimpse(batting_df)
summary(batting_df)

# Reading csv file team.csv
team_df <- read.csv('Data/team.csv')
glimpse(team_df)
summary(team_df)

# Cleaning/Merging dataset
# Merging batting and player data set into hitter_df
player_df <- player_df %>%
  mutate(full_name = paste(name_first, name_last, sep = " ") )
hitter_df <- inner_join(batting_df,
                        select(player_df,player_id,full_name,
                               weight,height,bats,throws, birth_year),
                        by = "player_id")
# Merging hitting_df and salary data set into hitter_df
hitter_df <- inner_join(hitter_df,
                        select(salary_df,player_id,year,team_id,salary),
                        by = c("player_id","year","team_id"))
# Cleaning fielding dataset
# Removing data for pitchers
fielding_df <- fielding_df %>%
  filter(pos != "P")
# Selecting the primary postion of the player
# based on the highest number of games played
# by a player in a specific year for a specific team
fielding_df <- fielding_df %>%
  group_by(player_id, year,team_id) %>%
  dplyr::slice(which.max(g))
# Merging hitting_df and fielding data set into hitter_df
hitter_df <- inner_join(hitter_df,
                        select(fielding_df,player_id,year,team_id,pos),
                        by = c("player_id","year","team_id"))
# Adding Age Column
hitter_df <- hitter_df %>%
  mutate(age = (year - birth_year))
glimpse(hitter_df)
# Merging team and salary data set into team_salary_df
team_salary_df <- hitter_df %>%
  group_by(team_id,year) %>%
  summarise(
    salary = sum(salary)
  ) %>%
  inner_join(team_df, by = c("team_id","year"))
#Handling NA values for particular columns
team_salary_df <- team_salary_df %>%
  mutate(
    ws_win = ifelse(is.na(ws_win), "", ws_win),
    div_win = ifelse(is.na(div_win), "", div_win),
    hbp = ifelse(is.na(hbp), 0, hbp),
    sf = ifelse(is.na(sf), 0, sf),
    made_playoffs = rank == 1 | div_win == "Y" | wc_win =="Y"
  )

# Analysis 1: Trends in player performance metrics over time
# Plotting line graphs to visualize player performance metrics over time
ggplot(hitter_df %>%
         group_by(year) %>%
         summarise(
           avg_runs = mean(r, na.rm = TRUE),
           avg_rbi = mean(rbi,na.rm = TRUE),
           avg_hr =  mean(hr, na.rm = TRUE),
           avg_double = mean(double, na.rm = TRUE),
           avg_triple = mean(triple, na.rm = TRUE),
           avg_sb = mean(sb, na.rm = TRUE),
           avg_sh = mean(sh, na.rm = TRUE),
           avg_sf = mean(sf, na.rm = TRUE),
         )
  , aes(x = year)) +
  geom_line(aes(y = avg_runs, color = "Runs")) +
  geom_line(aes(y = avg_rbi, color = "Rbis")) +
  geom_line(aes(y = avg_hr, color = "Home Runs")) +
  geom_line(aes(y = avg_double, color = "Doubles")) +
  geom_line(aes(y = avg_triple, color = "Triples")) +
  geom_line(aes(y = avg_sb, color = "Stolen Bases")) +
  geom_line(aes(y = avg_sh, color = "Sacrifice bunts")) +
  geom_line(aes(y = avg_sf, color = "Sacrifice fly")) +
  labs(x = "Year", y = "Performance metrics", title = "Player performance metrics over time") +
  scale_x_continuous(breaks = seq(min(hitter_df$year),
                                  max(hitter_df$year), by = 3)) +
  theme_minimal()

# Analysis 2: Player Salary Trends over time
# Plotting scatterplot to visualize the Salary changes over time
ggplot(hitter_df, aes(x=year, y=salary, color = league_id)) +
  geom_point() +
  labs(x = "Year", y = "Salary", title = "Salary changes over Time",color = "League") +
  scale_x_continuous(breaks = seq(min(hitter_df$year),
                                  max(hitter_df$year), by = 2)) +
  scale_y_continuous(labels = scales::comma,
                      breaks = seq(0, max(hitter_df$salary), by = 3000000))+
  theme_minimal()

# Analysis 3: Impact of Batting Hand on a player’s performance and salary
# Plotting pie chart to visualize distribution of batting hand of players
ggplot(hitter_df%>%
         group_by(bats) %>%
         summarise(count = n()) %>%
         mutate(percentage = count / sum(count))
  , aes(x = "", y= percentage, fill = bats)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "Batting Hand") +
  labs(title = "Hand baseball players use when batting") +
  geom_text(aes(label = paste0(round(percentage * 100), "%")), position = position_stack(vjust = 0.5)) +
  theme_void()

# Calculating average hits,rbi,salary and homerun by batting hand and year
avg_stats_by_hand <- hitter_df %>%
  group_by(bats, year) %>%
  summarise(
    avg_hits = mean(h, na.rm = TRUE),
    avg_rbi = mean(rbi,na.rm = TRUE),
    avg_salary = mean(salary, na.rm = TRUE),
    avg_hr =  mean(hr, na.rm = TRUE),
  )

# Plotting line graphs to visualize PLayer performance based on batting hand
grid.arrange(
  ggplot(avg_stats_by_hand, aes(x = year, y = avg_hits, color = bats, group = bats)) +
    geom_line() +
    labs(x = "Year", y = "Average Hits", title = "Average Hits by Batting hand and Year") +
    scale_x_continuous(breaks = seq(min(avg_stats_by_hand$year),
                                    max(avg_stats_by_hand$year), by = 5)) +
    theme_minimal(),
  ggplot(avg_stats_by_hand, aes(x = year, y = avg_hr, color = bats, group = bats)) +
    geom_line() +
    labs(x = "Year", y = "Average Home runs", title = "Average Home runs by Batting hand and Year") +
    scale_x_continuous(breaks = seq(min(avg_stats_by_hand$year),
                                    max(avg_stats_by_hand$year), by = 5)) +
    theme_minimal()+ theme(legend.position="none"),
  ggplot(avg_stats_by_hand, aes(x = year, y = avg_rbi, color = bats, group = bats)) +
    geom_line() +
    labs(x = "Year", y = "Average RBIs", title = "Average RBIs by Batting hand and Year") +
    scale_x_continuous(breaks = seq(min(avg_stats_by_hand$year),
                                    max(avg_stats_by_hand$year), by = 5)) +
    theme_minimal(),
  ggplot(avg_stats_by_hand, aes(x = year, y = avg_salary, color = bats, group = bats)) +
    geom_line() +
    labs(x = "Year", y = "Average Salary", title = "Average Salary by Batting hand and Year") +
    scale_x_continuous(breaks = seq(min(avg_stats_by_hand$year),
                                    max(avg_stats_by_hand$year), by = 5)) +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(0, max(avg_stats_by_hand$avg_salary), by = 1000000))+
    theme_minimal()+ theme(legend.position="none"),
  nrow = 2, ncol = 2, heights = c(1, 2))


# Analysis 4: Distribution of players based on fielding positions and age
# Plotting bar graph and histogram to visualize the distribution of players
# based on fielding positions and age
grid.arrange(
  ggplot(hitter_df %>%
    group_by(pos) %>%
    summarise(total_players = n()), aes(x = pos, y = total_players)) +
    geom_bar(stat = "identity", fill = "green") +
    labs(x = "Position", y = "Total Players", title = "Total Players per fielding Position") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(hitter_df, aes(x = age)) +
    geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
    scale_x_continuous(breaks = seq(min(hitter_df$age),
                                    max(hitter_df$age), by = 2)) +
    labs(x = "Age", y = "Total Players", title = "Total Players per Age") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ncol = 2
)

# Analysis 5: Player’s batting average based on fielding positions
# Adding Batting Average Column
hitter_df <- hitter_df %>%
  mutate(ba = round((h/ab), digits = 3))
# Calculating average batting average by position and age
average_ba_postion <- hitter_df %>%
  group_by(pos, age) %>%
  summarise(avg_ba = mean(ba,na.rm = TRUE))

# Calculating overall average batting average for each position
overall_avg_ba <- average_ba_postion %>%
  group_by(pos) %>%
  summarise(overall_avg_ba = mean(avg_ba, na.rm = TRUE))

# Creating function to plot points in graph to visualize the
# average batting average by position and age
plot_ba_position <- function(position_data) {
  avg_position <- (overall_avg_ba %>%
    filter(pos == position_data$pos[1]) %>%
    dplyr::slice(1))$overall_avg_ba
  ggplot(position_data, aes(x = age, y = avg_ba)) +
    geom_point() +
    scale_x_continuous(breaks = seq(min(position_data$age),
                                    max(position_data$age), by = 2)) +
    geom_hline(yintercept = avg_position, linetype = "dashed", color = "red") +
    labs(title = paste("Position:", unique(position_data$pos)), x = "Age", y = "Average Batting Average") +
    theme_minimal()
}

# Applying the function to plot graph for each position and arrange the graphs in a grid
postion_ba_plots <- lapply(split(average_ba_postion, average_ba_postion$pos), plot_ba_position)
grid.arrange(grobs = postion_ba_plots, ncol = 2)

# Analysis 6: Evolution of Player Size in the League Over Time
# Calulating the average salary, height and weight per year
weight_height_df <- hitter_df %>%
  group_by(year) %>%
  summarize(avg_weight = mean(weight), avg_height=mean(height), avg_salary= mean(salary))
# Plotting point graph to visualize average weight and height overtime
grid.arrange(
  ggplot(data = weight_height_df, aes(year, avg_weight)) +
    geom_point(color="blue3") + geom_smooth() +
    scale_x_continuous(breaks = seq(min(weight_height_df$year),
                                    max(weight_height_df$year), by = 5)) +
    labs(title="Average Weight by Year",x="YEAR", y="Average Weight"),
  ggplot(data = weight_height_df, aes(year, avg_height)) +
    geom_point(color="blue3") + geom_smooth() +
    scale_x_continuous(breaks = seq(min(weight_height_df$year),
                                    max(weight_height_df$year), by = 5)) +
    labs(title="Average Height by Year",x="YEAR", y="Average Height"),
  ncol = 2
)

# Analysis 7: Impact of a player's size on performance
# Calulating the average stolen bases, home runs, height and weight per player
performance_size_df <- hitter_df %>%
  group_by(player_id) %>%
  summarize(avg_sb=mean(sb), avg_hr=mean(hr),
            avg_weight = mean(weight), avg_height=mean(height))
# Plotting scatter plot to visualize player performance
# according to their height and weight
grid.arrange(
  ggplot(data = performance_size_df, aes(avg_height, avg_weight)) +
    geom_point(aes(color = avg_hr)) +
    geom_hline(yintercept= mean(performance_size_df$avg_weight,na.rm=TRUE)) +
    geom_vline(xintercept= mean(performance_size_df$avg_height,na.rm=TRUE)) +
    scale_x_continuous(breaks = seq(min(performance_size_df$avg_height),
                                    max(performance_size_df$avg_height), by = 4)) +
    scale_y_continuous(breaks = seq(min(performance_size_df$avg_weight),
                                    max(performance_size_df$avg_weight), by = 30)) +
    theme(panel.background = element_rect(fill = "grey")) +
    labs(title="Average Home Runs scored per height and weight", x="Height", y="Weight") +
    scale_colour_gradientn(colours=topo.colors(10))
    + theme_gray(),
  ggplot(data = performance_size_df, aes(avg_height, avg_weight)) +
    geom_point(aes(color = avg_sb)) +
    geom_hline(yintercept= mean(performance_size_df$avg_weight,na.rm=TRUE)) +
    geom_vline(xintercept= mean(performance_size_df$avg_height,na.rm=TRUE)) +
    scale_x_continuous(breaks = seq(min(performance_size_df$avg_height),
                                    max(performance_size_df$avg_height), by = 4)) +
    scale_y_continuous(breaks = seq(min(performance_size_df$avg_weight),
                                    max(performance_size_df$avg_weight), by = 30)) +
    labs(title="Average Bases Stolen per height and weight", x="Height", y="Weight") +
    scale_colour_gradientn(colours=topo.colors(10)) + theme_gray(),
   ncol = 2)

# Analysis 8: Impact of a player's size on salary
# Calulating the average height and weight per player and year
size_salary_df <- hitter_df %>%
  group_by(year, player_id) %>%
  summarize(weight = mean(weight),
            height=mean(height),
            salary = mean(salary)
  ) %>%
  left_join(weight_height_df, by = "year") %>%
  mutate(
    light = if_else(avg_weight > weight, "light", "heavy"),
    short = if_else(avg_height > height, "short", "tall")
  )
# Grouping the data by year and weight status
weight_salary_df <- size_salary_df %>%
  group_by(year,light) %>%
  summarize(
  weight = mean(weight),
  height=mean(height),
  salary = mean(salary)
  )
# Grouping the data by year and height status
height_salary_df <- size_salary_df %>%
  group_by(year,short) %>%
  summarize(
    weight = mean(weight),
    height=mean(height),
    salary = mean(salary)
  )
# Plotting line graph to visualize impact of a player's size on his salary
grid.arrange(
  ggplot(weight_salary_df,
         aes(x = year, y = salary, color = light)) +
  scale_x_continuous(breaks = seq(min(weight_salary_df$year),
                                  max(weight_salary_df$year), by = 4)) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, max(weight_salary_df$salary), by = 1000000))+
  geom_line() +
  scale_colour_discrete() +
  labs(title = "Impact of a player's weight on his salary",
       x = "Year",
       y = "Average Salary",
       color = "Weight Status") +
  theme_minimal(),
  ggplot(height_salary_df,
         aes(x = year, y = salary, color = short)) +
    scale_x_continuous(breaks = seq(min(height_salary_df$year),
                                    max(height_salary_df$year), by = 4)) +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(0, max(height_salary_df$salary), by = 1000000))+
    geom_line() +
    scale_colour_discrete() +
    labs(title = "Impact of a player's height on his salary",
         x = "Year",
         y = "Average Salary",
         color = "Height Status") +
    theme_minimal(),
  ncol = 2)

# Analysis 9: Correlation between salary, age and hitting stats
#Calculating the correlation between salary, age and hitting stats
stats_correlation <- cor(select(hitter_df, salary, age, g, ab, r, h, double, triple, hr,
                                rbi, sb, cs, bb, so, ibb, hbp, sh, sf, g_idp))
# Plotting heat map to visualize the correlation between salary, age and hitting stats
corrplot(stats_correlation, method = "color", type = "upper", addCoef.col = "black")

# Analysis 10: Identifying Standout Players Based on Their Hitting Statistics
# Adding On-base plus slugging Column
hitter_df <- hitter_df %>%
  mutate(
   ops = ((h+bb+hbp)/(ab+bb+hbp+sf))
      + (((h-double-triple-hr)+(2*double)+(3*triple)))/(ab)
  )
hitter_df$ops[is.nan(hitter_df$ops)] <- 0

standout_players  <- hitter_df %>%
      group_by(full_name,year) %>%
      summarise(avg_ops = mean(ops, na.rm = TRUE),
                total_gp  = sum(g)) %>%
      filter(total_gp > 50)
standout_players <- standout_players[order(standout_players$avg_ops, decreasing = TRUE), ]
# Plotting dot plot to visualize the Standout players based on their hitting statistics
ggplot(head(standout_players, 20), aes(x = full_name, y = avg_ops)) +
  geom_point(color = "blue") +
  geom_text(aes(label = year), vjust = -0.5, size = 3) +
  labs(title = "Standout players based on their hitting statistics", x = "Player", y = "On-base plus slugging in a single season") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analysis 11: Do higher-paid players tend to perform better?
# Calculating the top 10 and bottom 50 salary percentile and max salary per year
salary_percentile_per_year <- hitter_df %>%
  group_by(year) %>%
  summarise(
    bottom_50_salary = quantile(salary, probs = 0.5, na.rm = TRUE),
    max_salary = max(salary, na.rm = TRUE),
    top_10_salary = quantile(salary, probs = 0.9, na.rm = TRUE)
  )
# Finding the hitters whose salary in the top 10 percentile for a season
top_10_paid_player <- hitter_df %>%
  group_by(full_name,year) %>%
  summarise(avg_ops = mean(ops, na.rm = TRUE),
            total_gp  = sum(g),
            median_salary = median(salary),
  ) %>%
  filter(total_gp > 50) %>%
  inner_join(salary_percentile_per_year, by = "year") %>%
  filter(median_salary > top_10_salary)
# Plotting box plot to visualize if higher-paid players tend to perform better
ggplot(top_10_paid_player, aes(x = as.factor(year), y = avg_ops, fill = as.factor(year))) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.6) +
  geom_hline(yintercept = 0.7, color = "red") +
  geom_hline(yintercept = 0.8, color = "blue") +
  scale_y_continuous(breaks = seq(0, max(top_10_paid_player$avg_ops), by = 0.1))+
  labs(title = "Average OPS for Top 10 Paid Players by Year",
       x = "Year",
       y = "Average OPS",
       fill = "Year") +

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analysis 12: Identifying the most cost-effective hitters in baseball
# Finding the hitters whose ops is greater than 0.8 and
# has a salary in the bottom 50 percentile for a season
cost_effective_players  <- hitter_df %>%
  group_by(full_name,year) %>%
  summarise(avg_ops = mean(ops, na.rm = TRUE),
            total_gp  = sum(g),
            median_salary = median(salary),
  ) %>%
  filter(total_gp > 50 & avg_ops >= 0.8) %>%
  inner_join(salary_percentile_per_year, by = "year") %>%
  filter(median_salary < bottom_50_salary)

# Plotting dot plot to visualize the most cost effective players
ggplot(cost_effective_players, aes(x = full_name, y = (median_salary/max_salary) * 100)) +
  geom_point(color = "blue") +
  geom_text(aes(label = year), vjust = -0.5, size = 3) +
  labs(title = "The most cost effective players", x = "Player", y = "Percent of Max Salary (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analysis 13: Salary distributions across different teams
# Getting team data from 2015
team_salary_df_2015 <- team_salary_df %>%
  filter(year == 2015)

# Calculating average salary and attendance for exisiting team
team_salary_attendance_df <- team_salary_df %>%
  filter(team_id %in% unique(team_salary_df_2015$team_id)) %>%
  group_by(team_id) %>%
  summarise(
    salary = mean(salary),
    attendance = mean(attendance)
  ) %>%
  inner_join(select(team_salary_df_2015, name, team_id),by = "team_id")

# Calculating the difference in salary and attendance between each team and the league average
team_salary_attendance_df <- team_salary_attendance_df %>%
  mutate(
    diff_attendance = attendance - mean(team_salary_attendance_df$attendance),
    diff_salary = salary - mean(team_salary_attendance_df$salary)
  )

# Defining colors based on difference in attendance
colors <- ifelse(team_salary_attendance_df$diff_attendance > 0, "yes", "no")
# Plotting bar graph to visualize difference in salary between existing teams
ggplot(team_salary_attendance_df, aes(y = reorder(name, diff_salary), x = diff_salary, fill = colors)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(title = "Difference in salary between existing teams", x = "Difference in salary (team salary - league Average salary)", y = "Team") +
  scale_fill_manual(values = c("yes" = "blue4", "no" = "brown"), name = "Attendance Status", labels = c("Less than Average", "More than Average")) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Analysis 14: Does Spending Big on Salary Contribute to Winning?
# Viewing correlation between salary and winning
print(cor(team_salary_df$w,team_salary_df$salary))
# Accounting for salary Inflation by using z-score
team_salary_df$salary_zscore <- round(ave(team_salary_df$salary,team_salary_df$year,FUN = scale),2)
#PLotting scatter plot to view winning percentage based on salary
ggplot(data = team_salary_df,aes(x = salary_zscore,y = w/g)) +
  geom_point(aes(color = made_playoffs)) + geom_smooth() +
  guides(color=guide_legend(title="Made the Playoffs")) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = mean(team_salary_df$w/team_salary_df$g)) +
  labs(x= "Salary Z Score", y ="Winning Percentage",
       title = "Winning Percentage by Salary based on Making the playoffs")

# Analysis 15: Does having the best hitters Contribute to Winning?
# Adding Columns On-base plus slugging and win rate
team_salary_df <- team_salary_df %>%
  mutate(
    ops = ((h+bb+hbp)/(ab+bb+hbp+sf))
      + (((h-double-triple-hr)+(2*double)+(3*triple)))/(ab),
    win_rate = (w/g)
  )
# Calculating Ops and Win rate for Team with the highest Ops, Team with the lowest Win Rate per year and Team with the highest Win Rate per year
best_hitting_team_df <- team_salary_df  %>%
  group_by(year) %>%
  summarise(
    max_ops = max(ops),
    max_win_rate = max(win_rate),
    min_win_rate = min(win_rate),
    win_rate_for_max_ops = win_rate[which.max(ops)],
    ops_for_max_win_rate = ops[which.max(win_rate)],
    ops_for_min_win_rate = ops[which.min(win_rate)]
  )
# Plot line graphs to visualize Win Rate and OPS of Highest OPS Team vs Highest Win Rate Team vs Loweest Win Rate Team
grid.arrange(
  ggplot(best_hitting_team_df, aes(x = year))+
    geom_line(aes(y = max_ops, color = "Team with the highest Ops")) +
    geom_line(aes(y = ops_for_max_win_rate, color = "Team with the highest Win Rate")) +
    geom_line(aes(y = ops_for_min_win_rate, color = "Team with  the lowest Win Rate")) +
    labs(x = "Year", y = "On-base plus slugging", title = "Ops of Highest OPS Team vs Highest Win Rate Team vs Loweest Win Rate Team") +
    scale_x_continuous(breaks = seq(min(best_hitting_team_df$year),
                                    max(best_hitting_team_df$year), by = 1)) +
    scale_y_continuous(breaks = seq(min(best_hitting_team_df$ops_for_min_win_rate),
                                    max(best_hitting_team_df$max_ops), by = 0.02)) +
    guides(color=guide_legend(title="Team")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ,
  ggplot(best_hitting_team_df, aes(x = year))+
    geom_line(aes(y = max_win_rate, color = "Team with the highest Win Rate")) +
    geom_line(aes(y = win_rate_for_max_ops, color = "Team with the highest Ops")) +
    geom_line(aes(y = min_win_rate, color = "Team with the lowest Win Rate")) +
    labs(x = "Year", y = "Win Rate", title = "Win Rate of Highest OPS Team vs Highest Win Rate Team vs Loweest Win Rate Team") +
    scale_x_continuous(breaks = seq(min(best_hitting_team_df$year),
                                    max(best_hitting_team_df$year), by = 1)) +
    scale_y_continuous(breaks = seq(min(best_hitting_team_df$min_win_rate),
                                    max(best_hitting_team_df$max_win_rate), by = 0.05))+
  guides(color=guide_legend(title="Team")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ncol = 1
)

# Data Preparation for Machine Learning
unique_positions <- unique(hitter_df$pos)
unique_throws <- unique(hitter_df$throws)
unique_bats <- unique(hitter_df$bats)
unique_league <- unique(hitter_df$league_id)
# Converting categorical variables to numerical values
hitter_ml_df <- hitter_df %>%
  mutate(
  bin_pos = match(pos, unique_positions),
  bin_throws = match(throws, unique_throws),
  bin_league = match(league_id, unique_league),
  bin_bats = match(bats, unique_bats)) %>%
  select(player_id, full_name, year,age, g, ab, r, h, double, triple, hr,
             rbi, sb, cs, bb, so, ibb, hbp, sh, sf, g_idp,
             ba,ops,weight,height,bin_pos,bin_throws,bin_bats,bin_league,salary)

set.seed(123456)
# Splitting data into training and testing data
train_index <- createDataPartition(hitter_ml_df$salary, p = 0.7, list = FALSE)
train_data <- hitter_ml_df[train_index, ]
test_data <- hitter_ml_df[-train_index, ]

# ML 1: Predicting the Salary of players based on hitting stats using XG Boost
# Training the XGBoost model for feature selection
xgb_model <- xgboost(data = as.matrix(subset(train_data, select = -c(full_name, player_id, salary))),
                     label = train_data$salary,
                     objective = "reg:linear",
                     nrounds = 200,
                     eta = 0.4
)

# Getting feature importance
importance_scores <- xgb.importance(model = xgb_model)

print(importance_scores)
# Plotting bar graph to visualize Top 15 Important Feature to predict salary
xgb.plot.importance(importance_scores,
                    xlab = "Feature Importance", main = "Top 15 Important Feature to predict salary",
                    ylab = "Features", top_n = 15, col = "brown")
# Selecting the top 15 features from train and test data
top_features <-  importance_scores$Feature[1:15]
train_data_subset <- train_data[, c("salary", top_features)]
test_data_subset <- test_data[, c("salary", top_features)]
# Training the XGBoost model for linear regression
xgb_predict_model <- xgboost(data = as.matrix(train_data_subset),
                             label = train_data_subset$salary,
                             objective = "reg:linear",
                             nrounds = 200,
                             eta = 0.4
)
# Perfoming Linear Regression
test_data$predicted_salary <- predict(xgb_predict_model, newdata = as.matrix(test_data_subset))
test_data$prediction_error <- test_data$predicted_salary - test_data$salary
test_data$absolute_error <- abs(test_data$prediction_error)
# Checking Model Accuracy
# Calulating R-squared value
rsquared <- 1 - sum((test_data$prediction_error)^2) / sum((test_data$salary - mean(test_data$salary))^2)
# Calculating Root Mean Square Error (RMSE) value
rmse <- sqrt(mean(test_data$prediction_error^2))
# Calculating Mean Absolute Error (MAE) value
mae <- mean(test_data$absolute_error)

print(paste("R-squared:", rsquared))
print(paste("Root Mean Square Error (RMSE):", rmse))
print(paste("Mean Absolute Error (MAE):", mae))

# ML 2: Predicting if a team made the playoffs using Logistic Regression
set.seed(001)
# Selecting features for logistic regression
team_ml_df <- team_salary_df %>%
  mutate(
    r_per_game = r/g,
    ra_per_game = ra/g,
  ) %>%
  select(
    team_id,name,salary,r_per_game,ra_per_game,fp,win_rate,era, year, made_playoffs
  )
# Splitting data into training and testing data
team_train_index <- createDataPartition(team_ml_df$made_playoffs, p = 0.7, list = FALSE)
team_train_data <- team_ml_df[team_train_index, ]
team_test_data <- team_ml_df[-team_train_index, ]
# Perfoming Logistic Regression
logistic_model <- glm(made_playoffs ~ ., data = subset(team_train_data, select = -c(team_id, name)), family = binomial)
team_test_data$predicted_playoffs <- predict(logistic_model, newdata = team_test_data, type = "response")
team_test_data$final_prediction <- ifelse(team_test_data$predicted_playoffs >=0.5, TRUE, FALSE)
# Checking Model Accuracy via Confusion Matrix
confusion_matrix <- confusionMatrix(data = as.factor(team_test_data$made_playoffs),
                                    reference = as.factor(team_test_data$final_prediction))
confusion_matrix


# Ml 3: Clustering using K-Means
set.seed(100)
# Getting the hitter's total carrer home runs and stolen bases
cluster_vars <- hitter_df %>%
  group_by(full_name) %>%
  summarise(
    total_hr = sum(hr),
    total_sb = sum(sb),
  )
# Changing column full name to index
cluster_vars <- column_to_rownames(cluster_vars, var = "full_name")
# Scaling Data
scaled_data <- scale(cluster_vars)
# Determining optimal number of clusters using Elbow method
wss <- c()
# Calculating within-cluster sum of squares for different number of clusters
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i)
  wss[i] <- sum(kmeans_model$withinss)
}
elbow_data <- data.frame(Num_Clusters = 1:10, WCSS = wss)
# Plotting within-cluster sum of squares values
ggplot(elbow_data, aes(x = Num_Clusters, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method",
       x = "Number of Clusters",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  scale_x_continuous(breaks = seq(1,10, by = 1)) +
  theme_minimal()
# Performing KMeans clustering
kmeans_result <- kmeans(scaled_data, centers = 5)
print(kmeans_result)
summary(kmeans_result)
# Visualizing the clusters
fviz_cluster(kmeans_result, data = cluster_vars, pointsize = 2, labelsize = 5, show.clust.cent = TRUE)
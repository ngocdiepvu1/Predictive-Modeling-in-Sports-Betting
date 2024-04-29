# Load the dplyr library
library(dplyr)
library(ggplot2)


df <- read.csv("C:/Users/Yousef/OneDrive - University of Miami/Master/7-Spring24/Project/data/soccerdata.csv")

dim(df)
str(df)

#Change date format
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
df$FTR <-as.factor(df$FTR)
df$Div <- factor(df$Div)
df$HomeTeam <- factor(df$HomeTeam)
df$AwayTeam <- factor(df$AwayTeam)

summary(df)

# Summary statistics for numeric variables
summary(select_if(df, is.numeric))


# Calculate min and max dates for each division
result <- df %>%
  group_by(Div) %>%
  summarise(
    Min_Date = min(Date, na.rm = TRUE),
    Max_Date = max(Date, na.rm = TRUE)
  )

# View the result
print(result)

# Group by division and calculate min and max of AvgH and AvgA
df_summary <- df %>%
  group_by(Div) %>%
  summarise(
    Min_AvgH = min(AvgH, na.rm = TRUE),
    Max_AvgH = max(AvgH, na.rm = TRUE),
    Min_AvgA = min(AvgA, na.rm = TRUE),
    Max_AvgA = max(AvgA, na.rm = TRUE)
  )
# View the result
print(df_summary)

# Analyze match outcomes
df %>%
  group_by(FTR) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = FTR, y = Count, fill = FTR)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Frequency of Match Outcomes", x = "Final Match Result", y = "Number of Matches")

# Betting odds analysis
df %>%
  select(AvgH, AvgD, AvgA) %>%
  gather(key = "OddsType", value = "OddsValue", AvgH, AvgD, AvgA) %>%
  ggplot(aes(x = OddsValue, fill = OddsType)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~OddsType, scales = "free_x") +
  labs(title = "Distribution of Betting Odds", x = "Odds Value", y = "Frequency") +
  theme_minimal()




#Define the favorite team
df <- df %>%
  mutate(
    Favorite = if_else(AvgH < AvgA, "Home", "Away"),  # Determine the favorite based on odds
    FavoriteWin = case_when(
      (Favorite == "Home" & FTR == 0) ~ 1,  # Home is favorite and home wins
      (Favorite == "Away" & FTR == 2) ~ 1,  # Away is favorite and away wins
      TRUE ~ 0                             # All other cases, including draws
    )
  )



# Summary statistics for favorite's odds when they win
favorite_odds_win <- df %>%
  filter(FavoriteWin == 1) %>%
  summarise(
    Min_Odds = min(if_else(Favorite == "Home", AvgH, AvgA), na.rm = TRUE),
    Max_Odds = max(if_else(Favorite == "Home", AvgH, AvgA), na.rm = TRUE),
    Median_Odds = median(if_else(Favorite == "Home", AvgH, AvgA), na.rm = TRUE)
  )
print(favorite_odds_win)

# Calculate win rate by odds range
win_rates_by_odds <- df %>%
  group_by(Odds_Bin = cut(if_else(Favorite == "Home", AvgH, AvgA), breaks = seq(1, 10, by = 0.1))) %>%
  summarise(
    Win_Rate = mean(FavoriteWin),
    Count = n()
  ) %>%
  filter(Count > 50)  # Filtering to avoid bins with too few matches

print(win_rates_by_odds)


ggplot(win_rates_by_odds, aes(x = Odds_Bin, y = Win_Rate)) +
  geom_col() +  # This creates a bar plot
  theme_minimal() +
  labs(title = "Win Rate by Odds Bin",
       x = "Odds Bin",
       y = "Win Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for readability


# Just to include a sample structure here for completeness:
df <- df %>%
  mutate(
    Favorite = if_else(AvgH < AvgA, "Home", "Away"),
    FavoriteWin = case_when(
      (Favorite == "Home" & FTR == 0) ~ 1, 
      (Favorite == "Away" & FTR == 2) ~ 1,
      TRUE ~ 0
    )
  )

win_rates_by_odds <- df %>%
  group_by(Odds_Bin = cut(if_else(Favorite == "Home", AvgH, AvgA), breaks = seq(1, 10, by = 0.1))) %>%
  summarise(
    Win_Rate = mean(FavoriteWin),
    Count = n()
  ) %>%
  filter(Count > 50)  # This filters out bins with too few matches

# Plotting
ggplot(win_rates_by_odds, aes(x = Odds_Bin, y = Win_Rate)) +
  geom_col(fill = "steelblue") +  # This creates a bar plot with color specified
  geom_text(aes(label = Count), vjust = -0.5, color = "black") +  # Adds count above bars
  theme_minimal() +
  labs(title = "Win Rate by Odds Bin",
       x = "Odds Bin",
       y = "Win Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for readability    




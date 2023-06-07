# Calling Libraries
library(rvest)
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)


# Link of the website you want to web scrap from 
url = 'https://www.espncricinfo.com/records/tournament/batting-most-runs-career/indian-premier-league-2023-15129'

html_content = read_html(url)

table = html_content %>% html_table(fill=TRUE)

stats = table[[1]]

# As you can see every column in our tables is of chr datatype and the column names are also not right  
glimpse(stats)

# Changing Column names of table by replacing first row 
colnames(stats) = stats[c(1),]
stats = stats[-c(1),]

# Changing datatype of our columns 
stats$Mat = as.integer(as.character(stats$Mat))
stats$Inns = as.integer(as.character(stats$Inns))
stats$NO = as.integer(as.character(stats$NO))
stats$Runs = as.integer(as.character(stats$Runs))
stats$BF = as.integer(as.character(stats$BF))
stats$'4s' = as.integer(as.character(stats$'4s'))
stats$'6s' = as.integer(as.character(stats$'6s'))

stats$Ave = as.numeric(as.character(stats$Ave))
stats$SR = as.numeric(as.character(stats$SR))


# Remove span column
stats = select(stats,-Span)

# Separate team name and name from name column and creating new column for team name
player_names = stats$`Player`
Team = gsub(".*\\((.*)\\)", "\\1", player_names)
stats$Team = Team

stats$Player = gsub(" \\(.*\\)", "", stats$Player)


# Remove * from HS
stats$HS = gsub("\\*", "", stats$HS)


# Remove - from 100,50,0 and put 0 instead of -
stats$'100' = gsub("-", "0", stats$'100')
stats$'50' = gsub("-", "0", stats$'50')
stats$'0' = gsub("-", "0", stats$'0')


# And then covert HS,100,50,0 to integer dtype
stats$'100' = as.integer(as.character(stats$'100'))
stats$'50' = as.integer(as.character(stats$'50'))
stats$'0' = as.integer(as.character(stats$'0'))


# Remove Null from Ave
m = mean(stats$Ave,na.rm = TRUE)     
stats$Ave[is.na(stats$Ave)] = m


# Checking Null values
sum(is.na(stats))


# Saving the dataframe in a csv file
write.csv(stats,'iplstats.csv')


# Histogram of Runs
ggplot(stats, aes(x = Runs)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Runs", x = "Runs", y = "Frequency")


# Scatter plot of Batting Strike Rate vs. Average
ggplot(stats, aes(x = SR, y = Ave)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Batting Strike Rate vs. Average", x = "Strike Rate", y = "Average")


# Distribution of Runs by Team in Pie chart
ggplot(stats, aes(x = "", y = Runs, fill = Team)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Runs by Team", fill = "Team") +
  scale_fill_brewer(palette = "Set3") +
  theme_void()


# Bar plot for Top 10 Players with Most Runs
top_10_players <- stats[order(stats$Runs, decreasing = TRUE), ][1:10, ]

ggplot(top_10_players, aes(x = reorder(Player, -Runs), y = Runs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Players with Most Runs", x = "Player", y = "Runs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Bar graph for Top 10 Players with Most Innings
top_10_players <- stats[order(stats$Inns, decreasing = TRUE), ][1:10, ]

ggplot(top_10_players, aes(x = reorder(Player, -Inns), y = Inns)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Players with Most Innings", x = "Player", y = "Innings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


# Distribution of Sixes by Top 10 Players with the help of a pie chart
top_10_players <- stats[order(stats$'6s', decreasing = TRUE), ][1:10, ]

ggplot(top_10_players, aes(x = "", y = '6s', fill = Player)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Sixes by Top 10 Players", fill = "Player") +
  scale_fill_brewer(palette = "Set3") +
  theme_void()

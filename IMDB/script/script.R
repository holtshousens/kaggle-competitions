install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggrepel')
install.packages('scales')

#Load packages
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)


#Load data
Movie.Data <- read.csv("~/RStudio Projects/Kaggle/IMDB/data/movie_metadata.csv", header = TRUE, stringsAsFactors = FALSE)

str(Movie.Data)

#Remove duplicates based on name
Movie.Data <- Movie.Data[!duplicated(Movie.Data$movie_title),]

#Add profit and ROI
Movie.Data <- Movie.Data %>%
    mutate(profit = gross - budget,
           return_on_investment_perc = (profit/budget)*100)

#Top 20 based on absolute profit
Movie.Data.profit.20 <- Movie.Data %>%
    arrange(desc(profit)) %>%
    top_n(20, profit)

#Plot
ggplot(Movie.Data.profit.20, aes(x = budget/1000000, y = profit/1000000)) +
    geom_point(size = 1) + geom_text_repel(aes(label = movie_title), size = 2) +
    xlab("Budget $million") + ylab("Profit $million") + ggtitle("20 Most Profitable Movies")

#Top 20 based on ROI
Movie.Data.roi.20 <- Movie.Data %>%
    filter(budget > 10000) %>%
    arrange(desc(return_on_investment_perc)) %>%
    top_n(20, return_on_investment_perc)

#Plot
ggplot(Movie.Data.roi.20, aes(x = budget/1000000, y = profit/1000000)) +
    geom_point(size = 1) + geom_text_repel(aes(label = movie_title), size = 2)  +
    xlab("Budget $million") + ylab("Profit $million") + ggtitle("top 20 Movies Based on ROI")

#Top 20 most profitable directors
#use as.numric in the sum to avoid integer overflow
Movie.Data.directors.20 <- Movie.Data %>%
    group_by(director_name) %>%
    select(director_name, budget, gross, profit) %>%
    na.omit() %>%
    summarise(films = n(), budget = sum(as.numeric(budget)), gross = sum(as.numeric(gross)), profit = sum(as.numeric(profit))) %>%
    arrange(desc(profit)) %>%
    top_n(20, profit)

#Plot
ggplot(Movie.Data.directors.20, aes(x = films, y = profit/1000000)) +
    geom_point(size = 1) + geom_text_repel(aes(label = director_name), size = 2) +
    xlab("Number of Films") + ylab("Profit $millions") + ggtitle("Most Profitable Directors")

#Top 20 most profitable directors - average per film
Movie.Data.directors.20 <- Movie.Data.directors.20 %>%
    mutate(avg_per_film = profit/films) %>%
    arrange(desc(avg_per_film)) %>%
    top_n(20, avg_per_film)

#Plot
ggplot(Movie.Data.directors.20, aes(x = films, y = avg_per_film/1000000)) +
    geom_point(size = 1) + geom_text_repel(aes(label = director_name), size = 2) +
    xlab("Number of Films") + ylab("Avg Profit per Film $millions") + ggtitle("Most Profitable Directors - Avg per Film")

#Deeper analysis on Steven Speilberg
Movie.Data.Spielberg <- Movie.Data %>%
    filter(director_name == "Steven Spielberg") %>%
    select(title_year, profit, movie_title) %>%
    na.omit() %>%
    arrange(desc(title_year))

#Plot
ggplot(Movie.Data.Spielberg, aes(x = title_year, y = profit/1000000)) +
    geom_point() + geom_text_repel(aes(label = movie_title), size = 2) +
    xlab("Year") + ylab("Profit per Film $millions") + ggtitle("Steven Spielberg Films") + geom_hline(yintercept = 0, linetype = 3, alpha = 0.9) + geom_hline(yintercept = 100, linetype = 2, alpha = 0.6)

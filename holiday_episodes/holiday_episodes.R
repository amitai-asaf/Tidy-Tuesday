#Load librarys
library(tidytuesdayR)
library(tidyverse)
library(ggpubr)

#Load data
last_tuesday()
HolidayEpisodes <- tt_load("2023-12-19")

#View data sets
map(HolidayEpisodes,glimpse)

#Merge data sets
holiday_episodes_data <- merge(HolidayEpisodes$holiday_episodes,HolidayEpisodes$holiday_episode_genres,"tconst")

#Join similiar genres for ease of data interpertation
holiday_episodes_data$genres.y[holiday_episodes_data$genres.y %in% c("Biography","History","Documentary")] <- "Documentary"
holiday_episodes_data$genres.y[is.na(holiday_episodes_data$genres.y)|holiday_episodes_data$genres.y %in% c("News","Short","Music","Reality-TV","Sport","Talk-Show","Game-Show")]<-"Other"
holiday_episodes_data$genres.y[holiday_episodes_data$genres.y %in% c("War","Western")] <- "Action"
holiday_episodes_data$genres.y[holiday_episodes_data$genres.y %in% c("Sci-Fi")] <- "Fantasy"
holiday_episodes_data$genres.y[holiday_episodes_data$genres.y %in% c("Horror")] <- "Thriller"
holiday_episodes_data$genres.y[holiday_episodes_data$genres.y %in% c("Fantasy")] <- "Adventure"
holiday_episodes_data$genres.y <- fct_lump_min(holiday_episodes_data$genres.y,100,other_level = "Other")


#Are holiday episodes trending over time
ggplot(holiday_episodes_data,aes(year)) +
  geom_bar()

holiday_episodes_data %>% 
  group_by(genres.y) %>%
  count() %>%
  arrange(desc(n))

holiday_episodes_data %>%
  mutate(year = year %/% 10 * 10) %>%
    filter(!is.na(year)) %>%
      ggplot(aes(year,average_rating,fill=genres.y)) +
        geom_line(aes(color=genres.y),stat="summary",fun="mean")

episode_genre_plot<-holiday_episodes_data %>%
                      mutate(year= year %/% 10 * 10) %>%
                        filter(!is.na(year)) %>% 
                          count(year,genres.y) %>%
                            ggplot(aes(year,n,fill=genres.y)) +
                              geom_area(position="fill") +
                    #          scale_fill_brewer(palette="Paired")+
                    #          scale_fill_brewer(palette="Set1")+
                    #          scale_fill_manual(values = c("#e60049", "#0bb4ff", "#50e991", "#e6d800", "#9b19f5", "#ffa300", "#dc0ab4", "#b3d4ff", "#00bfa0"))+
                    #          scale_fill_manual(values = c("#fd7f6f", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a", "#ffee65", "#beb9db", "#fdcce5", "#8bd3c7"))+  
                              scale_x_continuous(breaks = seq(1940,2020,by = 10),labels = paste0(seq(1940,2020,by = 10),"s")) +
                              scale_y_continuous(labels = paste0(seq(0,100,by=25),"%")) +
                              labs(x="Decade", y="Percentage of Holiday Episodes",fill="Genre",title="Percentage of holiday episodes in each genre every decade",caption = "Source: IMDB") +
                              theme(plot.caption = element_text(hjust = 0))

episode_genre_plot

#save_plot
ggsave(filename = "Percentage of episode genre.PNG", plot = episode_genre_plot,unit="px",width=2000,height=1000)

#Do holiday episode rating correlate with the rating of the series
ggplot(holiday_episodes_data,aes(average_rating,parent_average_rating)) +
  geom_point()

ggscatter(holiday_episodes_data, x = "average_rating", y = "parent_average_rating", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Episode rating", ylab = "Series rating")

cor.test(holiday_episodes_data$average_rating,holiday_episodes_data$parent_average_rating)

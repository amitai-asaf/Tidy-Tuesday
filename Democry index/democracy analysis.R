library(tidyverse)
library(ggplot2)

democracy_data <- tidytuesdayR::tt_load('2024-11-05')$democracy_data

democracy_count_year <- democracy_data %>%
                          drop_na(is_democracy) %>%
                          group_by(year,is_democracy) %>%
                          count()

ggplot(democracy_count_year, aes(x=year,y =n,fill=is_democracy)) +
  geom_col() +
  scale_x_continuous(n.breaks= 20)
---------------------------
democracy_change_count <- democracy_data %>%
                            mutate(democracy_last_year = lag(is_democracy)) %>%
                            filter(democracy_last_year!=is_democracy & year != 1950) %>%
                            mutate(change_direction = if_else(is_democracy,"Turned Democratic","Turned Not Democratic")) %>%
                            group_by(year,change_direction) %>%
                            count()


democracy_change_count$change_direction = factor(democracy_change_count$change_direction, levels = c("Turned Not Democratic","Turned Democratic") )

ggplot(democracy_change_count, aes(x=year,y = n,fill=change_direction)) +
  geom_col() +
  scale_x_continuous(n.breaks= 20)
------------------------------

library("rnaturalearth")
library(gganimate)

------------------------------
democracy_data$country_code <- recode(democracy_data$country_code,GER= "DEU",ROM="ROU",ZAR="COD")
world <- ne_countries(scale = "medium", returnclass = "sf")
plotdata <- left_join(world,filter(democracy_data,year==2015), by = c("iso_a3_eh" = "country_code"))
plotdata$is_democracy <- as.character(plotdata$is_democracy)
plotdata$is_democracy <- replace_na(plotdata$is_democracy,"Undecided")


ggplot(data = plotdata) +
  geom_sf(aes(fill = is_democracy)) +
  ggtitle("Democracies - 2020") +
  scale_fill_manual(values=c("#F8766D","#00BFC4","grey"))

------------------------------
democracy_data$country_code <- recode(democracy_data$country_code,GER= "DEU",ROM="ROU",ZAR="COD")
world <- ne_countries(scale = "medium", returnclass = "sf")
plotdata <- left_join(world,democracy_data, by = c("iso_a3_eh" = "country_code"))


animation <- ggplot(data = plotdata) +
              geom_sf(aes(fill = is_democracy)) +
              labs(title = 'Democracies - {current_frame}') +
              scale_fill_manual(values=c("#F8766D","#00BFC4","grey"))+
              transition_manual(year)

animate(animation)
anim_save("democracies.gif")
------------------------------
animation <- ggplot(data = plotdata) +
  geom_sf(aes(fill = communism)) +
  labs(title = 'comunist - {current_frame}') +
  scale_fill_manual(values=c("#F8766D","#00BFC4","grey"))+
  transition_manual(year)

animate(animation)

------------------------------


animate(animation)
anim_save("democracies.gif")

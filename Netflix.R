library(tidyverse)
library(ggplot2)

#Import Netflix movies and TV shows dataset
data <-read.csv('netflix_titles.csv', header =T, na.string=c("","NA"))

#convert variable type and rating to factor, and convert date_added to date variable 
data <- mutate_at(data, vars(type,rating), as.factor) 
data <- mutate(data,date_added = as.Date(date_added,format="%B %d, %Y"))

# drop any duplicated value based on show_id
data<-data[!duplicated(data$show_id), ]

# Data Visualization ------------------------------------

# 1 -------------
# ------------------------------------------------------------------------- I could not sort the plot to the data
df_type <- data %>% 
  filter(!director  %in%  c("Youssef Chahine")) %>% 
  group_by(director) %>% 
  summarise(
    count = n(), Average_Release_Date = round(mean(release_year), 0)
  ) %>% na.omit()
#df_type$director <- df_type$director %>%  replace_na("Unknown")
df_type <- df_type[order(df_type$count, decreasing = TRUE), ]

df_type[1:30, ] %>% ggplot(aes(director, count, fill = Average_Release_Date))+
  geom_bar(stat="identity", linewidth=3, position="stack", alpha=0.5)+
  coord_flip()+
  scale_fill_gradient(low="blue", high="red")+
  theme_bw()+
  labs(x = "Director", y = "Mumber of Movies/TV Shows", fill = "Avirage Release Year", title = "Fuel efficience")


# 2 -------------
df_typeFig2 <- data %>%
  filter(!rating  %in%  c("66 min", "74 min", "84 min")) %>% 
  mutate(MinAge = case_when(rating == "G" ~ 0,
                            rating == "NC-17" ~ 17,
                            rating == "NR" ~ 18,
                            rating == "PG" ~ 8,
                            rating == "PG-13" ~ 13,
                            rating == "R" ~ 18,
                            rating == "TV-14" ~ 14,
                            rating == "TV-G" ~ 0,
                            rating == "TV-MA" ~ 17,
                            rating == "TV-PG" ~ 8,
                            rating == "TV-Y" ~ 2,
                            rating == "TV-Y7" ~ 7,
                            rating == "TV-Y7-FV" ~ 7,
                            rating == "UR" ~ NA))
 #          case_when(rating  ~  c("G", "NC-17", "NR", "PG", "PG-13", "R", "TV-14", "TV-G", "TV-MA", "TV-PG", "TV-Y", "TV-Y7", "TV-Y7-FV", "UR") ~
 #          c(0, 17, 18, 8, 13, 18, 14, 0, 17, 8, 2, 7, 7, NA))

  
    group_by(MinAge) %>% 
    summarise(
    count = n(), Average_Release_Date = round(mean(release_year), 0)
  ) %>% na.omit()

  df_typeFig2 %>% ggplot(aes(director, MinAge))+
  geom_point(size = MinAge, alpha=0.5)+
  labs(x = "Director", y = "Minimum Age", title = "Director's Content Orientation toward Customer's Age")

#
##
### Histogram for inundation
##
#

library(pacman)
p_load(tidyverse)

listxl <- list.files("Data/Inundation",
  pattern = '*.csv',
  full.names = TRUE)

addName <- function(x){
  xl <- read.csv(x)
  xl$District <- str_to_title(str_split_1(basename(x), ".csv")[1])
  xl
}

xl <- map(listxl,addName)

xl <- do.call(rbind, xl)

xl <- xl |> 
  mutate(afterFiltered.Count = str_replace_all(afterFiltered.Count, ",", ""),
    afterFiltered.Count = as.numeric(afterFiltered.Count))


xl |> 
  ggplot(aes(x = Band.Value,
    y = afterFiltered.Count)) +
  geom_vline(xintercept = -23) +
  geom_col(fill = 'green')  +
  scale_y_continuous(labels = scales::scientific_format())+
  labs(x = "db",
    y = "Number of Pixels") +
  facet_wrap(~District, ncol = 5)+
  theme_bw()

ggsave("Graphs/bimodal_10.jpeg", dpi = 300) 
  
xl |> 
    filter(District %in% c("Hooghly", "Howrah", "Murshidabad", "Paschim Medinipur", "Purba Barddhaman")) |> 
  ggplot(aes(x = Band.Value,
    y = afterFiltered.Count)) +
    geom_vline(xintercept = -23) +
    geom_col(fill = 'green')  +
    scale_y_continuous(labels = scales::scientific_format())+
    labs(x = "db",
      y = "Number of Pixels") +
    facet_wrap(~District)+
    theme_bw()
  
ggsave("Graphs/bimodal_5.jpeg", dpi = 300)


xl |> 
  filter(District %in% c("Hooghly", "Howrah", "Murshidabad", "Paschim Medinipur", "Purba Barddhaman")) |> 
  ggplot(aes(x = Band.Value,
    y = afterFiltered.Count,
    colour = District)) +
  geom_vline(xintercept = -23) +
  geom_area(aes(fill = District), alpha = 0.3)  +
  scale_y_continuous(labels = scales::scientific_format())+
  labs(x = "db",
    y = "Number of Pixels") +
  theme_bw() + 
  theme(
    legend.position = c(0.8, 0.3)
  )

ggsave("Graphs/bimodal_5.jpeg", dpi = 300)


xl |> 
  filter(District %in% c("Hooghly", "Howrah", "Murshidabad", "Paschim Medinipur", "Purba Barddhaman")) |> 
  ggplot(aes(x = Band.Value,
    y = afterFiltered.Count,
    colour = District)) +
  geom_vline(xintercept = -23) +
  geom_area(aes(fill = District), alpha = 0.3)  +
  scale_y_continuous(labels = scales::scientific_format())+
  labs(x = "db",
    y = "Number of Pixels") +
  theme_bw() + 
  theme(
    legend.position = c(0.8, 0.3)
  )

ggsave("Graphs/bimodal_5_areaPlot.jpeg", dpi = 300)


xl |> 
   ggplot(aes(x = Band.Value,
    y = afterFiltered.Count,
    colour = District)) +
  geom_vline(xintercept = -23) +
  geom_area(aes(fill = District), alpha = 0.3)  +
  scale_y_continuous(labels = scales::scientific_format())+
  labs(x = "db",
    y = "Number of Pixels") +
  theme_bw() + 
  theme(
    legend.position = c(0.8, 0.4)
  )

ggsave("Graphs/bimodal_10_areaPlot.jpeg", dpi = 300)




  
library(pacman)
p_load(tidyverse, sf, terra)

xl <- readxl::read_xlsx('Data/normal_currentRainfall.xlsx')
xl$Sno <- 1:dim(xl)[1]

#<-------------- Data Preparation -------------------------->
xl1 <- xl |> 
  pivot_longer(
    cols = !c("Sno", "District", "Block", "GP_Notific", "GP"),
    names_to = c('Date', 'Type'),
    names_sep = "_",
    values_to = 'Rainfall'
  ) |> 
  mutate(Date = as_date(as.POSIXct(Date, format = "%d-%m-%Y")),
         Type = as.factor(Type)) |> 
  group_by(District, Block, Date, Type) |> 
  summarise(RainfallMean = mean(Rainfall, na.rm = TRUE)) |> 
  ungroup() 

#<-------------- Graph Making ----------------------------->

xl1 |> 
  filter(District == 'BANKURA', 
         Block == 'ONDA') |> 
  ggplot(aes(x = Date,
             y = RainfallMean)) +
  geom_point(aes(color = Type))+
  ggalt::geom_xspline(aes(color = Type),
                      size = 0.8)+
  scale_x_date(date_breaks = '1 day', date_labels = "%b %d")+
  facet_wrap(~Block)+
  theme_bw()


xl1 |> 
  filter(District == 'BANKURA', 
         Block == 'ONDA') |> 
  ggplot(aes(x = Date,
             y = RainfallMean)) +
  geom_col(aes(fill = Type), position = 'dodge')+
  scale_x_date(date_breaks = '1 day', date_labels = "%b %d")+
  facet_wrap(~Block)+
  theme_bw()


######################################################
# District wise
######################################################


#<-------------- Data Preparation -------------------------->
xl1 <- xl |> 
  pivot_longer(
    cols = !c("Sno", "District", "Block", "GP_Notific", "GP"),
    names_to = c('Date', 'Type'),
    names_sep = "_",
    values_to = 'Rainfall'
  ) |> 
  mutate(Date = as_date(as.POSIXct(Date, format = "%d-%m-%Y")),
         Type = as.factor(Type)) |> 
  group_by(District, Date, Type) |> 
  summarise(RainfallMean = mean(Rainfall, na.rm = TRUE)) |> 
  ungroup() 

#<-------------- Graph Making ----------------------------->

xl1 |> 
  ggplot(aes(x = Date,
             y = RainfallMean)) +
  geom_point(aes(color = Type))+
  ggalt::geom_xspline(aes(color = Type),
                      size = 0.8)+
  scale_x_date(date_breaks = '1 day', date_labels = "%b %d")+
  facet_wrap(~District)+
  theme_bw()


xl1 |> 
  filter(District %in% c("BIRBHUM", "HOOGHLY", "HOWRAH", "MURSHIDABAD", "NADIA",
                         "NORTH 24 PARAGANAS", "PASCHIM BARDHAMAN", "PASCHIM MEDINIPUR",
                         "PURBA BARDHAMAN", "PURBA MEDINIPUR")) |> 
  ggplot(aes(x = Date,
             y = RainfallMean)) +
  geom_col(aes(fill = Type), position = 'dodge')+
  scale_fill_discrete(name = 'Rainfall:')+
  scale_x_date(date_breaks = '5 day', date_labels = "%d/%m")+
  facet_wrap(~District, ncol = 5)+
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    legend.title = element_text(face = 'bold')
  ) +
  labs(x = "",
    y = "Rainfall (mm)")
ggsave("Graphs/perDayRainfall.jpeg", dpi = 300)

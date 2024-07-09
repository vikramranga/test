#
##
### Graphs for acreages - line & point plots + boxplots for all the years. 
##
#

library(pacman)
p_load(tidyverse)

df <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/AcreageNRainComp/govt_rst_Recovered.xlsx',
                               sheet = "Sheet2")
dim(df)
#[1] 340  12
df <- df[complete.cases(df), ]
#[1] 325  12

distWs <- df |> 
  group_by(District) |> 
  summarise(govt_2018 = sum(govt_2018),
            rst_2018 = sum(rst_2018),
            govt_2019 = sum(govt_2019),
            rst_2019 = sum(rst_2019),
            govt_2020 = sum(govt_2020),
            rst_2020 = sum(rst_2020),
            govt_2021 = sum(govt_2021),
            rst_2021 = sum(rst_2021))
distWs |>
  pivot_longer(
    cols = !District,
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  ggplot() + 
  geom_line(aes(y = Acreage,
                x = Year,
                group = Type,
                colour = Type)) + 
  facet_wrap(~District, scales = 'free') + 
  theme_bw()

df |> 
  pivot_longer(
    cols = !c(District, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  filter(Year == 2021) |> 
  ggplot() + 
  geom_line(aes(x = Blocks,
                y = Acreage,
                group = Type,
                colour = Type),
            size = 1) + 
  geom_point(aes(x = Blocks,
                 y = Acreage,
                 colour = Type)) + 
  facet_wrap(~District, scales = 'free') + 
  theme_bw()

df |> 
  pivot_longer(
    cols = !c(District, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  filter(Year == 2019) |> 
  ggplot() + 
  geom_line(aes(x = Blocks,
                y = Acreage,
                group = Type,
                colour = Type),
            size = 1) + 
  geom_point(aes(x = Blocks,
                 y = Acreage,
                 colour = Type)) + 
  facet_wrap(~District, scales = 'free') + 
  theme_bw()

df |> 
  pivot_longer(
    cols = !c(District, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  filter(Year == 2020) |> 
  ggplot() + 
  geom_line(aes(x = Blocks,
                y = Acreage,
                group = Type,
                colour = Type),
            size = 1) + 
  geom_point(aes(x = Blocks,
                 y = Acreage,
                 colour = Type)) + 
  facet_wrap(~District, scales = 'free') + 
  theme_bw()

df |> 
  pivot_longer(
    cols = !c(District, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  filter(Year == 2021) |> 
  ggplot() + 
  geom_line(aes(x = Blocks,
                y = Acreage,
                group = Type,
                colour = Type),
            size = 1) + 
  geom_point(aes(x = Blocks,
                 y = Acreage,
                 colour = Type)) + 
  facet_wrap(~District, scales = 'free') + 
  theme_bw()

#<------------------ Boxplots -------------------------->

medVals <- df |> 
  pivot_longer(
    cols = !c(District, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  group_by(District, Blocks, Type) |> 
  summarise(MedianAcreage = median(Acreage))


df |> 
  pivot_longer(
    cols = !c(District, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  ggplot() + 
  geom_boxplot(aes(x = Blocks,
                y = Acreage,
                colour = Type)) + 
  geom_line(data = medVals,
            aes(x = Blocks,
                y = MedianAcreage,
                group = Type, 
                colour = Type)) + 
  facet_wrap(~District, scales = 'free', ncol = 4) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 0)
  )


#<--------------Boxplots without midline --------------------->

df |> 
  pivot_longer(
    cols = !c(District, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  ggplot() + 
  geom_boxplot(aes(x = Blocks,
                   y = Acreage,
                   colour = Type)) + 
  facet_wrap(~District, scales = 'free', ncol = 4) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 0)
  )

#
##
### Rainfall & acreages
##
#

rainAc <- readxl::read_xlsx("D:/Amnex/Kharif/analysis/graphs/graphs_area_rain.xlsx",
                            sheet = "Sheet3")
rainAc <- rainAc[complete.cases(rainAc), ]
rainAc |> 
  select(!c("2018", "2019", "2020", "2021", "2022", "Acr_2023")) |> 
  pivot_longer(
    cols = !c(District, Key, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Values"
  ) |> 
  ggplot(aes(x = Blocks)) + 
  geom_boxplot(aes(y = Values,
                   colour = Type)) +
  geom_boxplot(aes(y = Values,
                   colour = Type)) +
  scale_y_continuous(
    sec.axis = sec_axis(name = rain)
  )
  facet_wrap(~District, scales = 'free', ncol = 4) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 0)
  )


a <- rainAc |> 
  select(!c("2018", "2019", "2020", "2021", "2022", "Acr_2023")) |> 
  pivot_longer(
    cols = !c(District, Key, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Values"
  ) |> 
  pivot_wider(
    names_from = "Type",
    values_from = "Values") 

p <- a |> 
  filter(District == "Birbhum") |> 
  ggplot(aes(x = Blocks))+
  geom_line(aes(y = Acr,
                group = Year,
                color = Year))+
  theme_bw()+
  labs(title = "Acreage")
  

p1 <- a |> 
  filter(District == "Birbhum") |> 
  ggplot(aes(x = Blocks))+
  geom_line(aes(y = rain,
                   group = Year,
                   color = Year)) +
  theme_bw()+
  labs(title = "Rainfall")

p/p1



p <- a |> 
  filter(District == "Birbhum") |> 
  ggplot(aes(x = Blocks))+
  geom_boxplot(aes(y = Acr,
                color = Year))+
  theme_bw()+
  labs(title = "Acreage")


p1 <- a |> 
  filter(District == "Birbhum") |> 
  ggplot(aes(x = Blocks))+
  geom_boxplot(aes(y = rain,
                color = Year)) +
  theme_bw()+
  labs(title = "Rainfall")

p/p1


# only for one district Purba Bardhman
rainAc |> 
  select(!c("2018", "2019", "2020", "2021", "2022", "Acr_2023")) |> 
  pivot_longer(
    cols = !c(District, Key, Blocks),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Values"
  ) |> 
  pivot_wider(
    names_from = "Type",
    values_from = "Values") |> 
  filter(Year == 2018) |> 
  ggplot(aes(x = Acr)) +
  geom_point(aes(y = rain)) + 
  geom_smooth(aes(y = rain))+
  facet_wrap(~Year, scales = 'free') +
  theme_bw()
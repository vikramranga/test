library(pacman)
p_load(tidyverse)

areaCum <- readxl::read_xlsx("Data/Area_R_stats_paddy_allyrs.xlsx")
areaCum <- areaCum |> 
  pivot_longer(
    cols = !c(`Crop Name`, Block, District, Ago_zone),
    names_to = c("Fortnight", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) 
areaCum <- areaCum |> 
  mutate(Key = paste0(Block, Year, Fortnight, sep = ""))

rainCum <- readxl::read_xlsx("Data/Rainfal__allyrs.xlsx")
rainCum <- rainCum |> 
  pivot_longer(
    cols = !c(Block, District, Ago_zone),
    names_to = c("Year", "Fortnight"),
    names_sep = "_",
    values_to = "Rain"
  ) |> 
  filter(Year != "2020")
rainCum <- rainCum |> 
  mutate(Key = paste0(Block, Year, Fortnight, sep = ""))

# there is a mismatch in Darjeeling & Howrah

areaRainCum <- areaCum |> 
  left_join(select(rainCum, !c(District, Block, Ago_zone, Year, Fortnight)), by = "Key")

#removing NA for Howrah & Darjeeling
areaRainCum <- areaRainCum |> 
  filter(!is.na(Rain))

areaRainCum |> 
  mutate(
    Fortnight  = factor(Fortnight, levels = c("wbj1", "wbj12", "wbj12a1", 
                                        "wbj12a12", "wbj12a12s1", "wbj12a12s12"))
  ) |> 
  ggplot(aes(x = Acreage,
             y = Rain)) + 
  geom_point(aes(color = factor(Year))) +
  geom_smooth(method = lm,
              se = FALSE) +
  facet_wrap(~District, scales = 'free')+
  theme_bw()

areaRainCum |> 
  mutate(
    Fortnight  = factor(Fortnight, levels = c("wbj1", "wbj12", "wbj12a1", 
                                              "wbj12a12", "wbj12a12s1", "wbj12a12s12"))
  ) |> 
  filter(Year == "2022") |> 
  ggplot(aes(x = Acreage,
             y = Rain)) + 
  geom_point() +
  geom_smooth(method = lm,
              se = FALSE) +
  facet_wrap(~District, scales = 'free')+
  theme_bw()

areaRainCum |> 
  mutate(
    Fortnight  = factor(Fortnight, levels = c("wbj1", "wbj12", "wbj12a1", 
                                              "wbj12a12", "wbj12a12s1", "wbj12a12s12"))
  ) |>  
  filter(District != "KALIMPUNG") |> 
  ggplot(aes(x = Acreage,
             y = Rain)) + 
  geom_point() +
  geom_smooth(method = lm,
              se = FALSE) +
  facet_wrap(~District, scales = 'free')+
  theme_bw() +
  theme(strip.text = element_text(face = 'bold'))
ggsave("Graphs/RainNAcreage.jpeg", width = 11, height =  6, units = "in")

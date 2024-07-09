library(pacman)
p_load(tidyverse)

a <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/AcreageNRainComp/R2_2023.xlsx')

# District wise analysis
a |> 
  pivot_longer(
    cols = !c(OBJECTID, Block, District),
    names_to = c("Type", "Period"),
    names_sep = "_",
    values_to = 'Values'
  ) |> 
  pivot_wider(
    names_from = "Type",
    values_from = "Values"
  ) |> 
  filter(!is.na(area)) |> 
  mutate(
    Period  = factor(Period, levels = c("wbj1", "wbj12", "wbj12a1", 
                                        "wbj12a12", "wbj12a12s1", "wbj12a12s12"))
  ) |> 
  ggplot(aes(x = area,
             y = rain)) + 
  geom_point(aes(group = Block,
                 color = Period)) +
  geom_smooth(method = lm,
              se = FALSE) +
  facet_wrap(~District, scales = 'free')+
  theme_bw()

#Impact of irrigated and non-irrigated areas

a |> 
  pivot_longer(
    cols = !c(OBJECTID, Block, District),
    names_to = c("Type", "Period"),
    names_sep = "_",
    values_to = 'Values'
  ) |> 
  pivot_wider(
    names_from = "Type",
    values_from = "Values"
  ) |> 
  mutate(
    Period  = factor(Period, levels = c("wbj1", "wbj12", "wbj12a1", 
                                        "wbj12a12", "wbj12a12s1", "wbj12a12s12"))
  ) |> 
  filter(!is.na(area)) |> 
  filter(District == "MURSHIDABAD") |> 
  mutate(Direction = case_when(
    Block %in%  c("BARATPUR - I", "BARATPUR - II", "SAGARDIGHI",
                  'KHARGRAM', "BURWAN", "SAMSERGANJ", 'SUTI - I', "SUTI - II",
                  'FARAKKA', "NABAGRAM", "KANDI SADAR") ~ "West",
    .default = "East")
  ) |> 
  ggplot(aes(x = area,
             y = rain)) + 
  geom_point(aes(color = Direction)) +
  geom_smooth(method = lm,
              se = FALSE) +
  facet_wrap(~Direction, scales = 'free')+
  theme_bw()

# Take whole WB at once.
a |> 
  pivot_longer(
    cols = !c(OBJECTID, Block, District),
    names_to = c("Type", "Period"),
    names_sep = "_",
    values_to = 'Values'
  ) |> 
  pivot_wider(
    names_from = "Type",
    values_from = "Values"
  ) |> 
  filter(!is.na(area)) |> 
  mutate(
    Period  = factor(Period, levels = c("wbj1", "wbj12", "wbj12a1", 
                                        "wbj12a12", "wbj12a12s1", "wbj12a12s12"))
  ) |> 
  ggplot(aes(x = area,
             y = rain)) + 
  geom_point(aes(group = Block), 
             alpha = 1/3) +
  geom_smooth(method = lm,
              se = FALSE) +
  theme_bw()


# take only values where rainfall is below 1000
a |> 
  pivot_longer(
    cols = !c(OBJECTID, Block, District),
    names_to = c("Type", "Period"),
    names_sep = "_",
    values_to = 'Values'
  ) |> 
  pivot_wider(
    names_from = "Type",
    values_from = "Values"
  ) |> 
  filter(!is.na(area)) |> 
  mutate(
    Period  = factor(Period, levels = c("wbj1", "wbj12", "wbj12a1", 
                                        "wbj12a12", "wbj12a12s1", "wbj12a12s12"))
  ) |> 
  filter(rain <= 1000) |> 
  ggplot(aes(x = area,
             y = rain)) + 
  geom_point(aes(group = Block), 
             alpha = 1/3) +
  geom_smooth(method = lm,
              se = FALSE) +
  theme_bw()

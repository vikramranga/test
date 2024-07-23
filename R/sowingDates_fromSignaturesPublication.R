library(pacman)
p_load(tidyverse, gghighlight, ggalt)


#<---------------------------- Jul 2 ------------------------------->
sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "July2")

MeanSigJul2 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  group_by(Period) |> 
  summarise(AverageBS = mean(bs, na.rm = TRUE))|> 
  mutate(Sno = 1,
         Sowing = "July2FN")

#<---------------------------- Aug 1 ------------------------------->
sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "Aug1")


MeanSigAug1 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  group_by(Period) |> 
  summarise(AverageBS = mean(bs, na.rm = TRUE))|> 
  mutate(Sno = 2,
         Sowing = "August1FN")

#<---------------------------- Aug 2 ------------------------------->

sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "Aug2")

MeanSigAug2 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  group_by(Period) |> 
  summarise(AverageBS = mean(bs, na.rm = TRUE))|> 
  mutate(Sno = 3,
         Sowing = "August2FN")


#<---------------------------- Sep 1 ------------------------------->
sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "Sept1")


MeanSigSep1 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  group_by(Period) |> 
  summarise(AverageBS = mean(bs, na.rm = TRUE))|> 
  mutate(Sno = 4,
         Sowing = "Sept1FN")

MeanSig <- do.call(rbind, list(MeanSigJul2, MeanSigAug1, MeanSigAug2, MeanSigSep1))

MeanSig |> 
  mutate(Period = case_when(
    Period == "july_2FN" ~ "July2FN",
    Period == "Aug_1FN" ~ "Aug1FN",
    Period == "Aug_2FN" ~ "Aug2FN",
    Period == "Sept_1FN" ~ "Sept1FN",
    Period == "Sept_2FN" ~ "Sept2FN",
    Period == "Oct_1FN" ~ "Oct1FN",
    Period == "Oct_2Fn" ~ "Oct2FN",
    Period == "Nov_1FN" ~ "Nov1FN",
    Period == "Nov_2Fn" ~ "Nov2Fn",
    Period == "Dec_1Fn" ~ "Dec1Fn",
    .default = Period
  )) |> 
  mutate(Period = factor(Period, levels = c("July2FN", "Aug1FN", "Aug2FN", 
                                            "Sept1FN", "Sept2FN", "Oct1FN",
                                            "Oct2FN", "Nov1FN", "Nov2Fn", "Dec1Fn"))) |> 
  ggplot() + 
    geom_xspline(aes(x = Period,
                  y = AverageBS,
                  group = Sno,
                  colour = Sowing),
                 spline_shape = -0.4) + 
    geom_point(aes(x = Period,
                   y = AverageBS,
                   group = Sno,
                   colour = Sowing)) + 
  labs(colour = "Sowing Period",
       x = "",
       y = "Average Backscatter") +
    theme_bw() +
  theme(
    legend.position = "top"#c(0.87, 0.2)
  )



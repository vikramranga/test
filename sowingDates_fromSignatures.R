library(pacman)
p_load(tidyverse, gghighlight)


#<---------------------------- Jul 2 ------------------------------->
sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "July2")

MeanSig <- sig |> 
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
  mutate(Sno = 1)

jul2 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  ggplot() + 
  geom_line(aes(x = Period,
                y = bs,
                group = case_ID),
            colour = 'grey') + 
  geom_line(data = MeanSig,
            aes(x = Period,
                y = AverageBS,
                group = Sno),
            colour = 'yellow') +
  geom_point(data = MeanSig,
             aes(x = Period,
                 y = AverageBS),
             colour = 'purple') + 
  theme_bw()+
  labs(title = "July 2FN")


#<---------------------------- Aug 1 ------------------------------->

sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "Aug1")


MeanSig <- sig |> 
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
  mutate(Sno = 1)

aug1 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  ggplot() + 
  geom_line(aes(x = Period,
                y = bs,
                group = case_ID),
            colour = 'grey') + 
  geom_line(data = MeanSig,
            aes(x = Period,
                y = AverageBS,
                group = Sno),
            colour = 'yellow') +
  geom_point(data = MeanSig,
             aes(x = Period,
                 y = AverageBS),
             colour = 'purple') + 
  theme_bw()+
  labs(title = "August 1FN")


#<---------------------------- Aug 2 ------------------------------->

sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "Aug2")


MeanSig <- sig |> 
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
  mutate(Sno = 1)

# MeanSig|> 
#   ggplot()+
#   geom_line(aes(Period,
#                 AverageBS,
#                 group = Sno))+
#   theme_bw()


aug2 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  ggplot() + 
  geom_line(aes(x = Period,
                y = bs,
                group = case_ID),
            colour = 'grey') + 
  geom_line(data = MeanSig,
            aes(x = Period,
                y = AverageBS,
                group = Sno),
            colour = 'yellow') +
  geom_point(data = MeanSig,
             aes(x = Period,
                 y = AverageBS),
             colour = 'purple') + 
  theme_bw()+
  labs(title = "August 2FN")


#<---------------------------- Sep 1 ------------------------------->

sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "Sept1")


MeanSig <- sig |> 
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
  mutate(Sno = 1)

sep1 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  ggplot() + 
  geom_line(aes(x = Period,
                y = bs,
                group = case_ID),
            colour = 'grey') + 
  geom_line(data = MeanSig,
            aes(x = Period,
                y = AverageBS,
                group = Sno),
            colour = 'yellow') +
  geom_point(data = MeanSig,
             aes(x = Period,
                 y = AverageBS),
             colour = 'purple') + 
  theme_bw()+
  labs(title = "September 1FN")


#<---------------------------- Sep 2 ------------------------------->

sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "Sept2")


MeanSig <- sig |> 
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
  mutate(Sno = 1)

# MeanSig|> 
#   ggplot()+
#   geom_line(aes(Period,
#                 AverageBS,
#                 group = Sno))+
#   theme_bw()


sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  ggplot() + 
  geom_line(aes(x = Period,
                y = bs,
                group = case_ID),
            colour = 'grey') + 
  geom_line(data = MeanSig,
            aes(x = Period,
                y = AverageBS,
                group = Sno),
            colour = 'yellow') +
  geom_point(data = MeanSig,
             aes(x = Period,
                 y = AverageBS),
             colour = 'purple') + 
  theme_bw()


(jul2 | aug1) / (aug2 | sep1)



# District wise
#<---------------------------- Aug 1 ------------------------------->

sig <- readxl::read_xlsx('D:/laptop data backup/d/Kharif-2023/BSB/Publication/Threshold_paddy - Copy.xlsx',
                         sheet = "Aug1")


MeanSig <- sig |> 
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
  mutate(Sno = 1)

aug1 <- sig |> 
  pivot_longer(
    cols = !c(`Select Dis`, case_ID, x, y, CropNumber),
    names_to = "Period",
    values_to = 'bs'
  ) |> 
  mutate(Period = factor(Period, levels = c("july_2FN", "Aug_1FN", "Aug_2FN", 
                                            "Sept_1FN", "Sept_2FN", "Oct_1FN",
                                            "Oct_2Fn", "Nov_1FN", "Nov_2Fn", "Dec_1Fn" ))) |> 
  filter(!is.na(Period)) |> 
  ggplot() + 
  geom_line(aes(x = Period,
                y = bs,
                group = case_ID),
            colour = 'grey') + 
  geom_line(data = MeanSig,
            aes(x = Period,
                y = AverageBS,
                group = Sno),
            colour = 'yellow') +
  facet_wrap(~`Select Dis`, scales = 'free')+
  theme_bw()+
  labs(title = "August 1FN")

aug1
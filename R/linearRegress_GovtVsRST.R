ggplot(df)+
  geom_point(aes(x = govt_2018,
                 y = rst_2018)) + 
  geom_smooth(aes(x = govt_2018,
                  y = rst_2018),
              method = 'lm',
              se = FALSE)+
  theme_bw()

ggplot(df)+
  geom_point(aes(x = govt_2019,
                 y = rst_2019)) + 
  geom_smooth(aes(x = govt_2019,
                  y = rst_2019),
              method = 'lm',
              se = FALSE)+
  theme_bw()

ggplot(df)+
  geom_point(aes(x = govt_2020,
                 y = rst_2020)) + 
  geom_smooth(aes(x = govt_2020,
                  y = rst_2020),
              method = 'lm',
              se = FALSE)+
  theme_bw()

ggplot(df)+
  geom_point(aes(x = govt_2021,
                 y = rst_2021)) + 
  geom_smooth(aes(x = govt_2021,
                  y = rst_2021),
              method = 'lm',
              se = FALSE)+
  theme_bw()


df |> 
  select(!c("rst_2022", "rst_2023")) |> 
  filter(Blocks != c("KRISHNAGAR-I", "BONGAON")) |> 
  pivot_longer(
    cols = !c("District", "Blocks"),
    names_to = c("Type", "Year"),
    names_sep = "_",
    values_to = "Acreage"
  ) |> 
  pivot_wider(
    names_from = Type,
    values_from = Acreage
  ) |> 
  ggplot()+
  geom_point(aes(x = govt,
                 y = rst)) + 
  geom_smooth(aes(x = govt,
                  y = rst),
              method = 'lm',
              se = FALSE)+
  facet_wrap(~Year, scales = 'free')+
  labs(x = "Government Reported Acreage (ha)",
       y = "RST Derived Acreage (ha)") +
  theme_bw() 
ggsave("Graphs/GovtVsRST_LinearRegr_wayForward.jpeg", width = 12, height =  6, units = "in")

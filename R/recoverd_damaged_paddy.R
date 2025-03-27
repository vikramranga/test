library(pacman)
p_load(tidyverse, ggalt, cowplot, ggtext)
font_add(family = "TIMES", regular = "C:/WINDOWS/FONTS/TIMES.TTF")
xl <- readxl::read_xlsx('D:/laptop data backup/d/kharif-2024/WB/Publication/Paddy_Profile.xlsx')

g <- xl |> 
  pivot_longer(
    cols = !c(Flood, Type, case_ID),
    values_to = 'db',
    names_to = 'Dates'
  ) |>
  mutate(Dates = as.Date(Dates, "%d-%m-%Y")) |> 
  mutate(Type = case_when(
    Type == 'Damaged' ~ "Inundated & Damaged",
    Type == 'No Damage' ~ "No Inundation",
    Type == 'Recovered' ~ "Inundated & Recovered",
    .default = Type
  )) |> 
  ggplot() +
  ggalt::geom_xspline(aes(x = Dates, 
                          y = db,
                          colour = Type,
                group = case_ID),
                size = 2) +
  geom_vline(xintercept = as.Date('22-09-2024', "%d-%m-%Y"), colour = 'brown', size = 0.8)+
  geom_point(aes(x = Dates, 
                 y = db),
             shape = 16,
             size = 3, 
             colour = 'white')+
  geom_point(aes(x = Dates, 
                 y = db,
                 colour = Type),
             size = 2,
             shape = 16)+
  scale_x_date(date_breaks = "10 days", date_labels = "%b %d")+
  geom_curve(
    aes(xend = as.Date("05-08-2024", "%d-%m-%Y"),
        yend = -20, x = as.Date("25-07-2024", "%d-%m-%Y"), y = -15),
    curvature = -0.4,
    arrow = arrow(length = unit(0.03, "npc")),
    linewidth = 0.8) +
  geom_label(aes(x = as.Date("25-07-2024", "%d-%m-%Y"),
                y = -15.5),
            label = 'Time of Sowing')+
  geom_label(aes(x = as.Date("22-09-2024", "%d-%m-%Y"),
                 y = -14),
             label = 'Time of Inundation')+
  labs(x = '',
       y = 'Backscatter Values (db)')+
  facet_wrap(~Type, ncol = 1)+
  theme_half_open(12) + 
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 14, family = 'Times', face = 'bold',
      color = "white", fill = "#043927", box.color = "black",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    ),
    panel.grid.major = element_line(colour = 'gray90')
  )
ggsave(here::here("Graphs", "InundationDamagedUndamaged.jpeg"), width = 10, height = 8, units = 'in')
# jpeg(g, here::here("Graphs", "InundationDamagedUndamaged.jpeg"), width = 20, height = 20, units = "cm")
dev.off()
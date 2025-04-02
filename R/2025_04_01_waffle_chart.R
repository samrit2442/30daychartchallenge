library(pacman)
p_load(waffle, tidyverse, hrbrthemes, emojifont, extrafont, fontawesome,
       sysfonts, showtext, ggtext, magick)

font_add_google("Poppins", family = "poppins")
font_add_google("Inter", family = "inter")
showtext_auto()



# Meta Users Data in India 2025 (in Millions)
df <- tribble(~platform, ~users,
               "facebook", 378, 
               "whatsapp", 536, 
               "instagram", 414, 
               "linkedin", 150) |> 
  arrange(desc(users))

custom_labels <- c("378M", "414M", "150M", "536M")


waffle_chart <- df |> ggplot(aes(label = platform, values = users)) +
  geom_pictogram(n_rows = 10,
                 flip = TRUE,
                 make_proportional = TRUE,
                 aes(colour = platform),
                 family = "fontawesome-webfont") + 
  scale_label_pictogram(
    name = NULL,
    values = c("facebook", "instagram", "linkedin", "whatsapp"),
    labels = custom_labels
  ) +
  scale_color_manual(
    name = NULL,
    # guide = "legend",
    values = c("#1877f2", "#c33b93", "#0274b3", "#49c958"),
    labels = custom_labels
  ) +
  coord_equal() +
  theme_ipsum_rc(grid = "") +
  theme_enhance_waffle() +
  labs(title = paste0("The Social Media", 
                      # "<img src = './img/meta.png' height = '40'/>", 
                      " Analysis in India 2025"),
       subtitle = "Proportions of no of Users in Millions on different platforms of <br>
                    <img src = './img/meta.png' height = '40'/>
                    <img src = './img/microsoft.png' height = '40'/>",
       caption = "Data Source: Statista and statusbrew.com",
       x = NULL,
       y = NULL) +
  theme(text = element_text(family = "poppins"),
        legend.text = element_text(size = 15),
        plot.title = element_markdown(size = 30, hjust = 0.5, face = "bold", family = "poppins"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5, face = "italic", family = "poppins"),
        plot.caption = element_text(size = 12, hjust = 0.5, family = "poppins"))
        
waffle_chart
     
ggsave("./outputs/2025_04_01_waffle_chart.svg", waffle_chart,
       width = 15, height = 12, dpi = 900, units = "in", limitsize = T)







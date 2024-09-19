#Livestock types
livestock_data <- GHG.field %>%
  separate_rows(Livestock, sep = ",\\s*") %>%
  group_by(Pub..year,Livestock) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)

Fig1b <- ggplot(data = na.omit(livestock_data), 
                aes(x = Pub..year, y = Number, fill = Livestock)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Publication number") +
  scale_fill_manual(values = c(warm_colors,cold_colors)) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")),
            position = position_stack(vjust = 0.5),
            size = 3) +
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 20, by = 2),
                     expand = c(0.01, 0))+
  scale_x_continuous(limits = c(1990, 2023), 
                     breaks = seq(1990, 2023 , by = 5),
                     expand = c(0.03, 0))


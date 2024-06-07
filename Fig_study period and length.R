#To prepare figures for study period and study length
library(tidyverse);library(ggplot2)
library(reshape2);library(scatterpie)
library(stringr);library(ggpubr)
#read data
GHG <- read.csv("input/data of ghg emission.csv",header = T)
#obtain studies with field measurement 
GHG.field <- GHG[grepl("Field", GHG$GHG.source),]
GHG.field <- GHG.field[!grepl("^Grass$", GHG.field$Field.crop),]
GHG.field <- GHG.field[GHG.field$N2O == TRUE,]


# Define custom colors for CO2, N2O, CH4, and NH3
cold_colors <- c("#999999", "#009E73", "#0072B2", "#56B4E9")
warm_colors <- c("#D55E00", "#E69F00", "#F0E442", "#CC79A7")

# GHG.field <- GHG.field %>%
#           filter(str_detect(Technique, "Micrometeorology")|
#                   str_detect(Technique, "Soil chamber"))
#  sum(na.omit(GHG.field$Manure.type == "Liquid, Solid")) #8
#  sum(na.omit(GHG.field$Manure.type == "Liquid")) #26
#  sum(na.omit(GHG.field$Manure.type == "Solid")) #19



#Figure 2a
#study period
season_data <- GHG.field %>%
  separate_rows(Season, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Season) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)


sum(season_data$Number[(season_data$Season == "Year-Round" |
                        season_data$Season == "Non-Growing") &
                        season_data$Pub..year > 2015]) / (2023-2015) #14 and 2
sum(season_data$Number[season_data$Season == "Growing"  &
                       season_data$Pub..year > 2015])  / (2023-2015) #19 and 2.7
sum(season_data$Number[(season_data$Season == "Year-Round" |
                        season_data$Season == "Non-Growing") &
                        season_data$Pub..year <= 2015]) / (2015-1989) #13 and 0.5
sum(season_data$Number[season_data$Season == "Growing"  &
                       season_data$Pub..year <= 2015]) / (2015-1989) #19 sun and 0.7

sum(season_data$Number[season_data$Season == "Year-Round"]) #17



Fig2a <- ggplot(data = na.omit(season_data), 
                aes(x = Pub..year, y = Number, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication year", y = "Study count") +
  scale_fill_manual(values = c(cold_colors)) +
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
            size = 2) +
  scale_y_continuous(limits = c(0, 10), 
                     breaks = seq(0, 18, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0)) +
  annotate("text", x = 1991, y = 9.5, 
          label = "(a)",size = 6)
#Figure 2b
#study length
period_data <- GHG.field %>%
  separate_rows(Period, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Period) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)

#Figure for period
Fig2b <- ggplot(data = na.omit(period_data), 
                aes(x = Pub..year, y = Number, fill = Period)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication year", y = "Study count") +
  scale_fill_manual(values = c(cold_colors)) +
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
            size = 2) +
  scale_y_continuous(limits = c(0, 10), 
                     breaks = seq(0, 18, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0)) +
  annotate("text", x = 1991, y = 9.5, 
           label = "(b)",size = 6)


# combine figures together
ggsave("output/Fig2_study period and length.png",
       ggarrange(Fig2a, Fig2b,
                 ncol = 1, nrow = 2),
       width = 4800, height = 7200, units = "px",
       dpi = 600)

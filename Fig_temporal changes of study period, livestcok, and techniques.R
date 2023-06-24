library(tidyverse)
library(ggplot2)
library(reshape2)
library(scatterpie)
library(stringr)
library(ggpubr)
#read data
GHG <- read.csv("input/data of ghg emission.csv",header = T)
#obtain studies with field measurement 
GHG.field <- GHG[grepl("Field", GHG$GHG.source),]
GHG.field <- GHG.field[!grepl("^Grass$", GHG.field$Field.crop),]
GHG.field <- GHG.field[GHG.field$N2O == TRUE,]

# GHG.field <- GHG.field %>%
#           filter(str_detect(Technique, "Micrometeorology")|
#                   str_detect(Technique, "Soil chamber"))
#  sum(na.omit(GHG.field$Manure.type == "Liquid, Solid")) #8
#  sum(na.omit(GHG.field$Manure.type == "Liquid")) #26
#  sum(na.omit(GHG.field$Manure.type == "Solid")) #19


#GHG.field <- GHG.field[GHG.field$N2O == TRUE &
#                       GHG.field$CO2 == TRUE &
#                       GHG.field$CH4 == TRUE,]
# Define custom colors for CO2, N2O, CH4, and NH3
warm_colors <- c("#999999", "#009E73", "#0072B2", "#56B4E9")
cold_colors <- c("#D55E00", "#E69F00", "#F0E442", "#CC79A7")


#Figure 1
#Temporal analysis

#study period
period_data <- GHG.field %>%
  separate_rows(Season, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Season) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)



Fig1a <- ggplot(data = na.omit(period_data), 
                aes(x = Pub..year, y = Number, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Publication number") +
  scale_fill_manual(values = c(warm_colors)) +
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
  scale_y_continuous(limits = c(0, 18), 
                     breaks = seq(0, 18, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2023), 
                     breaks = seq(1990, 2023 , by = 5),
                     expand = c(0, 0))

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
                     expand = c(0, 0))+
  scale_x_continuous(limits = c(1990, 2023), 
                     breaks = seq(1990, 2023 , by = 5),
                     expand = c(0, 0))


#Techniques 
Tech_data <- GHG.field %>%
  separate_rows(Technique, sep = ",\\s*") %>%
  group_by(Pub..year,Technique) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)

Tech_data$Technique <- factor(Tech_data$Technique, 
                      levels = c("Incubation", "Mixed", "Modelling",
                                 "Micrometeorology", "Soil chamber"))

Tech_method <- Tech_data %>%
  mutate(method = ifelse(Technique %in% c("Incubation", "Mixed", "Modelling"), 
                         "Indoor", "Outdoor")) %>%
  group_by(Pub..year,method) %>%
  summarise(method.n = sum(Number)) %>%
  ungroup()


                                                              
Fig1c <- ggplot(Tech_data, aes(x = Pub..year, y = Number, fill = Technique)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Publication number") +
  scale_fill_manual(values = c(warm_colors[1:3],cold_colors[1:2])) +
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
  scale_y_continuous(limits = c(0, 18), 
                     breaks = seq(0, 18, by = 2),
                     expand = c(0, 0))+
  scale_x_continuous(limits = c(1990, 2023), 
                     breaks = seq(1990, 2023 , by = 5),
                     expand = c(0, 0))

#GHG types 
GHG_data <- GHG.field %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Soil chamber")) %>%
  separate_rows(Manure.type, sep = ",\\s*") %>%
  group_by(Pub..year, Manure.type) %>%
  summarize(
    CO2_count = sum(CO2),
    CH4_count = sum(CH4),
    N2O_count = sum(N2O),
    NH3_count = sum(NH3)
  ) %>%
  ungroup() %>%
  na.omit()


# Reshape the data to long format
GHG_stacked <- GHG_data %>%
  pivot_longer(cols = c(CO2_count, N2O_count, CH4_count, NH3_count),
               names_to = "gas", values_to = "count")

# Concatenate ManureType and Gas
GHG_stacked$variable <- paste(GHG_stacked$Manure.type,
                              GHG_stacked$gas, sep = " - ")
GHG_stacked$variable <- str_remove(GHG_stacked$variable, "_count")

#Percentage
GHG_stacked$Percentage <- round(GHG_stacked$count / 
                         ave(GHG_stacked$count, 
                             GHG_stacked$Pub..year, FUN = sum) * 100,1)
                        


Fig1d <- ggplot(GHG_stacked, aes(x = Pub..year, y = count, fill = variable)) +
  geom_col() +
  scale_fill_manual(values = c(warm_colors, cold_colors)) +
  labs(x = "Publication Year", y = "Publication number", fill = "GHG type") +
  theme_classic()+
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 40), 
                     breaks = seq(0, 40, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2023), 
                     breaks = seq(1990, 2023 , by = 5),
                     expand = c(0, 0)) +
  geom_text(aes(label = ifelse(Percentage == 0, NA, 
                  paste0(round(Percentage, 0), "%"))),
            position = position_stack(vjust = 0.5),
            size = 2) 



ggsave("output/Fig1.emf", 
       ggarrange(Fig1a, Fig1b, Fig1c, Fig1d,
                 ncol = 2, nrow = 2),
       width = 48, height = 24, units = "cm")

ggsave("output/Fig1.png", 
       ggarrange(Fig1a, Fig1b, Fig1c, Fig1d,
                 ncol = 2, nrow = 2),
       width = 48, height = 24, units = "cm",
       dpi = 300)

#To see the relationship between solid and liquid studies
plot(GHG_stacked$Pub..year[GHG_stacked$Manure.type=="Liquid"],
     GHG_stacked$count[GHG_stacked$Manure.type=="Liquid"])

points(GHG_stacked$Pub..year[GHG_stacked$Manure.type=="Solid"],
     GHG_stacked$count[GHG_stacked$Manure.type=="Solid"],
     pch = 17)

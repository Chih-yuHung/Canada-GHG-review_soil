#This script is to analyze the data for GHG emissions from manure-applied field
library(tidyverse)
library(ggplot2)
library(reshape2)
library(scatterpie)
library(stringr)
library(ggpubr)
#read data
GHG <- read.csv("input/data of ghg emission.csv",header = T)

#obtain studies with field measurement 
GHG.field <- GHG[grep("Field", GHG$GHG.source), ]

#We want to spatial distribution of the studies
prov <- unique(GHG.field$Region)[c(7,2,9,6,3,5,10,11,12,8,4,1)]
prov.n <- length(prov)

#NIR N2O data
N2O <- read.csv("input/N2O emission_NIR.csv", header = TRUE) %>%
  slice(c(2, 1, 9, 10, 7, 4, 3, 5, 6, 8, 11:16)) %>%
  mutate(across(2:11, as.numeric)) %>%
  mutate(across(2:11, round, digits = 3)) 

N2O[17,2:11] <- colSums(N2O[c(7:16),2:11])
N2O[17,1] <- "Other"
N2O <- N2O[c(1:6,17),]
N2O$Total <- rowSums(N2O[,2:11])

#convert to percentage
N2O.p <- as.data.frame(round(proportions(as.matrix(N2O[,2:12]),2)*100,1))
row.names(N2O.p) <- N2O[,1] 
N2O.p <- N2O.p %>%
  select(2,1,10,3,7,9,4,6,8,5,11)


###Summary statistics
#1. Spatial analysis
#province frequency
Prov <- unlist(strsplit(as.character(GHG.field$Region), ","))
prov_freq <- table(Prov)
prov_freq[11] <- sum(prov_freq)
#number of studies in AB, ON, and QC, 103/146 studies, 71%
#AB 38, ON, 32, QC 33.

#Livestock types
Livestock <- unlist(strsplit(as.character(GHG.field$Livestock), ", "))
table(Livestock)
#Beef 39, Dairy 70, Horse 5, poultry 12, sheep 6, swine 43. tot:175 

Live.prov <- GHG.field %>%
  separate_rows(Region, sep = ",") %>%
  mutate(Livestock = str_split(Livestock, ",\\s*")) %>%
  unnest(Livestock) %>%
  count(Region, Livestock) %>%
  pivot_wider(names_from = Region, values_from = n, values_fill = 0) %>%
  slice(c(1,2,7,3,4,5)) %>%
  relocate("National",.after = last_col()) %>%
  mutate(Total = rowSums(across(-Livestock, .names = "n_{.col}"))) %>%
  rbind(0) # add a row for other

prov_freq <- Live.prov[,2:13] %>%
    select(c(2,1,10,3,7,9,4,6,8,5,12)) %>%
    colSums()

#convert to percentage
Live.prov.p <- as.data.frame(round(proportions(as.matrix(Live.prov[,2:13]),2)*100,1))
Live.prov.p <- Live.prov.p %>%
  select(c(2,1,10,3,7,9,4,6,8,5,12))
colnames(Live.prov.p) <- colnames(N2O.p)
rownames(Live.prov.p) <- rownames(N2O.p)
#Put studies and N2O emissions together
Table1 <- rbind(Live.prov.p,N2O.p)
#Reorder to in descending order of national N2O
Table1 <- Table1 %>%
  slice(c(1,2,6,4,3,5,7,8,9,13,11,10,12,14))

#Do the Manny Whitney U test
for (i in 1:ncol(Table1)) {
  results <- wilcox.test(Table1[1:7,i],Table1[8:14,i])
  Table1[15,i] <- round(results$p.value,3)
}
#add the study frequency
Table1 <- rbind(prov_freq,Table1)
rownames(Table1) <- c("Study freq",rownames(Table1)[c(-1,-16)],"pvalue")

#Export file
write.csv(Table1,file = "output/Table1.csv",
          col.names = T, row.names = T)





#Figure 1
#2. Temporal analysis
my_colors <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#56B4E9", "#D55E00")

#study period
period_data <- GHG.field %>%
  separate_rows(Season, sep = ",\\s*") %>%
  group_by(Pub..year,Season) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)

Fig1a <- ggplot(data = na.omit(period_data), 
       aes(x = Pub..year, y = Number, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Publication number") +
  scale_fill_manual(values = my_colors) +
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
  scale_fill_manual(values = my_colors) +
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


Fig1c <- ggplot(Tech_data, aes(x = Pub..year, y = Number, fill = Technique)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Publication number") +
  scale_fill_manual(values = my_colors[1:5]) +
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

ggsave("output/Fig1.emf", 
       ggarrange(Fig1a, Fig1b, Fig1c,
                 ncol = 1, nrow = 3),
       width = 24, height = 36, units = "cm")

ggsave("output/Fig2.png", 
       ggarrange(Fig1a, Fig1b, Fig1c,
                 ncol = 1, nrow = 3),
       width = 24, height = 36, units = "cm",
       dpi = 300)







year_freq <- table(GHG.field$Pub..year)

# Year-Round vs. other, 53 studies, 53/141 =38%
GHG.year <- GHG.field[grep(("Year-Round"),GHG.field$Season),]



GHG.Dairy <- GHG.field[grep(("Dairy"),GHG.field$Livestock),] #69
GHG.Beef <- GHG.field[grep(("Beef"),GHG.field$Livestock),] #37
GHG.Swine <- GHG.field[grep(("Swine"),GHG.field$Livestock),] # 44
GHG.Poultry <- GHG.field[grep(("Poultry"),GHG.field$Livestock),] #12


#4. Manure types
GHG.Solid <- GHG.field[grep(("Solid"),GHG.field$Manure.type),] #87
GHG.Liquid <- GHG.field[grep(("Liquid"),GHG.field$Manure.type),] #94
GHG.both <- GHG.field[grep(("Liquid, Solid"),GHG.field$Manure.type),] #42



#GHG types
sum(GHG.field$CO2) #64
sum(GHG.field$CH4) #41
sum(GHG.field$N2O) #122
sum(GHG.field$NH3) #16

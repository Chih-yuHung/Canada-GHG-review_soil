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
GHG.field <- GHG[grepl("Field", GHG$GHG.source),]
GHG.field <- GHG.field[!grepl("^Grass$", GHG.field$Field.crop),]
GHG.field <- GHG.field[GHG.field$N2O==TRUE,]

#NIR N2O data
N2O <- read.csv("input/N2O emission_NIR.csv", header = TRUE) %>%
  slice(c(2, 1, 9, 10, 7, 4, 3, 5, 6, 8, 11:16)) %>%
  mutate(across(2:11, as.numeric)) %>%
  mutate(across(2:11, round, digits = 3)) 

N2O[17,2:11] <- colSums(N2O[c(7:16),2:11])
N2O[17,1] <- "Other"
N2O <- N2O[c(1:6,17),]
N2O$Total <- rowSums(N2O[,2:11])
N2O[8,2:12] <- colSums(N2O[,2:12])
N2O[8,1] <- "Total"


#convert to percentage
N2O.p <- as.data.frame(round(proportions(as.matrix(N2O[,2:11]),1)*100,1))
row.names(N2O.p) <- N2O[,1] 
N2O.p <- N2O.p %>%
  select(2,1,10,3,7,9,4,6,8,5)
#N2O.p$Total <- round(rowSums(N2O.p[,1:10]),0)


#province frequency
Prov <- unlist(strsplit(as.character(GHG.field$Region), ", "))
prov_freq <- table(Prov)
prov_freq[12] <- sum(prov_freq)
prov_freq <- prov_freq[c(2,1,11,3,8,10,5,7,9,6,12)]

#Livestock types
Livestock <- unlist(strsplit(as.character(GHG.field$Livestock), ", "))
live_freq <- table(Livestock)
live_freq <- c(live_freq[c(1,2,6,4,3,5)],0,sum(live_freq))
         
#Beef 28, Dairy 56, Horse 5, poultry 11, sheep 5. tot:138 

Live.prov <- GHG.field %>%
  separate_rows(Region, sep = ", ") %>%
  mutate(Livestock = str_split(Livestock, ",\\s*")) %>%
  unnest(Livestock) %>%
  count(Region, Livestock) %>%
  pivot_wider(names_from = Region, values_from = n, values_fill = 0) %>%
  slice(c(1,2,7,3,4,5)) %>%
  relocate("National",.after = last_col()) %>%
  mutate(Total = rowSums(across(-Livestock, .names = "n_{.col}"))) %>%
  # add a row for other, no other in my study
  rbind(0)  %>%
  {.[7, 1] <- "Other"; .} %>%
  add_row(Livestock = "Total", !!!colSums(.[2:13]))


#convert to percentage
Live.prov.p <- as.data.frame(round(proportions(as.matrix(Live.prov[1:8,2:11]),1)*100,1))
Live.prov.p <- Live.prov.p %>%
  select(c(2,1,10,3,7,9,4,6,8,5)) %>%
  slice(c(1,2,6,4,3,5,7,8))
Live.prov.p[7,] <- 0 
colnames(Live.prov.p) <- colnames(N2O.p)[1:10]
rownames(Live.prov.p) <- row.names(N2O.p)

Table1 <- rbind(Live.prov.p,N2O.p)

#Do the Manny Whitney U test
#Table1 <- rbind(Table1,rep(0,8))
# for (i in 1:nrow(N2O.p)) {
#   results <- wilcox.test(unlist(N2O.p[i,1:10]),
#                          unlist(Live.prov.p[i,1:10]),
#                          paired = TRUE)
#   Table1[21,i] <- round(results$p.value,3)
# }
#add the study frequency

Table1 <- rbind(Table1,prov_freq)
rownames(Table1)[17] <- "Study freq"

#Export file
write.csv(Table1,file = "output/Table1.csv",
          col.names = T, row.names = T)


#Soil texture data
texture_count <- GHG.field %>%
  filter(Texture != "NA") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Soil chamber")) %>%
  separate_rows(Region, sep = ", ") %>%
  separate_rows(Texture, sep= ", ") %>%
  group_by(Region, Texture) %>%
  count()


#Application 

GHG.application <- GHG.field %>%
  filter(Application.time != "NA") %>%
  filter(str_detect(Technique, "Micrometeorology")|
          str_detect(Technique, "Soil chamber")) %>%
  separate_rows(Manure.type, sep = ", ") %>%
  separate_rows(Season, sep = ", ") %>%
  separate_rows(Application.time, sep = ", ") %>%
  group_by(Application.time, Season, Manure.type) %>%
  summarise(Number = n()) %>%
  ungroup()





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

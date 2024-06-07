#This script is to analyze the data for GHG emissions from manure-applied field
library(tidyverse);library(ggplot2)
library(reshape2);library(scatterpie)
library(stringr);library(ggpubr)
library(openxlsx2)
#read data
GHG <- read.csv("input/data of ghg emission.csv",header = T)

#obtain studies with field measurement 
GHG.field <- GHG[grepl("Field", GHG$GHG.source),]
GHG.field <- GHG.field[!grepl("^Grass$", GHG.field$Field.crop),]
GHG.field <- GHG.field[GHG.field$N2O==TRUE,]
#exclude National studies
GHG.field <- GHG.field[GHG.field$Region!="National",]
GHG.field <- GHG.field %>%
             drop_na(Livestock)
#replace "National" to every provinces
#abandoned becasue national studies used modelling or other method only
#This substantially increased the study count in some provinces
#National<-paste(unique(GHG.field$Region)[c(-1,-3)], collapse = ", ")
#GHG.field$Region[GHG.field$Region == "National"] <- National


#NIR N2O data, convert to kT CO2e
N2O <- read.csv("input/N2O emission_NIR.csv", header = TRUE) %>%
  slice(c(2, 1, 9, 10, 7, 4, 3, 5, 6, 8, 11:16)) %>%
  mutate(across(2:11, as.numeric)) %>%
  mutate(across(2:11, ~.* 1000)) %>%
  mutate(across(2:11, round, digits = 1)) 

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

#change the province sequence for N2O emissions, must do this after obtaining percentage
N2O <- N2O %>%
  select(3,2,11,4,8,10,5,7,9,6,12)


#province frequency
Prov <- unlist(strsplit(as.character(GHG.field$Region), ", "))
prov_freq <- table(Prov)
prov_freq[11] <- sum(prov_freq)
prov_freq <- prov_freq[c(2,1,10,3,7,9,4,6,8,5,11)]

#Livestock types
Livestock <- unlist(strsplit(as.character(GHG.field$Livestock), ", "))
live_freq <- table(Livestock)
live_freq <- c(live_freq[c(1,2,6,4,3,5)],0,sum(live_freq)) 
#Beef 29, Dairy 55, swine 36, Horse 3, poultry 8, sheep 4, tot:135 (one NAs)

Live.prov <- GHG.field %>%
  separate_rows(Region, sep = ", ") %>%
  mutate(Livestock = str_split(Livestock, ",\\s*")) %>%
  unnest(Livestock) %>%
  count(Region, Livestock) %>%
  pivot_wider(names_from = Region, values_from = n, values_fill = 0) %>%
  slice(c(1,2,5,3,4,6)) %>%
  mutate(Total = rowSums(across(-Livestock, .names = "n_{.col}"))) %>%
  # add a row for other, no other in my study
  {.[7, 1] <- "Other"; .} %>%
  {.[7, 2:12] <- 0; .} %>%
  add_row(Livestock = "Total", !!!colSums(.[2:12]))


#convert to percentage
Live.prov.p <- as.data.frame(round(proportions(as.matrix(Live.prov[1:8,2:11]),1)*100,1))
rownames(Live.prov.p) <- row.names(N2O.p)
Live.prov.p[7,] <- 0 
Live.prov.p <- Live.prov.p %>%
  select(c(2,1,10,3,7,9,4,6,8,5))
colnames(Live.prov.p) <- colnames(N2O.p)[1:10]

#Do this after obtaining percentage
Live.prov <- Live.prov %>%
  select(c(3,2,11,4,8,10,5,7,9,6,12))

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


#Export file
write_xlsx(list(Table1=Table1,N2O = N2O,study = Live.prov),
           file = "output/Table1_N2O only_20240427.xlsx",
           col.names = T, row.names = T)














# 
# # Year-Round vs. other, 53 studies, 53/141 =38%
# GHG.year <- GHG.field[grep(("Year-Round"),GHG.field$Season),]
# GHG.Dairy <- GHG.field[grep(("Dairy"),GHG.field$Livestock),] #69
# GHG.Beef <- GHG.field[grep(("Beef"),GHG.field$Livestock),] #37
# GHG.Swine <- GHG.field[grep(("Swine"),GHG.field$Livestock),] # 44
# GHG.Poultry <- GHG.field[grep(("Poultry"),GHG.field$Livestock),] #12
# 
# 
# #4. Manure types
# GHG.Solid <- GHG.field[grep(("Solid"),GHG.field$Manure.type),] #87
# GHG.Liquid <- GHG.field[grep(("Liquid"),GHG.field$Manure.type),] #94
# GHG.both <- GHG.field[grep(("Liquid, Solid"),GHG.field$Manure.type),] #42
# 
# 

# 
# #To know how many studies quantify the contirbution of methane and CO2 for year-round emissions
# GHG.CH4yearround <- GHG.field %>%
#                     filter(str_detect(Season, "Year-Round")&
#                     str_detect(CH4, "TRUE") &
#                     str_detect(CO2, "TRUE") ) #Only 193 Ellert had measurements

#for the table of all studies used for analysis
library(tidyverse)
#read data
#guess_encoding("input/data of ghg emission.csv")
GHG <- read.csv("input/data of ghg emission.csv",header = T, 
                fileEncoding = "ISO-8859-2") #for French words

#obtain studies with field measurement 
GHG.field <- GHG[grepl("Field", GHG$GHG.source),]
GHG.field <- GHG.field[!grepl("^Grass$", GHG.field$Field.crop),]
GHG.field <- GHG.field[GHG.field$N2O == TRUE,]

#out table prepare
GHG.table <- GHG.field %>%
  select(Author = First.author, Year = Pub..year, Region, Method = Technique, Livestock, CO2, CH4, N2O)

#put the pub year with authors
GHG.table$Authors <- paste(GHG.table$Author, "et al.,"
                           , GHG.table$Year)
GHG.table <- GHG.table %>%
  select(-Author, Year) %>%
  select(Authors, Region, Livestock, Method, CO2, CH4, N2O, Year) #keep the year so I can use it to reorder


#Add the GHG types
GHG.table <- GHG.table %>%
  mutate(
    GHG = case_when(
      CO2 & !CH4 & !N2O ~ "CO₂",
      !CO2 & CH4 & !N2O ~ "CH₄",
      !CO2 & !CH4 & N2O ~ "N₂O",
      CO2 & !CH4 & N2O ~ "CO₂, N₂O",
      CO2 & CH4 & !N2O ~ "CO₂, CH₄",
      !CO2 & CH4 & N2O ~ "CH₄, N₂O",
      CO2 & CH4 & N2O ~ "CO₂, CH₄, N₂O",
      TRUE ~ "None"
    )
  )

GHG.table <- GHG.table %>%
  select(-CO2, -CH4, -N2O)

#Replace the Method
#Create a table for conversion
method_abbreviations <- c("Mixed" = "MX",
                "Incubation" = "OB",
                "Modelling" = "MD",
                "Soil chamber" = "OB",
                "Micrometeorology" = "OB")

#a function to replace the methods.
replace_methods <- function(method) {
    method <- strsplit(method, ", ")[[1]]
    abbreviations <- sapply(method, function(x) method_abbreviations[x])
    return(paste(abbreviations, collapse = ", "))
  }
# Apply the function to the method column in your dataframe
GHG.table$Method <- sapply(GHG.table$Method, replace_methods)


#Replace the province names
# Create a mapping between province names and their abbreviations
province_abbreviations <- c("Alberta" = "AB",
                            "British Columbia" = "BC",
                            "Manitoba" = "MB",
                            "New Brunswick" = "NB",
                            "Newfoundland and Labrador" = "NL",
                            "Nova Scotia" = "NS",
                            "Ontario" = "ON",
                            "Prince Edward Island" = "PE",
                            "Quebec" = "QC",
                            "Saskatchewan" = "SK",
                            "Northwest Territories" = "NT",
                            "Nunavut" = "NU",
                            "Yukon" = "YT")

# Function to replace province names with abbreviations
replace_province <- function(region) {
  if (region == "National") {
    return("Multiple")
  } else {
    provinces <- strsplit(region, ", ")[[1]]
    abbreviations <- sapply(provinces, function(x) province_abbreviations[x])
    return(paste(abbreviations, collapse = ", "))
  }
}

# Apply the function to the Region column in your dataframe
GHG.table$Region <- sapply(GHG.table$Region, replace_province)

GHG.table <- GHG.table %>%
  arrange(Region, Year) %>%
  select(-Year)

#Change the name
names(GHG.table) <- c("Authors and year", "Regions", "Animal types","Methods", "GHG types")

#These are the articles with only 1 or 2 authors. 
# Bhatt and Abassi, 2022
# Ejack and Whalen, 2021
# Ellert and Janzen, 2008
# Hao, 2015
# Hung and Whalen., 2021
# Lin and Hernandez-Remirez, 2020
# Pokharel and Chang, 2021
# Thomas and Hao, 2017
# Wagner-Riddle and Thurtell, 1998
# Zhu and Chang, 2018


#Export to csv file, but it won't show N2O, CH4, CO2 properly
#write.csv(GHG.table, "output/Table_all study for analysis_region group.csv")

#Use clip for the table
clipr::write_clip(GHG.table)

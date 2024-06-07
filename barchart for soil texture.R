#To create a figure for soil texture. 
library(tidyverse)
library(reshape2)
library(ggplot2)
#read data
GHG <- read.csv("input/data of ghg emission.csv",header = T)


#obtain studies with field measurement 
GHG.field <- GHG[grepl("Field", GHG$GHG.source),]
GHG.field <- GHG.field[!grepl("^Grass$", GHG.field$Field.crop),]
GHG.field <- GHG.field[GHG.field$N2O==TRUE,]
#replace "National" to every provinces
National<-paste(unique(GHG.field$Region)[c(-1,-3)], collapse = ", ")
GHG.field$Region[GHG.field$Region == "National"] <- National

#Soil texture data
texture_count <- GHG.field %>%
    filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Soil chamber")) %>%
  filter(Texture != "NA") %>% # 3 articles without texture data
  separate_rows(Region, sep = ", ") %>%
  separate_rows(Texture, sep= ", ") %>%
  group_by(Region, Texture) %>%
  count()

#obatin the total studies based on region
region_total <- texture_count %>%
  group_by(Region) %>%
  summarise(total = sum(n))

#merge the two tables
texture_count <- texture_count %>%
  left_join(region_total, by = "Region") %>%
  mutate(Percentage = n / total)

#Some summary data
sum(texture_count$n[texture_count$Texture=="Fine"]) #18
sum(texture_count$n[texture_count$Texture=="Med"]) #47
sum(texture_count$n[texture_count$Texture=="Coarse"]) #9


#read data from SLC
soiltexture <- read.csv("input/soil texture.csv",header = T)
soiltexture_long <- melt(soiltexture, id.vars = "Region")
soiltexture_long <- soiltexture_long %>% rename(Texture = variable)
#summaries the the Canada's soil texture data
soiltexture_total <- soiltexture_long %>%
  group_by(Region) %>%
  summarise(total = sum(value))

#merge the two tables
soiltexture_long <- soiltexture_long %>%
  left_join(soiltexture_total, by = "Region") %>%
  mutate(Percentage = value / total)

#make the soil texture values plotable by dividing 10^6
soiltexture_long$n <- soiltexture_long$value/10^6

#keep the data I want
soiltexture_long <- soiltexture_long[,(-3:-4)]
texture_count <- texture_count[,-4]

#total coarse and fine texture area 
sum(soiltexture_long$n[soiltexture_long$Texture=="Fine"]) #8.6
sum(soiltexture_long$n[soiltexture_long$Texture=="Coarse"]) # 7.8

#add a cloumn 
soiltexture_long$Source <- "SLC"
texture_count$Source <- "Study"

plottexture <- rbind(soiltexture_long,texture_count)

#replace the NA values
plottexture <- plottexture %>% 
               replace_na(list(n = 0, Percentage = 0))

province_lookup <- data.frame(
  Full_Name = c("British Columbia", "Alberta","Saskatchewan", "Manitoba",
                "Ontario", "Quebec", "New Brunswick", "Nova Scotia",
                "Prince Edward Island", "Newfounland"),
  Abbreviation = c("BC", "AB", "SK","MB",
                   "ON", "QC", "NB","NS",
                   "PE","NL"))

# replace values in the Region column with abbreviations
plottexture <- plottexture %>%
  mutate(Region = recode(Region, !!!setNames(province_lookup$Abbreviation, province_lookup$Full_Name)))


#make sure the order of provinces from west to east
plottexture$Region <- factor(plottexture$Region,
                               levels = c("BC", "AB", "SK","MB",
                                          "ON", "QC", "NB","NS",
                                          "PE","NL"))
#make sure the texture from coarse to fine
plottexture$Texture <- factor(plottexture$Texture,
                             levels = c("Coarse", "Med",
                                        "Fine"))
#make sure the study results at the left side
plottexture$Source <- factor(plottexture$Source,
                              levels = c("Study", "SLC"))



#The color in this study
warm_colors <- c("#999999", "#009E73", "#0072B2", "#56B4E9")
#Save the figure
png(file = "output/soil texture.png",
    width = 4800, height = 3600, res = 600)
ggplot(plottexture) +
  geom_bar(aes(x = Source, y = n, fill = Texture),
           position = "stack",
           stat = "identity") +
  theme_classic()+
  scale_fill_manual(values = c("Fine" = warm_colors[1],
                               "Med" = warm_colors[2],
                               "Coarse" = warm_colors[3]))+
  facet_grid(~ Region, switch = "x")+
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = "white"),
        panel.spacing = unit(-.01,"cm")) +
  theme(legend.position = c(0.85,0.85)) +
  scale_x_discrete(labels = c("SLC" = "A",
                              "Study" = "S")) +
  scale_y_continuous(name = "Study count",
      sec.axis = sec_axis( trans=~.*10^6, name="Area (ha)"))

dev.off()

  

#used to label percentage, not good looing, abandoned
# geom_text(aes(x = Source, y = n,
#               label = paste0(round(Percentage*100, 0), "%")),
#           position = position_stack(vjust = 0.8),
#           size = 2)
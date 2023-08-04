#To create Bubble chart to know the application time and spatial distribution
library(ggplot2)

#The color in this study
cold_colors <- c("#999999", "#009E73", "#0072B2", "#56B4E9")
warm_colors <- c("#D55E00", "#E69F00", "#F0E442", "#CC79A7")

#read data
GHG <- read.csv("input/data of ghg emission.csv",header = T)

#obtain studies with field measurement 
GHG.field <- GHG[grepl("Field", GHG$GHG.source),]
GHG.field <- GHG.field[!grepl("^Grass$", GHG.field$Field.crop),]
GHG.field <- GHG.field[GHG.field$N2O==TRUE,]
#replace "National" to every provinces
National<-paste(unique(GHG.field$Region)[c(-1,-3)], collapse = ", ")
GHG.field$Region[GHG.field$Region == "National"] <- National

#Application time
GHG.application <- GHG.field %>%
    filter(Application.time != "NA") %>% #Five studies didn't report application time
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Soil chamber")) %>%
  separate_rows(Region, sep =", ") %>%
  separate_rows(Manure.type, sep = ", ") %>%
  separate_rows(Season, sep = ", ") %>%
  separate_rows(Application.time, sep = ", ") %>%
  group_by( Application.time, Season, Manure.type) %>%
  summarise(Count = n()) %>%
  ungroup()


GHG.application$Region <- factor(GHG.application$Region,
                      levels = c("British Columbia", "Alberta",
                                 "Saskatchewan","Manitoba",
                                 "Ontario","Quebec",
                                 "New Brunswick",
                                 "Nova Scotia",
                                 "Prince Edward Island",
                                 "Newfounland"))

# Create bubble chart
png(file = "output/application time.png",
    width = 4800, height = 3600, res = 600)
ggplot(GHG.application, aes(x = Region, y = paste(Application.time, Season), color = Manure.type, size = Count)) +
  geom_point(alpha = 0.7) +
  labs(x = "Region", y = "Application Time/Season", color = "Manure Type", size = "Count") +
  scale_color_manual(values = c(cold_colors[2],warm_colors[2])) +
  theme_classic()+
  scale_x_discrete(labels = c("BC", "AB","SK","MB",
                              "ON", "QC","NB","NS",
                              "PE","NL"))+
  guides(color = guide_legend(override.aes = list(size = 10)))
dev.off()

#Export file
write_xlsx(GHG.application,
           file = "output/application time.xlsx",
           col.names = T, row.names = T)

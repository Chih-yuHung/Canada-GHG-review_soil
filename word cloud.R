library(pdftools)
library(tidyverse)
library(wordcloud)

#make a wordcloud
titles <- read.csv("input/Title and doi list.csv",header=T)
GHG <- read.csv("input/data of ghg emission.csv",header=T)
titles$GHG.soucre <- GHG$GHG.source
titles.field <- titles[grep("Field", titles$GHG.soucre), ]

#replace n2o and co2 to N2O and CO2
titles.field$Title <- gsub("n2o","N2O",
                           titles.field$Title,
                           ignore.case = TRUE)

wordcloud(titles.field$Title, min.freq = 5,
          colors = brewer.pal(8, "Dark2"),
          random.order = F)




#These was used to retrieve titles from the pdf files, 
#cannot retrieve title 100%
#retrieve titles from pdf files
art.dir <- "C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 8_GHG emission review/Article List/"
pdf_names <- list.files(art.dir, pattern = "*.pdf",recursive=TRUE)
pdf_list <- data.frame(number = c(1:length(pdf_names)),
                       names = pdf_names)
#obtain the number
for (i in 1:length(pdf_names)){
pdf_list[i,1] <- as.numeric(strsplit(pdf_names[i],"[.]")[[1]][1])
}
#reorder
pdf_list <- pdf_list[order(pdf_list$number),]

#the function to rertieve titles from pdf
pdftitle <- function(x){
  pdf_info(paste0(art.dir,x))$keys$Title
}

# a data frame to store titles
title <- data.frame(titles= as.character(c(1:nrow(pdf_list))))

for (i in 1:nrow(pdf_list)) {
  if (is.null(pdftitle(pdf_list$names[i]))) {
    title$titles[i] <- "untitled"
  } else {
    title$titles[i] <- pdftitle(pdf_list$names[i])
  }
}


#use str_length found titles are >50 characters.
title$titles <- ifelse(str_length(title$titles) > 50, title$titles, "untitled")
untitle <- which(title$titles == "untitled")

#retrieve the tiles from page 1
pdftitlep1 <- function(x){
  data = pdf_data(paste0(art.dir,x))
  #Get First page
  page_1 = data[[1]]
  # Get Title, here we assume it has the largest height
  title = page_1 %>%
  filter(height >= 13 & height < 20) %>%
  .$text %>%
    paste0(collapse = " ")
  print(title)
  }

for (i in untitle) {
   title$titles[i] <- pdftitlep1(pdf_list$names[i])
  }





# Render multiple lakes
# Daniel Nidzgorski
# November 13, 2017

library(tidyverse)
library(stringr)

lake.list<-c("Alice",
             "Allen",
             "Ames",
             "Angle",
             "Beaver-1",
             "Beaver-2",
             "Boren",
             "Cottage",
             "Desire",
             "Dolloff",
             "Echo-Shoreline",
             "Fivemile",
             "Forbes",
             "Geneva",
             "Green-1",
             "Joy",
             "Kathleen",
             "Killarney",
             "Lucerne",
             "Marcel",
             "Margaret",
             "McDonald",
             "Morton",
             "Neilson (Holm)",
             "Paradise",
             "Pine",
             "Pipe",
             "Retreat",
             "Sawyer",
             "Shadow",
             "Shady",
             "Spring",
             "Tuck",
             "Twelve",
             "Welcome",
             "Wilderness"
)

year<-2017

reports<-tibble(
  Lake=lake.list,
  output_file=sprintf("%s-%s.html",year,Lake),
  params=map(Lake,~list(lake=.,year=year,save=TRUE))
)

reports %>%
  select(output_file,params) %>%
  pwalk(rmarkdown::render,
        input="Lake-report-template.Rmd",
        output_format="html_document",
        output_dir=here::here(sprintf("%s/Outputs",year)),
        encoding="UTF-8")

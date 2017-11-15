# Render multiple lakes
# Daniel Nidzgorski
# November 13, 2017

library(tidyverse)
library(stringr)

lake.list<-c("Angle",
             "Boren",
             "Twelve",
             "Wilderness")

year<-2016

reports<-tibble(
  Lake=lake.list,
  output_file=sprintf("%s-%s.html",year,Lake),
  params=map(Lake,~list(lake=.,year=year))
)

reports %>%
  select(output_file,params) %>%
  pwalk(rmarkdown::render,
        input="Lake-report-template.Rmd",
        output_format="html_document",
        output_dir=here::here("Outputs"))

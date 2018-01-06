# Researching old lakes
# Daniel Nidzgorski
# January 5, 2018

library(tidyverse)
library(stringr)

lakes<-tribble(~Lake,~Year,
               "Bitter",2008,
               "Burien",2004,
               "Fenwick",2004,
               "Francis",2008,
               "Grass",2008,
               "Haller",2008,
               "Horseshoe",2008,
               "Langlois",2015,
               "Leota",2009,
               "Meridian",2004,
               "Mirror",2004,
               "Peterson Pond",2008,
               "Ravensdale",2003,
               "Star",2008,
               "Trout",2008,
               "Walker",2006)

# Removed: Jones, Panther


reports<-lakes %>%
  mutate(output_file=sprintf("%s-%s.html",Year,Lake),
         params=map2(Lake,Year,~list(lake=.x,year=.y,save=FALSE))
)

reports %>%
  select(output_file,params) %>%
  pwalk(rmarkdown::render,
        input="Lake-report-template.Rmd",
        output_format="html_document",
        output_dir=here::here("OldLakes"),
        encoding="UTF-8")

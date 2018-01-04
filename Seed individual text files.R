# Seed text files for lake reports
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

seed<-tribble( ~Section,
               "Description=\'\'\'",
               "",
               "\'\'\'",
               "Overview=\'\'\'",
               "",
               "\'\'\'",
               "WQ=\'\'\'",
               "",
               "\'\'\'",
               "Trends=\'\'\'",
               "",
               "\'\'\'",
               "TSI=\'\'\'",
               "",
               "\'\'\'"
)

textfiles<-map(lake.list,
                  function(x) {
                    sprintf("%s/Text/%s-%s.txt",year,year,x)
                  })
  
walk(textfiles,write_delim,
      x=seed,
      delim="$",
      col_names=F)

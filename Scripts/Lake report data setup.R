library(knitr)
library(tidyverse)
library(stringr)
library(lubridate)
library(Rgnuplot)
library(leaflet)
library(ggmap)
library(ggrepel)
library(trend)
library(scales)
library(kableExtra)
library(RcppTOML)
library(NADA)
library(EnvStats)
library(rkt)

source("Scripts/Multiplot function.R")
source("Scripts/Annual faceted plotting function.R")
source("Scripts/Hydrology plotting function.R")
source("Scripts/TSI plotting function.R")
source("Scripts/Seasonal trend analysis function.R")
source("Scripts/Data summary function.R")

# Get lake name

LakeName<-read_csv("lakelatlong.csv",
                   col_types=cols(
                     Lake = col_character(),
                     LakeName = col_character(),
                     Longitude = col_double(),
                     Latitude = col_double()
                   )) %>%
  filter(Lake==params$lake) %>%
  .$LakeName %>%
  str_to_title()


# Read in text chunks

# Set up a plain .txt file for each lake and year.
# I'm using TOML structure, where each section has the name, equals sign, and 
# the text is within sets of three *single* quotes:

# Overview='''
# Text goes here and can contain any characters and can be multiple paragraphs. 
#
# Leave blank lines between paragraphs or they'll be joined into a single paragraph.
#
# You can use Markdown syntax for formatting, like *italics* or **bold**.
#
# Bulleted lists:
# * Each item in the list
# * Starts with an asterisk and a space. 
#'''

# The order of text chunks doesn't matter, but their names do (must capitalize!).
# Currently used values are:
# Intro, Hydrology, WQ, Trends, TSI, TSImap, Alkalinity, UV

text.file<-sprintf("%s/Text/",params$year) %>%
  list.files() %>%
  {.[str_detect(.,params$lake)]}

if(length(text.file)>0) {
  text.input<-sprintf("%s/Text/%s",params$year,text.file) %>%
    parseTOML()
  
  t<-map(text.input,str_replace_all,"\\\\n","\n\n") %>%
    str_replace_all("Â","") %>% # parseTOML doesn't seem to read UTF-8 properly in Windows.
    str_trim() %>%
    as.list()
  names(t)<-names(text.input)
} else {t<-NULL}

# Use text chunks as inline text: `r if(!is.null(t$Intro)) t$Intro`

# Set up parameter labels and plotting maxima

# keepCap - if using text elsewhere than the beginning of label, keep capitals or convert to lower?
y<-tribble(
  ~Parameter, ~text, ~units, ~max, ~keepCap,
  "Secchi", "Secchi depth","m",10,T,
  "Temperature", "Water temperature","°C",30,F,
  "ChlorophyllA", "Chlorophyll-a","µg/L",40,F,
  "PheophytinA", "Pheophytin", "µg/L",40,F,
  "TotalNitrogen", "Total nitrogen","µg/L",1250,F,
  "NH3N", "Ammonia nitrogen","µg/L",1250,F,
  "NO23", "Nitrate/nitrite nitrogen","µg/L",1250,F,
  "TotalPhosphorus", "Total phosphorus","µg/L",100,F,
  "OPO4","Orthophosphate phosphorus","µg/L",100,F,
  "NPRatio", "N:P ratio","",100,T,
  "UV254", "UV254 absorbance","",1,T,
  'TotalAlk', "Total alkalinity","mg CaCO3",25,F
) %>%
  mutate(text=ordered(text,levels=str_c(text)),
         unitlabel=ifelse(units=="",units,sprintf("(%s)",units)),
         label=str_trim(sprintf("%s %s",text,unitlabel)),
         label=ordered(label,levels=str_c(label)),
         meanslabel=str_trim(sprintf("Average %s %s",
                                     ifelse(keepCap,paste(text),str_to_lower(text)),
                                     unitlabel)),
         meanslabel=ordered(meanslabel,levels=str_c(meanslabel))
  )


# Set up L2 data

# This looks for a "Small Lake Data" csv file in the directory
# from the web/SQL database

L2.file<-list.files() %>%
{Filter(x=.,function(d) str_detect(d,"Small Lake Data"))}
L2.input<-read_csv(L2.file,
                   col_types=cols(
                     .default = col_double(),
                     SiteName = col_character(),
                     CollectDate = col_datetime(format = ""),
                     DepthQ = col_character(),
                     SecchiQ = col_character(),
                     TempQ = col_character(),
                     ChloroQ = col_character(),
                     PheophQ = col_character(),
                     TotalNQ = col_character(),
                     NO23Q = col_character(),
                     NH3NQ = col_character(),
                     TotalPQ = col_character(),
                     OPO4Q = col_character(),
                     UV254Q = col_character(),
                     TotalAlkQ = col_character(),
                     AnatoxinQ = col_character(),
                     MicrocystinQ = col_character()
                   )
) %>%
  rename(Lake=SiteName,
         Date=CollectDate) %>%
  mutate(Date=as.Date(Date))

L2.data<-L2.input %>%
  gather(-Lake,-Date,-Depth,
         key=Parameter,value=Value) %>%
  filter(!str_detect(Parameter,"TSI"), # remove individual TSI values
         !str_detect(Parameter,"Q")) %>% # remove quals
  mutate(Value=as.numeric(Value)) %>%
  filter(!(Parameter %in% c("TotalPhosphorus","NPRatio") & Date<as.Date("1998-01-01")))
# remove TP and N:P before 1998 -- methods change

# Dataframe of L2 qualifiers -- used for profile data
L2.quals<-L2.input %>%
  gather(-Lake,-Date,-Depth,
         key=Parameter,value=Qual) %>%
  filter(str_detect(Parameter,"Q"),
         Parameter!="DepthQ") %>%
  mutate(BelowMDL=str_detect(Qual,"MDL"),
         BelowMDL=ifelse(is.na(BelowMDL),"FALSE",BelowMDL))

L2.quals$Parameter<-plyr::revalue(L2.quals$Parameter,
                                  c("SecchiQ" = "Secchi",
                                    "TempQ" = "Temperature",
                                    "ChloroQ" = "ChlorophyllA",
                                    "PheophQ" = "PheophytinA",
                                    "TotalNQ" = "TotalNitrogen",
                                    "NO23Q" = "NO23",
                                    "NH3NQ" = "NH3N",
                                    "TotalPQ" = "TotalPhosphorus",
                                    "OPO4Q" = "OPO4",
                                    "UV254Q" = "UV254",
                                    "TotalAlkQ" = "TotalAlk",
                                    "AnatoxinQ" = "Anatoxin",
                                    "MicrocystinQ" = "Microcystin"))


# Set up L1 weekly data

weekly.file<-list.files() %>%
{Filter(x=.,function(d) str_detect(d,"Small Lake Weekly Data"))}
weekly.input<-read_csv(weekly.file,
                       col_types=cols(
                         SiteName = col_character(),
                         CollectDate = col_datetime(format = ""),
                         Secchi = col_double(),
                         Temperature = col_double()
                       )
) %>% 
  rename(Lake=SiteName,
         Date=CollectDate) %>%
  mutate(Date=as.Date(Date))

# Needs to be in same Lake, Date, Depth, Parameter, Value format as L2 data for plotting function
weekly.data<-weekly.input %>%
  gather(-Lake,-Date,
         key=Parameter,value=Value) %>%
  mutate(Depth=1)

# Does this lake have weekly data
weekly.lakes<-weekly.data %>%
  filter(year(Date)==params$year,
         !is.na(Value)) %>%
  .$Lake %>%
  unique()

is.weekly<-params$lake %in% weekly.lakes

# Splice in L2 secchi/temp data to fill gaps
splice.L2.list<-tribble(
  ~Lake, ~Start, ~End, ~Parameter,
  "Ames","2017-06-26","2017-09-30","Secchi",
  "Ames","2017-06-26","2017-09-30","Temperature",
  "Echo-Shoreline","2017-05-01","2017-09-30","Secchi",
  "Green-1","2017-05-02","2017-09-30","Secchi",
  "Green-1","2017-05-02","2017-09-30","Temperature",
  "Shadow","2017-06-26","2017-09-30","Secchi",
  "Shadow","2017-06-26","2017-09-30","Temperature"
) %>%
  mutate(Start=as_date(Start),
         End=as_date(End)) %>%
  filter(Lake==params$lake)

if(nrow(splice.L2.list)>0) {
  splice.L2.data<-pmap_dfr(
    list(splice.L2.list$Lake,
         splice.L2.list$Start,
         splice.L2.list$End,
         splice.L2.list$Parameter),
    function(l,s,e,p) {
      filter(L2.data, 
             Lake==l,
             Date>=s,
             Date<=e,
             Parameter==p
      )}
  )
  
  # Filter out any L1 data in the spliced range
  # and bind in the L2 data
  weekly.data<-pmap_dfr(
    list(splice.L2.list$Lake,
         splice.L2.list$Start,
         splice.L2.list$End,
         splice.L2.list$Parameter),
    function(l,s,e,p) {
      filter(weekly.data, 
             Lake!=l |
               Date<s |
               Date>e |
               Parameter!=p
      )}
  ) %>%
    bind_rows(splice.L2.data)
}


# Set up L1 daily data

daily.file<-list.files() %>%
{Filter(x=.,function(d) str_detect(d,"Small Lake Daily Data"))}
daily.data<-read_csv(daily.file,
                     col_types=cols(
                       SiteName = col_character(),
                       CollectDate = col_datetime(format = ""),
                       Precipitation = col_double(),
                       Level = col_double(),
                       PrecipQ = col_character()
                     )) %>% 
  rename(Lake=SiteName,
         Date=CollectDate) %>%
  mutate(Date=as.Date(Date))
# doesn't need to be in gathered format

# Does this lake have daily data?
daily.lakes<-daily.data %>%
  filter(year(Date)==params$year,
         !is.na(Precipitation) | !is.na(Level)) %>%
  .$Lake %>%
  unique()

is.daily<-params$lake %in% daily.lakes

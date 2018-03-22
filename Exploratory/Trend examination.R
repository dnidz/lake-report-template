# Examine seasonal-Kendall trend results
# Daniel Nidzgorski
# November 22, 2017

# Assumes L2.data already set up from .Rmd

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

trends<-map_df(lake.list,lake.trend.seasonal,
               data=L2.data,
               year=2017,
               params.list="L2",
               overall.only=F)

# Find lake/parameter combos where one or more seasons are significant, but overall isn't
sig.season.only<-trends %>%
  filter(p<0.05,
         overall.p>=0.05) %>% 
  select(Lake,Parameter) %>%
  unique() %>%
  {anti_join(trends,.,by=c("Lake","Parameter"))} %>%
  select(Lake,Parameter) %>%
  unique()%>%
  {anti_join(trends,.,by=c("Lake","Parameter"))}

season.sig<-trends %>%
  filter(Month!="Overall",
         p<0.05)

sig.list<-season.sig %>%
  select(Lake,Parameter) %>%
  unique()

opposite<-map2_dfr(sig.list$Lake,sig.list$Parameter,function(x,y) {
  filter(season.sig,Lake==x,Parameter==y) %>%
    .$slope %>%
    {any(.>0) & any(.<0)} %>%
    {tibble(Lake=x,
            Parameter=y,
            Opposite=.)}
}) %>%
  filter(Opposite) %>%
  select(Lake,Parameter) %>%
  unique() %>%
  {anti_join(trends,.,by=c("Lake","Parameter"))} %>%
  select(Lake,Parameter) %>%
  unique()%>%
  {anti_join(trends,.,by=c("Lake","Parameter"))}
